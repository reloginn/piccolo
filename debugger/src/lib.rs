use piccolo::{
    compiler::LineNumber,
    opcode::{OpCode, Operation},
    thread::{vm::run_vm, Executor, Frame, LuaFrame},
    Context, Fuel, FunctionPrototype, String as LuaString, Value,
};
use std::mem::size_of;
use watch::{WatchEntry, WatchSpec};

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct PrototypeReference {
    chunk_name: String,
    prototype_index: usize,
}

pub use self::{
    breakpoints::{Breakpoint, Breakpoints},
    location::Location,
    stop_reason::StopReason,
    watch::WatchMode,
};

pub mod adapter;
pub mod breakpoints;
pub mod location;
pub mod stop_reason;
mod watch;

#[derive(Debug)]
pub struct Disassembled {
    symbol: &'static str,
    index: usize,
    line: usize,
    operation: Operation,
}

pub struct Debugger<'gc> {
    executor: Executor<'gc>,
    breakpoints: Breakpoints,
    watchpoints: Vec<WatchEntry<'gc>>,
}

impl<'gc> Debugger<'gc> {
    pub fn new(executor: Executor<'gc>) -> Self {
        Self {
            executor,
            breakpoints: Breakpoints::default(),
            watchpoints: Vec::new(),
        }
    }

    pub fn executor(&self) -> Executor<'gc> {
        self.executor
    }

    pub fn add_breakpoint(&mut self, chunk: &str, line: usize) -> usize {
        self.breakpoints.add(chunk, line)
    }

    pub fn remove_breakpoint(&mut self, chunk: &str, line: usize) {
        self.breakpoints.remove(chunk, line)
    }

    pub fn list_breakpoints(&self) -> impl Iterator<Item = (&str, Vec<usize>)> {
        self.breakpoints.list()
    }

    pub fn matches_breakpoint(&self, chunk: &str, line: usize) -> Vec<usize> {
        self.breakpoints.matches(chunk, line)
    }

    pub fn add_function_breakpoint(&mut self, ctx: Context<'gc>, name: &str) -> Option<usize> {
        self.resolve_function_breakpoint(ctx, name)
            .map(|(chunk, line)| self.add_breakpoint(&chunk, line))
    }

    pub fn resolve_function_breakpoint(
        &self,
        ctx: Context<'gc>,
        name: &str,
    ) -> Option<(String, usize)> {
        let env = ctx.globals();
        let mut segments = name.split('.');
        let first = segments.next()?;
        let mut current = env.get_value(ctx, ctx.intern(first.as_bytes()));
        for segment in segments {
            match current {
                Value::Table(t) => {
                    current = t.get_raw(Value::String(ctx.intern(segment.as_bytes())));
                }
                _ => return None,
            }
        }
        match current {
            Value::Function(piccolo::Function::Closure(closure)) => {
                let proto = closure.prototype();
                let chunk_name = lua_string_to_string(proto.chunk_name);
                let first_line = if proto.opcode_line_numbers.is_empty() {
                    0
                } else {
                    proto.opcode_line_numbers[0].1 .0 as usize
                };
                Some((chunk_name, first_line))
            }
            _ => None,
        }
    }

    pub fn continue_run(&mut self, ctx: Context<'gc>) -> StopReason {
        loop {
            match self.current_location(ctx) {
                Ok(Some(location)) => {
                    if let Some(stop_reason) = self.stop_if_breakpoint(&location) {
                        return stop_reason;
                    }
                }
                Ok(None) => {}
                Err(stop_reason) => return stop_reason,
            }
            match self.step_one(ctx) {
                StopReason::Step(_) => continue,
                other => return other,
            }
        }
    }

    pub fn step_into(&mut self, ctx: Context<'gc>) -> StopReason {
        let start_location = match self.current_location(ctx) {
            Ok(Some(location)) => location,
            Ok(None) => return StopReason::Finished,
            Err(stop_reason) => return stop_reason,
        };

        if let Some(stop_reason) = self.stop_if_breakpoint(&start_location) {
            return stop_reason;
        }

        loop {
            match self.step_one(ctx) {
                StopReason::Step(location) => {
                    if let Some(stop_reason) = self.stop_if_breakpoint(&location) {
                        return stop_reason;
                    }
                    if location.line() != start_location.line()
                        || location.chunk() != start_location.chunk()
                    {
                        return StopReason::Step(location);
                    }
                }
                StopReason::Watchpoint(hit) => return StopReason::Watchpoint(hit),
                other => return other,
            }
        }
    }

    pub fn step_over(&mut self, ctx: Context<'gc>) -> StopReason {
        let (start_depth, start_location) = match self.stack_depth_and_location(ctx) {
            Ok((depth, Some(location))) => (depth, location),
            Ok((_depth, None)) => return StopReason::Finished,
            Err(stop_reason) => return stop_reason,
        };

        if let Some(stop_reason) = self.stop_if_breakpoint(&start_location) {
            return stop_reason;
        }

        loop {
            match self.step_one(ctx) {
                StopReason::Step(location) => {
                    if let Some(stop_reason) = self.stop_if_breakpoint(&location) {
                        return stop_reason;
                    }
                    let depth = match self.current_stack_depth(ctx) {
                        Ok(depth) => depth,
                        Err(stop_reason) => return stop_reason,
                    };
                    if depth <= start_depth
                        && (location.line() != start_location.line()
                            || location.chunk() != start_location.chunk())
                    {
                        return StopReason::Step(location);
                    }
                }
                StopReason::Watchpoint(hit) => return StopReason::Watchpoint(hit),
                other => return other,
            }
        }
    }

    pub fn step_out(&mut self, ctx: Context<'gc>) -> StopReason {
        let start_depth = match self.current_stack_depth(ctx) {
            Ok(depth) => depth,
            Err(stop_reason) => return stop_reason,
        };

        loop {
            match self.step_one(ctx) {
                StopReason::Step(location) => {
                    if let Some(stop_reason) = self.stop_if_breakpoint(&location) {
                        return stop_reason;
                    }
                    let depth = match self.current_stack_depth(ctx) {
                        Ok(depth) => depth,
                        Err(stop_reason) => return stop_reason,
                    };
                    if depth < start_depth {
                        return StopReason::Step(location);
                    }
                }
                StopReason::Watchpoint(hit) => return StopReason::Watchpoint(hit),
                other => return other,
            }
        }
    }

    pub fn backtrace(&self, _ctx: Context<'gc>) -> Vec<Location> {
        let mut out = Vec::new();
        let _ = self.with_top_thread_state(|state| {
            out.reserve(state.frames.len());
            for frame in state.frames.iter() {
                if let Frame::Lua { closure, pc, .. } = frame {
                    let proto = closure.prototype();
                    let chunk_name = lua_string_to_string(proto.chunk_name);
                    let function_ref = proto.reference.map_strings(lua_string_to_string);
                    let line = opcode_index_to_line(&proto.opcode_line_numbers, *pc);
                    out.push(Location::new(chunk_name, function_ref, line));
                }
            }
        });
        out
    }

    pub fn read_register(&self, _ctx: Context<'gc>, register_index: usize) -> Option<Value<'gc>> {
        let mut value = None;
        let _ = self.with_top_thread_state(|state| {
            if let Some(Frame::Lua {
                base, stack_size, ..
            }) = state.frames.last()
            {
                let base = *base;
                let size = *stack_size;
                if register_index < size {
                    value = Some(state.stack[base + register_index]);
                }
            }
        });
        value
    }

    pub fn read_global(&self, ctx: Context<'gc>, name: &str) -> Value<'gc> {
        let key = ctx.intern(name.as_bytes());
        ctx.globals().get_value(ctx, key)
    }

    pub fn step_one(&mut self, ctx: Context<'gc>) -> StopReason {
        const FUEL_NON_LUA_CALLBACK_STEP: i32 = 11;
        const FUEL_NON_LUA_SEQUENCE_STEP: i32 = 7;
        const FUEL_NON_LUA_ERROR_STEP: i32 = 3;

        match self.executor.mode() {
            piccolo::thread::ExecutorMode::Stopped => return StopReason::Finished,
            piccolo::thread::ExecutorMode::Result => return StopReason::Finished,
            piccolo::thread::ExecutorMode::Suspended => return StopReason::Suspended,
            piccolo::thread::ExecutorMode::Running => return StopReason::Error,
            piccolo::thread::ExecutorMode::Normal => {}
        }

        let (is_lua_top, non_lua_fuel) =
            match self.with_top_thread_state(|state| match state.frames.last() {
                Some(Frame::Lua { .. }) => (true, 0),
                Some(Frame::Callback { .. }) => (false, FUEL_NON_LUA_CALLBACK_STEP),
                Some(Frame::Sequence { .. }) => (false, FUEL_NON_LUA_SEQUENCE_STEP),
                Some(Frame::Error(_)) => (false, FUEL_NON_LUA_ERROR_STEP),
                Some(Frame::Result { .. }) => (false, -1),
                Some(Frame::WaitThread) | Some(Frame::Yielded) | Some(Frame::Start(_)) => {
                    (false, 3)
                }
                None => (false, -1),
            }) {
                Ok(value) => value,
                Err(stop_reason) => return stop_reason,
            };

        if non_lua_fuel == -1 {
            return StopReason::Finished;
        }

        if is_lua_top {
            let _ = self.with_top_thread_state_mut(ctx, |thread, state| {
                let mut fuel = Fuel::with(1_000_000);
                let lua_frame = LuaFrame {
                    state,
                    thread,
                    fuel: &mut fuel,
                };
                let _ = run_vm(ctx, lua_frame, 1);
            });
        } else {
            let mut fuel = Fuel::with(non_lua_fuel);
            let _ = self.executor.step(ctx, &mut fuel);
        }

        let location = match self.current_location(ctx) {
            Ok(Some(location)) => location,
            Ok(None) => return StopReason::Finished,
            Err(stop_reason) => return stop_reason,
        };
        if let Some(hit) = self.poll_watchpoints(ctx) {
            return StopReason::Watchpoint(hit);
        }
        StopReason::Step(location)
    }

    fn current_stack_depth(&self, _ctx: Context<'gc>) -> Result<usize, StopReason> {
        self.with_top_thread_state(|state| state.frames.len())
    }

    fn stack_depth_and_location(
        &self,
        _ctx: Context<'gc>,
    ) -> Result<(usize, Option<Location>), StopReason> {
        self.with_top_thread_state(|state| {
            let depth = state.frames.len();
            let loc = current_location_from_state_in_thread(state);
            (depth, loc)
        })
    }

    fn current_location(&self, _ctx: Context<'gc>) -> Result<Option<Location>, StopReason> {
        self.with_top_thread_state(|state| current_location_from_state_in_thread(state))
    }

    fn with_top_thread_state<R>(
        &self,
        f: impl FnOnce(&piccolo::thread::ThreadState<'gc>) -> R,
    ) -> Result<R, StopReason> {
        let guard = self.executor.0.borrow();
        let thread = *guard.thread_stack.last().ok_or(StopReason::Finished)?;
        drop(guard);
        let thread_state = thread.into_inner().borrow();
        Ok(f(&thread_state))
    }

    fn with_top_thread_state_mut<R>(
        &self,
        ctx: Context<'gc>,
        f: impl FnOnce(piccolo::Thread<'gc>, &mut piccolo::thread::ThreadState<'gc>) -> R,
    ) -> Result<R, StopReason> {
        let guard = self.executor.0.borrow();
        let top_thread = *guard.thread_stack.last().ok_or(StopReason::Finished)?;
        drop(guard);
        let mut thread_state = top_thread.into_inner().borrow_mut(&ctx);
        let r = f(top_thread, &mut thread_state);
        drop(thread_state);
        Ok(r)
    }

    pub fn read_registers(&self, _ctx: Context<'gc>) -> Option<Vec<Value<'gc>>> {
        let mut out = None;
        let _ = self.with_top_thread_state(|state| {
            if let Some(Frame::Lua {
                base, stack_size, ..
            }) = state.frames.last()
            {
                let base = *base;
                let size = *stack_size;
                out = Some(state.stack[base..base + size].to_vec());
            }
        });
        out
    }

    pub fn read_upvalues(&self, _ctx: Context<'gc>) -> Option<Vec<Value<'gc>>> {
        let mut out = None;
        let _ = self.with_top_thread_state(|state| {
            if let Some(Frame::Lua { closure, .. }) = state.frames.last() {
                let mut values = Vec::new();
                for &upvalue in closure.upvalues() {
                    if let piccolo::closure::UpValueState::Closed(value) = upvalue.get() {
                        values.push(value);
                    } else {
                        values.push(Value::Nil);
                    }
                }
                out = Some(values);
            }
        });
        out
    }

    pub fn stack_snapshot(&self, _ctx: Context<'gc>) -> Vec<(Location, Vec<Value<'gc>>)> {
        let mut out = Vec::new();
        let _ = self.with_top_thread_state(|state| {
            out.reserve(state.frames.len());
            for frame in state.frames.iter() {
                if let Frame::Lua {
                    base,
                    stack_size,
                    closure,
                    pc,
                    ..
                } = frame
                {
                    let proto = closure.prototype();
                    let chunk_name = lua_string_to_string(proto.chunk_name);
                    let function_ref = proto.reference.map_strings(lua_string_to_string);
                    let line = opcode_index_to_line(&proto.opcode_line_numbers, *pc);
                    let location = Location::new(chunk_name, function_ref, line);
                    let base = *base;
                    let size = *stack_size;
                    out.push((location, state.stack[base..base + size].to_vec()));
                }
            }
        });
        out
    }

    pub fn disassemble(
        &self,
        memory_reference: &str,
        instruction_offset: Option<usize>,
        instruction_count: Option<usize>,
    ) -> Option<Vec<Disassembled>> {
        let (ty, s) = memory_reference.split_once(':').unwrap_or(("frame", ""));
        match ty {
            "frame" => self.disassemble_frame(
                if s.is_empty() {
                    usize::MAX
                } else {
                    s.parse().ok()?
                },
                instruction_offset,
                instruction_count,
            ),
            "prototype" => self.disassemble_prototype(s, instruction_offset, instruction_count),
            _ => None,
        }
    }

    fn disassemble_frame(
        &self,
        frame: usize,
        instruction_offset: Option<usize>,
        instruction_count: Option<usize>,
    ) -> Option<Vec<Disassembled>> {
        self.with_top_thread_state(|state| {
            if let Some(Frame::Lua { closure, pc, .. }) = if frame == usize::MAX {
                state.frames.last()
            } else {
                state.frames.get(frame)
            } {
                let prototype = closure.prototype();
                let current_pc = *pc;
                Some(self.disassemble_prototype_opcodes(
                    &prototype,
                    Some(current_pc),
                    instruction_offset,
                    instruction_count,
                ))
            } else {
                None
            }
        })
        .ok()?
    }

    fn disassemble_prototype(
        &self,
        prototype: &str,
        instruction_offset: Option<usize>,
        instruction_count: Option<usize>,
    ) -> Option<Vec<Disassembled>> {
        let (chunk, index) = prototype.split_once(':')?;

        let index: usize = index.parse().ok()?;

        self.with_top_thread_state(|state| {
            for frame in state.frames.iter() {
                if let Frame::Lua { closure, .. } = frame {
                    let proto = closure.prototype();
                    let frame_chunk = lua_string_to_string(proto.chunk_name);

                    if frame_chunk == chunk {
                        if let Some(nested_proto) = proto.prototypes.get(index) {
                            return Some(self.disassemble_prototype_opcodes(
                                nested_proto,
                                None,
                                instruction_offset,
                                instruction_count,
                            ));
                        }
                    }
                }
            }
            None
        })
        .ok()?
    }

    fn disassemble_prototype_opcodes(
        &self,
        prototype: &FunctionPrototype,
        current_pc: Option<usize>,
        instruction_offset: Option<usize>,
        instruction_count: Option<usize>,
    ) -> Vec<Disassembled> {
        let start_offset = instruction_offset.unwrap_or(0);
        let max_count = instruction_count.unwrap_or(prototype.opcodes.len());

        let end_offset = std::cmp::min(start_offset + max_count, prototype.opcodes.len());

        let mut disassembled = Vec::with_capacity(end_offset - start_offset);

        for index in start_offset..end_offset {
            if let Some(opcode) = prototype.opcodes.get(index) {
                let operation = (*opcode).decode();
                let line = opcode_index_to_line(&prototype.opcode_line_numbers, index) + 1;
                let symbol = if Some(index) == current_pc {
                    "=>"
                } else {
                    "  "
                };
                disassembled.push(Disassembled {
                    symbol,
                    index,
                    line,
                    operation,
                });
            }
        }

        disassembled
    }

    pub fn read_memory(
        &self,
        memory_reference: &str,
        byte_offset: Option<usize>,
        byte_count: Option<usize>,
    ) -> Option<Vec<u8>> {
        let (ty, s) = memory_reference.split_once(':').unwrap_or(("frame", ""));

        match ty {
            "frame" => self.read_frame_memory(
                if s.is_empty() {
                    usize::MAX
                } else {
                    s.parse().ok()?
                },
                byte_offset,
                byte_count,
            ),
            "prototype" => self.read_prototype_memory(s, byte_offset, byte_count),
            _ => None,
        }
    }

    fn read_frame_memory(
        &self,
        frame: usize,
        byte_offset: Option<usize>,
        byte_count: Option<usize>,
    ) -> Option<Vec<u8>> {
        self.with_top_thread_state(|state| {
            if let Some(piccolo::thread::Frame::Lua { closure, .. }) = if frame == usize::MAX {
                state.frames.last()
            } else {
                state.frames.get(frame)
            } {
                let prototype = closure.prototype();
                Some(self.read_opcodes(&prototype.opcodes, byte_offset, byte_count))
            } else {
                None
            }
        })
        .ok()?
    }

    fn read_prototype_memory(
        &self,
        prototype: &str,
        byte_offset: Option<usize>,
        byte_count: Option<usize>,
    ) -> Option<Vec<u8>> {
        let (chunk, index) = prototype.split_once(':')?;

        let index: usize = index.parse().ok()?;

        self.with_top_thread_state(|state| {
            for frame in state.frames.iter() {
                if let Frame::Lua { closure, .. } = frame {
                    let proto = closure.prototype();
                    let frame_chunk = lua_string_to_string(proto.chunk_name);

                    if frame_chunk == chunk {
                        if let Some(nested_proto) = proto.prototypes.get(index) {
                            return Some(
                                (self.read_opcodes(&nested_proto.opcodes, byte_offset, byte_count)),
                            );
                        }
                    }
                }
            }
            None
        })
        .ok()?
    }

    fn read_opcodes(
        &self,
        opcodes: &[OpCode],
        byte_offset: Option<usize>,
        byte_count: Option<usize>,
    ) -> Vec<u8> {
        fn to_bytes<T: Copy>(value: T) -> Vec<u8> {
            let mut bytes = Vec::with_capacity(size_of::<T>());

            // SAFETY: We are copying the bytes of a plain-old-data (POD) type T into a Vec<u8> with enough capacity.
            // The Vec is pre-allocated with the correct size, and we set the length after copying.
            unsafe {
                std::ptr::copy_nonoverlapping(
                    std::ptr::addr_of!(value).cast::<u8>(),
                    bytes.as_mut_ptr(),
                    size_of::<T>(),
                );
                bytes.set_len(size_of::<T>());
            }

            bytes
        }

        let mut raw = Vec::new();

        for opcode in opcodes {
            let bytes = to_bytes(*opcode);
            raw.extend_from_slice(&bytes);
        }

        let byte_offset = byte_offset.unwrap_or(0);
        let byte_count = byte_count.unwrap_or(raw.len());

        let start = std::cmp::min(byte_offset, raw.len());
        let end = std::cmp::min(start + byte_count, raw.len());

        raw[start..end].to_vec()
    }

    pub fn watch_register(&mut self, register: usize) {
        self.watchpoints.push(WatchEntry {
            spec: WatchSpec::Register(register),
            last: None,
            mode: WatchMode::Modify,
            last_line_seen: None,
        })
    }

    pub fn watch_global(&mut self, name: impl Into<String>) {
        self.watchpoints.push(WatchEntry {
            spec: WatchSpec::Global(name.into()),
            last: None,
            mode: WatchMode::Modify,
            last_line_seen: None,
        })
    }

    pub fn watch_table_key(&mut self, table: piccolo::Table<'gc>, key: Value<'gc>) {
        self.watchpoints.push(WatchEntry {
            spec: WatchSpec::TableKey(table, key),
            last: None,
            mode: WatchMode::Modify,
            last_line_seen: None,
        })
    }

    pub fn watch_register_with_mode(&mut self, register: usize, mode: WatchMode) {
        self.watchpoints.push(WatchEntry {
            spec: WatchSpec::Register(register),
            last: None,
            mode,
            last_line_seen: None,
        })
    }

    pub fn watch_global_with_mode(&mut self, name: impl Into<String>, mode: WatchMode) {
        self.watchpoints.push(WatchEntry {
            spec: WatchSpec::Global(name.into()),
            last: None,
            mode,
            last_line_seen: None,
        })
    }

    pub fn watch_table_key_with_mode(
        &mut self,
        table: piccolo::Table<'gc>,
        key: Value<'gc>,
        mode: WatchMode,
    ) {
        self.watchpoints.push(WatchEntry {
            spec: WatchSpec::TableKey(table, key),
            last: None,
            mode,
            last_line_seen: None,
        })
    }

    pub fn clear_watchpoints(&mut self) {
        self.watchpoints.clear();
    }

    fn poll_watchpoints(&mut self, ctx: Context<'gc>) -> Option<String> {
        for index in 0..self.watchpoints.len() {
            let Some(entry) = self.watchpoints.get(index) else {
                continue;
            };
            let spec = match &entry.spec {
                WatchSpec::Register(i) => WatchSpec::Register(*i),
                WatchSpec::Global(n) => WatchSpec::Global(n.clone()),
                WatchSpec::TableKey(t, k) => WatchSpec::TableKey(*t, *k),
            };
            let last = entry.last;

            let current = self.eval_watch(ctx, &spec);
            let should_fire = match entry.mode {
                WatchMode::Modify => match (last, current) {
                    (None, Some(_)) => true,
                    (Some(_), None) => true,
                    (Some(left), Some(right)) => !values_equal(left, right),
                    (None, None) => false,
                },
                WatchMode::Access => {
                    if current.is_some() {
                        let current_line = self
                            .current_location(ctx)
                            .ok()
                            .flatten()
                            .map(|l| (l.chunk().to_owned(), l.line()));
                        if let (Some((chunk, line)), Some(entry)) =
                            (current_line, self.watchpoints.get(index))
                        {
                            if entry.last_line_seen != Some((chunk.to_owned(), line)) {
                                if let Some(entry) = self.watchpoints.get_mut(index) {
                                    entry.last_line_seen = Some((chunk.to_owned(), line));
                                }
                                true
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
            };
            if should_fire {
                let message = format!(
                    "watchpoint hit: {}",
                    self.describe_watch_change(&spec, last, current)
                );
                if let Some(entry) = self.watchpoints.get_mut(index) {
                    entry.last = current;
                }
                return Some(message);
            }
            if let Some(entry) = self.watchpoints.get_mut(index) {
                entry.last = current;
            }
        }
        None
    }

    fn eval_watch(&self, ctx: Context<'gc>, spec: &WatchSpec<'gc>) -> Option<Value<'gc>> {
        match spec {
            WatchSpec::Register(index) => {
                let mut out = None;
                let _ = self.with_top_thread_state(|state| {
                    if let Some(Frame::Lua {
                        base, stack_size, ..
                    }) = state.frames.last()
                    {
                        let index = *index;
                        let base = *base;
                        let size = *stack_size;
                        if index < size {
                            out = Some(state.stack[base + index]);
                        }
                    }
                });
                out
            }
            WatchSpec::Global(name) => {
                let key = ctx.intern(name.as_bytes());
                Some(ctx.globals().get_value(ctx, key))
            }
            WatchSpec::TableKey(table, key) => Some(table.get_raw(*key)),
        }
    }

    fn describe_watch_change(
        &self,
        spec: &WatchSpec<'gc>,
        old: Option<Value<'gc>>,
        new: Option<Value<'gc>>,
    ) -> String {
        let spec = match spec {
            WatchSpec::Register(index) => format!("register[{}]", index),
            WatchSpec::Global(name) => format!("global['{}']", name),
            WatchSpec::TableKey(t, k) => format!(
                "table({:p})[{}]",
                piccolo::table::Table::into_inner(*t).as_ptr(),
                value_to_string(*k)
            ),
        };
        format!(
            "{}: {} -> {}",
            spec,
            old.map(|value| value_to_string(value))
                .unwrap_or("<none>".into()),
            new.map(|value| value_to_string(value))
                .unwrap_or("<none>".into())
        )
    }

    fn stop_if_breakpoint(&self, location: &Location) -> Option<StopReason> {
        let breakpoint_ids = self.matches_breakpoint(location.chunk(), location.line());
        if breakpoint_ids.is_empty() {
            None
        } else {
            Some(StopReason::Breakpoint {
                location: location.to_owned(),
                breakpoint_ids,
            })
        }
    }
}

fn current_location_from_state_in_thread<'gc>(
    thread_state: &piccolo::thread::ThreadState<'gc>,
) -> Option<Location> {
    match thread_state.frames.last() {
        Some(Frame::Lua { closure, pc, .. }) => {
            let proto = closure.prototype();
            let chunk_name = lua_string_to_string(proto.chunk_name);
            let line = opcode_index_to_line(&proto.opcode_line_numbers, *pc);
            Some(Location::new(
                chunk_name,
                proto.reference.map_strings(lua_string_to_string),
                line,
            ))
        }
        _ => None,
    }
}

fn opcode_index_to_line(op_lines: &[(usize, LineNumber)], pc: usize) -> usize {
    if op_lines.is_empty() {
        return 0;
    }
    match op_lines.binary_search_by_key(&pc, |(opi, _)| *opi) {
        Ok(i) => op_lines[i].1 .0 as usize,
        Err(0) => op_lines[0].1 .0 as usize,
        Err(i) => op_lines[i - 1].1 .0 as usize,
    }
}

fn values_equal<'gc>(left: Value<'gc>, right: Value<'gc>) -> bool {
    use Value::*;
    match (left, right) {
        (Nil, Nil) => true,
        (Boolean(x), Boolean(y)) => x == y,
        (Integer(x), Integer(y)) => x == y,
        (Number(x), Number(y)) => x.to_bits() == y.to_bits(),
        (String(x), String(y)) => x == y,
        (Table(x), Table(y)) => x == y,
        (Function(x), Function(y)) => x == y,
        (Thread(x), Thread(y)) => x == y,
        (UserData(x), UserData(y)) => x == y,
        _ => false,
    }
}

fn value_to_string<'gc>(value: Value<'gc>) -> String {
    format!("{}", value.display())
}

fn lua_string_to_string(s: LuaString<'_>) -> String {
    format!("{}", s.display_lossy())
}
