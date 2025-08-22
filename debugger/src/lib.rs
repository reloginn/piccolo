use piccolo::{
    compiler::{FunctionRef, LineNumber}, opcode::{OpCode, Operation}, thread::{vm::run_vm, Executor, Frame, LuaFrame}, Closure, Context, Fuel, FunctionPrototype, Lua, String as LuaString, Value
};
use std::{collections::HashMap, mem::size_of, sync::{atomic::AtomicPtr, mpsc::{Receiver, Sender}, Arc, RwLock}, thread::JoinHandle};
use watch::{WatchEntry, WatchSpec};

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct PrototypeReference {
    chunk_name: String,
    prototype_index: usize,
}

#[derive(Debug)]
pub struct Dead;

pub use self::{
    breakpoints::{Breakpoint, Breakpoints},
    location::Location,
    stop_reason::StopReason,
    watch::WatchMode,
};

// pub mod adapter;
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

#[derive(Debug)]
pub enum RequestMessage {
    AddBreakpoint {
        chunk: String,
        line: usize
    },
    AddFunctionBreakpoint(String),
    RemoveBreakpoint {
        chunk: String,
        line: usize
    },
    ListBreakpoints,
    MatchesBreakpoint {
        chunk: String,
        line: usize
    },
    WatchRegister {
        register: usize,
        mode: Option<WatchMode>
    },
    WatchGlobal {
        name: String,
        mode: Option<WatchMode>
    },
    ContinueRun,
    StepInto,
    StepOver,
    StepOut,
    Backtrace,
    StackSnapshot,
    ReadRegister(usize),
    ReadUpvalues,
    ReadRegisters,
    ReadGlobal(String),
    ReadMemory(String, Option<usize>, Option<usize>),
    Disassemble(String, Option<usize>, Option<usize>),
}

#[derive(Debug)]
pub enum ResponseMessage {
    BreakpointAdded(usize),
    BreakpointRemoved,
    FunctionBreakpointAdded(Option<usize>),
    ListBreakpoints(HashMap<String, Vec<usize>>),
    MatchesBreakpoint(Vec<usize>),
    WatchRegisterAdded,
    WatchGlobalAdded,
    ContinueRun(StopReason),
    StepInto(StopReason),
    StepOver(StopReason),
    StepOut(StopReason),
    Backtrace(Result<Vec<Location>, StopReason>),
    StackSnapshot(Result<Vec<(Location, Vec<String>)>, StopReason>),
    ReadRegister(Result<Option<String>, StopReason>),
    ReadUpvalues(Result<Option<Vec<String>>, StopReason>),
    ReadRegisters(Result<Option<Vec<String>>, StopReason>),
    ReadGlobal(String),
    ReadMemory(Option<Vec<u8>>),
    Disassemble(Option<Vec<Disassembled>>),
}

pub struct Session {
    id: usize,
    source: String,
    path: String,
    thread: Option<JoinHandle<()>>,
    sender: Option<Sender<RequestMessage>>,
    receiver: Option<Receiver<ResponseMessage>>
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

fn value_to_string<'gc>(value: Value<'gc>) -> String {
    format!("{}", value.display())
}

fn lua_string_to_string(s: LuaString<'_>) -> String {
    format!("{}", s.display_lossy())
}

fn current_stack_depth<'gc>(executor: Executor<'gc>) -> Result<usize, StopReason> {
    with_top_thread_state(executor, |state| state.frames.len())
}

fn stack_depth_and_location<'gc>(executor: Executor<'gc>) -> Result<(usize, Option<Location>), StopReason> {
    with_top_thread_state(executor, |state| {
        let depth = state.frames.len();
        let loc = current_location_from_state_in_thread(state);
        (depth, loc)
    })
}

fn current_location<'gc>(executor: Executor<'gc>) -> Result<Option<Location>, StopReason> {
    with_top_thread_state(executor, |state| current_location_from_state_in_thread(state))
}

fn with_top_thread_state<'gc, R>(
    executor: Executor<'gc>,
    f: impl FnOnce(&piccolo::thread::ThreadState<'gc>) -> R,
) -> Result<R, StopReason> {
    let guard = executor.0.borrow();
    let thread = *guard.thread_stack.last().ok_or(StopReason::Finished)?;
    drop(guard);
    let thread_state = thread.into_inner().borrow();
    Ok(f(&thread_state))
}

fn with_top_thread_state_mut<'gc, R>(
    executor: Executor<'gc>,
    ctx: Context<'gc>,
    f: impl FnOnce(piccolo::Thread<'gc>, &mut piccolo::thread::ThreadState<'gc>) -> R,
) -> Result<R, StopReason> {
    let guard = executor.0.borrow();
    let top_thread = *guard.thread_stack.last().ok_or(StopReason::Finished)?;
    drop(guard);
    let mut thread_state = top_thread.into_inner().borrow_mut(&ctx);
    let r = f(top_thread, &mut thread_state);
    drop(thread_state);
    Ok(r)
}

fn stop_if_breakpoint(location: &Location, breakpoints: &Breakpoints) -> Option<StopReason> {
    let breakpoint_ids = breakpoints.matches(location.chunk(), location.line());
    if breakpoint_ids.is_empty() {
        None
    } else {
        Some(StopReason::Breakpoint {
            location: location.to_owned(),
            breakpoint_ids,
        })
    }
}

fn eval_watch<'gc>(spec: &WatchSpec, executor: Executor<'gc>, ctx: Context<'gc>) -> Option<String> {
    match spec {
        WatchSpec::Register(index) => {
            with_top_thread_state(executor, |state| {
                if let Some(Frame::Lua {
                    base, stack_size, ..
                }) = state.frames.last()
                {
                    let index = *index;
                    let base = *base;
                    let size = *stack_size;
                    if index < size {
                        return Some(state.stack[base + index].display().to_string());
                    }
                }
                None
            }).ok()?
        }
        WatchSpec::Global(name) => {
            let key = ctx.intern(name.as_bytes());
            Some(ctx.globals().get_value(ctx, key).display().to_string())
        }
    }
}

fn resolve_function_breakpoint<'gc>(
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

fn describe_watch_change<'gc>(
    spec: &WatchSpec,
    old: &Option<String>,
    new: &Option<String>,
) -> String {
    let spec = match spec {
        WatchSpec::Register(index) => format!("register[{}]", index),
        WatchSpec::Global(name) => format!("global['{}']", name),
    };
    format!(
        "{}: {} -> {}",
        spec,
        old.as_ref().unwrap_or(&"<none>".to_string()),
        new.as_ref().unwrap_or(&"<none>".to_string())
    )
}

fn poll_watchpoints<'gc>(watchpoints: &mut Vec<WatchEntry>, executor: Executor<'gc>, ctx: Context<'gc>) -> Option<String> {
    for index in 0..watchpoints.len() {
        let Some(entry) = watchpoints.get(index) else {
            continue;
        };
        let entry = entry.to_owned();
        let spec = match &entry.spec {
            WatchSpec::Register(i) => WatchSpec::Register(*i),
            WatchSpec::Global(n) => WatchSpec::Global(n.clone()),
        };
        let last = entry.last;

        let current = eval_watch(&spec, executor, ctx);
        let should_fire = match entry.mode {
            WatchMode::Modify => match (last.as_ref(), current.as_ref()) {
                (None, Some(_)) => true,
                (Some(_), None) => true,
                (Some(left), Some(right)) => left != right,
                (None, None) => false,
            },
            WatchMode::Access => {
                if current.is_some() {
                    let current_line = current_location(executor)
                        .ok()
                        .flatten()
                        .map(|l| (l.chunk().to_owned(), l.line()));
                    if let (Some((chunk, line)), Some(entry)) =
                        (current_line, watchpoints.get(index))
                    {
                        if entry.last_line_seen != Some((chunk.to_owned(), line)) {
                            if let Some(entry) = watchpoints.get_mut(index) {
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
                describe_watch_change(&spec, &last, &current)
            );
            if let Some(entry) = watchpoints.get_mut(index) {
                entry.last = current;
            }
            return Some(message);
        }
        if let Some(entry) = watchpoints.get_mut(index) {
            entry.last = current;
        }
    }
    None
}

pub fn step_one<'gc>(executor: Executor<'gc>, ctx: Context<'gc>, watchpoints: &mut Vec<WatchEntry>) -> StopReason {
    const FUEL_NON_LUA_CALLBACK_STEP: i32 = 11;
    const FUEL_NON_LUA_SEQUENCE_STEP: i32 = 7;
    const FUEL_NON_LUA_ERROR_STEP: i32 = 3;

    match executor.mode() {
        piccolo::thread::ExecutorMode::Stopped => return StopReason::Finished,
        piccolo::thread::ExecutorMode::Result => return StopReason::Finished,
        piccolo::thread::ExecutorMode::Suspended => return StopReason::Suspended,
        piccolo::thread::ExecutorMode::Running => return StopReason::Error,
        piccolo::thread::ExecutorMode::Normal => {}
    }

    let (is_lua_top, non_lua_fuel) =
        match with_top_thread_state(executor, |state| match state.frames.last() {
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
        let _ = with_top_thread_state_mut(executor, ctx, |thread, state| {
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
        let _ = executor.step(ctx, &mut fuel);
    }

    let location = match current_location(executor) {
        Ok(Some(location)) => location,
        Ok(None) => return StopReason::Finished,
        Err(stop_reason) => return stop_reason,
    };
    if let Some(hit) = poll_watchpoints(watchpoints, executor, ctx) {
        return StopReason::Watchpoint(hit);
    }
    StopReason::Step(location)
}

pub fn continue_run<'gc>(executor: Executor<'gc>, ctx: Context<'gc>, breakpoints: &Breakpoints, watchpoints: &mut Vec<WatchEntry>) -> StopReason {
    loop {
        match current_location(executor) {
            Ok(Some(location)) => {
                if let Some(stop_reason) = stop_if_breakpoint(&location, &breakpoints) {
                    return stop_reason;
                }
            }
            Ok(None) => {}
            Err(stop_reason) => return stop_reason,
        }
        match step_one(executor, ctx, watchpoints) {
            StopReason::Step(_) => continue,
            other => return other,
        }
    }
}

pub fn step_into<'gc>(executor: Executor<'gc>, ctx: Context<'gc>, breakpoints: &Breakpoints, watchpoints: &mut Vec<WatchEntry>) -> StopReason {
        let start_location = match current_location(executor) {
            Ok(Some(location)) => location,
            Ok(None) => return StopReason::Finished,
            Err(stop_reason) => return stop_reason,
        };

        if let Some(stop_reason) = stop_if_breakpoint(&start_location, breakpoints) {
            return stop_reason;
        }

        loop {
            match step_one(executor, ctx, watchpoints) {
                StopReason::Step(location) => {
                    if let Some(stop_reason) = stop_if_breakpoint(&location, breakpoints) {
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

    pub fn step_over<'gc>(executor: Executor<'gc>, ctx: Context<'gc>, breakpoints: &Breakpoints, watchpoints: &mut Vec<WatchEntry>) -> StopReason {
        let (start_depth, start_location) = match stack_depth_and_location(executor) {
            Ok((depth, Some(location))) => (depth, location),
            Ok((_depth, None)) => return StopReason::Finished,
            Err(stop_reason) => return stop_reason,
        };

        if let Some(stop_reason) = stop_if_breakpoint(&start_location, breakpoints) {
            return stop_reason;
        }

        loop {
            match step_one(executor, ctx, watchpoints) {
                StopReason::Step(location) => {
                    if let Some(stop_reason) = stop_if_breakpoint(&location, breakpoints) {
                        return stop_reason;
                    }
                    let depth = match current_stack_depth(executor) {
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

    pub fn step_out<'gc>(executor: Executor<'gc>, ctx: Context<'gc>, breakpoints: &Breakpoints, watchpoints: &mut Vec<WatchEntry>) -> StopReason {
        let start_depth = match current_stack_depth(executor) {
            Ok(depth) => depth,
            Err(stop_reason) => return stop_reason,
        };

        loop {
            match step_one(executor, ctx, watchpoints) {
                StopReason::Step(location) => {
                    if let Some(stop_reason) = stop_if_breakpoint(&location, breakpoints) {
                        return stop_reason;
                    }
                    let depth = match current_stack_depth(executor) {
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

    pub fn backtrace<'gc>(executor: Executor<'gc>) -> Result<Vec<Location>, StopReason> {
        with_top_thread_state(executor, |state| {
            let mut out = Vec::with_capacity(state.frames.len());
            for frame in state.frames.iter() {
                if let Frame::Lua { closure, pc, .. } = frame {
                    let proto = closure.prototype();
                    let chunk_name = lua_string_to_string(proto.chunk_name);
                    let function_ref = proto.reference.map_strings(lua_string_to_string);
                    let line = opcode_index_to_line(&proto.opcode_line_numbers, *pc);
                    out.push(Location::new(chunk_name, function_ref, line));
                }
            }
            out
        })
    }

    pub fn read_register<'gc>(executor: Executor<'gc>, register_index: usize) -> Result<Option<String>, StopReason> {
        with_top_thread_state(executor, |state| {
            if let Some(Frame::Lua {
                base, stack_size, ..
            }) = state.frames.last()
            {
                let base = *base;
                let size = *stack_size;
                if register_index < size {
                    return Some(value_to_string(state.stack[base + register_index]));
                }
            }
            None
        })
    }

    pub fn read_global<'gc>(ctx: Context<'gc>, name: &str) -> String {
        let key = ctx.intern(name.as_bytes());
        value_to_string(ctx.globals().get_value(ctx, key))
    }

    pub fn read_registers<'gc>(executor: Executor<'gc>) -> Result<Option<Vec<String>>, StopReason> {
        with_top_thread_state(executor, |state| {
            if let Some(Frame::Lua {
                base, stack_size, ..
            }) = state.frames.last()
            {
                let base = *base;
                let size = *stack_size;
                return Some(state.stack[base..base + size].iter().map(|v| value_to_string(*v)).collect());
            }
            None
        })
    }

    pub fn read_upvalues<'gc>(executor: Executor<'gc>) -> Result<Option<Vec<String>>, StopReason> {
        with_top_thread_state(executor, |state| {
            if let Some(Frame::Lua { closure, .. }) = state.frames.last() {
                let mut values = Vec::new();
                for &upvalue in closure.upvalues() {
                    if let piccolo::closure::UpValueState::Closed(value) = upvalue.get() {
                        values.push(value_to_string(value)); 
                    } else {
                        values.push(String::new());
                    }
                }
                return Some(values);
            }
            None
        })
    }

    pub fn stack_snapshot<'gc>(executor: Executor<'gc>) -> Result<Vec<(Location, Vec<String>)>, StopReason> {
        with_top_thread_state(executor, |state| {
            let mut out = Vec::with_capacity(state.frames.len());
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
                    out.push((location, state.stack[base..base + size].iter().map(|v| value_to_string(*v)).collect()));
                }
            }
            out
        })
    }

    fn disassemble_frame<'gc>(
        executor: Executor<'gc>,
        frame: usize,
        instruction_offset: Option<usize>,
        instruction_count: Option<usize>,
    ) -> Option<Vec<Disassembled>> {
        with_top_thread_state(executor, |state| {
            if let Some(Frame::Lua { closure, pc, .. }) = if frame == usize::MAX {
                state.frames.last()
            } else {
                state.frames.get(frame)
            } {
                let prototype = closure.prototype();
                let current_pc = *pc;
                Some(disassemble_prototype_opcodes(
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

    fn disassemble_prototype<'gc>(
        executor: Executor<'gc>,
        prototype: &str,
        instruction_offset: Option<usize>,
        instruction_count: Option<usize>,
    ) -> Option<Vec<Disassembled>> {
        let (chunk, index) = prototype.split_once(':')?;

        let index: usize = index.parse().ok()?;

        with_top_thread_state(executor, |state| {
            for frame in state.frames.iter() {
                if let Frame::Lua { closure, .. } = frame {
                    let proto = closure.prototype();
                    let frame_chunk = lua_string_to_string(proto.chunk_name);

                    if frame_chunk == chunk {
                        if let Some(nested_proto) = proto.prototypes.get(index) {
                            return Some(disassemble_prototype_opcodes(
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

    fn disassemble_prototype_opcodes<'gc>(
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

    pub fn disassemble<'gc>(
        executor: Executor<'gc>,
        memory_reference: &str,
        instruction_offset: Option<usize>,
        instruction_count: Option<usize>,
    ) -> Option<Vec<Disassembled>> {
        let (ty, s) = memory_reference.split_once(':').unwrap_or(("frame", ""));
        match ty {
            "frame" => disassemble_frame(
                executor,
                if s.is_empty() {
                    usize::MAX
                } else {
                    s.parse().ok()?
                },
                instruction_offset,
                instruction_count,
            ),
            "prototype" => disassemble_prototype(executor, s, instruction_offset, instruction_count),
            _ => None,
        }
    }

    pub fn read_memory<'gc>(
        executor: Executor<'gc>,
        memory_reference: &str,
        byte_offset: Option<usize>,
        byte_count: Option<usize>,
    ) -> Option<Vec<u8>> {
        let (ty, s) = memory_reference.split_once(':').unwrap_or(("frame", ""));

        match ty {
            "frame" => read_frame_memory(
                executor,
                if s.is_empty() {
                    usize::MAX
                } else {
                    s.parse().ok()?
                },
                byte_offset,
                byte_count,
            ),
            "prototype" => read_prototype_memory(executor, s, byte_offset, byte_count),
            _ => None,
        }
    }

    fn read_frame_memory<'gc>(
        executor: Executor<'gc>,
        frame: usize,
        byte_offset: Option<usize>,
        byte_count: Option<usize>,
    ) -> Option<Vec<u8>> {
        with_top_thread_state(executor, |state| {
            if let Some(piccolo::thread::Frame::Lua { closure, .. }) = if frame == usize::MAX {
                state.frames.last()
            } else {
                state.frames.get(frame)
            } {
                let prototype = closure.prototype();
                Some(read_opcodes(&prototype.opcodes, byte_offset, byte_count))
            } else {
                None
            }
        })
        .ok()?
    }

    fn read_prototype_memory<'gc>(
        executor: Executor<'gc>,
        prototype: &str,
        byte_offset: Option<usize>,
        byte_count: Option<usize>,
    ) -> Option<Vec<u8>> {
        let (chunk, index) = prototype.split_once(':')?;

        let index: usize = index.parse().ok()?;

        with_top_thread_state(executor, |state| {
            for frame in state.frames.iter() {
                if let Frame::Lua { closure, .. } = frame {
                    let proto = closure.prototype();
                    let frame_chunk = lua_string_to_string(proto.chunk_name);

                    if frame_chunk == chunk {
                        if let Some(nested_proto) = proto.prototypes.get(index) {
                            return Some(
                                read_opcodes(&nested_proto.opcodes, byte_offset, byte_count),
                            );
                        }
                    }
                }
            }
            None
        })
        .ok()?
    }

    fn read_opcodes<'gc>(
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

impl<'gc> Session {
    pub fn new(id: usize, source: impl Into<String>, path: impl Into<String>) -> Self {

        Self {
            id,
            source: source.into(),
            path: path.into(),
            thread: None,
            sender: None,
            receiver: None,
        }
    }

    pub fn launch(&mut self) {
        let (first, second) = std::sync::mpsc::channel();
        let (thirst, fourth) = std::sync::mpsc::channel();
        self.sender = Some(first);
        self.receiver = Some(fourth);

        let source = self.source.as_bytes().to_vec();
        let path = self.path.to_owned();
        let thread = std::thread::spawn(move || {
            let mut lua = Lua::full();
            lua.enter(|ctx| {
                let mut breakpoints = Breakpoints::default();
                let mut watchpoints = Vec::new();
                let closure = Closure::load(ctx, Some(&path), &source).unwrap();
                let executor = Executor::start(ctx, closure.into(), ());

                loop {
                    match second.recv() {
                        Ok(message) => {
                            match message {
                                RequestMessage::AddBreakpoint { chunk, line } => {
                                    let id = breakpoints.add(chunk, line);
                                    thirst.send(ResponseMessage::BreakpointAdded(id));
                                }
                                RequestMessage::RemoveBreakpoint { chunk, line } => {
                                    breakpoints.remove(&chunk, line);
                                    thirst.send(ResponseMessage::BreakpointRemoved);
                                }
                                RequestMessage::AddFunctionBreakpoint(name) => {
                                    let id = resolve_function_breakpoint(ctx, name.as_str())
                                        .map(|(chunk, line)| breakpoints.add(chunk, line));
                                    thirst.send(ResponseMessage::FunctionBreakpointAdded(id));
                                }
                                RequestMessage::ListBreakpoints => {
                                    let list = breakpoints.list();
                                    thirst.send(ResponseMessage::ListBreakpoints(list));
                                }
                                RequestMessage::MatchesBreakpoint { chunk, line } => {
                                    let matches = breakpoints.matches(&chunk, line);
                                    thirst.send(ResponseMessage::MatchesBreakpoint(matches));
                                }
                                RequestMessage::WatchRegister { register, mode } => {
                                    watchpoints.push(WatchEntry {
                                        spec: WatchSpec::Register(register),
                                        last: None,
                                        mode: mode.unwrap_or(WatchMode::Modify),
                                        last_line_seen: None,
                                    });
                                    thirst.send(ResponseMessage::WatchRegisterAdded);
                                }
                                RequestMessage::WatchGlobal { name, mode } => {
                                    watchpoints.push(WatchEntry {
                                        spec: WatchSpec::Global(name),
                                        last: None,
                                        mode: mode.unwrap_or(WatchMode::Modify),
                                        last_line_seen: None,
                                    });
                                    thirst.send(ResponseMessage::WatchGlobalAdded);
                                }
                                RequestMessage::ContinueRun => {
                                    let stop_reason = continue_run(executor, ctx, &breakpoints, &mut watchpoints);
                                    thirst.send(ResponseMessage::ContinueRun(stop_reason));
                                }
                                RequestMessage::StepInto => {
                                    let stop_reason = step_into(executor, ctx, &breakpoints, &mut watchpoints);
                                    thirst.send(ResponseMessage::StepInto(stop_reason));
                                }
                                RequestMessage::StepOver => {
                                    let stop_reason = step_over(executor, ctx, &breakpoints, &mut watchpoints);
                                    thirst.send(ResponseMessage::StepOver(stop_reason));
                                }
                                RequestMessage::StepOut => {
                                    let stop_reason = step_out(executor, ctx, &breakpoints, &mut watchpoints);
                                    thirst.send(ResponseMessage::StepOut(stop_reason));
                                }
                                RequestMessage::Backtrace => {
                                    let backtrace = backtrace(executor);
                                    thirst.send(ResponseMessage::Backtrace(backtrace));
                                }
                                RequestMessage::StackSnapshot => {
                                    let stack_snapshot = stack_snapshot(executor);
                                    thirst.send(ResponseMessage::StackSnapshot(stack_snapshot));
                                }
                                RequestMessage::ReadRegister(register) => {
                                    let register = read_register(executor, register);
                                    thirst.send(ResponseMessage::ReadRegister(register));
                                }
                                RequestMessage::ReadRegisters => {
                                    let registers = read_registers(executor);
                                    thirst.send(ResponseMessage::ReadRegisters(registers));
                                }
                                RequestMessage::ReadGlobal(name) => {
                                    let global = read_global(ctx, name.as_str());
                                    thirst.send(ResponseMessage::ReadGlobal(global));
                                }
                                RequestMessage::ReadMemory(memory_reference, byte_offset, byte_count) => {
                                    let memory = read_memory(executor, memory_reference.as_str(), byte_offset, byte_count);
                                    thirst.send(ResponseMessage::ReadMemory(memory));
                                }
                                RequestMessage::Disassemble(memory_reference, instruction_offset, instruction_count) => {
                                    let disassembled = disassemble(executor, memory_reference.as_str(), instruction_offset, instruction_count);
                                    thirst.send(ResponseMessage::Disassemble(disassembled));
                                }
                                RequestMessage::ReadUpvalues => {
                                    let upvalues = read_upvalues(executor);
                                    thirst.send(ResponseMessage::ReadUpvalues(upvalues));
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("Error receiving message: {}", e);
                        }
                    }
                }
            });
        });

        self.thread = Some(thread);
    }

    pub fn restart(&mut self) {
        self.thread.take().map(|thread| thread.join().unwrap());
        self.launch();
    }

    pub fn disconnect(&mut self) {
        self.sender.take();
    }

    pub fn terminate(&mut self) {
        self.thread.take().map(|thread| thread.join().unwrap());
        self.sender.take();
    }

    pub fn add_breakpoint(&mut self, chunk: String, line: usize) -> Result<usize, Dead> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Dead);
        };

        sender.send(RequestMessage::AddBreakpoint { chunk, line });
        
        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Dead);
        };

        match receiver.recv() {
            Ok(ResponseMessage::BreakpointAdded(id)) => Ok(id),
            _ => Err(Dead)
        }
    }

    pub fn add_function_breakpoint(&mut self, name: String) -> Result<Option<usize>, Dead> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Dead);
        };

        sender.send(RequestMessage::AddFunctionBreakpoint(name));
        
        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Dead);
        };

        match receiver.recv() {
            Ok(ResponseMessage::FunctionBreakpointAdded(id)) => Ok(id),
            _ => Err(Dead)
        }
    }

    pub fn remove_breakpoint(&mut self, chunk: String, line: usize) -> Result<(), Dead> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Dead);
        };

        sender.send(RequestMessage::RemoveBreakpoint { chunk, line });
        
        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Dead);
        };

        match receiver.recv() {
            Ok(ResponseMessage::BreakpointRemoved) => Ok(()),
            _ => Err(Dead)
        }
    }

    pub fn list_breakpoints(&self) -> Result<HashMap<String, Vec<usize>>, Dead> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Dead);
        };

        sender.send(RequestMessage::ListBreakpoints);
        
        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Dead);
        };

        match receiver.recv() {
            Ok(ResponseMessage::ListBreakpoints(list)) => Ok(list),
            _ => Err(Dead)
        }
    }

    pub fn matches_breakpoint(&self, chunk: String, line: usize) -> Result<Vec<usize>, Dead> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Dead);
        };

        sender.send(RequestMessage::MatchesBreakpoint { chunk, line });
        
        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Dead);
        };

        match receiver.recv() {
            Ok(ResponseMessage::MatchesBreakpoint(matches)) => Ok(matches),
            _ => Err(Dead)
        }
    }

    pub fn watch_register(&mut self, register: usize, mode: Option<WatchMode>) -> Result<(), Dead> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Dead);
        };

        sender.send(RequestMessage::WatchRegister { register, mode });

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Dead);
        };

        match receiver.recv() {
            Ok(ResponseMessage::WatchRegisterAdded) => Ok(()),
            _ => Err(Dead)
        }
    }

    pub fn watch_global(&mut self, name: impl Into<String>, mode: Option<WatchMode>) -> Result<(), Dead> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(Dead);
        };

        sender.send(RequestMessage::WatchGlobal { name: name.into(), mode });

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(Dead);
        };

        match receiver.recv() {
            Ok(ResponseMessage::WatchGlobalAdded) => Ok(()),
            _ => Err(Dead)
        }   
    }

    pub fn continue_run(&mut self) -> StopReason {
        let Some(sender) = self.sender.as_ref() else {
            return StopReason::Error;
        };

        sender.send(RequestMessage::ContinueRun);

        let Some(receiver) = self.receiver.as_ref() else {
            return StopReason::Error;
        };

        match receiver.recv() {
            Ok(ResponseMessage::ContinueRun(stop_reason)) => stop_reason,
            _ => StopReason::Error,
        }
    }

    pub fn step_into(&mut self) -> StopReason {
        let Some(sender) = self.sender.as_ref() else {
            return StopReason::Error;
        };

        sender.send(RequestMessage::StepInto);

        let Some(receiver) = self.receiver.as_ref() else {
            return StopReason::Error;
        };

        match receiver.recv() {
            Ok(ResponseMessage::StepInto(stop_reason)) => stop_reason,
            _ => StopReason::Error,
        }
    }

    pub fn step_over(&mut self) -> StopReason {
        let Some(sender) = self.sender.as_ref() else {
            return StopReason::Error;
        };

        sender.send(RequestMessage::StepOver);

        let Some(receiver) = self.receiver.as_ref() else {
            return StopReason::Error;
        };

        match receiver.recv() {
            Ok(ResponseMessage::StepOver(stop_reason)) => stop_reason,
            _ => StopReason::Error,
        }
    }

    pub fn step_out(&mut self) -> StopReason {
        let Some(sender) = self.sender.as_ref() else {
            return StopReason::Error;
        };

        sender.send(RequestMessage::StepOut);

        let Some(receiver) = self.receiver.as_ref() else {
            return StopReason::Error;
        };

        match receiver.recv() {
            Ok(ResponseMessage::StepOut(stop_reason)) => stop_reason,
            _ => StopReason::Error,
        }
    }

    pub fn backtrace(&mut self) -> Result<Vec<Location>, StopReason> {
        let Some(sender) = self.sender.as_ref() else {
            println!("no sender");
            return Err(StopReason::Error);
        };

        sender.send(RequestMessage::Backtrace);

        let Some(receiver) = self.receiver.as_ref() else {
            println!("no receiver");
            return Err(StopReason::Error);
        };

        match receiver.recv() {
            Ok(ResponseMessage::Backtrace(backtrace)) => backtrace,
            other => {
                println!("no backtrace: {:?}", other.unwrap());
                Err(StopReason::Error)
            }
        }
    }

    pub fn stack_snapshot(&mut self) -> Result<Vec<(Location, Vec<String>)>, StopReason> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(StopReason::Error);
        };

        sender.send(RequestMessage::StackSnapshot);

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(StopReason::Error);
        };

        match receiver.recv() {
            Ok(ResponseMessage::StackSnapshot(stack_snapshot)) => stack_snapshot,
            _ => Err(StopReason::Error)
        }
    }

    pub fn read_register(&mut self, register: usize) -> Result<Option<String>, StopReason> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(StopReason::Error);
        };

        sender.send(RequestMessage::ReadRegister(register));

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(StopReason::Error);
        };

        match receiver.recv() {
            Ok(ResponseMessage::ReadRegister(register)) => register,
            _ => Err(StopReason::Error)
        }
    }

    pub fn read_upvalues(&mut self) -> Result<Option<Vec<String>>, StopReason> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(StopReason::Error);
        };
        
        sender.send(RequestMessage::ReadUpvalues);

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(StopReason::Error);
        };

        match receiver.recv() {
            Ok(ResponseMessage::ReadUpvalues(upvalues)) => upvalues,
            _ => Err(StopReason::Error)
        }
    }

    pub fn read_registers(&mut self) -> Result<Option<Vec<String>>, StopReason> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(StopReason::Error);
        };

        sender.send(RequestMessage::ReadRegisters);

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(StopReason::Error);
        };

        match receiver.recv() {
            Ok(ResponseMessage::ReadRegisters(registers)) => registers,
            _ => Err(StopReason::Error)
        }
    }

    pub fn read_global(&mut self, name: String) -> Result<String, StopReason> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(StopReason::Error);
        };

        sender.send(RequestMessage::ReadGlobal(name));

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(StopReason::Error);
        };

        match receiver.recv() {
            Ok(ResponseMessage::ReadGlobal(global)) => Ok(global),
            _ => Err(StopReason::Error)
        }
    }

    pub fn read_memory(&mut self, memory_reference: String, byte_offset: Option<usize>, byte_count: Option<usize>) -> Result<Option<Vec<u8>>, StopReason> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(StopReason::Error);
        };

        sender.send(RequestMessage::ReadMemory(memory_reference, byte_offset, byte_count));

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(StopReason::Error);
        };

        match receiver.recv() {
            Ok(ResponseMessage::ReadMemory(memory)) => Ok(memory),
            _ => Err(StopReason::Error)
        }
    }

    pub fn disassemble(&mut self, memory_reference: String, instruction_offset: Option<usize>, instruction_count: Option<usize>) -> Result<Option<Vec<Disassembled>>, StopReason> {
        let Some(sender) = self.sender.as_ref() else {
            return Err(StopReason::Error);
        };

        sender.send(RequestMessage::Disassemble(memory_reference, instruction_offset, instruction_count));

        let Some(receiver) = self.receiver.as_ref() else {
            return Err(StopReason::Error);
        };

        match receiver.recv() {
            Ok(ResponseMessage::Disassemble(disassemble)) => Ok(disassemble),   
            _ => Err(StopReason::Error)
        }
    }
}

pub struct Debugger {
    next_session: usize,
    sessions: Vec<Session>,
}

impl Debugger {
    pub fn new() -> Self {
        Self {
            next_session: 0,
            sessions: Vec::new(),
        }
    }

    pub fn launch(&mut self, session_id: usize) {
        self.sessions.get_mut(session_id).map(|session| session.launch());
    }

    pub fn restart(&mut self, session_id: usize) {
        self.sessions.get_mut(session_id).map(|session| session.restart());
    }

    pub fn disconnect(&mut self, session_id: usize) {
        self.sessions.get_mut(session_id).map(|session| session.disconnect());
    }

    pub fn terminate(&mut self, session_id: usize) {
        self.sessions.get_mut(session_id).map(|session| session.terminate());
    }

    pub fn add_session(&mut self, source: impl Into<String>, path: impl Into<String>) -> usize {
        let id = self.next_session;
        self.next_session += 1;
        let source = source.into();
        let path = path.into();
        let session = Session::new(id, source, path);
        self.sessions.push(session);
        id
    }

    pub fn add_breakpoint(&mut self, session_id: usize, chunk: String, line: usize) -> Result<usize, Dead> {
        self.sessions.get_mut(session_id).map(|session| session.add_breakpoint(chunk, line))
            .unwrap_or(Err(Dead))
    }

    pub fn add_function_breakpoint(&mut self, session_id: usize, name: String) -> Result<Option<usize>, Dead> {
        self.sessions.get_mut(session_id).map(|session| session.add_function_breakpoint(name))
            .unwrap_or(Err(Dead))
    }

    pub fn remove_breakpoint(&mut self, session_id: usize, chunk: String, line: usize) -> Result<(), Dead> {
        self.sessions.get_mut(session_id).map(|session| session.remove_breakpoint(chunk, line))
            .unwrap_or(Err(Dead))
    }

    pub fn list_breakpoints(&self, session_id: usize) -> Result<HashMap<String, Vec<usize>>, Dead> {
        self.sessions
            .get(session_id)
            .map(|session| session.list_breakpoints())
            .unwrap_or(Err(Dead))
    }

    pub fn matches_breakpoint(&self, session_id: usize, chunk: String, line: usize) -> Result<Vec<usize>, Dead> {
        self.sessions
            .get(session_id)
            .map(|session| session.matches_breakpoint(chunk, line))
            .unwrap_or(Err(Dead))
    }

    pub fn watch_register(&mut self, session_id: usize, register: usize, mode: Option<WatchMode>) -> Result<(), Dead> {
        self.sessions.get_mut(session_id).map(|session| session.watch_register(register, mode))
            .unwrap_or(Err(Dead))
    }

    pub fn watch_global(&mut self, session_id: usize, name: impl Into<String>, mode: Option<WatchMode>) -> Result<(), Dead> {
        self.sessions.get_mut(session_id).map(|session| session.watch_global(name, mode))
            .unwrap_or(Err(Dead))
    }

    pub fn continue_run(&mut self, session_id: usize) -> StopReason {
        self.sessions.get_mut(session_id).map(|session| session.continue_run())
            .unwrap_or(StopReason::Error)
    }

    pub fn step_into(&mut self, session_id: usize) -> StopReason {
        self.sessions.get_mut(session_id).map(|session| session.step_into())
            .unwrap_or(StopReason::Error)
    }

    pub fn step_over(&mut self, session_id: usize) -> StopReason {
        self.sessions.get_mut(session_id).map(|session| session.step_over())
            .unwrap_or(StopReason::Error)
    }

    pub fn step_out(&mut self, session_id: usize) -> StopReason {
        self.sessions.get_mut(session_id).map(|session| session.step_out())
            .unwrap_or(StopReason::Error)
    }

    pub fn backtrace(&mut self, session_id: usize) -> Result<Vec<Location>, StopReason> {
        self.sessions.get_mut(session_id).map(|session| session.backtrace())
            .unwrap_or(Err(StopReason::Error))
    }

    pub fn stack_snapshot(&mut self, session_id: usize) -> Result<Vec<(Location, Vec<String>)>, StopReason> {
        self.sessions.get_mut(session_id).map(|session| session.stack_snapshot())
            .unwrap_or(Err(StopReason::Error))
    }

    pub fn read_register(&mut self, session_id: usize, register: usize) -> Result<Option<String>, StopReason> {
        self.sessions.get_mut(session_id).map(|session| session.read_register(register))
            .unwrap_or(Err(StopReason::Error))
    }

    pub fn read_upvalues(&mut self, session_id: usize) -> Result<Option<Vec<String>>, StopReason> {
        self.sessions.get_mut(session_id).map(|session| session.read_upvalues())
            .unwrap_or(Err(StopReason::Error))
    }

    pub fn read_registers(&mut self, session_id: usize) -> Result<Option<Vec<String>>, StopReason> {
        self.sessions.get_mut(session_id).map(|session| session.read_registers())
            .unwrap_or(Err(StopReason::Error))
    } 

    pub fn read_global(&mut self, session_id: usize, name: String) -> Result<String, StopReason> {
        self.sessions.get_mut(session_id).map(|session| session.read_global(name))
            .unwrap_or(Err(StopReason::Error))
    }

    pub fn read_memory(&mut self, session_id: usize, memory_reference: String, byte_offset: Option<usize>, byte_count: Option<usize>) -> Result<Option<Vec<u8>>, StopReason> {
        self.sessions.get_mut(session_id).map(|session| session.read_memory(memory_reference, byte_offset, byte_count))
            .unwrap_or(Err(StopReason::Error))
    }

    pub fn disassemble(&mut self, session_id: usize, memory_reference: String, instruction_offset: Option<usize>, instruction_count: Option<usize>) -> Result<Option<Vec<Disassembled>>, StopReason> {
        self.sessions.get_mut(session_id).map(|session| session.disassemble(memory_reference, instruction_offset, instruction_count))
            .unwrap_or(Err(StopReason::Error))
    }
}
