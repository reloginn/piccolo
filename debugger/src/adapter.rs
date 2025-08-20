use std::mem::ManuallyDrop;

use base64::Engine;
use piccolo::{Context, Executor};

use super::Debugger;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum State {
    NotLaunched,
    Launched,
    Suspended,
    Terminated,
}

pub enum Error {
    AdapterNotLaunched,
    AdapterSuspended,
    AdapterTerminated,
}

impl Error {
    pub fn reason(state: State) -> Self {
        match state {
            State::Suspended => Error::AdapterSuspended,
            State::Terminated => Error::AdapterTerminated,
            State::NotLaunched => Error::AdapterNotLaunched,
            _ => unreachable!(),
        }
    }
}

#[allow(dead_code)]
pub struct Capabilities {
    supports_configuration_done_request: bool,
    supports_function_breakpoints: bool,
    supports_conditional_breakpoints: bool,
    supports_hit_conditional_breakpoints: bool,
    supports_evaluate_for_hovers: bool,
    supports_step_back: bool,
    supports_set_variable: bool,
    supports_restart_frame: bool,
    supports_goto_targets_request: bool,
    supports_step_in_targets_request: bool,
    supports_completions_request: bool,
    supports_modules_request: bool,
    supports_restart_request: bool,
    supports_exception_options: bool,
    supports_value_formatting_options: bool,
    supports_exception_info_request: bool,
    support_terminate_debuggee: bool,
    support_suspend_debuggee: bool,
    supports_delayed_stack_trace_loading: bool,
    supports_loaded_sources_request: bool,
    supports_log_points: bool,
    supports_terminate_threads_request: bool,
    supports_set_expression: bool,
    supports_terminate_request: bool,
    supports_data_breakpoints: bool,
    supports_read_memory_request: bool,
    supports_write_memory_request: bool,
    supports_disassemble_request: bool,
    supports_cancel_request: bool,
    supports_breakpoint_locations_request: bool,
    supports_clipboard_context: bool,
    supports_stepping_granularity: bool,
    supports_instruction_breakpoints: bool,
    supports_exception_filter_options: bool,
    supports_single_thread_execution_requests: bool,
    supports_data_breakpoint_bytes: bool,
    supports_ansi_styling: bool,
}

pub struct AttachResponse;

pub enum SteppingGranularity {}

pub struct StepOutArguments {
    thread_id: i64,
    single_thread: Option<bool>,
    granularity: Option<SteppingGranularity>,
}

pub struct StepBackResponse;

pub struct StepOutResponse;

pub struct StepInArguments {
    thread_id: i64,
    single_thread: Option<bool>,
    target_id: Option<i64>,
    granularity: Option<SteppingGranularity>,
}

pub struct StepInResponse;

pub struct StepInTargetsArguments {
    frame_id: i64,
}

pub struct StepInTarget {
    id: usize,
    label: String,
    line: Option<usize>,
    column: Option<usize>,
    end_line: Option<usize>,
    end_column: Option<usize>,
}

pub struct StepInTargetsResponse {
    targets: Vec<StepInTarget>,
}

#[derive(Clone)]
pub struct Source {
    name: Option<String>,
    path: Option<String>,
    source_reference: Option<usize>,
    presentation_hint: Option<String>,
    origin: Option<String>,
    sources: Option<Vec<Source>>,
    adapter_data: Option<String>,
    checksums: Option<Vec<String>>,
}

pub struct SourceBreakpoint {
    line: usize,
    column: Option<usize>,
    condition: Option<String>,
    hit_condition: Option<String>,
    log_message: Option<String>,
    mode: Option<String>,
}

pub struct SetBreakpointsArguments {
    source: Source,
    breakpoints: Option<Vec<SourceBreakpoint>>,
    lines: Option<Vec<usize>>,
    source_modified: Option<bool>,
}

pub struct Breakpoint {
    id: Option<usize>,
    verified: bool,
    message: Option<String>,
    source: Option<Source>,
    line: Option<usize>,
    column: Option<usize>,
    end_line: Option<usize>,
    end_column: Option<usize>,
    instruction_reference: Option<String>,
    offset: Option<usize>,
    reason: Option<String>,
}

pub struct SetBreakpointsResponse {
    breakpoints: Vec<Breakpoint>,
}

pub enum DataBreakpointAccessType {
    Read,
    Write,
    ReadWrite,
}

pub struct DataBreakpoint {
    data_id: String,
    access_type: DataBreakpointAccessType,
    condition: Option<String>,
    hit_condition: Option<String>,
}

pub struct SetDataBreakpointsArguments {
    breakpoints: Vec<DataBreakpoint>,
}

pub struct SetDataBreakpointsResponse {
    breakpoints: Vec<Breakpoint>,
}

pub struct FunctionBreakpoint {
    name: String,
    condition: Option<String>,
    hit_condition: Option<String>,
}

pub struct SetFunctionBreakpointsArguments {
    breakpoints: Vec<FunctionBreakpoint>,
}

pub struct SetFunctionBreakpointsResponse {
    breakpoints: Vec<Breakpoint>,
}

pub struct InstructionBreakpoint {
    instruction_reference: String,
    offset: Option<usize>,
    condition: Option<String>,
    hit_condition: Option<String>,
    mode: Option<String>,
}

pub struct SetInstructionBreakpointsArguments {
    breakpoints: Vec<InstructionBreakpoint>,
}

pub struct SetInstructionBreakpointsResponse {
    breakpoints: Vec<Breakpoint>,
}

pub struct StackTraceFormat {
    parameters: Option<bool>,
    parameter_types: Option<bool>,
    parameter_names: Option<bool>,
    parameter_values: Option<bool>,
    line: Option<bool>,
    module: Option<bool>,
    include_all: Option<bool>,
}

pub struct StackTraceArguments {
    thread_id: i64,
    start_frame: Option<i64>,
    levels: Option<i64>,
    format: Option<StackTraceFormat>,
}

pub struct StackTraceResponse {
    stack_frames: Vec<StackFrame>,
    total_frames: usize,
}

pub struct StackFrame {
    id: usize,
    name: String,
    source: Option<Source>,
    line: usize,
    column: usize,
    end_line: Option<usize>,
    end_column: Option<usize>,
    can_restart: Option<bool>,
    instruction_pointer_reference: Option<String>,
    module_id: Option<String>,
    presentation_hint: Option<String>,
}

pub struct ReadMemoryArguments {
    memory_reference: String,
    offset: Option<usize>,
    count: Option<usize>,
}

pub struct ReadMemoryResponse {
    address: String,
    unreadable_bytes: Option<usize>,
    data: Option<String>,
}

pub struct DisassembleArguments {
    memory_reference: String,
    offset: Option<usize>,
    instruction_offset: Option<usize>,
    instruction_count: Option<usize>,
    resolve_symbols: Option<bool>,
}

pub struct DisassembledInstruction {
    address: String,
    instruction_bytes: Option<String>,
    instruction: String,
    symbol: Option<String>,
    location: Option<Source>,
    line: Option<usize>,
    column: Option<usize>,
    end_line: Option<usize>,
    end_column: Option<usize>,
    presentation_hint: Option<String>,
}

pub struct DisassembleResponse {
    instructions: Vec<DisassembledInstruction>,
}

pub struct ContinueArguments {
    thread_id: usize,
    single_thread: Option<bool>,
}

pub struct ContinueResponse {
    all_threads_continued: Option<bool>,
}

pub struct PauseArguments {
    thread_id: usize,
}

pub struct PauseResponse;

pub struct BreakpointLocationsArguments {
    source: Source,
    line: usize,
    column: Option<usize>,
    end_line: Option<usize>,
    end_column: Option<usize>,
}

pub struct BreakpointLocation {
    line: usize,
    column: Option<usize>,
    end_line: Option<usize>,
    end_column: Option<usize>,
}

pub struct BreakpointLocationsResponse {
    breakpoints: Vec<BreakpointLocation>,
}

pub struct DisconnectArguments {
    restart: Option<bool>,
    terminate_debuggee: Option<bool>,
    suspend_debuggee: Option<bool>,
}

pub struct DisconnectResponse;

pub struct LaunchArguments {
    no_debug: Option<bool>,
}

pub struct Adapter<'gc> {
    debugger: Option<Debugger<'gc>>,
    state: State,
}

impl<'gc> Adapter<'gc> {
    pub fn new() -> Self {
        Self {
            debugger: None,
            state: State::NotLaunched,
        }
    }

    pub fn initialize() -> Capabilities {
        Capabilities {
            supports_configuration_done_request: false,
            supports_function_breakpoints: true,
            supports_conditional_breakpoints: false,
            supports_hit_conditional_breakpoints: false,
            supports_evaluate_for_hovers: false,
            supports_step_back: true,
            supports_set_variable: true,
            supports_restart_frame: false,
            supports_goto_targets_request: false,
            supports_step_in_targets_request: true,
            supports_completions_request: false,
            supports_modules_request: false,
            supports_restart_request: true,
            supports_exception_options: false,
            supports_value_formatting_options: false,
            supports_exception_info_request: false,
            support_terminate_debuggee: true,
            support_suspend_debuggee: true,
            supports_delayed_stack_trace_loading: true,
            supports_loaded_sources_request: false,
            supports_log_points: false,
            supports_terminate_threads_request: false,
            supports_set_expression: false,
            supports_terminate_request: true,
            supports_data_breakpoints: true,
            supports_read_memory_request: true,
            supports_write_memory_request: false,
            supports_disassemble_request: true,
            supports_cancel_request: false,
            supports_breakpoint_locations_request: true,
            supports_clipboard_context: false,
            supports_stepping_granularity: false,
            supports_instruction_breakpoints: true,
            supports_exception_filter_options: false,
            supports_single_thread_execution_requests: false,
            supports_data_breakpoint_bytes: false,
            supports_ansi_styling: false,
        }
    }

    pub fn attach(&mut self, executor: Executor<'gc>) -> Result<AttachResponse, Error> {
        if self.state == State::NotLaunched {
            self.debugger = Some(Debugger::new(executor));
            self.state = State::Launched;
            Ok(AttachResponse)
        } else {
            Err(Error::reason(self.state))
        }
    }

    pub fn step_in(
        &mut self,
        ctx: Context<'gc>,
        _arguments: StepInArguments,
    ) -> Result<StepInResponse, Error> {
        if self.state != State::Launched {
            return Err(Error::reason(self.state));
        }
        let Some(ref mut debugger) = self.debugger else {
            return Err(Error::AdapterNotLaunched);
        };
        debugger.step_into(ctx);
        Ok(StepInResponse)
    }

    pub fn step_out(
        &mut self,
        ctx: Context<'gc>,
        _arguments: StepOutArguments,
    ) -> Result<StepOutResponse, Error> {
        if self.state != State::Launched {
            return Err(Error::reason(self.state));
        }
        let Some(ref mut debugger) = self.debugger else {
            return Err(Error::AdapterNotLaunched);
        };
        debugger.step_out(ctx);
        Ok(StepOutResponse)
    }

    pub fn set_breakpoints(
        &mut self,
        arguments: SetBreakpointsArguments,
    ) -> SetBreakpointsResponse {
        SetBreakpointsResponse {
            breakpoints: vec![],
        }
    }

    pub fn set_data_breakpoints(
        &mut self,
        arguments: SetDataBreakpointsArguments,
    ) -> SetDataBreakpointsResponse {
        SetDataBreakpointsResponse {
            breakpoints: vec![],
        }
    }

    pub fn set_function_breakpoints(
        &mut self,
        arguments: SetFunctionBreakpointsArguments,
        ctx: piccolo::Context<'gc>,
    ) -> SetFunctionBreakpointsResponse {
        SetFunctionBreakpointsResponse {
            breakpoints: vec![],
        }
    }

    pub fn set_instruction_breakpoints(
        &mut self,
        arguments: SetInstructionBreakpointsArguments,
    ) -> SetInstructionBreakpointsResponse {
        SetInstructionBreakpointsResponse {
            breakpoints: vec![],
        }
    }

    pub fn stack_trace(
        &mut self,
        arguments: StackTraceArguments,
        ctx: piccolo::Context<'gc>,
    ) -> StackTraceResponse {
        let mut stack_frames = Vec::new();

        if let Some(ref debugger) = self.debugger {
            let backtrace = debugger.backtrace(ctx);
            let start_frame = arguments.start_frame.unwrap_or(0) as usize;
            let levels = arguments.levels.map(|l| l as usize);

            let frames_to_take = if let Some(levels) = levels {
                levels
            } else {
                backtrace.len().saturating_sub(start_frame)
            };

            for (index, location) in backtrace
                .iter()
                .skip(start_frame)
                .take(frames_to_take)
                .enumerate()
            {
                let frame_id = start_frame + index;
                let function_name = location.function_ref().to_string();

                stack_frames.push(StackFrame {
                    id: frame_id,
                    name: function_name,
                    source: Some(Source {
                        name: Some(location.chunk().to_string()),
                        path: Some(location.chunk().to_string()),
                        source_reference: None,
                        presentation_hint: None,
                        origin: None,
                        sources: None,
                        adapter_data: None,
                        checksums: None,
                    }),
                    line: location.line(),
                    column: 1, // Default to column 1
                    end_line: None,
                    end_column: None,
                    can_restart: Some(false),
                    instruction_pointer_reference: Some(format!("frame:{}", frame_id)),
                    module_id: None,
                    presentation_hint: None,
                });
            }

            StackTraceResponse {
                stack_frames,
                total_frames: backtrace.len(),
            }
        } else {
            StackTraceResponse {
                stack_frames: vec![],
                total_frames: 0,
            }
        }
    }

    pub fn read_memory(
        &mut self,
        arguments: ReadMemoryArguments,
    ) -> Result<ReadMemoryResponse, Error> {
        if self.state != State::Launched {
            return Err(Error::reason(self.state));
        }

        let Some(ref debugger) = self.debugger else {
            return Err(Error::AdapterNotLaunched);
        };

        let Some(bytes) = debugger.read_memory(
            &arguments.memory_reference,
            arguments.offset,
            arguments.count,
        ) else {
            return Ok(ReadMemoryResponse {
                address: arguments.memory_reference,
                unreadable_bytes: arguments.count,
                data: None,
            });
        };

        let data = base64::engine::general_purpose::STANDARD.encode(&bytes);
        Ok(ReadMemoryResponse {
            address: arguments.memory_reference,
            unreadable_bytes: None,
            data: Some(data),
        })
    }

    pub fn disassemble(
        &mut self,
        arguments: DisassembleArguments,
    ) -> Result<DisassembleResponse, Error> {
        if self.state != State::Launched {
            return Err(Error::reason(self.state));
        }

        let Some(ref debugger) = self.debugger else {
            return Err(Error::AdapterNotLaunched);
        };

        let Some(disassembled) = debugger.disassemble(
            &arguments.memory_reference,
            arguments.instruction_offset,
            arguments.instruction_count,
        ) else {
            return Ok(DisassembleResponse {
                instructions: vec![],
            });
        };

        Ok(DisassembleResponse {
            instructions: disassembled
                .into_iter()
                .map(|disassembled| DisassembledInstruction {
                    address: disassembled.index.to_string(),
                    instruction_bytes: None,
                    instruction: format!("{:?}", disassembled.operation),
                    symbol: Some(disassembled.symbol.to_string()),
                    location: None,
                    line: Some(disassembled.line),
                    column: None,
                    end_line: None,
                    end_column: None,
                    presentation_hint: None,
                })
                .collect(),
        })
    }

    pub fn r#continue(
        &mut self,
        _arguments: ContinueArguments,
        ctx: Context<'gc>,
    ) -> Result<ContinueResponse, Error> {
        if self.state != State::Launched {
            return Err(Error::reason(self.state));
        }

        let Some(ref mut debugger) = self.debugger else {
            return Err(Error::AdapterNotLaunched);
        };

        debugger.continue_run(ctx);

        Ok(ContinueResponse {
            all_threads_continued: Some(true),
        })
    }

    pub fn pause(&mut self, _arguments: PauseArguments) -> Result<PauseResponse, Error> {
        self.state = State::Suspended;
        Ok(PauseResponse)
    }

    pub fn breakpoint_locations(
        &mut self,
        arguments: BreakpointLocationsArguments,
    ) -> BreakpointLocationsResponse {
        BreakpointLocationsResponse {
            breakpoints: vec![],
        }
    }

    pub fn restart(&mut self, executor: Executor<'gc>) {
        self.debugger.take();
        self.debugger = Some(Debugger::new(executor));
        self.state = State::Launched;
    }

    pub fn disconnect(
        &mut self,
        arguments: DisconnectArguments,
        executor: Executor<'gc>,
    ) -> Result<DisconnectResponse, Error> {
        let terminate = arguments.terminate_debuggee.unwrap_or(false);
        let suspend = arguments.suspend_debuggee.unwrap_or(false);
        let restart = arguments.restart.unwrap_or(false);

        if terminate {
            self.terminate();
        } else if suspend {
            self.pause(PauseArguments { thread_id: 0 })?;
        }

        if restart {
            self.restart(executor);
        }

        Ok(DisconnectResponse)
    }

    pub fn terminate(&mut self) {
        self.state = State::Terminated;
        self.debugger.take();
    }

    pub fn launch(&mut self, executor: Executor<'gc>, arguments: LaunchArguments) {
        let no_debug = matches!(arguments.no_debug, Some(true));
        if !no_debug {
            self.debugger = Some(Debugger::new(executor));
        }
    }
}
