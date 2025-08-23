use std::sync::mpsc::{Receiver, Sender};

use base64::Engine;

use crate::StopReason;

use super::{Debugger, Error};

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

pub struct AttachArguments;

pub struct AttachResponse;

pub enum SteppingGranularity {}

pub struct StepOutArguments {
    thread_id: usize,
    single_thread: Option<bool>,
    granularity: Option<SteppingGranularity>,
}

pub struct StepBackResponse;

pub struct StepOutResponse;

pub struct StepInArguments {
    thread_id: usize,
    single_thread: Option<bool>,
    target_id: Option<usize>,
    granularity: Option<SteppingGranularity>,
}

pub struct StepInResponse;

pub struct StepInTargetsArguments {
    frame_id: usize,
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
    thread_id: usize,
    start_frame: Option<usize>,
    levels: Option<usize>,
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
    thread_id: Option<usize>,
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
    thread_id: Option<usize>,
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
    /// Name of the thread to launch (may be file path/name or function name)
    name: String,
    /// Source code to launch
    source: String,
}

pub struct RestartResponse;

pub struct TerminateArguments {
    restart: Option<bool>,
}

pub struct TerminateResponse;

pub struct TerminateThreadsArguments {
    thread_ids: Vec<usize>,
}

pub struct TerminateThreadsResponse;

pub struct Thread {
    id: usize,
    name: String,
}

pub struct ThreadsResponse {
    threads: Vec<Thread>,
}

pub enum BreakpointReason {
    Changed,
    New,
    Removed,
}

pub enum StartMethod {
    Launch,
    Attach,
}

pub enum StoppedReason {
    Step,
    Breakpoint,
    Pause,
    FunctionBreakpoint,
    DataBreakpoint,
    InstructionBreakpoint,
}

pub enum ThreadReason {
    Started,
    Exited,
}

pub enum Event {
    Initialized,
    Breakpoint {
        thread_id: usize,
        reason: BreakpointReason,
        breakpoint: Breakpoint,
    },
    Continued {
        thread_id: usize,
        all_threads_continued: Option<bool>,
    },
    Process {
        thread_id: usize,
        name: String,
        system_process_id: Option<usize>,
        is_local_process: Option<bool>,
        start_method: Option<StartMethod>,
        pointer_size: Option<usize>,
    },
    Stopped {
        reason: StoppedReason,
        description: Option<String>,
        thread_id: Option<usize>,
        preserve_focus_hint: Option<bool>,
        text: Option<String>,
        all_threads_stopped: Option<bool>,
        hit_breakpoint_ids: Option<Vec<usize>>,
    },
    Terminated {
        restart: Option<bool>,
    },
    Thread {
        reason: ThreadReason,
        thread_id: usize,
    },
}

pub struct Adapter {
    debugger: Debugger,
    sender: Sender<Event>,
}

impl Adapter {
    pub fn new() -> (Self, Receiver<Event>) {
        let (sender, receiver) = std::sync::mpsc::channel();
        let adapter = Self {
            sender,
            debugger: Debugger::new(),
        };
        (adapter, receiver)
    }

    pub fn from_debuggee(debuggee: Debugger) -> (Self, Receiver<Event>) {
        let (sender, receiver) = std::sync::mpsc::channel();
        let adapter = Self {
            sender,
            debugger: debuggee,
        };
        (adapter, receiver)
    }

    pub fn initialize(&self) -> Result<Capabilities, Error> {
        let capabilities = Capabilities {
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
            supports_terminate_threads_request: true,
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
        };
        self.sender
            .send(Event::Initialized)
            .map_err(|_| Error::ReceiverDead)?;
        Ok(capabilities)
    }

    pub fn threads(&self) -> ThreadsResponse {
        let threads = self
            .debugger
            .sessions
            .iter()
            .map(|session| Thread {
                id: session.id(),
                name: session.name().to_string(),
            })
            .collect::<Vec<_>>();
        ThreadsResponse { threads }
    }

    pub fn attach(&mut self) -> AttachResponse {
        AttachResponse
    }

    pub fn restart(&mut self, _arguments: AttachArguments) -> RestartResponse {
        self.debugger.sessions.iter_mut().for_each(|session| {
            session.restart();
        });
        RestartResponse
    }

    pub fn restart_session(&mut self, session_id: usize) {
        self.debugger.restart(session_id);
    }

    pub fn disconnect(
        &mut self,
        arguments: DisconnectArguments,
    ) -> Result<DisconnectResponse, Error> {
        let terminate = arguments.terminate_debuggee.unwrap_or(true);
        let suspend = arguments.suspend_debuggee.unwrap_or(false);
        let restart = arguments.restart.unwrap_or(false);

        match (terminate, suspend, restart) {
            (true, _, true) => {
                self.terminate(TerminateArguments {
                    restart: Some(true),
                })?;
            }
            (true, _, false) => {
                self.terminate(TerminateArguments { restart: None })?;
            }
            (false, true, _) => {
                self.debugger.sessions.iter_mut().for_each(|session| {
                    session.disconnect(); // pause
                });
            }
            (false, false, _) => {
                self.debugger.sessions.iter_mut().for_each(|session| {
                    session.disconnect();
                });
            }
        }

        Ok(DisconnectResponse)
    }

    pub fn disconnect_session(&mut self, session_id: usize) {
        self.debugger.disconnect(session_id);
    }

    pub fn terminate(&mut self, arguments: TerminateArguments) -> Result<TerminateResponse, Error> {
        self.debugger.sessions.iter_mut().for_each(|session| {
            session.terminate();
        });
        self.sender
            .send(Event::Terminated {
                restart: arguments.restart,
            })
            .map_err(|_| Error::ReceiverDead)?;
        Ok(TerminateResponse)
    }

    pub fn terminate_session(&mut self, session_id: usize) {
        self.debugger.terminate(session_id);
    }

    pub fn terminate_threads(
        &mut self,
        arguments: TerminateThreadsArguments,
    ) -> TerminateThreadsResponse {
        arguments
            .thread_ids
            .iter()
            .for_each(|id| self.debugger.terminate(*id));
        TerminateThreadsResponse
    }

    pub fn launch(&mut self, arguments: LaunchArguments) -> Result<(), Error> {
        let no_debug = matches!(arguments.no_debug, Some(true));
        if no_debug {
            unreachable!("TODO")
        } else {
            let thread_id = self
                .debugger
                .add_session(&arguments.source, &arguments.name);
            self.sender
                .send(Event::Thread {
                    reason: ThreadReason::Started,
                    thread_id,
                })
                .map_err(|_| Error::ReceiverDead)?;
            self.debugger.launch(thread_id);
            self.sender
                .send(Event::Process {
                    thread_id,
                    name: arguments.name,
                    system_process_id: None,
                    is_local_process: Some(true),
                    start_method: Some(StartMethod::Launch),
                    pointer_size: None,
                })
                .map_err(|_| Error::ReceiverDead)?;
        }
        Ok(())
    }

    pub fn r#continue(&mut self, arguments: ContinueArguments) -> Result<ContinueResponse, Error> {
        self.debugger.continue_run(arguments.thread_id)?;

        self.sender
            .send(Event::Continued {
                thread_id: arguments.thread_id,
                all_threads_continued: None,
            })
            .map_err(|_| Error::ReceiverDead)?;

        Ok(ContinueResponse {
            all_threads_continued: None,
        })
    }

    pub fn step_in(&mut self, arguments: StepInArguments) -> Result<StepInResponse, Error> {
        let reason = self.debugger.step_into(arguments.thread_id)?;
        self.sender
            .send(Event::Stopped {
                reason: match reason {
                    StopReason::Breakpoint { .. } => StoppedReason::Breakpoint,
                    StopReason::Step { .. } => StoppedReason::Step,
                    StopReason::Watchpoint { .. } => StoppedReason::DataBreakpoint,
                    StopReason::Suspended => StoppedReason::Pause,
                },
                description: Some(reason.reason().to_string()),
                thread_id: Some(arguments.thread_id),
                preserve_focus_hint: None,
                text: Some(reason.to_string()),
                all_threads_stopped: None,
                hit_breakpoint_ids: match reason {
                    StopReason::Breakpoint { breakpoint_ids, .. } => Some(breakpoint_ids),
                    _ => None,
                },
            })
            .map_err(|_| Error::ReceiverDead)?;
        Ok(StepInResponse)
    }

    pub fn step_out(&mut self, arguments: StepOutArguments) -> Result<StepOutResponse, Error> {
        let reason = self.debugger.step_out(arguments.thread_id)?;
        self.sender
            .send(Event::Stopped {
                reason: match reason {
                    StopReason::Breakpoint { .. } => StoppedReason::Breakpoint,
                    StopReason::Step { .. } => StoppedReason::Step,
                    StopReason::Watchpoint { .. } => StoppedReason::DataBreakpoint,
                    StopReason::Suspended => StoppedReason::Pause,
                },
                description: Some(reason.reason().to_string()),
                thread_id: Some(arguments.thread_id),
                preserve_focus_hint: None,
                text: Some(reason.to_string()),
                all_threads_stopped: None,
                hit_breakpoint_ids: match reason {
                    StopReason::Breakpoint { breakpoint_ids, .. } => Some(breakpoint_ids),
                    _ => None,
                },
            })
            .map_err(|_| Error::ReceiverDead)?;
        Ok(StepOutResponse)
    }

    pub fn breakpoint_locations(
        &mut self,
        arguments: BreakpointLocationsArguments,
    ) -> BreakpointLocationsResponse {
        BreakpointLocationsResponse {
            breakpoints: vec![],
        }
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
    ) -> Result<StackTraceResponse, Error> {
        let mut stack_frames = Vec::new();

        let backtrace = self.debugger.backtrace(arguments.thread_id)?;

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
                column: 1,
                end_line: None,
                end_column: None,
                can_restart: Some(false),
                instruction_pointer_reference: Some(format!("frame:{}", frame_id)),
                module_id: None,
                presentation_hint: None,
            });
        }

        Ok(StackTraceResponse {
            stack_frames,
            total_frames: backtrace.len(),
        })
    }

    pub fn read_memory(
        &mut self,
        arguments: ReadMemoryArguments,
    ) -> Result<ReadMemoryResponse, Error> {
        let Some(bytes) = self.debugger.read_memory(
            arguments.thread_id.unwrap_or(
                self.debugger
                    .sessions
                    .last()
                    .map(|session| session.id())
                    .unwrap_or_default(),
            ),
            arguments.memory_reference.to_owned(),
            arguments.offset,
            arguments.count,
        )?
        else {
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
        let Some(disassembled) = self.debugger.disassemble(
            arguments.thread_id.unwrap_or(
                self.debugger
                    .sessions
                    .last()
                    .map(|session| session.id())
                    .unwrap_or_default(),
            ),
            arguments.memory_reference,
            arguments.instruction_offset,
            arguments.instruction_count,
        )?
        else {
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
}
