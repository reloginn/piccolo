use super::Location;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StopReason {
    Breakpoint {
        location: Location,
        breakpoint_ids: Vec<usize>,
    },
    Step(Location),
    Watchpoint(String),
    Finished,
    Suspended,
    Error,
}
