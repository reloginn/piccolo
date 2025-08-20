use super::{WatchMode, WatchSpec};
use piccolo::Value;

#[derive(Debug, Clone)]
pub struct WatchEntry<'gc> {
    pub spec: WatchSpec<'gc>,
    pub last: Option<Value<'gc>>,
    pub mode: WatchMode,
    pub last_line_seen: Option<(String, usize)>,
}
