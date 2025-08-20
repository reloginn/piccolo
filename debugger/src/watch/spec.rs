use piccolo::Value;

#[derive(Debug, Clone)]
pub enum WatchSpec<'gc> {
    Register(usize),
    Global(String),
    TableKey(piccolo::Table<'gc>, Value<'gc>),
}
