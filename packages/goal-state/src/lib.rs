pub mod doc;
pub mod formatter;
mod goal_state_of_po_res;
pub mod operators;

pub use formatter::prettify;
pub use goal_state_of_po_res::{
    NoGoalState, format_goal_state, with_po_res_from_twine, with_po_res_from_zip,
};
