use std::path::Path;

use goal_state::{format_goal_state_from_art, with_po_res_from_zip};

#[test]
fn read_po_res_art_zip() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/test_data/po_res_art.zip");
    let result = with_po_res_from_zip(&path, |po_res| format_goal_state_from_art(po_res));
    match result {
        Ok(Ok(goal_state)) => {
            println!("Goal state:\n{goal_state}");
        }
        Ok(Err(no_gs)) => {
            let reason = match no_gs {
                goal_state::NoGoalState::Proved => "proved",
                goal_state::NoGoalState::CounterModel => "counter model",
                goal_state::NoGoalState::Error => "error",
            };
            println!("No goal state: {reason}");
        }
        Err(e) => panic!("Failed to deserialize: {e:#}"),
    }
}
