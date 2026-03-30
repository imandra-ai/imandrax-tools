use std::path::Path;

use goal_state::{format_goal_state, with_po_res_from_zip};
use insta::assert_debug_snapshot;

#[test]
fn read_po_res_art_zip() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/test_data/po_res_art.zip");
    let result = with_po_res_from_zip(&path, |po_res| format_goal_state(po_res));
    assert_debug_snapshot!(result, @r#"
    Ok(
        Ok(
            "1 subgoal -\n ⊢ List.length (x @ y) = List.length x + List.length y\n\n",
        ),
    )
    "#);
}
