use ::goal_state::{NoGoalState, format_goal_state, with_po_res_from_twine, with_po_res_from_zip};
use pyo3::create_exception;
use pyo3::exceptions::PyRuntimeError;
use pyo3::prelude::*;

create_exception!(
    imandrax_tools.goal_state,
    GoalStateProved,
    pyo3::exceptions::PyException
);
create_exception!(
    imandrax_tools.goal_state,
    GoalStateCounterModel,
    pyo3::exceptions::PyException
);

fn no_goal_state_to_err(e: NoGoalState) -> PyErr {
    match e {
        NoGoalState::Proved => GoalStateProved::new_err("proof is complete"),
        NoGoalState::CounterModel => GoalStateCounterModel::new_err("counter-model found"),
        NoGoalState::Error => PyRuntimeError::new_err("error in proof object"),
    }
}

/// Format goal state from raw twine binary data.
///
/// Returns the formatted goal state string when there are open subgoals.
/// Raises ``GoalStateProved`` if the proof is complete.
/// Raises ``GoalStateCounterModel`` if a counter-model was found.
/// Raises ``RuntimeError`` for other errors.
#[pyfunction]
fn format_goal_state_from_bytes(data: &[u8]) -> PyResult<String> {
    let result = with_po_res_from_twine(data, |po_res| format_goal_state(po_res))
        .map_err(|e| PyRuntimeError::new_err(e.to_string()))?;
    result.map_err(no_goal_state_to_err)
}

/// Format goal state from a ``po_res_art.zip`` file path.
///
/// Returns the formatted goal state string when there are open subgoals.
/// Raises ``GoalStateProved`` if the proof is complete.
/// Raises ``GoalStateCounterModel`` if a counter-model was found.
/// Raises ``RuntimeError`` for I/O or other errors.
#[pyfunction]
fn format_goal_state_from_zip(path: &str) -> PyResult<String> {
    let path = std::path::Path::new(path);
    let result = with_po_res_from_zip(path, |po_res| format_goal_state(po_res))
        .map_err(|e| PyRuntimeError::new_err(e.to_string()))?;
    result.map_err(no_goal_state_to_err)
}

#[pymodule]
fn goal_state(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(format_goal_state_from_bytes, m)?)?;
    m.add_function(wrap_pyfunction!(format_goal_state_from_zip, m)?)?;
    m.add("GoalStateProved", m.py().get_type::<GoalStateProved>())?;
    m.add(
        "GoalStateCounterModel",
        m.py().get_type::<GoalStateCounterModel>(),
    )?;
    Ok(())
}
