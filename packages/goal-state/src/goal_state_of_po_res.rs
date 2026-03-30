use imandrax_api::*;

use crate::formatter::TermFormatter;

type Term<'a> = MirTerm<'a>;
type Ty<'a> = MirType<'a>;
type Sequent<'a> = CommonSequentT_poly<'a, &'a Term<'a>>;
#[allow(non_camel_case_types)]
type PO_res<'a> = TasksPO_resShallow_poly<'a, Term<'a>, Ty<'a>>;

const WIDTH: usize = 80;

fn format_term(fmt: &TermFormatter, labeled: &(Option<&str>, &Term)) -> String {
    let (label, term) = labeled;
    let pretty = fmt.prettify(term);
    match label {
        Some(lbl) => format!("{}: {}", lbl, pretty),
        None => pretty,
    }
}

fn format_sequent(fmt: &TermFormatter, sg: &Sequent) -> String {
    let hyps: Vec<String> = sg.hyps.iter().map(|h| format_term(fmt, h)).collect();
    let hyps_str = hyps.join(", ");
    let concls: Vec<String> = sg.concls.iter().map(|c| format_term(fmt, c)).collect();
    let concls_str = concls.join(", ");
    if hyps_str.is_empty() {
        format!("⊢ {}", concls_str)
    } else {
        format!("{} ⊢ {}", hyps_str, concls_str)
    }
}

fn format_subgoals(fmt: &TermFormatter, ok: bool, subgoals: &[&Sequent]) -> String {
    let show_proven = false;
    let no_subgoals = subgoals.is_empty();

    if !(show_proven || !no_subgoals || (no_subgoals && !ok)) {
        return String::new();
    }

    let mut buf = String::with_capacity(512);

    if subgoals.is_empty() {
        if ok {
            buf.push_str("∎\n");
        } else {
            buf.push_str("No subgoals reported after error.\n");
        }
    } else {
        let k = subgoals.len();
        buf.push_str(&format!("{} subgoal{} ", k, if k > 1 { "s" } else { "" }));
        for sg in subgoals {
            buf.push_str(&format!("-\n {}\n", format_sequent(fmt, sg)));
        }
    }
    buf.push('\n');
    buf
}

pub enum NoGoalState {
    Proved,
    CounterModel,
    Error,
}

pub fn format_goal_state(po_res: &PO_res) -> Result<String, NoGoalState> {
    let fmt = TermFormatter::new(WIDTH);
    let subgoals: Vec<&Sequent> = match po_res.res {
        Ok(_) => return Err(NoGoalState::Proved),
        Err(TasksPO_resError::No_proof(np)) if np.counter_model.is_none() => np.subgoals.to_vec(),
        Err(TasksPO_resError::No_proof(_)) => return Err(NoGoalState::CounterModel),
        _ => {
            return Err(NoGoalState::Error);
        }
    };

    Ok(format_subgoals(&fmt, true, &subgoals))
}
