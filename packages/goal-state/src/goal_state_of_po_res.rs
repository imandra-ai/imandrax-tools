use std::io::Read as _;
use std::path::Path;

use anyhow::{Context, Result};
use bumpalo::Bump;
use imandrax_api::*;

use crate::formatter::TermFormatter;

type Term<'a> = MirTerm<'a>;
type Ty<'a> = MirType<'a>;
type Sequent<'a> = CommonSequentT_poly<'a, &'a Term<'a>>;
#[allow(non_camel_case_types)]
type PO_res<'a> = TasksPO_resShallow_poly<'a, Term<'a>, Ty<'a>>;

/// A version of `TasksPO_resShallow_poly` for deserializing from artifact twine,
/// where the `from_` field is a tagged store pointer that the standard type can't decode.
#[allow(non_camel_case_types, dead_code)]
pub struct PO_res_from_art<'a> {
    pub res: &'a core::result::Result<
        &'a TasksPO_resSuccess<'a, Term<'a>, Ty<'a>>,
        &'a TasksPO_resError<'a, Term<'a>, Ty<'a>>,
    >,
    pub stats: Stat_time,
    pub report: &'a In_mem_archiveRaw<'a>,
    pub sub_res: &'a [&'a [&'a TasksPO_resSub_res<'a, Term<'a>>]],
}

impl<'a> FromTwine<'a> for PO_res_from_art<'a> {
    fn read(
        d: &twine::Decoder<'a>,
        bump: &'a Bump,
        off: twine::types::Offset,
    ) -> anyhow::Result<Self> {
        let mut offsets = vec![];
        d.get_array(off, &mut offsets)?;
        anyhow::ensure!(
            offsets.len() == 5,
            "expected 5-element array for PO_res, got {}",
            offsets.len()
        );
        // offsets[0] is `from_` (tagged store pointer) — skip it
        Ok(PO_res_from_art {
            res: FromTwine::read(d, bump, offsets[1])?,
            stats: FromTwine::read(d, bump, offsets[2])?,
            report: FromTwine::read(d, bump, offsets[3])?,
            sub_res: FromTwine::read(d, bump, offsets[4])?,
        })
    }
}

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

/// Read twine binary bytes from an artifact and deserialize, then call `f` with the result.
///
/// Uses `PO_res_from_art` internally to handle the tagged `from_` field in artifact format.
pub fn with_po_res_from_twine<R>(
    data: &[u8],
    f: impl for<'a> FnOnce(&PO_res_from_art<'a>) -> R,
) -> Result<R> {
    let decoder = twine::Decoder::new(data)?;
    let bump = Bump::new();
    let entry = decoder.entrypoint()?;
    let po_res: PO_res_from_art = FromTwine::read(&decoder, &bump, entry)?;
    Ok(f(&po_res))
}

/// Read a PO_res from a ZIP file, extract `data.twine`, and deserialize.
pub fn with_po_res_from_zip<R>(
    path: &Path,
    f: impl for<'a> FnOnce(&PO_res_from_art<'a>) -> R,
) -> Result<R> {
    let file =
        std::fs::File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    let mut archive = zip::ZipArchive::new(file)?;
    let mut entry = archive
        .by_name("data.twine")
        .context("data.twine not found in archive")?;
    let mut data = Vec::with_capacity(entry.size() as usize);
    entry.read_to_end(&mut data)?;
    with_po_res_from_twine(&data, f)
}

#[allow(non_camel_case_types)]
type Res<'a> = core::result::Result<
    &'a TasksPO_resSuccess<'a, Term<'a>, Ty<'a>>,
    &'a TasksPO_resError<'a, Term<'a>, Ty<'a>>,
>;

fn format_goal_state_from_res(res: &Res) -> Result<String, NoGoalState> {
    let fmt = TermFormatter::new(WIDTH);
    let subgoals: Vec<&Sequent> = match res {
        Ok(_) => return Err(NoGoalState::Proved),
        Err(TasksPO_resError::No_proof(np)) if np.counter_model.is_none() => np.subgoals.to_vec(),
        Err(TasksPO_resError::No_proof(_)) => return Err(NoGoalState::CounterModel),
        _ => {
            return Err(NoGoalState::Error);
        }
    };

    Ok(format_subgoals(&fmt, true, &subgoals))
}

pub fn format_goal_state(po_res: &PO_res) -> Result<String, NoGoalState> {
    format_goal_state_from_res(po_res.res)
}

pub fn format_goal_state_from_art(po_res: &PO_res_from_art) -> Result<String, NoGoalState> {
    format_goal_state_from_res(po_res.res)
}
