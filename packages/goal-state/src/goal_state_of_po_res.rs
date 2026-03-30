use imandrax_api::*;

type Term<'a> = MirTerm<'a>;
type Ty<'a> = MirType<'a>;
type Sequent<'a> = CommonSequentT_poly<'a, &'a Term<'a>>;

#[allow(non_camel_case_types)]
type PO_res<'a> = TasksPO_resShallow_poly<'a, Term<'a>, Ty<'a>>;

pub fn subgoals_of_po_res<'a>(po_res: &PO_res<'a>) -> Vec<&'a Sequent<'a>> {
    match po_res.res {
        Ok(_) => vec![],
        Err(TasksPO_resError::No_proof(no_proof)) if no_proof.counter_model.is_none() => {
            no_proof.subgoals.to_vec()
        }
        Err(_) => vec![],
    }
}
