from typing import Final

TPL_SIG: Final[str] = """\
(** A symbolic template of events *)
module type TPL_SIG = sig
  type state
  (** Type of the state machine state *)

  type t
  (** Symbolic event *)

  type c
  (** Concrete event *)

  val concrete : t -> c -> state -> bool
  (** Mapping from a symbolic event to the concrete event it matches *)
end
"""

SM_SIG: Final[str] = """\
(** Representation of a (potentially infinite) state machine *)
module type SM_SIG = sig
  type state
  (** Type of the state machine state *)

  type event
  (** Type of a state machine event *)

  val init_state : state
  (** Initial state of the state machine *)

  val step : event -> state -> state
  (** Transition function of the state machine *)

  val is_valid : event -> state -> bool
  (** Validity function for a transition of the state machine *)
end
"""

DSM_SIG: Final[str] = """\
(** Representation of a symbolically decomposible (potentially infinite) state machine *)
module type DSM_SIG = sig
  module State_machine : SM_SIG
  (** State machine to decompose *)

  module Template :
    TPL_SIG
      with type c = State_machine.event
      with type state = State_machine.state
  (** Symbolic template of events *)

  val module_name : string
  (** Reflected name of the current module *)
end
"""


SIG = """\
module type SM = sig
    type state
    type event
    val is_valid : event -> state -> bool
    val step : event -> state -> state
    val initial_state : state
end

module Sm : SM

module type TPL = sig
    type t
    type c
    val concrete : t -> c -> Sm.state -> bool
end

val message_flows : Tpl.t list
"""
