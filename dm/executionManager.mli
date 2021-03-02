(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2019       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** The execution manager is in charge of the actual execution of tasks (as
    defined by the scheduler), caching execution states and invalidating
    them. *)

open Scheduler
open Document

type sentence_id = Stateid.t
type ast = Vernacexpr.vernac_control

type state
(** Execution state, includes the cache *)

type progress_hook = unit -> unit

type execution
type events = execution Sel.event list

val local_feedback : execution Sel.event

val handle_event : execution -> state -> (state option * events)
val pr_event : execution -> Pp.t

type prepared_task

val build_tasks_for : progress_hook:progress_hook -> document -> state -> sentence_id -> Vernacstate.t * prepared_task list

val query : sentence_id -> state -> ast -> state

val invalidate : schedule -> sentence_id -> state -> state

val execute : doc_id:Feedback.doc_id -> state -> Vernacstate.t * events * bool -> prepared_task -> (state * Vernacstate.t * events * bool)

val errors : state -> (sentence_id * Loc.t option * string) list
val feedback : state -> (sentence_id * Feedback.level * Loc.t option * string) list
val shift_locs : state -> int -> int -> state
val executed_ids : state -> sentence_id list
val is_executed : state -> sentence_id -> bool
val is_remotely_executed : state -> sentence_id -> bool

val get_parsing_state_after : state -> sentence_id -> Vernacstate.Parser.t option
val get_proofview : state -> sentence_id -> Proof.data option

val init_master : Vernacstate.t -> state

module ProofWorkerProcess : sig
  type options
  val parse_options : string list -> options * string list
  val main : st:Vernacstate.t -> options -> unit
end

module TacticWorkerProcess : sig
  type options
  val parse_options : string list -> options * string list
  val main : st:Vernacstate.t -> options -> unit
end
