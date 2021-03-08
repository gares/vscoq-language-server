(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2019       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* This component handles delegation to workers. It gathers all the code for
   process management across supported OSes (fork on Unix, create_process +
   marshall on Windows).
   
   For each job to delegate one has to instantiaet the MakeWorker functor
   passin a Job description. Since on Windows we can't fork, one has to
   provide a Job.binary_name to invoke. That process must be a Coq toplevel
   parsing extra argument using [parse_options] then sets up
   a link with master via [setup_plumbing] which provides a function to be
   called to send updates back *)

type sentence_id = Stateid.t

module type Job = sig
  (** The data out of which the job can be performed. Must be marshalable *)
  type t

  (** Used to craft a job specific command line option for
     the worker process used when fork is not available *)
  val name : string

  (** Name of the worker process binary used when fork is not available *)
  val binary_name : string

  (** Max number of workers for this kind job *)
  val pool_size : int

  type update_request

  (** Called to handle feedback sent by the worker process *)
  val appendFeedback : sentence_id -> (Feedback.level * Loc.t option * Pp.t) -> update_request
end

type execution_status =
  | Success of Vernacstate.t option
  | Error of string Loc.located * Vernacstate.t option (* State to use for resiliency *)

type job_id
val cancel_job : job_id -> unit
val mk_job_id : unit -> job_id

module MakeWorker (Job : Job) : sig

(** Event for the main loop *)
type delegation
val pr_event : delegation -> Pp.t
type events = delegation Sel.event list

(** handling an event may require an update to a sentence in the exec state,
    e.g. when a feedback is received *)
val handle_event : delegation -> (Job.update_request option * events)

(* When a worker is available and the [jobs] queue can be popped the
   event becomes ready; in turn the event triggers the action:
   - if we can fork, job is passed to fork_action
   - otherwise Job.binary_name is spawn and the job sent to it *)
val worker_available :
  jobs:((job_id * Job.t) Queue.t) ->
  fork_action:(Job.t -> send_back:(Job.update_request -> unit) -> unit) ->
  delegation Sel.event

(* for worker toplevels *)
type options
val parse_options : string list -> options * string list
(* the sentence ids of the remote_mapping being delegated *)
val setup_plumbing : options -> ((Job.update_request -> unit) * Job.t)

(* CDebug aware print *)
val log : string -> unit

end

(* To be put in in the initial set of events in order to receive locally
   feedback which generate remotely *)
val local_feedback : (sentence_id * (Feedback.level * Loc.t option * Pp.t)) Sel.event
