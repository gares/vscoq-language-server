(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2019       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** This toplevel implements an LSP-based server language for VsCode,
    used by the VsCoq extension. *)

let log msg = Format.eprintf "%d] @[%s@]@\n%!" (Unix.getpid ()) msg

let main_worker options ~opts:_ state =
  let initial_vernac_state = Vernacstate.freeze_interp_state ~marshallable:false in
  try ExecutionManager.TacticWorkerProcess.main ~st:initial_vernac_state options
  with exn ->
    let bt = Printexc.get_backtrace () in
    log Printexc.(to_string exn);
    log bt;
    flush_all ()

let vscoqtop_specific_usage = Usage.{
  executable_name = "vscoqtop";
  extra_args = "";
  extra_options = "";
}


let _ =
  Coqinit.init_ocaml ();
  let opts, emoptions = Coqinit.parse_arguments ~parse_extra:ExecutionManager.TacticWorkerProcess.parse_options ~usage:vscoqtop_specific_usage () in
  let injections = Coqinit.init_runtime opts in
  Coqinit.start_library ~top:Coqargs.(dirpath_of_top opts.config.logic.toplevel_name) injections;
  log @@ "[PW] started";
  Sys.(set_signal sigint Signal_ignore);
  main_worker emoptions
