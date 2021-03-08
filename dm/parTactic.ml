
let debug_par = CDebug.create ~name:"vscoq.parTactic" ()

let log msg = debug_par Pp.(fun () ->
  str @@ Format.asprintf "        [%d] %s" (Unix.getpid ()) msg)

type sentence_id = Stateid.t

module TacticJob = struct
  type solution =
    | Solved of Constr.t * UState.t
    | NoProgress
    | Error of Pp.t
  type update_request =
    | UpdateSolution of Evar.t * solution
    | AppendFeedback of sentence_id * (Feedback.level * Loc.t option * Pp.t)
  let appendFeedback id fb = AppendFeedback(id,fb)

  type t =  {
    state    : Vernacstate.t;
    ast      : ComTactic.interpretable;
    goalno   : int;
    goal     : Goal.goal;
    name     : string }
  let name = "tactic"
  let binary_name = "vscoqtop_tactic_worker.opt"
  let pool_size = 2

end

module TacticWorker = DelegationManager.MakeWorker(TacticJob)


let assign_tac ~abstract res : unit Proofview.tactic =
  Proofview.(Goal.enter begin fun g ->
    let gid = Goal.goal g in
    try
      let pt, uc = List.assoc gid res in
      let open Notations in
      let push_state ctx =
          Proofview.tclEVARMAP >>= fun sigma ->
          Proofview.Unsafe.tclEVARS (Evd.merge_universe_context sigma ctx)
      in
      (if abstract then Abstract.tclABSTRACT None else (fun x -> x))
          (push_state uc <*> Tactics.exact_no_check (EConstr.of_constr pt))
    with Not_found ->
      log @@ "nothing for " ^ Pp.string_of_ppcmds @@ Evar.print gid;
      tclUNIT ()
  end)

let command_focus = Proof.new_focus_kind ()

let worker_solve_one_goal { TacticJob.state; ast; goalno; goal; name } ~send_back =
  let focus_cond = Proof.no_cond command_focus in
  Vernacstate.unfreeze_interp_state state;
  try
    Vernacstate.LemmaStack.with_top (Option.get state.Vernacstate.lemmas) ~f:(fun pstate ->
    let pstate = Declare.Proof.map pstate ~f:(Proof.focus focus_cond () goalno) in
    let pstate = ComTactic.solve ~pstate Goal_select.SelectAll ~info:None ast ~with_end_tac:false in
    let { Proof.sigma } = Declare.Proof.fold pstate ~f:Proof.data in
    match Evd.(evar_body (find sigma goal)) with
    | Evd.Evar_empty ->
        log @@ "no progress on goal " ^ Pp.string_of_ppcmds (Goal.pr_goal goal);
        send_back (TacticJob.UpdateSolution (goal,TacticJob.NoProgress))
    | Evd.Evar_defined t ->
        let t = Evarutil.nf_evar sigma t in
        let evars = Evarutil.undefined_evars_of_term sigma t in
        if Evar.Set.is_empty evars then
          let t = EConstr.Unsafe.to_constr t in
          log @@ "closed goal " ^ Pp.string_of_ppcmds (Goal.pr_goal goal);
          send_back (TacticJob.UpdateSolution (goal,TacticJob.Solved(t, Evd.evar_universe_context sigma)))
        else
          CErrors.user_err ~hdr:"parTactic"
            Pp.(str"The par: selector requires a tactic that makes no progress or fully" ++
                str" solves the goal and leaves no unresolved existential variables. The following" ++
                str" existentials remain unsolved: " ++ prlist (Termops.pr_existential_key sigma) (Evar.Set.elements evars))
    )
  with e when CErrors.noncritical e ->
    send_back (TacticJob.UpdateSolution (goal, TacticJob.Error Pp.(CErrors.print e ++ spc() ++ str "(for subgoal "++int goalno ++ str ")")))

let interp_par ~pstate ~info ast ~abstract ~with_end_tac : Declare.Proof.t =
  let state = Vernacstate.freeze_interp_state ~marshallable:true in
  let queue = Queue.create () in
  let events, job_ids = List.split @@
    Declare.Proof.fold pstate ~f:(fun p ->
     (Proof.data p).Proof.goals |> CList.map_i (fun goalno goal ->
       let job = { TacticJob.state; ast; goalno = goalno + 1; goal; name = Goal.uid goal} in
       let job_id = DelegationManager.mk_job_id () in
       Queue.push (job_id,job) queue;
       TacticWorker.worker_available ~jobs:queue ~fork_action:worker_solve_one_goal, job_id
        ) 0) in
  let rec wait ready evs =
    log @@ "waiting for events: " ^ string_of_int @@ List.length evs;
    let more_ready, waiting = Sel.wait evs in
    let updates, more_waiting_l = List.split (List.map TacticWorker.handle_event more_ready) in
    let rec do_updates acc = function
      | [] ->
          let events = List.concat (waiting :: more_waiting_l) in
          if events = [] then (log @@ "done waiting for tactic workers"; acc)
          else wait acc events
      | None :: updates -> do_updates acc updates
      | Some(TacticJob.UpdateSolution(ev,TacticJob.Solved(c,u))) :: updates ->
          log @@ "got solution for evar " ^ Pp.string_of_ppcmds @@ Evar.print ev;
          do_updates ((ev,(c,u)) :: acc) updates
      | Some(TacticJob.AppendFeedback _) :: updates ->
          log @@ "got feedback";
          do_updates acc updates
      | Some(TacticJob.UpdateSolution(ev,TacticJob.NoProgress)) :: updates ->
          log @@ "got no progress for " ^ Pp.string_of_ppcmds @@ Evar.print ev;
          do_updates acc updates
      | Some(TacticJob.UpdateSolution(ev,TacticJob.Error err)) :: updates ->
          log @@ "got error for " ^ Pp.string_of_ppcmds @@ Evar.print ev;
          List.iter DelegationManager.cancel_job job_ids;
          CErrors.user_err ~hdr:"parTactic" err in
    do_updates ready updates in
  let results = wait [] events in
  Declare.Proof.map pstate ~f:(fun p ->
    let p,_,() = Proof.run_tactic (Global.env()) (assign_tac ~abstract results) p in
    p)

let () = ComTactic.set_par_implementation interp_par

module TacticWorkerProcess = struct
  type options = TacticWorker.options
  let parse_options = TacticWorker.parse_options
  let main ~st:initial_vernac_state options =
    let send_back, job = TacticWorker.setup_plumbing options in
    worker_solve_one_goal job ~send_back;
    exit 0
  let log = TacticWorker.log
end

