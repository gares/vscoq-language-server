## The document Manager

### internal

Some internal modules are actually not private in the `dune` sense

- [scheduler](sheduler.mli) is in charge of analyzing a document and plan
  its execution. It knows which Coq sentences change the parser and which
  sentences bracket a proof. A schedule is built incrementally, as sentences
  are discovered. The component holds a notion of dependency among sentences
  which is used to invalidate upon change. This component is internal.

- [parTactic](parTactic.mli) implements the `par:` combinator based on SEL.
  This component is internal.

- [delegationManager](delegationManager.mli) implements an OS agnostic way to
  delegate a job to a worker process.

- [executionManager](executionManager.mli) holds the Coq state associated to
  sentences which it can execute based on a schedule and potentially delegating
  some work via the delegation manager.

- [document](document.mli) holds the text as seen by the user and its validated
  (parsed) form. Since parsing and execution are entangled in Coq, validation
  stops at the first sentence which has a parsing effect.

### public

- [documentManager](documentManager.mli) is the main entry point for user
  actions, like interpret_to_position.

