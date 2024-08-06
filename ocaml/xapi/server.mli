module Make : functor
  (_ : Custom_actions.CUSTOM_ACTIONS)
  (_ : Custom_actions.CUSTOM_ACTIONS)
  -> sig
  val dispatch_call :
    Http.Request.t -> Unix.file_descr -> Rpc.call -> Rpc.response
end
