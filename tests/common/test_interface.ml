
module Interface(R : Idl.RPC) = struct
  open R

  let int_p = Idl.Param.mk Rpc.Types.int
  let int_p_named_1 = Idl.Param.mk ~name:"int1" ~description:["first int param"] Rpc.Types.int
  let int_p_named_2 = Idl.Param.mk ~name:"int2" ~description:["second int param"] Rpc.Types.int
  let int_p_result = Idl.Param.mk ~name:"int" ~description:["int result"] Rpc.Types.int

  let add = R.declare "add"
      ["Add two numbers"]
      (int_p @-> int_p @-> returning int_p Idl.DefaultError.err)

  let sub = R.declare "sub"
      ["Subtract two numbers"]
      (int_p_named_1 @-> int_p @-> returning int_p Idl.DefaultError.err)

  let mul = R.declare "mul"
      ["Multiply two numbers"]
      (int_p_named_1 @-> int_p_named_2 @-> returning int_p Idl.DefaultError.err)

  let div = R.declare "div"
      ["Divide two numbers"]
      (int_p_named_1 @-> int_p_named_2 @-> returning int_p_result Idl.DefaultError.err)

  let implementation = implement
      { Idl.Interface.name = "Calc"; namespace = Some "Calc"; description = ["interface"]; version = (1,0,0) }
end

