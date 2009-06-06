module D = Debug.Debugger(struct let name="xapi" end) 
open D

let create = Xapi_pif.vLAN_create
let destroy = Xapi_pif.vLAN_destroy
