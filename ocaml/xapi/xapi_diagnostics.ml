module D = Debug.Make(struct let name="xapi_diagnostics" end)
open D

let gc_compact ~__context ~session ~host =
  raise Api_errors.(Server_error(not_implemented, [ "gc_compact" ]))

let gc_stats ~__context ~session ~host =
  raise Api_errors.(Server_error(not_implemented, [ "gc_stats" ]))

let db_stats ~__context ~session ~host =
  raise Api_errors.(Server_error(not_implemented, [ "db_stats" ]))

let license_stats ~__context ~session ~host =
  raise Api_errors.(Server_error(not_implemented, [ "license_stats" ]))

let network_stats ~__context ~session ~host =
  raise Api_errors.(Server_error(not_implemented, [ "network_stats" ]))

