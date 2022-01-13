(* Datamodel_roles.ml *)

let role_pool_admin = "pool-admin"

let role_pool_operator = "pool-operator"

let role_vm_power_admin = "vm-power-admin"

let role_vm_admin = "vm-admin"

let role_vm_operator = "vm-operator"

let role_read_only = "read-only"

let role_client_cert = "client-cert"

let role_name (name, _, _) = name

let role_name_with_description (name, description, _) = (name, description)

let role_name_with_internal (name, _, internal) = (name, internal)

let ordered =
  [
    (* in decreasing total linear order of privileges *)
    ( role_pool_admin
    , "The Pool Administrator role has full access to all features and \
       settings, including accessing Dom0 and managing subjects, roles and \
       external authentication"
    , false
    )
  ; ( role_pool_operator
    , "The Pool Operator role manages host- and pool-wide resources, including \
       setting up storage, creating resource pools and managing patches, high \
       availability (HA) and workload balancing (WLB)"
    , false
    )
  ; ( role_vm_power_admin
    , "The VM Power Administrator role has full access to VM and template \
       management and can choose where to start VMs and use the dynamic memory \
       control and VM snapshot features"
    , false
    )
  ; ( role_vm_admin
    , "The VM Administrator role can manage VMs and templates"
    , false
    )
  ; ( role_vm_operator
    , "The VM Operator role can use VMs and interact with VM consoles"
    , false
    )
  ; ( role_client_cert
    , "The Client Certificate role has access to functionality granted to \
       those who authenticate using a client certificate"
    , true
    )
  ; ( role_read_only
    , "The Read-Only role can log in with basic read-only access"
    , false
    )
  ]

let roles_all = List.map role_name ordered

let role_description = List.map role_name_with_description ordered

let role_internal = List.map role_name_with_internal ordered

(* obtain all roles with at least the specified role privileges *)
let roles_gte role =
  let rec gte = function
    | [] ->
        failwith "invalid role"
    | x :: xs ->
        if x = role then [x] else x :: gte xs
  in
  gte roles_all

(* shortcuts to subsets of greater than or equal roles *)
let _R_LOCAL_ROOT_ONLY = Some []
(* only local root, emergency and pool-secret *)

let _R_POOL_ADMIN = Some (roles_gte role_pool_admin)

let _R_POOL_OP = Some (roles_gte role_pool_operator)

let _R_VM_POWER_ADMIN = Some (roles_gte role_vm_power_admin)

let _R_VM_ADMIN = Some (roles_gte role_vm_admin)

let _R_VM_OP = Some (roles_gte role_vm_operator)

let _R_READ_ONLY = Some (roles_gte role_read_only) (* = all *)

let _R_ALL = _R_READ_ONLY

(* Only client-cert; added to one of the above where required in the datamodel *)
let _R_CLIENT_CERT = Some [role_client_cert]

let union a b =
  match (a, b) with
  | Some x, Some y ->
      Some (x @ y)
  | Some x, None ->
      Some x
  | None, Some y ->
      Some y
  | None, None ->
      None

let ( ++ ) = union
