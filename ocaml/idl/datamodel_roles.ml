(* Datamodel_roles.ml *)

let role_pool_admin = "pool-admin"
let role_pool_operator = "pool-operator"
let role_vm_power_admin = "vm-power-admin"
let role_vm_admin = "vm-admin"
let role_vm_operator = "vm-operator"
let role_read_only = "read-only"
let roles_all =
  [ (* in decreasing total linear order of privileges *)
    role_pool_admin;
    role_pool_operator;
    role_vm_power_admin;
    role_vm_admin;
    role_vm_operator;
    role_read_only
  ]
let role_description = [
  role_pool_admin,"The Pool Administrator role has full access to all features and settings, including accessing Dom0 and managing subjects, roles and external authentication";
  role_pool_operator,"The Pool Operator role manages host- and pool-wide resources, including setting up storage, creating resource pools and managing patches, high availability (HA) and workload balancing (WLB)";
  role_vm_power_admin,"The VM Power Administrator role has full access to VM and template management and can choose where to start VMs and use the dynamic memory control and VM snapshot features";
  role_vm_admin,"The VM Administrator role can manage VMs and templates";
  role_vm_operator,"The VM Operator role can use VMs and interact with VM consoles";
  role_read_only,"The Read-Only role can log in with basic read-only access";
]
(* obtain all roles with at least the specified role privileges *)
let roles_gte role =
  let rec gte = function []->failwith "invalid role"
                       |x::xs->if x=role then x::[] else x::gte xs in
  gte roles_all
(* shortcuts to subsets of greater than or equal roles *)
let _R_LOCAL_ROOT_ONLY = Some([]) (* only local root, emergency and pool-secret *)
let _R_POOL_ADMIN = Some(roles_gte role_pool_admin)
let _R_POOL_OP = Some(roles_gte role_pool_operator)
let _R_VM_POWER_ADMIN = Some(roles_gte role_vm_power_admin)
let _R_VM_ADMIN = Some(roles_gte role_vm_admin)
let _R_VM_OP = Some(roles_gte role_vm_operator)
let _R_READ_ONLY = Some(roles_gte role_read_only) (* = all *)
let _R_ALL = _R_READ_ONLY
