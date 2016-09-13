module D = Debug.Make(struct let name="xapi" end)
open D

type serverType = XenServer | ESXServer | VirtualCenter | HyperVServer
type jobState = Created | Queued | Running | Completed | Aborted | UserAborted
type serviceCred = { username:string; password:string}
type serverInfo = {serverType:serverType; hostname:string; cred:serviceCred}
type importInfo = { sRuuid:string }
type jobInfo = {source:serverInfo; sourceVmUUID:string; sourceVmName:string; importInfo:importInfo}
type dateTime = Stdext.Date.iso8601
type jobInstance = {
  id:string; jobName:string; jobDesc:string ; xenServerName:string;
  sRName:string; createdTime:dateTime; startTime:dateTime;
  completedTime:dateTime; errorString:string; compressedBytesRead:int64;
  uncompressedBytesWritten:int64; stateDesc:string;
  percentComplete:int64; state:jobState; clientIpEndPoint:string; jobInfo:jobInfo
}
type vmInstance = {
  uUID:string; name:string; powerState:int32; oSType :string;
  committedStorage:int64; uncommittedStorage:int64; template:bool
}

let rpc_type_error x structname expected =
  let msg = Printf.sprintf "'%s_of_rpc:got '%s' when '%s' was expected"
      structname (Rpc.to_string x) expected in
  raise (Api_errors.Server_error(Api_errors.xenapi_plugin_failure, [ "unexpected XMLRPC result"; msg ]))

let get_dict dict key structname =
  let error_key x =
    let msg = Printf.sprintf "'%s_of_rpc:caught exception '%s' while looking for key '%s' in '%s'\n"
        structname (Printexc.to_string x) key (Rpc.to_string dict) in
    raise (Api_errors.Server_error(Api_errors.xenapi_plugin_failure, [ "unexpected XMLRPC result"; msg ]))
  in
  match dict with
  | Rpc.Dict assoc -> (try List.assoc key assoc with x -> error_key x)
  | x -> rpc_type_error x structname "Dict"

let get_string_dict dict key structname =
  let value = get_dict dict key structname in
  match value with
  | Rpc.String s -> s
  | x -> rpc_type_error x structname "String(string)"

let get_int64_dict dict key structname =
  let value = get_dict dict key structname in
  match value with
  | Rpc.Int v -> v
  | x -> rpc_type_error x structname "Int(int64)"

let get_bool_dict dict key structname =
  let value = get_dict dict key structname in
  match value with
  | Rpc.Bool v -> v
  | x -> rpc_type_error x structname "Bool(bool)"

let string_of_serverType = function
  | XenServer -> "XenServer"
  | ESXServer -> "ESXServer"
  | VirtualCenter -> "VirtualCenter"
  | HyperVServer -> "HyperVServer"

let serverType_of_string = function
  | "default"
  | "XenServer" -> XenServer
  | "ESXServer" -> ESXServer
  | "VirtualCenter" -> VirtualCenter
  | "HyperVServer" -> HyperVServer
  | x -> raise (Api_errors.Server_error (Api_errors.invalid_value, [x]))

let serverType_of_rpc x =
  match x with
  | Rpc.Int n -> (match (Int64.to_int n) with
      | 0 -> XenServer
      | 1 -> ESXServer
      | 2 -> VirtualCenter
      | 3 -> HyperVServer
      | _ -> rpc_type_error x "ServerType" "Int(int64)")
  | y -> rpc_type_error y "ServerType" "Int(int64)"

let rpc_of_serverType = function
  | XenServer -> Rpc.rpc_of_int 0
  | ESXServer -> Rpc.rpc_of_int 1
  | VirtualCenter -> Rpc.rpc_of_int 2
  | HyperVServer -> Rpc.rpc_of_int 3

let string_of_jobState = function
  | Created -> "Created"
  | Queued -> "Queued"
  | Running -> "Running"
  | Completed -> "Completed"
  | Aborted -> "Aborted"
  | UserAborted -> "UserAborted"

let jobState_of_rpc x =
  match x with
  | Rpc.Int n ->
    (match (Int64.to_int n) with
     | 0 -> Created
     | 1 -> Queued
     | 2 -> Running
     | 3 -> Completed
     | 4 -> Aborted
     | 5 -> UserAborted
     | _ -> rpc_type_error x "JobState" "Int(int64)")
  | e -> rpc_type_error e "JobState" "Int(int64)"

let rpc_of_jobState = function
  | Created -> Rpc.rpc_of_int 0
  | Queued -> Rpc.rpc_of_int 1
  | Running -> Rpc.rpc_of_int 2
  | Completed -> Rpc.rpc_of_int 3
  | Aborted -> Rpc.rpc_of_int 4
  | UserAborted -> Rpc.rpc_of_int 5

let rpc_of_serviceCred v =
  Rpc.Dict [
    ("Username", (Rpc.String v.username));
    ("Password", (Rpc.String v.password))
  ]

let serviceCred_of_rpc r =
  let sn = "ServiceCred" in
  {
    username = (get_string_dict r "Username" sn);
    password = (get_string_dict r "Password" sn)
  }

let rpc_of_serverInfo v =
  Rpc.Dict [
    "ServerType", (rpc_of_serverType v.serverType);
    "Hostname", (Rpc.String v.hostname); "Username", (Rpc.String v.cred.username);
    "Password", (Rpc.String v.cred.password)
  ]

let serverInfo_of_rpc r =
  let sn = "ServerInfo" in
  {
    serverType = serverType_of_rpc (get_dict r "ServerType" sn);
    hostname = get_string_dict r "Hostname" sn;
    cred = {
      username = get_string_dict r "Username" sn;
      password = get_string_dict r "Password" sn;
    }
  }
let rpc_of_dateTime v = Rpc.DateTime (Stdext.Date.to_string v)

let dateTime_of_rpc r =
  match r with
  | Rpc.DateTime v -> Stdext.Date.of_string v
  | x -> rpc_type_error x "DateTime" "DateTime(datetime)"

let rpc_of_importInfo v = Rpc.Dict[ "SRuuid", (Rpc.String v.sRuuid) ]

let importInfo_of_rpc r = {sRuuid = get_string_dict r "SRuuid" "ImportInfo"}

let rpc_of_jobInfo v =
  Rpc.Dict [
    "Source", (rpc_of_serverInfo v.source);
    "SourceVmUUID", (Rpc.String v.sourceVmUUID);
    "SourceVmName", (Rpc.String v.sourceVmName);
    "ImportInfo", (rpc_of_importInfo v.importInfo)
  ]

let jobInfo_of_rpc r =
  let sn = "JobInfo" in
  {
    source = serverInfo_of_rpc (get_dict r "Source" sn);
    sourceVmUUID = get_string_dict r "SourceVmUUID" sn;
    sourceVmName = get_string_dict r "SourceVmName" sn;
    importInfo = importInfo_of_rpc (get_dict r "ImportInfo" sn)
  }

let rpc_of_jobInstance v =
  Rpc.Dict [
    "Id", (Rpc.String v.id);
    "JobName", (Rpc.String v.jobName);
    "JobDesc", (Rpc.String v.jobDesc);
    "XenServerName", (Rpc.String v.xenServerName);
    "SRName", (Rpc.String v.sRName);
    "CreatedTime", (rpc_of_dateTime v.createdTime);
    "StartTime", (rpc_of_dateTime v.startTime);
    "CompletedTime", (rpc_of_dateTime v.completedTime);
    "ErrorString", (Rpc.String v.errorString);
    "CompressedBytesRead", (Rpc.Int v.compressedBytesRead);
    "UncompressedBytesWritten", (Rpc.Int v.uncompressedBytesWritten);
    "StateDesc", (Rpc.String v.stateDesc);
    "PercentComplete", (Rpc.Int v.percentComplete);
    "State", (rpc_of_jobState v.state);
    "ClientIpEndPoint", (Rpc.String v.clientIpEndPoint);
    "JobInfo", (rpc_of_jobInfo v.jobInfo)
  ]

let jobInstance_of_rpc r =
  let sn = "JobInstance" in
  {
    id = get_string_dict r "Id" sn;
    jobName = get_string_dict r "JobName" sn;
    jobDesc = get_string_dict r "JobDesc" sn;
    xenServerName = get_string_dict r "XenServerName" sn;
    sRName = get_string_dict r "SRName" sn;
    createdTime = dateTime_of_rpc (get_dict r "CreatedTime" sn);
    startTime = dateTime_of_rpc (get_dict r "StartTime" sn);
    completedTime = dateTime_of_rpc (get_dict r "CompletedTime" sn);
    errorString = get_string_dict r "ErrorString" sn;
    compressedBytesRead = get_int64_dict r "CompressedBytesRead" sn;
    uncompressedBytesWritten = get_int64_dict r "UncompressedBytesWritten" sn;
    stateDesc = get_string_dict r "StateDesc" sn;
    percentComplete = get_int64_dict r "PercentComplete" sn;
    state = jobState_of_rpc (get_dict r "State" sn);
    clientIpEndPoint = get_string_dict r "ClientIpEndPoint" sn;
    jobInfo = jobInfo_of_rpc (get_dict r "JobInfo" sn)
  }

let rpc_of_vmInstance v =
  Rpc.Dict [
    "UUID", (Rpc.String v.uUID);
    "Name", (Rpc.String v.name);
    "PowerState", (Rpc.Int (Int64.of_int32 v.powerState));
    "OSType", (Rpc.String v.oSType);
    "CommittedStorage", (Rpc.Int v.committedStorage);
    "UncommittedStorage", (Rpc.Int v.uncommittedStorage);
    "Template", (Rpc.Bool v.template)
  ]

let vmInstance_of_rpc r =
  let sn = "VmInstance" in
  {
    uUID = get_string_dict r "UUID" sn;
    name = get_string_dict r "Name" sn;
    powerState = Int64.to_int32 (get_int64_dict r "PowerState" sn);
    oSType = get_string_dict r "OSType" sn;
    committedStorage = get_int64_dict r "CommittedStorage" sn;
    uncommittedStorage = get_int64_dict r "UncommittedStorage" sn;
    template = get_bool_dict r "Template" sn
  }

let array_of_rpc lr typename =
  match lr with
  | Rpc.Enum l -> l
  | x -> rpc_type_error x ("[" ^ typename ^ "]") "Enum(t)"

let array_of_vmInstances_of_rpc lr =
  let l = array_of_rpc lr "VmInstance" in
  List.map (fun r -> vmInstance_of_rpc r) l

let array_of_jobInstances_of_rpc lr =
  let l = array_of_rpc lr "JobInstance" in
  List.map (fun r -> jobInstance_of_rpc r) l

let vpxrpc ip call =
  let open Xmlrpc_client in
  let transport = SSL(SSL.make (), ip, 443) in
  (*	debug "call = %s" (Xmlrpc.string_of_call call); *)
  XMLRPC_protocol.rpc ~transport:transport
    ~http:(xmlrpc ~version:"1.0" "/") call
