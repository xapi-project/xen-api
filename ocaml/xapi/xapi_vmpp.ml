(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
module D = Debug.Make(struct let name="xapi" end)
open D

let raise_removed () =
  raise (Api_errors.Server_error (Api_errors.message_removed, []))

let protect_now ~__context ~vmpp =
  raise_removed ()

let archive_now ~__context ~snapshot =
  raise_removed ()

let add_to_recent_alerts ~__context ~vmpp ~value =
  raise_removed ()

let create_alert ~__context ~vmpp ~name ~priority ~body ~data =
  raise_removed ()

let get_alerts ~__context ~vmpp ~hours_from_now =
  raise_removed ()

let set_is_backup_running ~__context ~self ~value =
  raise_removed ()

let set_is_archive_running ~__context ~self ~value =
  raise_removed ()

let set_backup_frequency ~__context ~self ~value =
  raise_removed ()

let set_archive_frequency ~__context ~self ~value =
  raise_removed ()

let set_archive_target_type ~__context ~self ~value =
  raise_removed ()

let set_is_alarm_enabled ~__context ~self ~value =
  raise_removed ()

let set_backup_schedule ~__context ~self ~value =
  raise_removed ()

let add_to_backup_schedule ~__context ~self ~key ~value =
  raise_removed ()

let set_archive_target_config ~__context ~self ~value =
  raise_removed ()

let add_to_archive_target_config ~__context ~self ~key ~value =
  raise_removed ()

let set_archive_schedule ~__context ~self ~value =
  raise_removed ()

let add_to_archive_schedule ~__context ~self ~key ~value =
  raise_removed ()

let set_alarm_config ~__context ~self ~value =
  raise_removed ()

let add_to_alarm_config ~__context ~self ~key ~value =
  raise_removed ()

let remove_from_backup_schedule ~__context ~self ~key =
  raise_removed ()

let remove_from_archive_target_config ~__context ~self ~key =
  raise_removed ()

let remove_from_archive_schedule ~__context ~self ~key =
  raise_removed ()

let remove_from_alarm_config ~__context ~self ~key =
  raise_removed ()

let set_backup_last_run_time ~__context ~self ~value =
  raise_removed ()

let set_archive_last_run_time ~__context ~self ~value =
  raise_removed ()

let set_backup_retention_value ~__context ~self ~value =
  raise_removed ()

let create ~__context ~name_label ~name_description ~is_policy_enabled
    ~backup_type ~backup_retention_value ~backup_frequency ~backup_schedule
    ~archive_target_type ~archive_target_config ~archive_frequency ~archive_schedule
    ~is_alarm_enabled ~alarm_config
  : API.ref_VMPP =
  raise_removed ()

let destroy ~__context ~self =
  raise_removed ()

