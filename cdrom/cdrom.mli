type cdrom_drive_status =
	| NO_INFO
	| NO_DISC
	| TRAY_OPEN
	| DRIVE_NOT_READY
	| DISC_OK

val string_of_cdrom_drive_status : cdrom_drive_status -> string

type cdrom_disc_status =
	| DISC_NO_INFO
	| DISC_NO_DISC
	| AUDIO
	| DATA_1
	| DATA_2
	| XA_2_1
	| XA_2_2
	| MIXED

val string_of_cdrom_disc_status : cdrom_disc_status -> string

external _query_cdrom_drive_status : Unix.file_descr -> cdrom_drive_status
                                   = "stub_CDROM_DRIVE_STATUS"
external _query_cdrom_disc_status : Unix.file_descr -> cdrom_disc_status
                                  = "stub_CDROM_DISC_STATUS"
external _query_cdrom_mcn : Unix.file_descr -> string = "stub_CDROM_GET_MCN"

val with_cdrom : string -> (Unix.file_descr -> 'a) -> 'a
val query_cdrom_status : string -> cdrom_drive_status * cdrom_disc_status
val query_cdrom_drive_status : string -> cdrom_drive_status
val query_cdrom_mcn : string -> string
