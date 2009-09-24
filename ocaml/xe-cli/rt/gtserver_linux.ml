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

module Protocol = Gtcomms.Make_Protocol (struct type t=Gtmessages.message end)
module Client = Gtcomms.Client (Protocol)
module Server = Gtcomms.Server (Protocol)

module GuestOp=Gtlinuxops

open Gtmessages

class server n np =
  object (self)
    inherit [message] Server.server n np

    method shutdown s timeout =
      self#send s (CmdResult "Shutdown called!");
      ignore(GuestOp.shutdown timeout)

    method reboot s timeout =
      self#send s (CmdResult "Reboot called!");
      ignore(GuestOp.reboot timeout)

    method crash s =
      ignore(GuestOp.crash ());
      self#send s (CmdResult "You might not get this...")
	
    method test s =
      self#send s (CmdResult "Test worked OK!")

    method checkcds s devices =
      self#send s (GuestOp.checkcds devices true)
	
    method checkcdsfail s devices =
      self#send s (GuestOp.checkcds devices false)

    method checkvif s device =
      self#send s (GuestOp.checkvifs device)  
	
    method checkdisks s devices =
      self#send s (GuestOp.checkdisks devices)

    method checkmountdisks s devices =
      self#send s (GuestOp.checkmountdisks devices)

    method setuptestdisk s device =
      self#send s (GuestOp.setuptestdisk device)

    method process s = 
      try
	begin
	  match self#receive s with
              Shutdown timeout -> self#shutdown s timeout
	    | Reboot timeout -> self#reboot s timeout
	    | Test -> self#test s 
	    | Crash -> self#crash s
	    | CheckCD devs -> self#checkcds s devs
	    | CheckVIF dev -> self#checkvif s dev
	    | CheckDisks devs -> self#checkdisks s devs
	    | CheckMountDisks devs -> self#checkmountdisks s devs
	    | SetupTestDisk dev -> self#setuptestdisk s dev
	    | CheckCDFail devs -> self#checkcdsfail s devs
	    | _ -> ()
	end;
	Unix.close s
      with
	  e -> GuestOp.logerr (Printexc.to_string e)
	
  end

let _ =
  let port = 8085 in
  let s = new server port 1 in
  s#start

