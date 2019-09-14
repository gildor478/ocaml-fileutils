(******************************************************************************)
(*  ocaml-fileutils: files and filenames common operations                    *)
(*                                                                            *)
(*  Copyright (C) 2003-2014, Sylvain Le Gall                                  *)
(*                                                                            *)
(*  This library is free software; you can redistribute it and/or modify it   *)
(*  under the terms of the GNU Lesser General Public License as published by  *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at   *)
(*  your option) any later version, with the OCaml static compilation         *)
(*  exception.                                                                *)
(*                                                                            *)
(*  This library is distributed in the hope that it will be useful, but       *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of                *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file         *)
(*  COPYING for more details.                                                 *)
(*                                                                            *)
(*  You should have received a copy of the GNU Lesser General Public License  *)
(*  along with this library; if not, write to the Free Software Foundation,   *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA             *)
(******************************************************************************)

open FileUtilTypes
open FileUtilSTAT
open FileUtilTEST


let touch ?atime ?mtime ?(create=true) ?(time=Touch_now) fln =

  let atime, mtime =
    match atime, mtime with
    | None, None -> true, true
    | Some b, None -> b, false
    | None, Some b -> false, b
    | Some b1, Some b2 -> b1, b2
  in

  let set_time () =
    let fatime, fmtime =
      match time with
      | Touch_now -> 0.0, 0.0
      | Touch_timestamp time_ref -> time_ref, time_ref
      | Touch_file_time fln_ref ->
        let st = stat fln_ref in
        st.access_time, st.modification_time
    in
    let fatime, fmtime =
      if not (atime && mtime) then begin
        let st = stat fln in
        (if atime then fatime else st.access_time),
        (if mtime then fmtime else st.modification_time)
      end else begin
        fatime, fmtime
      end
    in
    Unix.utimes fln fatime fmtime
  in
    (* Create file if required *)
    if test_exists fln then begin
      set_time ()
    end else if create then begin
      close_out (open_out fln);
      set_time ()
    end
