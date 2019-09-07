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
open FilePath
open FileUtilMisc
open FileUtilPWD
open FileUtilRM
open FileUtilCP
open FileUtilTEST


exception MvError of string

type mv_error =
  [ `Exc of exn
  | `MvCp of filename * filename * string * cp_error
  | `MvRm of  filename * string * rm_error
  | `NoSourceFile ]


let rec mv
      ?(error=fun str _ -> raise (MvError str))
      ?(force=Force)
      fln_src fln_dst =
  let handle_error, _ =
    handle_error_gen "mv" error
      (function
         | `NoSourceFile ->
             "Cannot move an empty list of files."
         | `MvCp (fn_src, fn_dst, str, _) ->
             Printf.sprintf
               "Recursive error in 'mv %s %s' for 'cp %s %s': %s"
               fn_src fn_dst fn_src fn_dst str
         | `MvRm (fn, str, _) ->
             Printf.sprintf "Recursive error in 'mv %s ..' for 'rm %s': %s"
               fn fn str
         | #exc -> "")
  in
  let fln_src_abs =  make_absolute (pwd ()) fln_src in
  let fln_dst_abs =  make_absolute (pwd ()) fln_dst in
  if compare fln_src_abs fln_dst_abs <> 0 then begin
    if test_exists fln_dst_abs && doit force fln_dst then begin
        rm [fln_dst_abs];
        mv fln_src_abs fln_dst_abs
    end else if test Is_dir fln_dst_abs then begin
      mv ~force ~error
        fln_src_abs
        (make_absolute
           fln_dst_abs
           (basename fln_src_abs))
    end else if test_exists fln_src_abs then begin
      try
        Sys.rename fln_src_abs fln_dst_abs
      with Sys_error _ ->
        cp ~force
          ~error:(fun str e ->
                    handle_error ~fatal:true
                      (`MvCp (fln_src_abs, fln_dst_abs, str, e)))
          ~recurse:true [fln_src_abs] fln_dst_abs;
        rm ~force
          ~error:(fun str e ->
                    handle_error ~fatal:true
                      (`MvRm (fln_src_abs, str, e)))
          ~recurse:true [fln_src_abs]
    end else
      handle_error ~fatal:true `NoSourceFile
  end
