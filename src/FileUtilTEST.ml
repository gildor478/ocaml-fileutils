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
open FileUtilSize
open FileUtilSTAT


let compile_filter ?(match_compile=(fun s fn -> s = fn)) flt =
  let cflt =
    let rec cc =
      function
      | True                   -> `Val true
      | False                  -> `Val false
      | Is_dev_block           -> `Stat (`Kind Dev_block)
      | Is_dev_char            -> `Stat (`Kind Dev_char)
      | Is_dir                 -> `Stat (`Kind Dir)
      | Is_file                -> `Stat (`Kind File)
      | Is_socket              -> `Stat (`Kind Socket)
      | Is_pipe                -> `Stat (`Kind Fifo)
      | Is_link                -> `Is_link
      | Is_set_group_ID        -> `Stat `Is_set_group_ID
      | Has_sticky_bit         -> `Stat `Has_sticky_bit
      | Has_set_user_ID        -> `Stat `Has_set_user_ID
      | Is_readable            -> `Stat `Is_readable
      | Is_writeable           -> `Stat `Is_writeable
      | Is_exec                -> `Stat `Is_exec
      | Size_not_null          -> `Stat (`Size (`Bigger, B 0L))
      | Size_bigger_than sz    -> `Stat (`Size (`Bigger, sz))
      | Size_smaller_than sz   -> `Stat (`Size (`Smaller, sz))
      | Size_equal_to sz       -> `Stat (`Size (`Equal, sz))
      | Size_fuzzy_equal_to sz -> `Stat (`Size (`FuzzyEqual, sz))
      | Is_owned_by_user_ID ->
          `Stat (`Is_owned_by_user_ID (Unix.geteuid ()))
      | Is_owned_by_group_ID ->
          `Stat (`Is_owned_by_group_ID (Unix.getegid ()))
      | Exists                 -> `Stat `Exists
      | Is_newer_than fn1      -> `Stat (`Newer (stat fn1).modification_time)
      | Is_older_than fn1      -> `Stat (`Older (stat fn1).modification_time)
      | Is_newer_than_date(dt) -> `Stat (`Newer dt)
      | Is_older_than_date(dt) -> `Stat (`Older dt)
      | Has_extension ext      -> `Has_extension ext
      | Has_no_extension       -> `Has_no_extension
      | Is_current_dir         -> `Is_current_dir
      | Is_parent_dir          -> `Is_parent_dir
      | Basename_is s          -> `Basename_is s
      | Dirname_is s           -> `Dirname_is s
      | Custom f               -> `Custom f
      | Match str              -> `Custom (match_compile str)
      | And(flt1, flt2) ->
          begin
            match cc flt1, cc flt2 with
              | `Val true, cflt | cflt, `Val true -> cflt
              | `Val false, _ | _,  `Val false -> `Val false
              | cflt1, cflt2 -> `And (cflt1, cflt2)
          end
      | Or(flt1, flt2) ->
          begin
            match cc flt1, cc flt2 with
              | `Val true, _ | _, `Val true -> `Val true
              | `Val false, cflt | cflt,  `Val false -> cflt
              | cflt1, cflt2 -> `Or (cflt1, cflt2)
          end
      | Not flt ->
          begin
            match cc flt with
              | `Val b -> `Val (not b)
              | cflt -> `Not cflt
          end
    in
    cc flt
  in
  let need_statL, need_stat =
    let rec dfs =
      function
        | `Val _ | `Has_extension _ | `Has_no_extension | `Is_current_dir
        | `Is_parent_dir | `Basename_is _ | `Dirname_is _
        | `Custom _ ->
            false, false
        | `Stat _ ->
            true, false
        | `Is_link ->
            false, true
        | `And (cflt1, cflt2) | `Or (cflt1, cflt2) ->
            let need_stat1, need_statL1 = dfs cflt1 in
            let need_stat2, need_statL2 = dfs cflt2 in
              need_stat1 || need_stat2, need_statL1 || need_statL2
        | `Not cflt ->
            dfs cflt
    in
      dfs cflt
  in
    (* Compiled function to return. *)
    fun ?st_opt ?stL_opt fn ->
      let st_opt =
        if need_stat && st_opt = None then begin
          try
            match stL_opt with
              | Some st when not st.is_link -> stL_opt
              | _ -> Some (stat fn)
          with FileDoesntExist _ ->
            None
        end else
          st_opt
      in
      let stL_opt =
        if need_statL && stL_opt = None then begin
          try
            match st_opt with
              | Some st when not st.is_link -> st_opt
              | _ -> Some (stat ~dereference:true fn)
          with FileDoesntExist _ ->
            None
        end else
          stL_opt
      in
      let rec eval =
        function
        | `Val b -> b
        | `Has_extension ext ->
            begin
              try
                check_extension fn ext
              with FilePath.NoExtension _ ->
                false
            end
        | `Has_no_extension ->
            begin
              try
                let _str: filename = chop_extension fn in
                  false
              with FilePath.NoExtension _ ->
                true
            end
        | `Is_current_dir -> is_current (basename fn)
        | `Is_parent_dir -> is_parent (basename fn)
        | `Basename_is bn -> (FilePath.compare (basename fn) bn) = 0
        | `Dirname_is dn -> (FilePath.compare (dirname fn) dn) = 0
        | `Custom f -> f fn
        | `Stat e ->
            begin
              match stL_opt, e with
              | Some _, `Exists -> true
              | Some stL, `Kind knd -> stL.kind = knd
              | Some stL, `Is_set_group_ID -> stL.permission.group.sticky
              | Some stL, `Has_sticky_bit -> stL.permission.other.sticky
              | Some stL, `Has_set_user_ID -> stL.permission.user.sticky
              | Some stL, `Size (cmp, sz) ->
                  begin
                    let diff = size_compare stL.size sz in
                      match cmp with
                      | `Bigger -> diff > 0
                      | `Smaller -> diff < 0
                      | `Equal -> diff = 0
                      | `FuzzyEqual ->
                          (size_compare ~fuzzy:true stL.size sz) = 0
                  end
              | Some stL, `Is_owned_by_user_ID uid -> uid = stL.owner
              | Some stL, `Is_owned_by_group_ID gid -> gid = stL.group_owner
              | Some stL, `Is_readable ->
                  let perm = stL.permission in
                    perm.user.read || perm.group.read || perm.other.read
              | Some stL, `Is_writeable ->
                  let perm = stL.permission in
                    perm.user.write || perm.group.write || perm.other.write
              | Some stL, `Is_exec ->
                  let perm = stL.permission in
                    perm.user.exec || perm.group.exec || perm.other.exec
              | Some stL, `Newer dt -> stL.modification_time > dt
              | Some stL, `Older dt -> stL.modification_time < dt
              | None, _ -> false
            end
        | `Is_link ->
            begin
              match st_opt with
                | Some st -> st.is_link
                | None -> false
            end
        | `And (cflt1, cflt2) -> (eval cflt1) && (eval cflt2)
        | `Or (cflt1, cflt2) -> (eval cflt1) || (eval cflt2)
        | `Not cflt -> not (eval cflt)
      in
      eval cflt


let test ?match_compile tst =
  let ctst = compile_filter ?match_compile tst in
  fun fln -> ctst (solve_dirname fln)


let filter flt lst = List.filter (test flt) lst


let test_exists = test (Or(Exists, Is_link))

