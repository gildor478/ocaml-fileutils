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

let byte_of_size sz =
  let rec mul_1024 n i =
    if n > 0 then
      mul_1024
        (n - 1)
        (Int64.mul 1024L i)
    else
      i
  in
    match sz with
      | B i  -> i
      | KB i -> mul_1024 1 i
      | MB i -> mul_1024 2 i
      | GB i -> mul_1024 3 i
      | TB i -> mul_1024 4 i


let size_add sz1 sz2 =
  B (Int64.add (byte_of_size sz1) (byte_of_size sz2))


let size_compare ?(fuzzy=false) sz1 sz2 =
  let by1 =
    byte_of_size sz1
  in
  let by2 =
    byte_of_size sz2
  in
    if fuzzy then begin
      let rec fuzzy_comp n1 n2 =
        if n1 = n2 then
          0
        else begin
          let up_unit_n1 =
            Int64.div n1 1024L
          in
          let up_unit_n2 =
            Int64.div n2 1024L
          in
            if up_unit_n1 <> 0L && up_unit_n2 <> 0L then
              fuzzy_comp up_unit_n1 up_unit_n2
            else
              Int64.compare n1 n2
        end
      in
        fuzzy_comp by1 by2
    end else
      Int64.compare by1 by2


let string_of_size ?(fuzzy=false) sz =
  let szstr i unt (cur_i, cur_unt, tl) =
    let tl =
      (cur_i, cur_unt) :: tl
    in
      i, unt, tl
  in

  let rec decomp_continue fup i unt acc =
    if i = 0L then
      szstr i unt acc
    else begin
      (* Continue with upper unit *)
      let r =
        Int64.rem i 1024L
      in
      let q =
        Int64.div i 1024L
      in
        decomp_start (szstr r unt acc) (fup q)
    end

  and decomp_start acc sz =
    (* Decompose size for current unit and try
     * to use upper unit
     *)
    match sz with
      | TB i ->
          szstr i "TB" acc
      | GB i ->
          decomp_continue (fun n -> TB n) i "GB" acc
      | MB i ->
          decomp_continue (fun n -> GB n) i "MB" acc
      | KB i ->
          decomp_continue (fun n -> MB n) i "KB" acc
      | B i ->
          decomp_continue (fun n -> KB n) i "B" acc
  in

  (* Only accumulate significant unit in tail *)
  let only_significant_unit (cur_i, cur_unt, lst) =
    let significant_lst =
      List.filter
        (fun (i, _) -> i <> 0L)
        ((cur_i, cur_unt) :: lst)
    in
      match significant_lst with
        | [] -> cur_i, cur_unt, []
        | (cur_i, cur_unt) :: tl -> (cur_i, cur_unt, tl)
  in

  let main_i, main_unt, rem_lst =
    only_significant_unit (decomp_start (0L, "B", []) sz)
  in

    if fuzzy then begin
      let _, rem =
        List.fold_left
          (fun (div, acc) (i, _unt) ->
             let acc =
               acc +. ((Int64.to_float i) /. div)
             in
               div *. 1024.0,
               acc)
          (1024.0, 0.0)
          rem_lst
      in
        Printf.sprintf "%.2f %s"
          ((Int64.to_float main_i) +. rem)
          main_unt
    end else begin
      String.concat
        " "
        (List.map
           (fun (i, unt) -> Printf.sprintf "%Ld %s" i unt)
           ((main_i, main_unt) :: rem_lst))
    end
