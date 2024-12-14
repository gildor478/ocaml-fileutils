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
open FileUtilTEST

let rec seq_of_channel ch () =
  match input_char ch with
  | exception End_of_file -> Seq.Nil
  | char -> Seq.Cons (char, seq_of_channel ch)

let cmp ?(skip1 = 0) fln1 ?(skip2 = 0) fln2 =
  if (reduce fln1) = (reduce fln2) then
    None
  else if (test (And(Is_readable, Is_file)) fln1)
      && (test (And(Is_readable, Is_file)) fln2) then begin
    let fd1 = open_in_bin fln1 in
    let fd2 = open_in_bin fln2 in
    let clean_fd () =
      let () = try close_in fd1 with _ -> () in
      let () = try close_in fd2 with _ -> () in
        ()
    in

    let _ = seek_in fd1 skip1 in
    let _ = seek_in fd2 skip2 in
    let stream1 = seq_of_channel fd1 in
    let stream2 = seq_of_channel fd2 in
    let rec loop count s1 s2 =
      match s1, s2 with
      | Seq.Cons (v1, s1), Seq.Cons (v2, s2) when v1 = v2 -> loop (count + 1) (s1 ()) (s2 ())
      | Seq.Nil, Seq.Nil -> (-1)
      | _ -> count
    in
    let count = loop 0 (stream1 ()) (stream2 ()) in
    clean_fd ();
    match count with
    | (-1) -> None
    | x -> Some x
  end else
    Some (-1)


