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

    let test_empty st =
      try
        Stream.empty st;
        true
      with Stream.Failure ->
        false
    in

    let _ = seek_in fd1 skip1 in
    let _ = seek_in fd2 skip2 in
    let stream1 = Stream.of_channel fd1 in
    let stream2 = Stream.of_channel fd2 in
    try
      begin
        while ((Stream.next stream1) = (Stream.next stream2)) do
          ()
        done;
        clean_fd ();
        Some (Stream.count stream1)
      end
    with
      | Stream.Failure ->
          begin
            match ((test_empty stream1), (test_empty stream2)) with
                true, true  ->
                  None
              | true, false
              | false, true
              (* Don't know how this case could be... *)
              | false, false ->
                  clean_fd ();
                  Some (Stream.count stream1)
          end
      | e ->
          clean_fd ();
          raise e
  end else
    Some (-1)


