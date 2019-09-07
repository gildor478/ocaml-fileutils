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


(* What should be the fastest possible function in OCaml. *)
let rec simple fn =
  let st = Unix.lstat fn in
    match st.Unix.st_kind with
      | Unix.S_DIR ->
          begin
            let fd = Unix.opendir fn in
              try
                while true do
                  let bn = Unix.readdir fd in
                    if bn <> "." && bn <> ".." then
                      simple (Filename.concat fn bn)
                done
              with End_of_file ->
                Unix.closedir fd
          end
      | Unix.S_LNK ->
          ()
      | _ ->
          ()

let () =
  if not Sys.unix then exit 0;
  let dir = Sys.getenv "HOME" in
  let sys_find () =
    let _i: int =
      Sys.command ("find "^(Filename.quote dir)^" -name '*.mp3' \
                     | (echo -n 'Count: '; wc -l)")
    in
      ()
  in
  let fileutils_find () =
    let count =
      FileUtil.find
        (FileUtil.Has_extension "mp3")
        dir
        (fun i _ -> i + 1)
        0
    in
      Printf.eprintf "Count: %d\n%!" count
  in
  let time str f =
    let start_time =
      Unix.gettimeofday ()
    in
    let time =
      prerr_endline str;
      f ();
      (Unix.gettimeofday ()) -. start_time
    in
      Printf.eprintf "Time: %.2fs\n%!" time;
      time
  in
  let () =
    prerr_endline "System find (load)";
    sys_find ()
  in
  let time_ref =
    time "System find (reference)" sys_find
  in
  let time_fileutils =
    time "FileUtil find" fileutils_find
  in
  let _time_simple =
    time "Simple" (fun () -> simple dir)
  in
    Printf.eprintf "Performance: %.2f%%\n%!"
      (100.0 *. (time_ref /. time_fileutils))
