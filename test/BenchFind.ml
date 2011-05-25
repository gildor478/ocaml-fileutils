(********************************************************************************)
(*  ocaml-fileutils: files and filenames common operations                      *)
(*                                                                              *)
(*  Copyright (C) 2003-2011, Sylvain Le Gall                                    *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

let () = 
  let dir =
    "/home/gildor"
  in
  let sys_find () =
    let _i : int =
      Sys.command ("find "^(Filename.quote dir)^" -name '*.mp3' | wc -l")
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
      Printf.eprintf "%d\n%!" count
  in
  let time str f =
    let start_time =
      Unix.gettimeofday ()
    in
    let time = 
      prerr_string str; flush stderr;
      f ();
      (Unix.gettimeofday ()) -. start_time
    in
      Printf.eprintf "%.2fs\n%!" time;
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
    Printf.eprintf "Performance: %.2f%%\n%!"
      (100.0 *. (time_ref /. time_fileutils))
;;

