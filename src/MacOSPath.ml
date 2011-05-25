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

open FilePath_type;;

include CommonPath;;

let rec dir_writer lst =
 let buffer = Buffer.create path_length
 in
 let rec dir_writer_aux lst =
   match lst with
       Root s :: tl ->
         Buffer.add_string buffer s;
         Buffer.add_char   buffer ':';
         dir_writer_aux tl
     | (CurrentDir _) :: tl 
     | ParentDir  :: tl ->
         Buffer.add_char   buffer ':';
         dir_writer_aux tl
     | (Component "") :: tl ->
         dir_writer_aux tl
     | (Component s) :: [] ->
         Buffer.add_string buffer s;
         dir_writer_aux []
     | (Component s) :: tl ->
         Buffer.add_string buffer s;
         Buffer.add_char   buffer ':';
         dir_writer_aux tl
     | [] ->
         Buffer.contents buffer
 in
   match lst with
       ParentDir :: _ -> 
         dir_writer_aux ( (CurrentDir Long) :: lst )
     | [ CurrentDir Short ] ->
         ""
     | _ -> 
         dir_writer_aux lst
;;

let dir_reader str = 

  let rec dir_reader_aux =
    function
      | [""] ->
          []
      | "" :: tl ->
          ParentDir :: (dir_reader_aux tl)
      | str :: tl ->
          Component str :: (dir_reader_aux tl)
      | [] ->
          []
  in
    match StringExt.split ~map:(fun s -> s) ':' str with 
      | [] -> 
          [CurrentDir Short]
      | "" :: tl -> 
          CurrentDir Long :: (dir_reader_aux tl)
      | [id] ->
          [Component id]
      | root :: tl -> 
          Root root :: (dir_reader_aux tl)
;;

let path_writer lst = 
  String.concat ";" lst
;;

let path_reader = 
  StringExt.split ~map:(fun s -> s) ';'
;;
