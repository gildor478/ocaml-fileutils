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

type who = [`User | `Group | `Other | `All]
type wholist = [ who | `List of who list ]
type permcopy = [`User | `Group | `Other]
type perm = [ `Read | `Write | `Exec | `ExecX | `Sticky | `StickyO ]
type permlist = [ perm | `List of perm list ]
type actionarg = [ permlist | permcopy ]
type action = [ `Set of actionarg | `Add of actionarg | `Remove of actionarg]
type actionlist = [ action | `List of action list ]
type clause = [ `User of actionlist | `Group of actionlist
              | `Other of actionlist | `All of actionlist
              | `None of actionlist ]

type t = clause list


let all_masks =
  [
    `User,  `Sticky,  0o4000;
    `User,  `Exec,    0o0100;
    `User,  `Write,   0o0200;
    `User,  `Read,    0o0400;
    `Group, `Sticky,  0o2000;
    `Group, `Exec,    0o0010;
    `Group, `Write,   0o0020;
    `Group, `Read,    0o0040;
    `Other, `StickyO, 0o1000;
    `Other, `Exec,    0o0001;
    `Other, `Write,   0o0002;
    `Other, `Read,    0o0004;
  ]


let mask =
  let module M =
    Map.Make
      (struct
         type t = who * perm
         let compare = Stdlib.compare
       end)
  in
  let m =
    List.fold_left
      (fun m (who, prm, msk) -> M.add (who, prm) msk m)
      M.empty all_masks
  in
    fun who prm ->
      try
        M.find (who, prm) m
      with Not_found ->
        0


let of_int i =
  let user, group, other =
    List.fold_left
      (fun (user, group, other) (who, perm, mask) ->
         if (i land mask) <> 0 then begin
           match who with
           | `User -> perm :: user, group, other
           | `Group -> user, perm :: group, other
           | `Other -> user, group, perm :: other
         end else begin
           (user, group, other)
         end)
      ([], [], [])
      all_masks
  in
    [`User (`Set (`List user));
     `Group (`Set (`List group));
     `Other (`Set (`List other))]


let to_string =
  let perm =
    function
    | `Read -> "r"
    | `Write -> "w"
    | `Exec -> "x"
    | `Sticky -> "s"
    | `ExecX -> "X"
    | `StickyO -> "t"
  in
  let permlist =
    function
    | `List lst -> String.concat "" (List.map perm lst)
    | #perm as prm -> perm prm
  in
  let permcopy =
    function
    | `User -> "u"
    | `Group -> "g"
    | `Other -> "o"
  in
  let action act =
    let sact, arg =
      match act with
      | `Set arg -> "=", arg
      | `Add arg -> "+", arg
      | `Remove arg -> "-", arg
    in
    let sarg =
      match arg with
      | #permlist as lst -> permlist lst
      | #permcopy as prm -> permcopy prm
    in
      sact^sarg
  in
  let actionlist =
    function
    | `List lst -> String.concat "" (List.map action lst)
    | #action as act -> action act
  in
  let clause cls =
    let swho, lst =
      match cls with
      | `User lst -> "u", lst
      | `Group lst -> "g", lst
      | `Other lst -> "o", lst
      | `All lst -> "a", lst
      | `None lst -> "", lst
    in
      swho^(actionlist lst)
  in
    fun t -> String.concat "," (List.map clause t)


let apply ~is_dir ~umask i (t: t) =
  let set who prm b i =
    let m = mask who prm in
      if b then i lor m else i land (lnot m)
  in
  let get who prm i =
    let m = mask who prm in
      (i land m) <> 0
  in
  let permlist _who i lst =
    List.fold_left
      (fun acc ->
         function
         | `Exec | `Read | `Write | `Sticky | `StickyO as a -> a :: acc
         | `ExecX ->
             if is_dir ||
                List.exists (fun who -> get who `Exec i)
                  [`User; `Group; `Other] then
               `Exec :: acc
             else
               acc)
      []
      (match lst with
       | `List lst -> lst
       | #perm as prm -> [prm])
  in
  let permcopy _who i =
    List.fold_left
      (fun acc (who, prm, _) ->
         if get who prm i then
           prm :: acc
         else
           acc)
      [] all_masks
  in
  let args who i =
    function
    | #permlist as lst -> permlist who i lst
    | #permcopy as who -> permcopy who i
  in
  let rec action who i act =
    match act with
    | `Set arg ->
        action who
          (action who i (`Remove (`List (permcopy who i))))
          (`Add arg)
    | `Add arg ->
        List.fold_left (fun i prm -> set who prm true i) i (args who i arg)
    | `Remove arg ->
        List.fold_left (fun i prm -> set who prm false i) i (args who i arg) 
  in
  let actionlist who i lst =
    match lst with
    | `List lst -> List.fold_left (action who) i lst
    | #action as act -> action who i act
  in
  let actionlist_none i lst =
    let numask = lnot umask in
    let arg_set_if_mask who i arg b =
      List.fold_left
        (fun i prm ->
           if get who prm numask then
             set who prm b i
           else
             i)
        i (args who i arg)
    in
      List.fold_left
        (fun i who ->
           List.fold_left
             (fun i ->
                function
                | `Set _ -> i
                | `Add arg -> arg_set_if_mask who i arg true
                | `Remove arg -> arg_set_if_mask who i arg false)
             i
             (match lst with
              | `List lst -> lst
              | #action as act -> [act]))
        i [`User; `Group; `Other]
  in

  let rec clause i cls =
    match cls with
    | `User lst -> actionlist `User i lst
    | `Group lst -> actionlist `Group i lst
    | `Other lst -> actionlist `Other i lst
    | `All lst -> 
        List.fold_left clause i [`User lst; `Group lst; `Other lst]
    | `None lst -> actionlist_none i lst
  in
    List.fold_left clause i t
