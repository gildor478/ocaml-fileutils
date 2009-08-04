
(** Extended String module 
  *)

(** Split a string, separator not included
  *)
let split ?(start_acc=[]) ?(start_pos=0) ~map sep str =
  let str_len =
    String.length str
  in
  let rec split_aux acc pos =
    if pos < str_len then
      (
        let pos_sep = 
          try
            String.index_from str pos sep
          with Not_found ->
            str_len
        in
        let part = 
          String.sub str pos (pos_sep - pos) 
        in
        let acc = 
          (map part) :: acc
        in
          if pos_sep >= str_len then
            (
              (* Nothing more in the string *)
              List.rev acc
            )
          else if pos_sep = (str_len - 1) then
            (
              (* String end with a separator *)
              List.rev ((map "") :: acc)
            )
          else
            (
              split_aux acc (pos_sep + 1)
            )
      )
    else
      (
        List.rev acc
      )
  in
    split_aux start_acc start_pos
;;

(** Cut in two a string, separator not included
  *)
let break_at_first sep str =
  let pos_sep = 
    String.index str sep
  in
    (String.sub str 0 pos_sep),
    (String.sub str (pos_sep + 1) ((String.length str) - pos_sep - 1))
;;

