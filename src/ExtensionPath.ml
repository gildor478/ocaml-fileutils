
(** Manipulate path extension 
  *)

open FilePath_type;;

let get fn =
  let start_pos =
    (String.rindex fn '.') + 1
  in
  let fn_len =
    String.length fn
  in
    if start_pos = fn_len then
      ""
    else
      String.sub fn start_pos (fn_len - start_pos)
;;

let check fn ext =
  try
    (get fn) = ext
  with Not_found ->
    false
;;

let chop fn =
  try
    let end_pos = 
      String.rindex fn '.'
    in
      if end_pos = 0 then
        ""
      else
        String.sub fn 0 end_pos
  with Not_found ->
    fn
;;

let add fn ext =
  fn ^ "." ^ ext
;;

let replace fn ext =
  add (chop fn) ext
;;

