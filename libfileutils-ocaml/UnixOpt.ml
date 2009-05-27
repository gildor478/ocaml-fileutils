
open FilePath_type;;

include CommonPath;;

let rec dir_writer = 
  UnixPath.dir_writer
;;

let dir_reader fn =
  let sep =
    '/'
  in

  let fn_len = 
    String.length fn
  in

  let fn_part_of_string =
    function
      | "." -> CurrentDir Long
      | ".." -> ParentDir
      | str -> Component str
  in

  let fn_sub pos_start pos_sep = 
    if pos_start < pos_sep then
      (
        let part_str =
          String.sub fn pos_start (pos_sep - pos_start) 
        in
          fn_part_of_string part_str
      )
    else
      (
        assert(pos_start = pos_sep);
        Component ""
      )
  in

  let rec split_aux acc pos_start =
    if pos_start < fn_len then
      (
        let pos_sep = 
          try
            String.index_from fn pos_start sep
          with Not_found ->
            fn_len
        in
        let part = 
          fn_sub pos_start pos_sep
        in
        let acc = 
          part :: acc
        in
          if pos_sep >= fn_len then
            (
              (* Nothing more in the filename *)
              List.rev acc
            )
          else if pos_sep = (fn_len - 1) then
            (
              (* Filename end with '/' *)
              List.rev (Component "" :: acc)
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

    if fn_len > 0 then
      (
        if fn.[0] = sep then
          split_aux [Root ""] 1
        else
          split_aux [] 0
      )
    else
      (
        [CurrentDir Short]
      )
;;

let path_writer =
  UnixPath.path_writer
;;

let path_reader =
  UnixPath.path_reader
;;

let fast_concat fn1 fn2 =
  let fn1_len =
    String.length fn1
  in
    if fn1_len = 0 || fn1.[fn1_len - 1] = '/' then
      fn1 ^ fn2
    else
      fn1 ^ "/" ^ fn2
;;

let fast_basename fn =
  try
    let start_pos = 
      (String.rindex fn '/') + 1
    in
    let fn_len =
      String.length fn
    in
      if start_pos = fn_len then
        ""
      else
        String.sub fn start_pos (fn_len - start_pos) 
  with Not_found ->
    fn
;;

let fast_dirname fn =
  try
    let last_pos = 
      String.rindex fn '/'
    in
      if last_pos = 0 then
        "/"
      else
        String.sub fn 0 last_pos
  with Not_found ->
    ""
;;

let fast_is_relative fn =
  if String.length fn = 0 || fn.[0] <> '/' then
    true
  else 
    false
;;

let fast_is_current fn =
  if String.length fn = 0 || fn = "." then
    true
  else if fn.[0] <> '.' then
    false
  else
    raise CannotHandleFast
;;

let fast_is_parent fn =
  if fn = ".." then
    true
  else if String.length fn < 2 || fn.[0] <> '.' || fn.[1] <> '.' then
    false
  else
    raise CannotHandleFast
;;
