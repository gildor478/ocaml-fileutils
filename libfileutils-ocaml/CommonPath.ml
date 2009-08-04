
(** A fast operation cannot be done, will
    continue by trying more complex processing
  *)

module StringExt = FileStringExt;;

exception CannotHandleFast;;

let fast_concat _ _ =
  raise CannotHandleFast
;;

let fast_basename _ =
  raise CannotHandleFast
;;

let fast_dirname _ = 
  raise CannotHandleFast
;;

let fast_is_relative _ = 
  raise CannotHandleFast
;;

let fast_is_current _ =
  raise CannotHandleFast
;;

let fast_is_parent _ = 
  raise CannotHandleFast
;;
