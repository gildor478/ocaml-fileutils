
(** FileUtil with Str regexp match test
    @author Sylvain Le Gall
  *)

(** Compile [FileUtil.Match] expression using [Str.regexp]
  *)
let match_compile str =
  let regex = 
    Str.regexp str
  in
    fun fn -> Str.string_match regex fn 0
;;

(** See {!FileUtil.test}
  *)
let test =
  FileUtil.test ~match_compile:match_compile
;;

(** See {!FileUtil.find}
  *)
let find =
  FileUtil.test ~match_compile:match_compile
;;
