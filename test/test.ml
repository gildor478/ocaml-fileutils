open Fort;;
open Fileutils;;

print_string "Testing UNIX functions";
print_newline ();

expect_equal ~msg:"Reduce two path" ~printer:(fun x -> x)
	(reduce "/a/b/c") "/a/b/c";;
