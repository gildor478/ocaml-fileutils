open Fort;;
open Fileutils;;

let expect_equal_string = expect_equal ~printer:(fun x -> x)
in
expect_pass ~desc:"Reduce identity"
~body:( fun () ->
	expect_equal_string "/a/b/c" (reduce "/a/b/c")
);

expect_pass ~desc:"Reduce remove trailer"
~body:( fun () ->
	expect_equal_string "/a/b/c" (reduce "/a/b/c/")
);

expect_pass ~desc:"Reduce remove last .."
~body:(fun () ->
	expect_equal_string "/a/b/c" (reduce "/a/b/c/d/.." ) 
);

expect_pass ~desc:"Reduce remove last ."
~body:(fun () ->
	expect_equal_string "/a/b/c" (reduce "/a/b/c/." ) 
);

expect_pass ~desc:"Reduce remove inside .."
~body:(fun () ->
	expect_equal_string "/a/b/c" (reduce "/a/d/../b/c" ) 
);

expect_pass ~desc:"Reduce remove inside ."
~body:(fun () ->
	expect_equal_string "/a/b/c" (reduce "/a/./b/c" ) 
);

expect_pass ~desc:"Reduce remove last . and .." 
~body:(fun () ->
	expect_equal_string "/a/b/c" ( reduce "/a/b/c/d/./.." ) 
);

expect_pass ~desc:"Reduce remove last .. and ."
~body:(fun () ->
	expect_equal_string "/a/b/c" ( reduce "/a/b/c/d/../." ) 
);

expect_pass ~desc:"Reduce remove following . and .."
~body:(fun () ->
	expect_equal_string "/a/b/c" ( reduce "/a/b/d/./../c" ) 
);

expect_pass ~desc:"Reduce remove following .. and ."
~body:(fun () ->
	expect_equal_string "/a/b/c" ( reduce "/a/b/d/.././c"  ) 
);

expect_pass ~desc:"Reduce remove multiple .."
~body:(fun () ->
	expect_equal_string "/a/b/c" ( reduce "/a/b/../d/../b/c"  ) 
);

expect_pass ~desc:"Reduce remove multiple ."
~body:(fun () ->
	expect_equal_string "/a/b/c" ( reduce "/a/./././b/./c"  ) 
);

expect_pass ~desc:"Reduce remove multiple . and .."
~body:(fun () ->
	expect_equal_string "/a/b/c" ( reduce "/a/../a/./b/../c/../b/./c"  ) 
);

(*expect_pass ~desc:""
~body:(fun () ->
	expect_equal_string "/a/b/c" ( reduce ""  ) 
);
*)

();;
