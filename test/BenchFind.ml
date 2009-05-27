
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

