let run () =
  let chan = open_out "hello.out" in
  Printf.fprintf chan "Hello world!\n";
  close_out chan

let () = Db.Main.extend run
