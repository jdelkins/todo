let _ =
  Unix.close Unix.stdin;
  Unix.close Unix.stdout;
  Unix.close Unix.stderr;

  let fd = Unix.openfile "/tmp/blah" [Unix.O_RDWR; Unix.O_CREAT] 0o664 in
  Unix.dup2 fd Unix.stdin;
  Unix.dup2 fd Unix.stdout;
  Unix.dup2 fd Unix.stderr;

  Printf.printf "this is a test\n";
  Printf.fprintf stderr "stderr test\n";

  let buff = String.make 100 (char_of_int 0) in
  ignore (Unix.lseek Unix.stdin 0 Unix.SEEK_SET);
  ignore (Unix.read Unix.stdin buff 0 10);
  ignore (Unix.lseek Unix.stdout 0 Unix.SEEK_END);
  Printf.printf "first 10 chars repeated here:\n%s\n" buff;
  exit 0

