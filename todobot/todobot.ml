open Task
open Command
open Format

let permitted_buddies = ref []

exception Break_connection of string
exception Not_permitted_user

let assert_permitted_buddy full_jid =
  let short_jid = String.lowercase (Str.global_replace (Str.regexp "/.*") "" full_jid) in
  if not (List.mem short_jid !permitted_buddies) then raise Not_permitted_user

let print_ordered fmtr lst =
  let print_task h =
    let print_string s =
      if String.length s > 0 then begin
	pp_open_box fmtr 0;
	pp_print_string fmtr s;
	pp_close_box fmtr ();
	pp_print_space fmtr ()
      end
    in
    let print_list lst =
      pp_open_box fmtr 0;
      List.iter (fun s -> pp_print_string fmtr s; pp_print_space fmtr ()) lst;
      pp_close_box fmtr ();
    in
    let break_and_print_string s =
      let sl = Str.split (Str.regexp "[ \t]+") s in
      print_list sl
    in
    pp_print_cut fmtr ();
    pp_open_hbox fmtr ();
    List.iter (fun t -> pp_open_tag fmtr t) h.tags;
    fprintf fmtr "@[% 3d:@]@ " h.id;
    print_string h.complete_cancelled;
    print_string h.priority;
    print_list h.contexts;
    print_list h.dates;
    print_list h.projects;
    print_list h.waits;
    pp_print_string fmtr "|| ";
    break_and_print_string h.text;
    List.iter (fun t -> pp_close_tag fmtr ()) h.tags;
    pp_close_box fmtr ()
  in
  List.iter print_task lst

let print_grouped fmtr lst =
  let print_group g =
    fprintf fmtr "@,@[<h>@{<group>%s@}@]" g
  in
  List.iter (fun (grp,tl) -> print_group grp; print_ordered fmtr tl) lst

let local_parse_command user = function
  | ("quit"|"Quit"|"exit"|"Exit")::[]  -> `Quit(user)
  | ("whoami"|"Whoami")::[] -> `Whoami
  | c -> parse_command c

let local_do_command user c =
  pp_set_margin str_formatter 50;
  let print_result = function
    | Listing (Ordered l) -> pp_open_vbox str_formatter 0; print_ordered str_formatter l; pp_close_box str_formatter ();
    | Listing (Grouped l) -> pp_open_vbox str_formatter 0; print_grouped str_formatter l; pp_close_box str_formatter ();
    | Error s|Info s -> fprintf str_formatter "@[%s@]" s
    | Null -> fprintf str_formatter "@[OK@]"
  in
  let flush() = Bot.send user (flush_str_formatter()) in
  let result = match c with
    | `Quit u -> Bot.send user "Goodbye."; raise (Break_connection u)
    | `Whoami -> Info user
    | `Set _ -> raise Invalid_command
    | _ -> do_command(c)
  in
  print_result result;
  flush()

let handle_message frm msg =
  let _ =
    try
      assert_permitted_buddy frm;
      let ary = wordlist_of_string msg in
      local_do_command frm (local_parse_command frm ary)
    with
      | Not_permitted_user -> Bot.send frm "You are not allowed here. Buzz off."
      | File_modified -> Bot.send frm "File was modified, so the command failed."; read_files()
      | Invalid_command -> Bot.send frm "Invalid command."
  in
  ()

let _ =
(* signal handling is not working quite right *)
(*  let _ = Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> raise (Break_connection "LOCAL_TERMINAL"))) in *)
  let cf = Filename.concat (Sys.getenv "HOME") ".todobot.cf" in
  let (jid, password, bot_resource, photo, permit, logfile, server, port, todofile, donefile) =
    try
      let lexbuf = Lexing.from_channel (open_in cf) in
      Simpleconfig_parser.main Simpleconfig_lexer.token lexbuf
    with
      | Parsing.Parse_error -> Printf.printf "Parse error in configuration file %s. Aborting.\n" cf; exit 1
      | Sys_error e -> Printf.printf "Error reading config file: %s.\n" e; exit 1
      | Failure "int_of_string" -> Printf.printf "Specified port is not valid. Check configuration in %s.\n" cf; exit 1
  in
  permitted_buddies := permit;
  let resource = (Unix.gethostname() ^ "/" ^ bot_resource) in
  let todo_file =
    match todofile with
      | Some x -> x
      | None -> List.fold_left (fun p e -> Filename.concat p e) (Sys.getenv "HOME") ["todo"; "todo.txt"] in
  let done_file =
    match donefile with
      | Some x -> x
      | None -> List.fold_left (fun p e -> Filename.concat p e) (Sys.getenv "HOME") ["todo"; "done.txt"] in
  let _ = do_command (`Set(`StringV("todo", todo_file))) in
  let _ = do_command (`Set(`StringV("done", done_file))) in
  read_files();
  while true do
    try
      let rc = Bot.run ~jid ~resource ~password ~handler:handle_message ?photo ?server ?port ?logfile () in
      Bot.log (Printf.sprintf "Disconnected with code %d. Reconnecting in 60 seconds.\n" rc);
      flush stdout;
      Unix.sleep 60;
    with
      | Break_connection user ->
	  Bot.log (Printf.sprintf "Exiting on command from %s\n" user);
	  Bot.halt();
	  exit 0
  done
