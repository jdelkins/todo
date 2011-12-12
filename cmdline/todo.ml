(* vim:set ts=2 sw=2: *)

open Task
open Command
open Format

(******
 * GLOBALS
 * and functions needed for global variable initialization
 ******)
let color_print = ref true

let print_help fmtr =
  let soft_break str =
    let words = Str.split_delim (Str.regexp "[ \t]+") str in
    List.iter (fun s -> pp_print_string fmtr (if s = "" then " " else s); pp_print_space fmtr ()) words
  in
  let tt1 lab desc =
    let pad = String.make 24 ' ' in
    String.blit lab 0 pad 0 (String.length lab);
    pp_open_tbox fmtr ();
    pp_set_tab fmtr ();
    pp_open_tag fmtr "priob";
    pp_print_string fmtr pad;
    pp_close_tag fmtr ();
    pp_set_tab fmtr ();
    pp_open_box fmtr 2;
    soft_break desc;
    pp_close_box fmtr ();
    pp_print_cut fmtr ()
  in
  let tt lab desc =
    pp_open_tag fmtr "priob";
    pp_print_string fmtr lab;
    pp_close_tag fmtr ();
    pp_print_tbreak fmtr 0 24;
    pp_open_box fmtr 2;
    soft_break desc;
    pp_close_box fmtr ();
    pp_print_cut fmtr ()
  in
  let ttx lab desc =
    tt lab desc;
    pp_close_tbox fmtr ()
  in
  let ll = fprintf fmtr in
  pp_open_vbox fmtr 0;
  ll  "@{<group>Commands:@,";
  ll  "=========@}@,";
  tt1 "add [text]"             "Add a task with the given text. Any metadata tags in the text will be parsed and treated specially (see below).";
  tt  "move [i] [j]"           "Reorder task number i to position j.";
  tt  "append [i] [text]"      "Append some text onto the end of task.";
  tt  "delete [i]"             "Kill task i without completing or cancelling it. The task is not marked for archival (as with \"do\" and \"cancel\"); it is simply deleted from the todo file.";
  tt  "sub [i] [pat] [repl]"   "Replace all occurances in task number i of [pat] with [repl]. [pat] is a regular expression.";
  tt  "do [i]"                 "Mark task number i as complete.";
  tt  "cancel [i]"             "Mark task number i as canceled.";
  tt  "save"                   "Save the todo file. Not normally necessary, since the file is automatically saved after each modification, addition, or deletion of a task.";
  tt  "archive"                "Move completed and canceled tasks to the done file and remove them from the todo file.";
  tt  "reload"                 "Re-read the todo file from disk. Not normally necessary as the todo file is automatically checked for consistency and reloaded if necessary.";
  tt  "list {pat}"             "List the tasks (matching {pat} if given). {pat} is a regular expression.";
  tt  "group {pat}"            "List the tasks (matching {pat} if given), and arrange the output in groups according to the metadata. Tasks may be repeated because a given task often will have several pieces of metadata elegible for grouping. {pat} is a regular expression.";
  tt  "quit"                   "Quit the program.";
  tt  "set {[option] [value]}" "Show, or optionally set, the configuration variables.";
  ttx "help"                   "Present this help message.";
  ll  "@,";
  ll  "@{<group>Notes:@,";
  ll  "======@}@,";
  ll  "@[<2>- On@ disk,@ the@ todo@ file@ must@ have@ one@ task@ per@ line.@]@,";
  ll  "@[<2>- In@ the@ above@ command@ list,@ arguments@ presented@ with@ square@ braces ([])@ are@ required.@]@,";
  ll  "@[<2>- In@ the@ above@ command@ list,@ arguments@ presented@ with@ curly@ braces ({})@ are@ optional.@]@,";
  ll  "@[<2>- In@ defining@ tasks,@ the@ following@ tags@ are@ treated@ as@ special@ metadata,@ which@ are@ used@ ";
  ll  "to@ aggregate@ tasks@ together@ and@ to@ format@ the@ printout.@ Tasks@ may@ be@ given@ arbitrarily@ many@ ";
  ll  "of@ the@ constructs@ indicated@ with *.@]@,";
  tt1 "    @context"      "* Context";
  tt  "    w:waiting-for" "* Waiting for";
  tt  "    p:project"     "* Project";
  tt  "    (A) - (Z)"     "  Priority";
  tt  "    x:2007-01-01"  "  Completed on date";
  tt  "    X:2007-01-01"  "  Canceled on date (marked as dead without having been completed)";
  tt  "    d:2007-01-01"  "* Due date";
  ttx "    ||"            "  Ignored: a visual divider between the metadata and the text";
  ll  "@[<2>- Metadata@ of@ this@ type@ may@ be@ placed@ anywhere@ on@ the@ line.@ This@ program@ will@ reformat@ the@ metadata@ ";
  ll  "to@ be@ at@ the@ beginning@ of@ the@ line@ when@ saving@ the@ file.@]@,";
  ll  "@[<2>- Dates@ in@ d:@ constructs@ are@ flexible@ and@ can@ be@ specified@ in@ several@ formats.@ Formats@ ";
  ll  "without@ a@ year@ are@ interpreted@ to@ be@ the@ next@ nearby@ date@ (in@ the@ future).@ ";
  ll  "Some@ examples:@]@,";
  tt1 "    d:2007-01-01"   "";
  tt  "    d:1/1/07"       "";
  tt  "    d:1/1"          "";
  tt  "    d:today"        "";
  tt  "    d:tomorrow"     "";
  tt  "    d:."            "(interpreted as today)";
  tt  "    d:.+5"          "(5 days from today)";
  tt  "    d:2007-01-01+2" "(January 3, 2007)";
  ttx "    d:2007-01-01-1" "(December 31, 2006)";
  ll  "@[<2>- Text@ arguments@ can@ be@ quoted;@ this@ is@ useful@ when@ specifiying@ regex@ patterns@ containing@ spaces.@]@,";
  ll  "@[<2>- Backslashes@ make@ the@ subsequent@ character@ literal@ (use \\\\@ for@ a@ literal@ backslash@ as@ text).@]@,";
  ll  "@[<2>- Some@ commands@ have@ aliases:@ mv =@ move,@ del =@ delete,@ exit =@ quit,@ ls =@ list,@ gr =@ group.@]@,";
  ll  "@[<2>- The@ first@ character@ of@ the@ command@ may@ be@ capitalized,@ but@ otherwise@ the@ command@ names@ are@ case@ sensitive.@]@,";
  pp_close_box fmtr ()

let colors = [
  ("BLACK",        "\027[0;30m");
  ("RED",          "\027[0;31m");
  ("GREEN",        "\027[0;32m");
  ("BROWN",        "\027[0;33m");
  ("BLUE",         "\027[0;34m");
  ("PURPLE",       "\027[0;35m");
  ("CYAN",         "\027[0;36m");
  ("LIGHT_GREY",   "\027[0;37m");
  ("DARK_GREY",    "\027[1;30m");
  ("LIGHT_RED",    "\027[1;31m");
  ("LIGHT_GREEN",  "\027[1;32m");
  ("YELLOW",       "\027[1;33m");
  ("LIGHT_BLUE",   "\027[1;34m");
  ("LIGHT_PURPLE", "\027[1;35m");
  ("LIGHT_CYAN",   "\027[1;36m");
  ("WHITE",        "\027[1;37m");
  ("DEFAULT",      "\027[0m");
  ("BOLD",         "\027[1m");
]

let open_tag = function
  | "group"     -> List.assoc "LIGHT_CYAN" colors
  | "prioa"     -> List.assoc "RED" colors
  | "priob"     -> List.assoc "GREEN" colors
  | "priox"     -> List.assoc "BLUE" colors
  | "today"     -> List.assoc "RED" colors
  | "past"      -> List.assoc "GREEN" colors
  | "dead"      -> List.assoc "DARK_GREY" colors
  | "tasktext"  -> List.assoc "BOLD" colors
  | _ -> ""

let close_tag = function
  | "group"     -> List.assoc "DEFAULT" colors
  | "prioa"     -> List.assoc "DEFAULT" colors
  | "priob"     -> List.assoc "DEFAULT" colors
  | "priox"     -> List.assoc "DEFAULT" colors
  | "today"     -> List.assoc "DEFAULT" colors
  | "dead"      -> List.assoc "DEFAULT" colors
  | "past"      -> List.assoc "DEFAULT" colors
  | "tasktext"  -> List.assoc "DEFAULT" colors
  | _ -> ""

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
    pp_close_box fmtr ();
    pp_print_cut fmtr ()
  in
  List.iter print_task lst

let print_grouped fmtr lst =
  let print_group g =
    fprintf fmtr "@[<h>@{<group>%s@}@]@," g
  in
  List.iter (fun (grp,tl) -> print_group grp; print_ordered fmtr tl) lst

let setup_formatter fmtr =
  pp_set_formatter_tag_functions fmtr
    { mark_open_tag   = open_tag;
      mark_close_tag  = close_tag;
      print_open_tag  = (fun _ -> ());
      print_close_tag = (fun _ -> ()) };
  pp_set_tags fmtr !color_print

let do_local_command c =
  let print_result = function
    | Listing (Ordered l) -> pp_open_vbox str_formatter 0; print_ordered str_formatter l; pp_close_box str_formatter ();
    | Listing (Grouped l) -> pp_open_vbox str_formatter 0; print_grouped str_formatter l; pp_close_box str_formatter ();
    | Error s|Info s -> fprintf str_formatter "@[%s@]@," s
    | Null -> ()
  in
  let flush() = print_string (flush_str_formatter()); flush_all() in
  let result = match c with
    | `Set(`BoolV("color", b)) -> color_print := b; setup_formatter str_formatter; Null
    | `SetList ->
	let print_option opt valu =
	  fprintf str_formatter "@[%s:@ %s@]@\n" opt valu in
	print_option "color" (if !color_print then "on" else "off");
	print_option "todo" (get_todo_file());
	print_option "done" (get_done_file());
	Null
    | `Help -> print_help str_formatter; Null
    | `Quit -> exit 0
    | _ -> do_command c
  in
  print_result result;
  flush()

let parse_local_command = function
  | ("quit"|"Quit"|"exit"|"Exit")::[]  -> `Quit
  | ("help"|"Help")::_                 -> `Help
  | ("set"|"Set")::[]                  -> `SetList
  | c -> parse_command c

let toplevel() = 
  Sys.catch_break true;
  read_files();
  Readline.using_history();
  do_local_command(`Set(`BoolV("color", !color_print)));
  print_endline "Type \"help\" for assistance, or \"quit\" to leave.";
  while true do
    let s = match Readline.readline "todo> " with
      | None -> print_newline(); "quit"
      | Some k -> let _ = Readline.add_history k in k
    in
    begin
      try
	let ary = wordlist_of_string s in
	do_local_command (parse_local_command ary);
      with
	| File_modified ->
	    Printf.printf "Todo file %s was externally modified. It was reloaded and the command was cancelled.\n" (get_todo_file());
	    read_files();
	    flush_all()
	| Invalid_command -> print_endline "Invalid command"
	| Invalid_option -> print_endline "Invalid option"
    end
  done

let rec parse_args = function
  | ("-t"|"--todo")::s::t -> let _ = do_command (`Set (`StringV ("todo", s))) in parse_args t
  | ("-d"|"--done")::s::t -> let _ = do_command (`Set (`StringV ("done", s))) in parse_args t
  | "-nc"::t              -> color_print := false; parse_args t
  | h::t as cmd ->
      read_files();
      do_local_command (`Set(`BoolV("color", !color_print)));
      do_local_command (parse_local_command cmd)
  | [] -> toplevel()

(* set default values for globals, defined in Command module *)
let _ =
  let todo_file = List.fold_left (fun p e -> Filename.concat p e) (Sys.getenv "HOME") ["todo"; "todo.txt"] in
  let done_file = List.fold_left (fun p e -> Filename.concat p e) (Sys.getenv "HOME") ["todo"; "done.txt"] in
  let _ = do_command (`Set (`StringV ("todo", todo_file))) in
  let _ = do_command (`Set (`StringV ("done", done_file))) in
  if not !Sys.interactive then parse_args (List.tl (Array.to_list Sys.argv))

(*
 * Local Variables:
 * compile-command:"make todoopt"
 * End:
 *)
