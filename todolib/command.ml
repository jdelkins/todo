open Task
open Format

exception Invalid_command
exception File_modified
exception Invalid_option

type command_result =
  | Listing of listing
  | Error of string
  | Info of string
  | Null

type set_type =
    [  `StringV of string * string
    |  `BoolV of string * bool ]

type print_option =
    [ `Groups of taskmeta list
    | `Sort of taskmeta
    | `Filter of string list ]

type command =
    [  `Add of string
    |  `Move of int * int
    |  `Append of int * string
    |  `Delete of int
    |  `Sub of int * string * string
    |  `Replace of int * string
    |  `Do of int
    |  `Cancel of int
    |  `Save
    |  `Archive
    |  `Reload
    |  `List of print_option list
    |  `Group of print_option list
    |  `Set of set_type
    |  `Null ]

(******
 * GLOBALS
 * and functions needed for global variable initialization
 ******)

(* an application should at least set todo_file and done_file to a meaningful default path after startup *)
let todo_file = ref "/tmp/todo.txt"
let done_file = ref "/tmp/done.txt"
let md5 = ref ("" : Digest.t)

let get_filehash() = !md5
let get_todo_file() = !todo_file
let get_done_file() = !done_file

let read_files() =
  let infile = open_in !todo_file in
  md5 := Digest.channel infile (-1);
  seek_in infile 0;
  read_tasks infile;
  close_in infile

let test_md5 ?(testhash = !md5) () =
  let infile = open_in !todo_file in
  let md5_new = Digest.channel infile (-1) in
  close_in infile;
  md5_new = testhash

let assert_consistency ?(testhash = !md5) () =
  if not (test_md5 ~testhash ()) then raise File_modified

let parse_print_option lst =
  let filter = ref None in
  let groups = ref None in
  let sort = ref None in
  List.iter (fun i -> match i with
	       | `Filter s -> filter := Some s
	       | `Sort meta -> sort := Some meta
	       | `Groups lst -> groups := Some lst) lst;
  (!filter, !groups, !sort)

let rec do_command  (arg : [> command ]) =
  match arg with
    | `Null -> Null
    | `Add(s) ->
	assert_consistency();
	add_task s;
	do_command `Save
    | `Move(i, j) ->
	assert_consistency();
	move i j;
	do_command `Save
    | `Append(i, s) -> assert_consistency(); (get_task i)#append s; do_command `Save
    | `Delete(i) -> assert_consistency(); delete_task i; do_command `Save
    | `Sub(i, s1, s2) -> assert_consistency(); (get_task i)#sub s1 s2; do_command `Save
    | `Replace(i, s) -> assert_consistency(); (get_task i)#replace s; do_command `Save
    | `Do(i) -> assert_consistency(); (get_task i)#doit; do_command `Save
    | `Cancel(i) -> assert_consistency(); (get_task i)#cancel; do_command `Save
    | `Save -> assert_consistency(); let t = open_out !todo_file in write_todo t; close_out t; read_files(); Null
    | `Archive ->
	assert_consistency();
	let t = open_out !todo_file in
	let d = open_out_gen [Open_append; Open_creat] 0o0600 !done_file  in
	archive t d;
	close_out t;
	close_out d;
	Null
    | `Reload -> read_files(); Null
    | `List(lst) ->
	if not (test_md5()) then read_files();
	let (filt, _, srt) = parse_print_option lst in
	Listing (list_tasks_repr ?filter:filt ?sort:srt ())
    | `Group(lst) ->
	if not (test_md5()) then read_files();
	let (filt, grp, srt) = parse_print_option lst in
	Listing (group_tasks_repr ?filter:filt ?groups:grp ?sort:srt ())
    | `Set(`StringV(opt, value)) ->
	begin
	  match String.lowercase opt with
	    | "todo" -> todo_file := value
	    | "done" -> done_file := value
	    | _ -> raise Invalid_option
	end;
	Null
    | `Set(`BoolV(_, _)) -> raise Invalid_option
    | x -> raise Invalid_command

let string_of_sort = function
  | `Context   -> "context"
  | `Date      -> "date"
  | `Project   -> "project"
  | `Wait      -> "wait"
  | `Priority  -> "priority"
  | `Complete  -> "complete"
  | `Text      -> "text"
  | _          -> "number"

let sort_of_string = function
  | "done"     -> `Complete
  | "context"  -> `Context
  | "date"     -> `Date
  | "project"  -> `Project
  | "wait"     -> `Wait
  | "priority" -> `Priority
  | "complete" -> `Complete
  | "text"     -> `Text
  | _          -> `Number

let parse_command args =
  let eat_all_string sl =
    let s = List.fold_left (fun s e -> if s = "" then e else s ^ " " ^ e) "" sl in
    Str.global_replace (Str.regexp "\n") " " s
  in
  match args with
    | ("add"|"Add")::t                                    -> `Add (eat_all_string t)
    | ("move"|"Move"|"mv"|"Mv")::i::j::[]                 -> `Move (int_of_string i, int_of_string j)
    | ("append"|"Append"|"a"|"A")::i::t                   -> `Append (int_of_string i, eat_all_string t)
    | ("delete"|"Delete"|"del"|"Del")::i::[]              -> `Delete (int_of_string i)
    | ("sub"|"Sub")::i::w1::w2::[]                        -> `Sub (int_of_string i, w1, w2)
    | ("sub"|"Sub")::i::w1::[]                            -> `Sub (int_of_string i, w1, "") (* special case: parser kills all blank elements, but this is a common construct *)
    | ("replace"|"Replace")::i::t                         -> `Replace (int_of_string i, eat_all_string t)
    | ("do"|"Do")::i::[]                                  -> `Do (int_of_string i)
    | ("cancel"|"Cancel")::i::[]                          -> `Cancel(int_of_string i)
    | ("save"|"Save")::[]                                 -> `Save
    | ("archive"|"Archive")::[]                           -> `Archive
    | ("reload"|"Reload")::[]                             -> `Reload
    | ("list"|"List"|"ls"|"Ls")::[]                       -> `List ([])
    | ("list"|"List"|"ls"|"Ls")::"-s"::s::[]              -> `List ([`Sort (sort_of_string s)])
    | ("list"|"List"|"ls"|"Ls")::"-s"::s::t               -> `List ([`Sort (sort_of_string s); `Filter t])
    | ("list"|"List"|"ls"|"Ls")::t                        -> `List ([`Filter t])
    | ("group"|"Group"|"gr"|"Gr")::[]                     -> `Group ([])
    | ("group"|"Group"|"gr"|"Gr")::"-s"::s::[]            -> `Group ([`Sort (sort_of_string s)])
    | ("group"|"Group"|"gr"|"Gr")::"-s"::s::t             -> `Group ([`Sort (sort_of_string s); `Filter t])
    | ("group"|"Group"|"gr"|"Gr")::t                      -> `Group ([`Filter t])
    | ("set"|"Set")::o::("on"|"true"|"On"|"True")::[]     -> `Set (`BoolV(o, true))
    | ("set"|"Set")::o::("off"|"false"|"Off"|"False")::[] -> `Set (`BoolV(o, false))
    | ("set"|"Set")::o::v::[]                             -> `Set (`StringV(o, v))
    | ""::[]                                              -> `Null
    | []                                                  -> `Null
    | _                                                   -> raise Invalid_command

let rec bust_stream wd s =
  let rec bust_quoted_word c s =
    try
      match Stream.next s with
	| '\\'          -> (String.make 1 (Stream.next s)) ^ (bust_quoted_word c s)
	| k when k = c  -> ""
	| k             -> (String.make 1 k) ^ (bust_quoted_word c s)
    with Stream.Failure ->
      ""
  in
  try
    match Stream.next s with
      | ' '|'\t'      -> wd::(bust_stream "" s)
      | '\\'          -> let nextc = String.make 1 (Stream.next s) in bust_stream (wd ^ nextc) s
      | '"'|'\'' as c -> let w = bust_quoted_word c s in bust_stream (wd ^ w) s
      | c             -> bust_stream (wd ^ (String.make 1 c)) s
  with Stream.Failure ->
    [wd]

let wordlist_of_string s =
  let raw_list = bust_stream "" (Stream.of_string s) in
  let rev = List.fold_left (fun res c -> if c = "" then res else c::res) [] raw_list in
  List.rev rev
