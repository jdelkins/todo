open Format
open Command
open Task
open Json_type
open Json_type.Build

exception Invalid_variable of string

(*let cleanse_text = Netencoding.Html.encode_from_latin1*)
let cleanse_text = function s -> s

let make_help_jsonml () =
  let llg s = array [string "tr";
		     array [string "td";
			    objekt ["class", string "helpsection"; "colspan", string "2"];
			    string s]]
  in
  let ll1 lis = array [string "tr";
		       array [string "td";
			      objekt ["class", string "helpinfo"; "colspan", string "2"];
			      array ((string "ul")::(objekt ["class", string "helpinfo"])::lis)]]
  in
  let ll s = array [string "li"; objekt ["class", string "helpinfo"]; string s] in
  let tt1 tls = array [string "tr";
		       array [string "td"];
		       array [string "td"; array ((string "table")::tls)]]
  in
  let tt tag mark info = array [string "tr";
				array [string "td"; objekt ["class", string "helplist"]; string tag];
				array [string "td"; objekt ["class", string "helpmark"]; string mark];
				array [string "td"; objekt ["class", string "helpinfo"]; string info]]
  in
  array [
    string "table";
    objekt ["class", string "help"];
    array [string "colgroup";
	   array [string "col"; objekt ["width", string "150"]];
	   array [string "col"]];
    llg "Commands:";
    tt1 [
      tt  "list {[pat] {pat}...}"     "" "List the tasks (matching all of the {pat}'s if given). {pat} is a regular expression.";
      tt  "group {[pat] {pat}...}"    "" "List the tasks (matching {pat} if given), and arrange the output in groups according to the metadata. Tasks may be repeated because a given task often will have several pieces of metadata elegible for grouping. {pat} is a regular expression.";
      tt  "filter {[pat] {pat}...}"   "" "Filter (i.e. limit) the listing futher by adding one or more additional regular expression patterns to the filter list.";
      tt  "add [text]"                "" "Add a task with the given text. Any metadata tags in the text will be parsed and treated specially (see below). The non-exclusive patterns currently selected will be automatically added to the list. This will ensure that the pattern always shows up after adding it, UNLESS it also matches one of the exclusive patterns. See notes below regarding exclusive patterns. If one of the non-exclusive patterns is a regular expression with wildcards (e.g., [A-Z]+), this is probably not what you want, as the text is added literally: use add* instead.";
      tt  "add* [text]"               "" "Add a task with the given text, but don't automatically append the active non-exclusive patterns.";
      tt  "edit [i]"                  "" "Bring up an editing screen to edit task number i. Double click on the task text as a shortcut.";
      tt  "move [i] [j]"              "" "Reorder task number i to position j.";
      tt  "append [i] [text]"         "" "Append some text onto the end of task.";
      tt  "delete [i]"                "" "Kill task i without completing or cancelling it. The task is not marked for archival (as with \"do\" and \"cancel\"); it is simply deleted from the todo file.";
      tt  "replace [i] [text]"        "" "Replace all of the text--metadata and all--of task number i with [text].";
      tt  "sub [i] [pat] [repl]"      "" "Replace all occurances in task number i of [pat] with [repl]. [pat] is a regular expression.";
      tt  "do [i]"                    "" "Mark task number i as complete.";
      tt  "cancel [i]"                "" "Mark task number i as canceled.";
      tt  "archive"                   "" "Move completed and canceled tasks to the done file and remove them from the todo file.";
      tt  "help"                      "" "Present this help message." ];
    llg "Notes:";
    ll1 [
      ll  "Data of this application are stored in a plain text file. If you have access to this file, the format is simple: one task per line. Refer below for a discussion about metadata formats.";
      ll  "In the above command list, arguments presented with [square braces] are required.";
      ll  "In the above command list, arguments presented with {curly braces} are optional.";
      ll  "Filters may be exclusive or non-exlusive. Exclusive patterns start with ^ and exclude all tasks matching the subsequent regular expression. Examples:"];
    tt1 [
      tt  "p:secret"                  "" "Matches tasks labeled as \"project secret\"";
      tt  "^w:"                       "" "Matches tasks for which you are NOT waiting-for a labeled event";
      tt  "@office ^x: ^@phone"       "" "Tasks to be done at the office, not already done or cancelled, and not requiring a phone call"];
    ll1 [
      ll  "In defining tasks, the following tags are treated as special metadata, which are used to aggregate tasks together and to format the printout. Tasks may be given arbitrarily many of the constructs indicated with *.";];
    tt1 [
      tt  "@context"      "*" "Context";
      tt  "w:waiting-for" "*" "Waiting for";
      tt  "p:project"     "*" "Project";
      tt  "(A) - (Z)"     ""  "Priority";
      tt  "x:2007-01-01"  ""  "Completed on date";
      tt  "X:2007-01-01"  ""  "Canceled on date (marked as dead without having been completed)";
      tt  "d:2007-01-01"  "*" "Due date";
      tt  "||"            ""  "Ignored: a visual divider between the metadata and the text. This frontend does not show || but it is saved in the file." ];
    ll1 [
      ll  "Metadata of this type may be placed anywhere on the line. This program will reformat the metadata to be at the beginning of the line when saving the file.";
      ll  "Dates provided with the d:, x:, and X: constructs are flexible and can be specified in several formats. Formats without a year are interpreted to be the next nearby date (in the future). Some examples:";];
    tt1 [
      tt  "d:2007-01-01"   "" "";
      tt  "d:1/1/07"       "" "";
      tt  "d:1/1"          "" "";
      tt  "d:today"        "" "";
      tt  "d:tomorrow"     "" "";
      tt  "d:friday"       "" "(Next Friday. If today were Friday, it would mean one week from today. Same functionality for other days of the week as well, of course.)";
      tt  "d:."            "" "(interpreted as today)";
      tt  "d:.+5"          "" "(5 days from today)";
      tt  "d:2007-01-01+2" "" "(January 3, 2007)";
      tt  "d:2007-01-01-1" "" "(December 31, 2006)"];
    ll1 [
      ll  "Text arguments can be quoted; this is useful when specifiying regex patterns containing spaces.";
      ll  "Backslashes make the subsequent character literal (use \\\\ for a literal backslash as text).";
      ll  "Some commands have aliases: mv = move, del = delete, ls = list, gr = group, f = filter, a = append.";
      ll  "The first character of commands may be capitalized, but otherwise the command names are case sensitive." ]]

let make_edit_input tnum =
  let thetask = get_task tnum in
  let thetext = thetask#plain_output in
  array [string "div";
	 array [string "label";
		objekt ["for", string "edittask"; "class", string "edittask"];
		string "Edit task number ";
		array [string "span";
			objekt ["id", string "edittask_number"];
			string (string_of_int tnum)]];
	 array [string "br"];
	 array [string "textarea";
		objekt ["class", string "command";
			"id", string "edittask";
			"rows", string "3";
			"cols", string "100"];
		string thetext];
	 array [string "br"];
	 array [string "button";
		objekt ["id", string "edittask_apply"];
		string "Apply"];
	 array [string "button";
		objekt ["id", string "edittask_cancel"];
		string "Cancel"]]

let string_of_object c =
  let m = Marshal.to_string c [] in
  Netencoding.Base64.encode m

let object_of_string s =
  let m = Netencoding.Base64.decode s in
  Marshal.from_string m 0

let trs_of_reprlist reprs =
  let classes_of_tags = function
    | [] -> objekt []
    | lst -> objekt [ "class",  string(String.concat " " lst) ]
  in
  let td_of_repr_field (attrs,value) =
    array (match attrs with
      | [] -> [ string "td"; string value ]
      | _  -> [ string "td"; classes_of_tags attrs; string value])
  in
  let tr_of_task repr =
    let global_tags = repr.tags in
    let td_list = 
      (array [string "td";
	     objekt ["class", string (String.concat " " ("taskcheckbox"::global_tags))];
	     array [string "input"; objekt ["type", string "checkbox";
					    "class", string ("taskcheckbox" ^
							       (if repr.complete_cancelled = "" then "" else " dead"));
					    "taskid", string (string_of_int repr.id)]]])::
      (array [string "td";
	     objekt ["class", string (String.concat " " ("taskediticon"::global_tags))];
	     array [string "img"; objekt ["src", string "accessories-text-editor.png";
					  "class", string "taskediticon";
					  "taskid", string (string_of_int repr.id)]]])::
      (List.map td_of_repr_field
	 [ "taskmeta"::"id"::global_tags, Printf.sprintf "%d:" repr.id;
	   "taskmeta"::"done"::global_tags, cleanse_text repr.complete_cancelled;
	   "taskmeta"::"priority"::global_tags, cleanse_text repr.priority;
	   "taskmeta"::"contexts"::global_tags, cleanse_text (String.concat " " repr.contexts);
	   "taskmeta"::"dates"::global_tags, cleanse_text (String.concat " " repr.dates);
	   "taskmeta"::"projects"::global_tags, cleanse_text (String.concat " " repr.projects);
	   "taskmeta"::"waits"::global_tags, cleanse_text (String.concat " " repr.waits);
	   "tasktext"::global_tags, cleanse_text repr.text ])
    in
    match global_tags with
      | [] -> array ((string "tr")::td_list)
      | t  -> array ([string "tr"; classes_of_tags t]@td_list)
  in
  List.map tr_of_task reprs

let rec trs_of_grouped = function
  | (groupname,lst)::t -> ((array (
      [ string "tr";
	objekt ["class", string "group"];
	array [
	  string "td";
	  objekt ["class", string "group"; "colspan", string "10"];
	  string (cleanse_text groupname)]]))::
      (trs_of_reprlist lst))@
      (trs_of_grouped t)
  | [] -> []

let table_of_listing lsting =
  let tr_lst = match lsting with
    | Ordered l -> trs_of_reprlist l
    | Grouped l -> trs_of_grouped l
  in
  let tr_th = array (
    [ string "tr";
      objekt [ "class", string "taskhead" ]] @
      (List.map
	 (fun (text,id) -> array [ string "th"; objekt ["class", string (id ^ " taskhead")]; array [ string "span"; objekt [ "id", string id; "class", string "taskhead" ]; string (cleanse_text text) ]])
	 [ "Cp", "done"; "Ed", "edit"; "#","number"; "Done","complete"; "Pri","priority"; "Contexts","context"; "Dates","date"; "Projects","project"; "W4","wait"; "Task","text" ]))
  in
  array ((string "table")::(objekt [ "class", string "tasklist" ])::tr_th::tr_lst)

let local_parse_command = function
  | ("help"|"Help")::_ -> `Help
  | ("add*"|"Add*")::t -> `AddWithoutFilters t
  | ("edit"|"Edit")::i::[] -> `Edit (int_of_string i)
  | ("filter"|"Filter"|"f"|"F")::t -> `AddFilters t
  | l -> parse_command l

let local_do_command real_command listing_command =
  let rec extract_some_filters not_matching = function
    | (`Filter l)::_ ->
	let _, df = List.partition not_matching l in
	df
    | _::t -> extract_some_filters not_matching t
    | _ -> []
  in
  let positive_filters = match listing_command with
    | `List x -> extract_some_filters (fun f -> f.[0] = '^') x
    | `Group x -> extract_some_filters (fun f -> f.[0] = '^') x
    | _ -> []
  in
  match real_command with
    | `Edit i -> Info (Json_io.string_of_json (make_edit_input i))
    | `Help -> 
	let help_json = make_help_jsonml() in
	Info (Json_io.string_of_json help_json)
    | `Set _ -> Error "'set' command is disabled"
    | `Save -> Error "'save' command is disabled"
    | `Reload -> Error "'reload' command is disabled"
    | `AddWithoutFilters s -> do_command (parse_command ("add"::s))
    | `Add x -> do_command (`Add (String.concat " " (x::positive_filters)))
    | _ -> do_command real_command

let main (cgi: Netcgi.cgi) =
  todo_file := Filename.concat "todo" "todo.txt";
  done_file := Filename.concat "todo" "done.txt";
  read_files();
  let out = cgi#out_channel#output_string in
  let cmdtext = cgi#argument_value "command" in
  let sort = match cgi#argument_value "sort" with
    | "done" -> `Complete
    | "context" -> `Context
    | "date" -> `Date
    | "project" -> `Project
    | "wait" -> `Wait
    | "priority" -> `Priority
    | "complete" -> `Complete
    | "text" -> `Text
    | _ -> `Number
  in
  let (cmd, default_listing) =
    let dc = try object_of_string (cgi#argument_value "default_listing") with _ -> `Null in
    let rec kill_sort_and_filter = function
      | (`Sort _)::t -> kill_sort_and_filter t
      | (`Filter f)::t ->
	  let remove_this_filter = cgi#argument_value "kill_filter" in
	  let (_, keep) = List.partition ((=) remove_this_filter) f in
	  if keep = [] then kill_sort_and_filter t else ((`Filter keep)::(kill_sort_and_filter t))
      | h::t -> h::(kill_sort_and_filter t)
      | [] -> []
    in
    let fixed_dc = match dc with
      | `List x -> `List ((`Sort sort)::(kill_sort_and_filter x))
      | `Group x -> `Group ((`Sort sort)::(kill_sort_and_filter x))
      | _ -> `List [`Sort sort]
    in
    let fixed_dc_with_more_filters newfilts =
      let found = ref false in
      let rec add_on_filters = function
	| (`Filter oldfilts)::t -> found := true; (`Filter (oldfilts@newfilts))::(add_on_filters t)
	| h::t -> h::(add_on_filters t)
	| [] -> if !found then [] else [`Filter newfilts]
      in
      match fixed_dc with
	| `List x -> `List (add_on_filters x)
	| `Group x -> `Group (add_on_filters x)
    in
    try
      match local_parse_command (wordlist_of_string cmdtext) with
	| `List opts  -> let c = `List ((`Sort sort)::opts) in (`Null, c)
	| `Group opts -> let c = `Group ((`Sort sort)::opts) in (`Null, c)
	| `AddFilters newf -> (`Null, (fixed_dc_with_more_filters newf))
	| c -> (c, fixed_dc)
    with Invalid_command -> (`Bad_news, fixed_dc)
  in
  let (md5_trial: Digest.t) = try object_of_string (cgi#argument_value "md5") with _ -> "" in
  let result =
    try
      let _ = match cmd with `Null|`Help|`Set _|`Reload|`AddFilters _ -> () | _ -> assert_consistency ~testhash:md5_trial () in
      local_do_command cmd default_listing
    with
      | Invalid_command    -> Error (Printf.sprintf "Command \"%s\" is not valid" cmdtext)
      | Invalid_option     -> Error "Failed attempt to set an option."
      | File_modified      -> Error "File was modifed externally; command not completed."
      | Not_found          -> Error "A referenced task was not found."
      | Date.Invalid_date  -> Error "A date tag was specified in an invalid format, so the command failed."
      | _                  -> Error "An error occurred."
  in
  let filter =
    let rec find_filter = function
      | (`Filter f)::_ -> f
      | h::t -> find_filter t
      | [] -> []
    in
    match default_listing with
      | `List x -> find_filter x
      | `Group x -> find_filter x
      | _ -> []
  in
  let listing = match local_do_command default_listing `Null with
    | Listing t -> table_of_listing t
    | _ -> array [string "p"; objekt ["class", string "error"]; string "Something weird is going on: the default_listing did not return a listing."]
  in
  let response = match result with
    | Command.Null -> objekt [
	"md5", string (string_of_object !md5);
	"listing", (Json_io.save_json "/tmp/listing.json" listing; listing);
	"default_listing", string (string_of_object default_listing);
	"filter", array (List.map string filter) ]
    | Command.Error e -> objekt [
	"md5", string (string_of_object !md5);
	"listing", listing;
	"error", array [string "p"; objekt ["class", string "error"]; string e];
	"default_listing", string (string_of_object default_listing);
	"filter", array (List.map string filter) ]
    | Command.Info i -> objekt [
	"md5", string (string_of_object !md5);
	"listing", Json_io.json_of_string i;
	"default_listing", string (string_of_object default_listing)]
    | _ -> objekt [
	"md5", string (string_of_object !md5);
	"error", array [string "p"; objekt ["class", string "error"]; string "What?? A serious bug lurks..."];
	"default_listing", string (string_of_object default_listing) ]
  in
  cgi#set_header ~content_type:"application/json" ();
  out (Json_io.string_of_json response);
  cgi#out_channel#commit_work()

let _ = if not !Sys.interactive then
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  Netcgi_cgi.run ~output_type:(`Transactional buffered) main

(*
 * Local Variables:
 * tuareg-interactive-program:"ocaml -I +pcre -I +netsys -I +netstring -I json-wheel unix.cma str.cma date.cmo task.cmo pcre.cma netsys.cma netstring.cma jsonwheel.cma"
 * compile-command:"make todo.cgi"
 * End:
 *)

