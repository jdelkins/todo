open Format
open Command

exception Invalid_variable of string

let cleanse_text = Netencoding.Html.encode_from_latin1
let in_dead_row = ref false (* this is an annoying hack to accomodate firefox, which doesn't apply text-decoration properly to tr elements *)

let string_of_object c =
  let m = Marshal.to_string c [] in
  Netencoding.Base64.encode m

let object_of_string s =
  let m = Netencoding.Base64.decode s in
  Marshal.from_string m 0

let open_tag = function
  | "tr.group" -> "<tr><td colspan=8 class=\"group\">"
  | "tr.taskdead" -> in_dead_row := true; "<tr class=\"taskdead\">"
  | w when Str.string_match (Str.regexp "^\\([a-zA-Z0-9]+\\)\\.\\([a-zA-Z0-9]+\\)$") w 0 ->
      let tag = Str.matched_group 1 w in
      let cls = (Str.matched_group 2 w) ^ (if tag = "td" && !in_dead_row then " dead" else "") in
      Printf.sprintf "<%s class=\"%s\">" tag cls
  | _ -> ""

let close_tag = function
  | "tr.group" -> "</td></tr>"
  | "tr.taskdead" -> in_dead_row := false; "</tr>"
  | w when Str.string_match (Str.regexp "^\\([a-z]+\\)\\.\\([a-z]+\\)$") w 0 ->
      let tag = Str.matched_group 1 w in
      Printf.sprintf "</%s>" tag
  | _ -> ""

let begin_page cgi title =
  (* Output the beginning of the page with the passed [title]. *)
  let out = cgi#out_channel#output_string in
  out "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
  out "<HTML>\n";
  out "<HEAD>\n";
  out ("<TITLE>" ^ cleanse_text title ^ "</TITLE>\n")

let end_page cgi =
  let out = cgi#out_channel#output_string in
  out "</BODY>\n";
  out "</HTML>\n";
  cgi#out_channel#commit_work()

let page_list (cgi: Netcgi.cgi) sort cmd =
  let out = cgi#out_channel#output_string in
  let opts = match cmd with
    | `List x -> x
    | `Group x -> x
    | _ -> []
  in
  let filtertext = List.fold_left
    (fun s k ->
       match k with
	 | `Filter f -> cleanse_text (Printf.sprintf "Filtered by \"%s\"" f)
	 | _ -> s)
    "Not filtered" opts
  in
  begin_page cgi "Todo List";
  out "<SCRIPT TYPE=\"text/javascript\">\n";
  out "function sortby(col) {\n";
  out (Printf.sprintf "  document.location = \"%s&sort=\" + col;\n" (cleanse_text (cgi#url ~with_query_string:(`This [Netcgi.Argument.simple "page" "list"])  ())));
  out "}\n";
  out "function setfiltertext(txt) {\n";
  out "  parent.frames['commander'].document.getElementById('filtertext').innerHTML = txt;\n";
  out "}\n";
  out "</SCRIPT>\n";
  out "<STYLE TYPE=\"text/css\">\n";
  out "body { background: white; color: black }\n";
  out "table { border-collapse: collapse; }\n";
  out ".heading { background: gray; color: black; border: medium solid }\n";
  out "th { font-size: medium; font-weight: bold; font-family: \"bitstream vera sans\", verdana, arial, sans-serif; text-align: left }\n";
  out "th.sorted { color: rgb(100,0,0); text-decoration: underline }\n";
  out "td { border-style: dotted hidden; border-color: rgb(200,200,200); border-width: thin; background: white; font-size: small; font-family: \"bitstream vera sans\", verdana, arial, sans-serif  }\n";
  out "td.label { background: #dddddd; color: black; font-weight: bold; border: thin solid }\n";
  out "td.taskid   { font-weight: bold; text-align: right }\n";
  out "td.tasktext { font-size: small }\n";
  out "td.group { background: #999999; font-size: medium }\n";
  out "td.dead { color: rgb(150,150,150); text-decoration: line-through }\n";
  out "tr.taskprioa { color: rgb(255,100,100) }\n";
  out "tr.taskpriob { color: rgb(100,255,100) }\n";
  out "tr.taskpriox { color: rgb(100,100,255) }\n";
  out "tr.tasktoday { color: rgb(255,50,50); font-weight: bold }\n";
  out "tr.taskpast  { color: rgb(50,255,50); font-weight: bold }\n";
  out "tr.taskdead  { color: rgb(150,150,150); text-decoration: line-through }\n";
  out "</STYLE>\n";
  out "</HEAD>\n";
  out (Printf.sprintf "<BODY onload=\"setfiltertext('%s')\">\n" filtertext);
  out "<table><tr class=\"heading\">";
  List.iter
    (fun (label, var, sortcol) ->
       out "<th";
       if var = sort then out " class=\"sorted\"";
       out (Printf.sprintf "><span onclick=sortby(\"%s\")>" sortcol);
       out (cleanse_text label);
       out "</span></th>")
    [ "#", `Number, "number";
      "Done", `Complete, "complete";
      "Priority", `Priority, "priority";
      "Contexts", `Context, "context";
      "Dates", `Date, "date";
      "Projects", `Project, "project";
      "Waiting", `Wait, "wait";
      "Task", `Text, "text" ];
  out "\n";
  do_command str_formatter cmd;
  out (flush_str_formatter());
  out "</table>\n";
  out "<SCRIPT TYPE=\"text/javascript\">\n";
  out (Printf.sprintf "parent.frames['commander'].document.commander.filehash.value = \"%s\";\n" (string_of_object !md5));
  out "</SCRIPT>\n";
  end_page cgi

let page_commander (cgi: Netcgi.cgi) =
  let out = cgi#out_channel#output_string in
  begin_page cgi "Todo Commander";
  out "<SCRIPT type=\"text/javascript\">\n";
  out "  function commander_submit() {\n";
  out "    document.commander.command.value = document.dummy.command.value;\n";
  out "    document.dummy.command.value = \"\";\n";
  out "    document.commander.submit();\n";
  out "    return false;\n";
  out "  }\n";
  out "</SCRIPT>\n";
  out "<BODY>\n";
  out "<form action=\"\" onsubmit=\"return commander_submit()\" method=\"post\" name=\"dummy\">\n";
  out "<span style=\"font-family: monospace; font-size: small\">todo&gt;</span> <input style=\"font-family: monospace; font-size: small\" type=\"text\" name=\"command\" size=80 />\n";
  out "<label name=\"filtertext\" id=\"filtertext\">Not filtered</label><br>\n";
  out "</form>\n";
  out (Printf.sprintf "<form name=\"commander\" method=\"post\" target=\"list\" action=\"%s\">\n" (cgi#url()));
  out "<input type=\"hidden\" name=\"command\" />\n";
  out "<input type=\"hidden\" name=\"filehash\" value=\"\" />\n";
  out "<input type=\"hidden\" name=\"page\" value=\"list\" />\n";
  out "</form>\n";
  end_page cgi

let page_master (cgi: Netcgi.cgi) =
  let out = cgi#out_channel#output_string in
  begin_page cgi "Todo";
  out "<SCRIPT type=\"text/javascript\">\n";
  out "function reload_list() {\n";
  out "  frm = document.getElementsByName(\"list\")[0];\n";
  out (Printf.sprintf "  frm.src = \"%s\";\n" (cleanse_text (cgi#url ~with_query_string:(`This [Netcgi.Argument.simple "page" "list"]) ())));
  out "}\n";
  out "</SCRIPT>\n";
  out "<BODY onload=\"setInterval('reload_list()', 60000)\">\n";
  out "<TABLE width=100% border=0><tr><td>\n";
  out (Printf.sprintf "<IFRAME name=\"commander\" id=\"commander\" frameborder=0 width=100%% height=40 src=\"%s\">\n" (cgi#url ~with_query_string:(`This [Netcgi.Argument.simple "page" "commander"]) ()));
  out "[commander]</IFRAME>\n</td></tr><tr><td>\n";
  out (Printf.sprintf "<IFRAME name=\"list\" id=\"list\" frameborder=0 width=100%% height=10000 src=\"%s\">\n" (cgi#url ~with_query_string:(`This [Netcgi.Argument.simple "page" "list"]) ()));
  out "[list]</IFRAME>\n";
  out "</td></tr></TABLE>\n";
  end_page cgi

let main (cgi: Netcgi.cgi) =
  let cookies = ref [] in
  pp_set_formatter_tag_functions str_formatter
    { mark_open_tag = open_tag;
      mark_close_tag = close_tag;
      print_open_tag = (fun _ -> ());
      print_close_tag = (fun _ -> ()) };
  pp_set_tags str_formatter true;
  todo_file := Filename.concat "todo" "todo.txt";
  done_file := Filename.concat "todo" "done.txt";
  read_files();
  let sorttext =
    let t = cgi#argument_value "sort" in
    if t = "" then
      try
	Netcgi_common.Cookie.value (cgi#environment#cookie "sort")
      with Not_found -> "number"
    else begin
      cookies := (Netcgi_common.Cookie.make "sort" t)::!cookies;
      t
    end
  in
  let sort = match sorttext with
    | "context" -> `Context
    | "date" -> `Date
    | "project" -> `Project
    | "wait" -> `Wait
    | "priority" -> `Priority
    | "complete" -> `Complete
    | "text" -> `Text
    | _ -> `Number
  in
  let cmdtext = cgi#argument_value "command" in
  let default_cmd =
    try
      let rec kill_sort = function
	| (`Sort _)::t -> kill_sort t
	| h::t -> h::(kill_sort t)
	| [] -> []
      in
      let dc = object_of_string (Netcgi_common.Cookie.value (cgi#environment#cookie "defaultcommand")) in
      match dc with
	| `List x -> `List ((`Sort sort)::(kill_sort x))
	| `Group x -> `Group ((`Sort sort)::(kill_sort x))
	| x -> x
    with Not_found ->`List [`Sort sort; `Cleanse cleanse_text]
  in
  let cmd =
    if cmdtext <> "" then
      match parse_command (wordlist_of_string cmdtext) with
	| `List opts as x -> cookies := (Netcgi_common.Cookie.make "defaultcommand" (string_of_object x))::!cookies; `List ((`Sort sort)::opts)
	| `Group opts as x -> cookies := (Netcgi_common.Cookie.make "defaultcommand" (string_of_object x))::!cookies; `Group ((`Sort sort)::opts)
	| c ->
	    let (trial_md5 : Digest.t) = object_of_string (cgi#argument_value "filehash") in
	    assert_consistency ~testhash:trial_md5 ();
	    do_command str_formatter c;
	    default_cmd
    else
      default_cmd
  in
  if !cookies <> [] then
    cgi#set_header ~set_cookies:!cookies ~content_type:"text/html" ();
  match cgi#argument_value "page" with
    | "commander" -> page_commander cgi
    | "list" -> page_list cgi sort cmd
    | "" -> page_master cgi
    | _ as v -> raise (Invalid_variable (Printf.sprintf "unknown value for \"page\": \"%s\")" v))

let _ = if not !Sys.interactive then
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  Netcgi_cgi.run ~output_type:(`Transactional buffered) main
