(* vim:set sw=2 ts=2: *)

open Date
open Format

type taskmeta = [ `Complete | `Context | `Date | `Number | `Priority | `Project | `Text | `Wait | `All | `Other ]

type listing =
    Ordered of taskrepr list
  | Grouped of (string * taskrepr list) list
and taskrepr = {
  tags: string list;
  id: int;
  contexts: string list;
  dates: string list;
  projects: string list;
  waits: string list;
  priority: string;
  text: string;
  complete_cancelled: string
}
    

class task id s = object (self)
  val mutable id = id
  val mutable contexts = []
  val mutable dates = []
  val mutable projects = []
  val mutable waits = []
  val mutable priority = ""
  val mutable complete = None
  val mutable canceled = None
  val orig_format = s
  val mutable text = ""
  val mutable touched = false
  initializer self#prepare s
  method get_id = id
  method set_id i = id <- i
  method contexts = contexts
  method dates = dates
  method projects = projects
  method waits = waits
  method priority = priority
  method text = text
  method complete = match complete with Some _ -> true | None -> false
  method canceled = match canceled with Some _ -> true | None -> false
  method private prepare line =
    complete <- None; canceled <- None; contexts <- []; dates <- []; projects <- []; waits <- []; priority <- ""; text <- "";
    let lst = Str.split (Str.regexp "[ \t]+") line in
    let nth_word n = try Some (List.nth lst n) with Failure "nth" -> None in
    let stream = Stream.from nth_word in
    let dispatch_word = function
      |	w when Str.string_match (Str.regexp "^@\\(.*\\)") w 0		-> contexts <- (Str.matched_group 1 w)::contexts
      | w when Str.string_match (Str.regexp "^[dD]:\\(.*\\)") w 0	-> dates <- (date_of_string (Str.matched_group 1 w))::dates
      | w when Str.string_match (Str.regexp "^[pP]:\\(.*\\)") w 0	-> projects <- (Str.matched_group 1 w)::projects
      | w when Str.string_match (Str.regexp "^[wW]:\\(.*\\)") w 0	-> waits <- (Str.matched_group 1 w)::waits
      | w when Str.string_match (Str.regexp "^x:\\(.*\\)") w 0		-> complete <- Some (date_of_string (Str.matched_group 1 w))
      | w when Str.string_match (Str.regexp "^X:\\(.*\\)") w 0		-> canceled <- Some (date_of_string (Str.matched_group 1 w))
      | w when Str.string_match (Str.regexp "^(\\([A-Za-z]\\))$") w 0	-> priority <- String.uppercase (Str.matched_group 1 w)
      | w when w = "||"							-> ()
      | w								-> text <- (if text = "" then "" else text ^ " ") ^ w
    in
    Stream.iter dispatch_word stream;
    contexts <- List.rev contexts; dates <- List.rev dates; projects <- List.rev projects; waits <- List.rev waits
  method to_string =
    Printf.sprintf "% 3d: %s" id self#plain_output
  method is_today =
    let tod = today() in
    List.exists (fun d -> d#cmp tod = 0) dates
  method is_tomorrow =
    let tom = add_days (today()) 1 in
    List.exists (fun d -> d#cmp tom = 0) dates
  method is_this_week =
    let sun = next_sunday() in
    let tod = today() in
    List.exists (fun d -> d#cmp sun <= 0 && d#cmp tod > 0) dates
  method is_past =
    let tod = today() in
    List.exists (fun d -> d#cmp tod < 0) dates
  method is_future =
    let sun = next_sunday() in
    List.exists (fun d -> d#cmp sun > 0) dates
  method to_taskrepr =
    {
      tags = List.fold_left (fun l (b,t) -> if b then t::l else l) []
	[ self#is_today, "today";
	  self#is_tomorrow, "tomorrow";
	  self#is_this_week, "this_week";
	  self#is_past, "past";
	  self#is_future, "future";
	  self#complete || self#canceled, "dead";
	  priority = "A", "prioa";
	  priority = "B", "priob";
	  priority <> "A" && priority <> "B" && priority <> "", "priox"; ];
      id = id;
      contexts = List.map ((^) "@") contexts;
      dates = List.map (fun d ->  "d:" ^ d#to_string) dates;
      projects = List.map ((^) "p:") projects;
      waits = List.map ((^) "w:") waits;
      priority = if priority = "" then "" else ("(" ^ priority ^ ")");
      text = text;
      complete_cancelled =
	match complete, canceled with
	  | Some d, _ -> "x:" ^ d#to_string
	  | _, Some d -> "X:" ^ d#to_string
	  | _ -> ""
    }
  method plain_output =
    (* only one of canceled or complete is acceptable, complete overrides *)
    let rec cleanse_list = function
      | ""::t -> cleanse_list t
      | h::t -> h::(cleanse_list t)
      | [] -> []
    in
    let canceled_string = match canceled with None -> "" | Some d -> "X:" ^ d#to_string in
    let complete_string = match complete with None -> "" | Some d -> "x:" ^ d#to_string in
    let priority_string = if priority = "" then "" else "(" ^ priority ^ ")" in
    let contexts_string = List.fold_left (fun k e -> if k = "" then "@" ^ e else k ^ " @" ^ e) "" contexts in
    let dates_string = List.fold_left (fun k e -> if k = "" then "d:" ^ e#to_string else k ^ " d:" ^ e#to_string) "" dates in
    let projects_string = List.fold_left (fun k e -> if k = "" then "p:" ^ e else k ^ " p:" ^ e) "" projects in
    let waits_string = List.fold_left (fun k e -> if k = "" then "w:" ^ e else k ^ " w:" ^ e) "" waits in
    let string_list = [ canceled_string ^ complete_string; priority_string; contexts_string; dates_string; projects_string; waits_string] in
    let s = String.concat " " (cleanse_list string_list) in
    if s = "" then text else s ^ " || " ^ text
  method output oc =
    output_string oc (Printf.sprintf "%s\n" (if touched then self#plain_output else orig_format))
  method append newtext =
    let s = self#plain_output ^ " " ^ newtext in
    self#prepare s;
    touched <- true
  method sub oldt newt =
    let s = Str.global_replace (Str.regexp oldt) newt self#plain_output in
    self#prepare s;
    touched <- true
  method replace news =
    self#prepare news;
    touched <- true
  method doit =
    complete <- Some (today());
    canceled <- None;
    touched <- true
  method cancel =
    canceled <- Some (today());
    complete <- None;
    touched <- true
end
  
let all_tasks = ref []
  
let read_tasks f =
  let i = ref 1 in
  let rec make_task_list f =
    try
      let t = new task !i (input_line f) in
      incr i;
      t::(make_task_list f)
    with End_of_file -> []
  in
  all_tasks := make_task_list f
    
let all_priorities tl =
  let rec all_priorities_rec l = function
    | h::t when List.mem h#priority l || h#priority = "" -> all_priorities_rec l t
    | h::t -> all_priorities_rec (h#priority::l) t
    | [] -> l
  in
  List.sort compare (all_priorities_rec [] tl)
    
let all_x lister tl =
  let s = List.fold_left
    (fun l t ->
       l @ (List.fold_left
	      (fun m c -> if List.mem c (l@m) then m else c::m) [] (lister t)))
    [] tl
  in
  List.sort compare s
    
let all_contexts = all_x (fun t -> t#contexts)
let all_dates = all_x (fun t -> t#dates)
let all_waits = all_x (fun t -> t#waits)
let all_projects = all_x (fun t -> t#projects)

let priority_tasks s = List.filter (fun t -> t#priority = s)
let any_priority_tasks = List.filter (fun t -> t#priority <> "")
let date_tasks filter = List.filter (fun t -> List.exists filter t#dates)
let today_tasks = date_tasks (fun k -> k#cmp (today()) = 0)
let tomorrow_tasks = date_tasks (fun k -> k#cmp (add_days (today()) 1) = 0)
let this_week_tasks = date_tasks (fun k -> k#cmp (next_sunday()) <= 0 && k#cmp (today()) > 0)
let past_tasks = date_tasks (fun k -> k#cmp (today()) < 0)
let future_tasks = date_tasks (fun k -> k#cmp (next_sunday()) > 0)

let context_tasks c = List.filter (fun t -> List.exists (fun k -> k = c) t#contexts)
let project_tasks p = List.filter (fun t -> List.exists (fun k -> k = p) t#projects)
let wait_tasks w = List.filter (fun t -> List.exists (fun k -> k = w) t#waits)
let any_wait_tasks = List.filter (fun t -> t#waits <> [])

let move id1 id2 =
  if not (List.exists (fun a -> a#get_id = id1) !all_tasks) then raise Not_found;
  (* constrain target to range of used ids *)
  let max_id = List.fold_left (fun a b -> max a b#get_id) 1 !all_tasks in
  let id2 = max 1 id2 in
  let id2 = min max_id id2 in
  (* renumber the tasks between id1 and id2 inclusive *)
  let (middle, _) = List.partition (fun a -> a#get_id >= (min id1 id2) && a#get_id <= (max id1 id2)) !all_tasks in
  let idelta = if id1 > id2 then 1 else (-1) in
  List.iter (fun a -> if a#get_id = id1 then a#set_id id2 else a#set_id (a#get_id + idelta)) middle;
  all_tasks := List.sort (fun a b -> compare a#get_id b#get_id) !all_tasks

let get_task id =
  List.find (fun a -> a#get_id = id) !all_tasks
    
let delete_task id =
  let (_, keep) = List.partition (fun a -> a#get_id = id) !all_tasks in
  List.iter (fun a -> if a#get_id > id then a#set_id (pred a#get_id)) keep;
  all_tasks := keep
    
let add_task text =
  let newid = succ (List.fold_left (fun m t -> max m t#get_id) 0 !all_tasks) in
  let t = new task newid text in
  t#append ""; (* this is to "touch" the task *)
  all_tasks := !all_tasks @ [t]
    
let write_todo todof =
  List.iter (fun a -> a#output todof) !all_tasks
    
let archive todof donef =
  let (finished, keep) = List.partition (fun a -> a#complete || a#canceled) !all_tasks in
  List.iter (fun a -> delete_task a#get_id) finished;
  write_todo todof;
  List.iter (fun a -> a#output donef) finished
    
let sortfunction = function
  | `Context ->
      (fun x y ->
	 (* let minctxt a = List.fold_left min "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" a#contexts in *)
	 let minctxt a = try List.hd a#contexts with _ -> "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" in
	 compare (minctxt x) (minctxt y))
  | `Date ->
      (fun x y ->
	 (* let mindate a = List.fold_left (fun d1 d2 -> if d1#cmp d2 < 0 then d1 else d2) (new date 9999 1 1) a#dates in *)
	 let mindate a = try List.hd a#dates with _ -> new date 9999 1 1 in
	 (mindate x)#cmp (mindate y))
  | `Project ->
      (fun x y ->
	 (* let minprj a = List.fold_left min "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" a#projects in *)
	 let minprj a = try List.hd a#projects with _ -> "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" in
	 compare (minprj x) (minprj y))
  | `Wait ->
      (fun x y ->
	 (* let minwait a = List.fold_left min "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" a#waits in *)
	 let minwait a = try List.hd a#waits with _ -> "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" in
	 compare (minwait x) (minwait y))
  | `Priority ->
      (fun x y ->
	 match (x#priority, y#priority) with
	   | ("", "") -> 0
	   | ("", _) -> 1
	   | (_, "") -> (-1)
	   | (a, b) -> compare a b)
  | `Complete ->
      (fun x y ->
	 match (x#complete || x#canceled, y#complete || y#canceled) with
	   | (true, true) -> 0
	   | (true, false) -> 1
	   | (false, true) -> (-1)
	   | (false, false) -> 0)
  | `Text ->
      (fun x y -> compare x#text y#text)
  | `Number | _ ->
      (fun x y -> compare x#get_id y#get_id)
	
let group_some_tasks_repr ~tl ~groups ~sorter () =
  let shown = ref [] in
  let rec render_sublist = function
    | (tag,sublist,category)::t when  List.length sublist > 0 && (List.memq `All groups || List.memq category groups) ->
	(tag, List.map (fun task -> shown := task::!shown; task#to_taskrepr) (List.stable_sort sorter sublist))::(render_sublist t)
    | _::t -> render_sublist t
    | [] -> []
  in
  let grouplist =
(*    (List.map (fun tag -> (("PRIORITY " ^ tag), priority_tasks tag tl, `Priority)) (all_priorities tl)) @ *)
    [ "PRIORITY", any_priority_tasks tl, `Priority ]@
      [ "PAST", past_tasks tl, `Date;
	"TODAY", today_tasks tl, `Date;
	"THIS WEEK", this_week_tasks tl, `Date;
	"FUTURE", future_tasks tl, `Date ] @
      (List.map (fun tag -> (("CONTEXT @" ^ tag), context_tasks tag tl, `Context)) (all_contexts tl)) @
      (List.map (fun tag -> (("PROJECT " ^ tag), project_tasks tag tl, `Project)) (all_projects tl)) @
(*      (List.map (fun tag -> (("WAITING FOR " ^ tag), wait_tasks tag tl, `Wait)) (all_waits tl)) *)
      [ "WAITING", any_wait_tasks tl, `Wait ]
  in
  let mainlist = render_sublist grouplist in
  let otherlist = [ "OTHER", (let (_, not_shown) = List.partition (fun a -> List.memq a !shown) tl in not_shown), `Other ] in
  Grouped (mainlist @ (render_sublist otherlist))

let filter_tasks filts =
  if filts = [] then
    !all_tasks
  else
    List.fold_left
      (fun l filt ->
	 let re, gt_lt, lt_gt =
	   if filt.[0] = '^' then
	     (Str.regexp_case_fold (String.sub filt 1 ((String.length filt) - 1))), (>) 0, true
	   else
	     Str.regexp_case_fold filt, (<=) 0, false
	 in
	 let testf =
	   (fun t -> try gt_lt (Str.search_forward re t#plain_output 0) with Not_found -> lt_gt)
	 in
	 List.filter testf l)
      !all_tasks
      filts

let group_tasks_repr ?(filter = []) ?(groups = [`All]) ?(sort = `Number) () =
  let tl = filter_tasks filter in
  let sorter = sortfunction sort in
  group_some_tasks_repr ~tl ~groups ~sorter ()

let list_tasks_repr ?(filter = []) ?(sort = `Number) () =
  let tl = filter_tasks filter in
  let sorted_tasks = List.stable_sort (sortfunction sort) tl in
  Ordered (List.map (fun t -> t#to_taskrepr) sorted_tasks)
