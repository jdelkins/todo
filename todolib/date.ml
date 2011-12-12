exception Invalid_date

let is_leap year =
  let divis m =
    if year mod m = 0 then 1 else 0
  in
  (divis 400) = 1 || (divis 100) lxor (divis 4) = 1

let days_in_month year month =
  match month with
    |x when x < 1 || x > 12 -> raise Invalid_date
    |2        -> if (is_leap year) then 29 else 28
    |4|6|9|11 -> 30
    |_        -> 31

class date year month day =
object (self : 'a)
  val julian = 367*year - 7*(year + ((month + 9)/12))/4 + 275*month/9 + day + 1721014
  method year   = year
  method month  = month
  method day    = day
  method julian = julian
  method cmp (other : 'a) = compare julian other#julian
  method to_string = Printf.sprintf "%04d-%02d-%02d" year month day
  method to_tuple = (year, month, day)
  method days_in_month = days_in_month year month
  initializer
    if (month < 1) || (month > 12) then
      raise Invalid_date;
    if (day < 1) || (day > days_in_month year month) then
      raise Invalid_date;
  method weekday =
    let cc = 2 * (3 - ((year/100) mod 4)) in
    let yy = 5 * (year mod 100) / 4 in
    let mt_normal = [| 0; 3; 3; 6; 1; 4; 6; 2; 5; 0; 3; 5 |] in
    let mt_leap   = [| 6; 2; 3; 6; 1; 4; 6; 2; 5; 0; 3; 5 |] in
    (cc + yy + (if is_leap year then mt_leap else mt_normal).(month - 1) + day) mod 7
end

(* This is the baum4 algorithm from http://vsg.cape.com/~pbaum/date/injdimp.htm *)
let date_of_julian jd =
  let (y, m, d) =
    let z = jd - 1721119 in
    let h = 100 * z - 25 in
    let a = h / 3652425 in
    let b = a - a / 4 in
    let year = (100 * b + h) / 36525 in
    let c = b + z - 365 * year - year/4 in
    let month = (5 * c + 456) / 153 in
    let day = c - (153 * month - 457) / 5 in
    if month > 12 then (year+1, month-12, day) else (year, month, day)
  in
  new date y m d

let add_days d1 delta =
  date_of_julian (d1#julian + delta)

let today () =
  let tm = Unix.localtime (Unix.gettimeofday()) in
  new date (1900 + tm.Unix.tm_year) (1 + tm.Unix.tm_mon) tm.Unix.tm_mday

let next_dow d =
  let map = [("sunday", 0); ("monday", 1); ("tuesday", 2); ("wednesday", 3); ("thursday", 4); ("friday", 5); ("saturday", 6)] in
  let i = List.assoc (String.lowercase d) map in
  let rec find_day d j = if d#weekday = j then d else (find_day (add_days d 1) j) in
  find_day (add_days (today()) 1) i

let date_of_string s =
  let basedate, mark = match s with
    | v when Str.string_match
	(Str.regexp_case_fold "^\\(sunday\\|monday\\|tuesday\\|wednesday\\|thursday\\|friday\\|saturday\\)") v 0 ->
	(next_dow (Str.matched_group 1 v), Str.match_end())
    | v when Str.string_match (Str.regexp_case_fold "^\\(\\.\\|today\\|tod\\|tomorrow\\|yesterday\\)") v 0 ->
	let d = Str.match_end() in
	begin
	  match String.lowercase (Str.matched_group 1 v) with
	    | "yesterday" -> (add_days (today()) (-1), d)
	    | "tomorrow"  -> (add_days (today()) 1, d)
	    | _ -> (today(), d)
	end
    | v when Str.string_match
	(Str.regexp "^\\([0-9]?[0-9]\\)[-./]\\([0-9]?[0-9]\\)[-./]\\([0-9][0-9]\\([0-9][0-9]\\)?\\)") v 0 ->
	let yy =
	  let g = int_of_string (Str.matched_group 3 v) in
	  if g < 100 then
	    if g >= 70 then g + 1900 else g + 2000
	  else
	    g
	in
	let mm = int_of_string (Str.matched_group 1 v) in
	let dd = int_of_string (Str.matched_group 2 v) in
	(new date yy mm dd, Str.match_end())
    | v when Str.string_match
	(Str.regexp "^\\(\\([0-9][0-9][0-9][0-9]\\)[-./]\\)?\\([0-9]?[0-9]\\)[-./]\\([0-9]?[0-9]\\)") v 0 ->
	let mm = int_of_string (Str.matched_group 3 v) in
	let dd = int_of_string (Str.matched_group 4 v) in
	let yy =
	  try
	    int_of_string (Str.matched_group 2 v)
	  with
	      Not_found ->
		let tod = today() in
		let trial = new date tod#year mm dd in
		if trial#cmp tod < 0 then
		  tod#year + 1
		else
		  tod#year
	in
	(new date yy mm dd, Str.match_end())
    | _ -> raise Invalid_date
  in
  let delta = match s with
    | v when Str.string_match (Str.regexp "\\([+-]\\)\\([0-9]+\\)$") v mark ->
	if Str.matched_group 1 v = "-" then
	  int_of_string (Str.matched_string v)
	else
	  int_of_string (Str.matched_group 2 v)
    | v when Str.string_match (Str.regexp ".") v mark -> raise Invalid_date
    | _ -> 0
  in
  add_days basedate delta

let next_sunday() = next_dow "sunday"
