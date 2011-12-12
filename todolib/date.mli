(* vim:set sw=2 ts=2: *)

exception Invalid_date

class date : int -> int -> int ->
object ('a)
  method year : int
  method month : int
  method day : int
  method julian : int
  method cmp : 'a -> int
  method to_string : string
  method to_tuple : (int * int * int)
  method days_in_month : int
  method weekday : int
end

val add_days : date -> int -> date
val date_of_string : string -> date
val today : unit -> date
val next_dow : string -> date
val next_sunday : unit -> date

