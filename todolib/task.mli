(* vim:set ts=2 sw=2: *)
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

class task : int -> string ->
object
  method get_id : int
  method set_id : int -> unit
  method contexts : string list
  method dates : Date.date list
  method projects : string list
  method waits : string list
  method priority : string
  method text : string
  method complete : bool
  method canceled : bool
  method plain_output : string
  method to_string : string
  method is_today : bool
  method is_tomorrow : bool
  method is_this_week : bool
  method is_past : bool
  method is_future : bool
  method to_taskrepr : taskrepr
  method output : out_channel -> unit
  method append : string -> unit
  method sub : string -> string -> unit
  method replace : string -> unit
  method doit : unit
  method cancel : unit
end

(* action on tasks *)
val move : int -> int -> unit
val get_task : int -> task
val delete_task : int -> unit
val add_task : string -> unit

(* file operations *)
val read_tasks : in_channel -> unit
val write_todo : out_channel -> unit
val archive : out_channel -> out_channel -> unit

(* listing and printing *)
val group_tasks_repr : ?filter:string list -> ?groups:taskmeta list -> ?sort:taskmeta -> unit -> listing
val list_tasks_repr : ?filter:string list -> ?sort:taskmeta -> unit -> listing
