open Format
open Task

exception Invalid_command
exception File_modified
exception Invalid_option

type command_result =
  | Listing of listing
  | Error of string
  | Info of string
  | Null

type set_type =
  [ `StringV of string * string
  | `BoolV of string * bool ]

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

val get_filehash : unit -> Digest.t
val get_todo_file : unit -> string
val get_done_file : unit -> string

val assert_consistency : ?testhash:Digest.t -> unit -> unit
val read_files : unit -> unit
val do_command : [> command] -> command_result
val parse_command : string list -> [> command]
val wordlist_of_string : string -> string list

val sort_of_string : string -> taskmeta
val string_of_sort : taskmeta -> string
