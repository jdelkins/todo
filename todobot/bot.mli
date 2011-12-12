
type photo = string * string

val run : jid:string -> resource:string -> password:string -> handler:(string -> string -> unit) ->
  ?photo:photo -> ?server:string -> ?port:int ->
  ?logfile:string -> unit -> int

external send : string -> string -> unit
  = "send_message"

external halt : unit -> unit
  = "halt"

external log : string -> unit
  = "logmessage"
