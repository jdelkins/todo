
type photo = string * string

external run_no_server : (string * string * string * string * string) -> (string -> string -> unit) -> string -> int
  = "run"

external run_server : (string * string * string * string * string) -> (string -> string -> unit) -> string -> (string * int) -> int
  = "run_server"

let run ~jid ~resource ~password ~handler ?photo ?server ?(port = -1) ?(logfile = "/dev/null") unit =
  let (photo_type, photo_path) =
    match photo with
      | None -> "", ""
      | Some (t, p) -> t, p
  in
  match server with
    | None -> run_no_server (jid, resource, password, photo_type, photo_path) handler logfile
    | Some srv  -> run_server (jid, resource, password, photo_type, photo_path) handler logfile (srv, port)

external send : string -> string -> unit
  = "send_message"

external halt : unit -> unit
  = "halt"

external log : string -> unit
  = "logmessage"
