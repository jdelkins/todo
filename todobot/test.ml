let (jid, password, resource, permit, logfile, server, port) =
      let lexbuf = Lexing.from_channel (open_in "/home/jde/.todobot.cf") in
      Simpleconfig_parser.main Simpleconfig_lexer.token lexbuf
;;
