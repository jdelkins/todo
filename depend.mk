todolib/command.cmo: todolib/task.cmi todolib/command.cmi 
todolib/command.cmx: todolib/task.cmx todolib/command.cmi 
todolib/command.cmi: todolib/task.cmi 
todolib/date.cmo: todolib/date.cmi 
todolib/date.cmx: todolib/date.cmi 
todolib/task.cmo: todolib/date.cmi todolib/task.cmi 
todolib/task.cmx: todolib/date.cmx todolib/task.cmi 
todolib/task.cmi: todolib/date.cmi 
cmdline/todo.cmo: todolib/task.cmi todolib/command.cmi 
cmdline/todo.cmx: todolib/task.cmx todolib/command.cmx 
cgi/cgi.cmo: todolib/task.cmi /usr/lib/ocaml/3.10.2/netstring/netencoding.cmi \
    /usr/lib/ocaml/3.10.2/netstring/netchannels.cmi \
    /usr/lib/ocaml/3.10.2/netcgi2/netcgi_cgi.cmi \
    /usr/lib/ocaml/3.10.2/netcgi2/netcgi.cmi cgi/jsonml.cmo \
    cgi/json-wheel/json_type.cmi cgi/json-wheel/json_io.cmi todolib/date.cmi \
    todolib/command.cmi 
cgi/cgi.cmx: todolib/task.cmx /usr/lib/ocaml/3.10.2/netstring/netencoding.cmi \
    /usr/lib/ocaml/3.10.2/netstring/netchannels.cmi \
    /usr/lib/ocaml/3.10.2/netcgi2/netcgi_cgi.cmi \
    /usr/lib/ocaml/3.10.2/netcgi2/netcgi.cmi cgi/jsonml.cmx \
    cgi/json-wheel/json_type.cmx cgi/json-wheel/json_io.cmx todolib/date.cmx \
    todolib/command.cmx 
cgi/jsonml.cmo: cgi/json-wheel/json_type.cmi 
cgi/jsonml.cmx: cgi/json-wheel/json_type.cmx 
todobot/bot.cmo: todobot/bot.cmi 
todobot/bot.cmx: todobot/bot.cmi 
todobot/todobot.cmo: todolib/task.cmi todolib/command.cmi todobot/bot.cmi 
todobot/todobot.cmx: todolib/task.cmx todolib/command.cmx todobot/bot.cmx 
