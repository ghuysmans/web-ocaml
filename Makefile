all:
	ocamlbuild -pkg cohttp.lwt -pkg tyxml -pkg tyxml main.byte
