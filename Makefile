all: talk

clean:
	rm -rf _build talk talk.annot talk.cmo talk.cmi

talk: talk.ml
	ocamlfind ocamlc -annot -linkpkg -thread -package core -package async \
    talk.ml -o talk
