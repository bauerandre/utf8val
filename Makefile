.PHONY: default all opt doc install uninstall clean test
default: all opt
utf8uni.ml: urange.ml
	ocamlc -o urange urange.ml
	./urange > utf8uni.ml
all: utf8uni.ml
	ocamlc -c -g utf8uni.mli
	ocamlc -c -g utf8uni.ml
	ocamlc -c -g utf8val.mli
	ocamlc -c -g utf8val.ml
	ocamlc -a -g -o utf8val.cma utf8uni.cmo utf8val.cmo
opt:
	ocamlc -c -g utf8uni.mli
	ocamlopt -c -g utf8uni.ml
	ocamlc -c -g utf8val.mli
	ocamlopt -c -g utf8val.ml
	ocamlopt -a -g -o utf8val.cmxa utf8uni.cmx utf8val.cmx
doc:
	mkdir -p html
	ocamldoc -html -d html utf8uni.mli utf8val.mli
test:
	ocamlopt -o test_utf8val \
          utf8uni.mli utf8uni.ml utf8val.mli utf8val.ml test_utf8val.ml
	./test_utf8val
install:
	ocamlfind install utf8val META \
		$$(ls *.mli *.cm[ioxa] *.cmxa *.o *.a 2>/dev/null)
uninstall:
	ocamlfind remove utf8val
clean:
	rm -f *.cm[ioxa] *.o *.cmxa *.a *~
	rm -rf html
	rm -f urange utf8uni.ml test_utf8val
