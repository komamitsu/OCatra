all: main test example
main: ocatra.cma ocatra.cmxa
test: test.byte test.opt
example: example.byte example.opt

SOURCES = ocatraCommon.ml ocatraHttpCommon.ml ocatraHttpRequest.ml ocatraHttpResponse.ml ocatraHttpServer.ml ocatra.ml
PACKAGES = unix,threads,str,lwt,lwt.unix,lwt.syntax
TEST_PACKAGES = $(PACKAGES),oUnit

ocatra.cma: $(SOURCES)
	ocamlfind c -a -o $@ -thread -package $(PACKAGES) $^

ocatra.cmxa: $(SOURCES)
	ocamlfind opt -a -o $@ -thread -package $(PACKAGES) $^

test.byte: ocatra.cma test.ml
	ocamlfind c -o $@ -thread -linkpkg -package $(TEST_PACKAGES) $^

test.opt: ocatra.cmxa test.ml
	ocamlfind opt -o $@ -thread -linkpkg -package $(TEST_PACKAGES) $^

example.byte: ocatra.cma example.ml
	ocamlfind c -o $@ -thread -linkpkg -package $(PACKAGES) $^

example.opt: ocatra.cmxa example.ml
	ocamlfind opt -o $@ -thread -linkpkg -package $(PACKAGES) $^

clean:
	rm -f *.cmo *.cmi *.cmx *.cma *.cmxa *.o *.a test.byte test.opt example.byte example.opt

.PHONY: all main test example
