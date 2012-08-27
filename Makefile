all: ocatra.cmo ocatra.cmx

SOURCES = httpCommon.ml httpRequest.ml httpResponse.ml httpServer.ml ocatra.ml

TEST_SOURCES = $(SOURCES) test.ml

EXAMPLE_SOURCES = $(SOURCES) example.ml

ocatra.cmo: $(SOURCES)
	ocamlfind c -c -thread -linkpkg -package unix,threads,str $(SOURCES)

ocatra.cmx: $(SOURCES)
	ocamlfind opt -c -thread -linkpkg -package unix,threads,str $(SOURCES)

test: $(TEST_SOURCES)
	ocamlfind c -o test -thread -linkpkg -package unix,threads,str,oUnit  $(TEST_SOURCES)

test.opt: $(TEST_SOURCES)
	ocamlfind opt -o test.opt -thread -linkpkg -package unix,threads,str,oUnit $(TEST_SOURCES)

example: $(EXAMPLE_SOURCES)
	ocamlfind c -o example -thread -linkpkg -package unix,threads,str $(EXAMPLE_SOURCES)

example.opt: $(EXAMPLE_SOURCES)
	ocamlfind opt -o example.opt -thread -linkpkg -package unix,threads,str $(EXAMPLE_SOURCES)

clean:
	rm -f *.cmo *.cmi *.cmx *.cma *.o test test.opt example example.opt

