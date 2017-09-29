include $(shell ocamlc -where)/Makefile.config

OCAMLC = ocamlc -bin-annot
OCAMLFLAGS = -w +A-4-17-27-32-44-45 -I +compiler-libs -safe-string

.PHONY: all
all: common.cmo ch02.cmo ch03.cmo

# Pattern rules

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

clean:
	rm -f *.cm{i,o,t}
