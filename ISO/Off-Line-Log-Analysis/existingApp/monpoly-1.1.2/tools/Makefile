OCAMLOPT = ocamlopt
OCAMLC = ocamlc

# Set here which complier will be used
OCAML = $(OCAMLOPT)

FLAGS = -I ../src

ifeq "$(OCAML)" "$(OCAMLC)"
  O = cmo
  A = cma
  LIBS = -custom unix.$(A) str.$(A)
else
  O = cmx
  A = cmxa
  LIBS = unix.$(A) str.$(A) 
  OCAMLDEPFLAGS = -native
endif

OCAMLEXE = $(OCAML) $(FLAGS) $(LIBS)

MONPOLY_LIB = monpoly.$(A)

$(MONPOLY_LIB): 
	cd ../src && $(MAKE) $@

mfotl2sql: $(MONPOLY_LIB) mfotl2sql.ml
	$(OCAMLEXE) -I ../src/ ../src/$(MONPOLY_LIB) mfotl2sql.ml -o $@

table2log: $(MONPOLY_LIB) table2log.cmi table2log.$(O)
	$(OCAMLEXE) -I ../src/ ../src/$(MONPOLY_LIB) table2log.ml -o $@

%.cmi: %.mli $(MONPOLY_LIB)
	$(OCAMLC) $(FLAGS) -c $<

%.cmo: %.ml $(MONPOLY_LIB) 
	$(OCAMLC) $(FLAGS) -c $<

%.cmx: %.ml $(MONPOLY_LIB) 
	$(OCAMLOPT) $(FLAGS) -c $<


clean:
	rm -f *.cm[iox]
