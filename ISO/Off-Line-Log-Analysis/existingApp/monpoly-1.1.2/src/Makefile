OCAMLOPT = ocamlopt
OCAMLC = ocamlc

# Set here which complier will be used
OCAML = $(OCAMLOPT)
# FLAGS = -p

ifeq "$(OCAML)" "$(OCAMLOPT)"
  O = cmx
  A = cmxa
  LIBS = unix.$(A) str.$(A) 
  OCAMLDEPFLAGS = -native
else
  O = cmo
  A = cma
  LIBS = -custom unix.$(A) str.$(A)
endif

OCAMLEXE = $(OCAML) $(FLAGS) $(LIBS)


# The list of interface files
MLI = mqueue.mli dllist.mli misc.mli predicate.mli MFOTL.mli perf.mli \
      tuple.mli relation.mli table.mli db.mli \
      rewriting.mli filter_rel.mli filter_empty_tp.mli sliding.mli

OURMLI = $(MLI) log.mli algorithm.mli

ALLMLI = $(OURMLI) formula_parser.mli log_parser.mli


# The lists of implementation files
OURML = $(OURMLI:.mli=.ml) main.ml
ALLML = $(ALLMLI:.mli=.ml) formula_lexer.ml log_lexer.ml 

# The lists of object files
OBJ = $(MLI:.mli=.$(O)) \
       formula_parser.$(O) log_parser.$(O) \
       formula_lexer.$(O) log_lexer.$(O) \
       log.$(O) algorithm.$(O) 

OBJC = $(OBJ:.$(O)=.cmo)
OBJX = $(OBJ:.$(O)=.cmx)



# Targets
all: monpoly

monpoly: .depend $(OBJ) version.ml main.ml
	$(OCAMLEXE) $(OBJ) version.ml main.ml -o $@ 

monpoly.cma: .depend $(OBJC)
	$(OCAMLC) $(FLAGS) -a $(OBJC) -o $@

monpoly.cmxa: .depend $(OBJX)
	$(OCAMLOPT) $(FLAGS) -a $(OBJX) -o $@


# update version with the latest revision number
version.ml: FORCE
	@echo "let ver = \"1.1.2 (`svnversion -cn`)\"" > version.ml
FORCE:


# Documentation
doc: $(OURMLI:.mli=.cmi)
	ocamldoc -html -d ../doc $(OURMLI)


# Clean-up
clean:
	rm -f .depend.init .depend
	rm -f *.cm[aiox] *.cmxa $(OBJ:.$(O)=.o)
	rm -f *parser.mli *parser.ml *lexer.ml version.ml
	rm -f *~


# Dependencies
%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmo: %.ml
	$(OCAMLC) $(FLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(FLAGS) -c $<

%.ml: %.mll
	ocamllex $<

%.ml: %.mly
	ocamlyacc $<

%.mli: %.mly
	ocamlyacc $<

.depend: .depend.init $(ALLMLI) $(ALLML) 
	ocamldep $(OCAMLDEPFLAGS) $(ALLMLI) $(ALLML) > .depend

.depend.init: 
	ocamldep $(OCAMLDEPFLAGS) $(OURMLI) $(OURML) > .depend.init

depend: .depend

-include .depend











