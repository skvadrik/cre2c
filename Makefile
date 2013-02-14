O            ?= obj
HI           ?= hi
PROGRAM       = cre2c
GHCFLAGS     += -O2 -hidir $(HI) -odir $(O) -i$(O):$(HI) -rtsopts $(PROF)
GHCFLAGS     += -Wall -W -fno-warn-name-shadowing

HAPPY_SOURCES       = RegexpParser.y SourceParser.y
HAPPY_AUTOGENERATED = $(HAPPY_SOURCES:%.y=%.hs)
HAPPY_OBJECTS       = $(HAPPY_SOURCES:%.y=$(O)/%.o)
HAPPY_INTERFACES    = $(HAPPY_SOURCES:%.y=$(HI)/%.hi)

SOURCES             = Types.hs CFA.hs RE2CFA.hs CFA2CPP.hs $(HAPPY_AUTOGENERATED)
OBJECTS             = $(SOURCES:%.hs=$(O)/%.o) Main.o
INTERFACES          = $(SOURCES:%.hs=$(HI)/%.hi) Main.hi




all: $(PROGRAM)

$(PROGRAM): $(HAPPY_AUTOGENERATED)
	ghc --make $(GHCFLAGS) cre2c.hs

%.hs: %.y
	happy -o $@ $< || rm $@




.PHONY: clean clean_happy clean_all

clean:
	-rm -fv $(PROGRAM)
#	-rm -fv $(OBJECTS) $(INTERFACES) $(PROGRAM)

clean_happy:
	-rm -fv $(HAPPY_AUTOGENERATED) $(HAPPY_OBJECTS) $(HAPPY_INTERFACES) $(PROGRAM)

clean_all:
	-rm -fv $(HAPPY_AUTOGENERATED) $(HAPPY_OBJECTS) $(HAPPY_INTERFACES) $(PROGRAM)
#	-rm -fv $(HAPPY_AUTOGENERATED) $(OBJECTS) $(INTERFACES) $(PROGRAM)

.SUFFIXES:

.PRECIOUS: $(HAPPY_AUTOGENERATED)
