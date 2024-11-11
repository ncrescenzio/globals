SHELL :=/bin/bash
RULES         = $(CURDIR)/Rules_make/Rules.make
include $(RULES)

OBJDIR        = $(CURDIR)/$(ODIR)
MODDIR        = $(CURDIR)/$(MDIR)
LIBDIR        = $(CURDIR)/$(LDIR)
DOXDIR        = $(CURDIR)/$(DDIR)
DOCDIR        = $(CURDIR)/doc

INCLUDE       += -I$(MODDIR)

FFLAGS        = $(OFLAGS) $(MODEL)  $(INCLUDE) 

LIBSRCS       = ../code 

DEST	      = .

EXTHDRS	      =

HDRS	      =

LIBS	      = -lglobals -lblas -llapack

LIBMODS       = 

LDFLAGS	      = $(MODEL)  $(INCLUDE) -L. -L/usr/lib -L/usr/local/lib -L$(LIBDIR)

LINKER	      = $(FC)

MAKEFILE      = Makefile

PRINT	      = pr

CAT	      = cat

PROGRAM	      = get_data.out

SRCS	      = main.f90

OBJS	      = main.o

PRJS= $(SRCS:jo=.prj)

OBJECTS        = $(SRCS:%.f90=$(OBJDIR)/%.o)

MODULES        = $(addprefix $(MODDIR)/,$(MODS))

.SUFFIXES: .prj .f90

print-%  : 
		@echo $* = $($*)

.f.prj:
	ftnchek -project -declare -noverbose $<

.f90.o:
	$(FC) $(FFLAGS) $(INCLUDE) -c  $< 

all::		$(PROGRAM) 

$(PROGRAM):     $(LIBS) $(MODULES) $(OBJECTS)
		$(LINKER) -o $(PROGRAM) $(LDFLAGS) $(OBJECTS) $(LIBS)

$(LIBS):
		@set -e; for i in $(LIBSRCS); do cd $$i; $(MAKE) --no-print-directory -e CURDIR=$(CURDIR); cd ~-; done


$(OBJECTS): $(OBJDIR)/%.o: %.f90 
		$(FC) $(CPPFLAGS) $(FFLAGS) -o $@ -c $<

.PHONY:         doc

doc: 
		@-rm -f $(DOXDIR)/README.md
		@cat $(DOXDIR)/README.tpl $(DOXDIR)/*.txt > $(DOXDIR)/README.md
		doxygen $(DOXDIR)/DoxygenConfigFortran 

dirs: 
		@-mkdir -p $(DOCDIR) $(OBJDIR) $(MODDIR) $(LIBDIR)

clean-emacs:
		@-rm -f $(CURDIR)/*.*~ 
		@-rm -f $(CURDIR)/*\#* 

check: $(PRJS)
	ftnchek -noverbose -declare $(PRJS) -project -noextern -library > $(PROGRAM).ftn

profile:;       @make "FFLAGS=$(PFLAGS) $(MODEL) " "CFLAGS=$(PFLAGS) $(MODEL)" "LDFLAGS=$(PFLAGS) $(LDFLAGS)" $(PROGRAM)

debug:;         @make "FFLAGS=$(DFLAGS) $(MODEL) $(INCLUDE)" "LDFLAGS=$(DFLAGS) $(LDFLAGS)" $(PROGRAM)

openmp:;         @make "FFLAGS=$(OFLAGS) $(OPENMP) $(MODEL) $(INCLUDE)" "LDFLAGS=$(LDFLAGS) $(OPENMP)" $(PROGRAM)

clean:;		@rm -f $(OBJECTS) $(MODULES) $(PROGRAM).cat $(PROGRAM).ftn
		@set -e; for i in $(LIBSRCS); do cd $$i; $(MAKE) --no-print-directory clean; cd ~-; done

clobber:;	@rm -f $(OBJECTS) $(MODULES) $(PROGRAM).cat $(PROGRAM).ftn $(PROGRAM)
		@-rm -rf $(DOCDIR)/latex/* $(DOCDIR)/html/* $(OBJDIR) $(MODDIR) $(LIBDIR)
		@-rm -f $(CURDIR)/*.*~ 
		@-rm -f $(CURDIR)/*\#* 

.PHONY:		mods

mods:;		
		# mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)
		./scripts/f90_mod_deps.py $(SRCS) > Rules_make/dependencies
		./scripts/f90_source_deps.py $(SRCS)>> Rules_make/dependencies

index:;		ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		install -s $(PROGRAM) $(DEST)

print:;		$(PRINT) $(HDRS) $(SRCS)

cat:;		$(CAT) $(HDRS) $(SRCS) > $(PROGRAM).cat

program:        $(PROGRAM)

profile:        $(PROFILE)

tags:           $(HDRS) $(SRCS); ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

# this file is created by "make mods" It needs to exist (even if empty)
# for the makefile to work
include Rules_make/dependencies
