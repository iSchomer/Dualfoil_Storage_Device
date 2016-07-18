VPATH = $(PWD)
OBJDIR= .

F90_SRC = $(wildcard *.f90)
C_SRC = $(wildcard *.c)
F_SRC = $(wildcard *.f)
CXX_SRC = $(wildcard *.cxx)

include Makefile.include

F90MOD_SRC = 
OBJS =  $(F_SRC:%.f=$(OBJDIR)/%.o) $(F90_SRC:%.f90=$(OBJDIR)/%.o) $(C_SRC:%.c=$(OBJDIR)/%.o) $(CXX_SRC:%.cxx=$(OBJDIR)/%.o) 
F90_MOD_INCLUDE=$(F90_MOD_INCLUDE_PREFIX). $(F90_MOD_INCLUDE_PREFIX)$(CGNSHOME)/include $(F90_MOD_INCLUDE_PREFIX)/usr/local/include

LIBS = $(COMMON_LIBS) 
F90FLAGS += $(F90_MOD_INCLUDE)

#EXECUTABLES = prepare_dualfoil_chartran_input process_dualfoil_chartran_output dualfoil
EXECUTABLES = dualfoil
SCRIPTS = $(wildcard *.py)

TARGETS = $(SCRIPTS) $(EXECUTABLES)

all: .depend $(OBJS)
	$(RM) $(EXECUTABLES)
	- $(MAKE) $(TARGETS)
	@ for i in `echo $(EXECUTABLES)`; do \
	   if [ -f $$i ]; then \
                echo "SUCCESS building $$i" >> $(CAEBAT_ROOT)/.make-result;\
        else \
                echo "***FAILURE building $$i" >> $(CAEBAT_ROOT)/.make-result;\
        fi;\
	done;

prepare_dualfoil_chartran_input: prepare_dualfoil_chartran_input.o 
	$(F90) -o  $@ $^  $(LIBS)

process_dualfoil_chartran_output: process_dualfoil_chartran_output.o 
	$(F90) -o  $@ $^  $(LIBS)

dualfoil: dualfoil5.2.o
	$(F90) -o  $@ $^  $(LIBS)

install:
	$(INSTALL) $(EXECUTABLES) $(SCRIPTS) $(CAEBAT_ROOT)/bin

clean:
	$(RM) $(OBJS) *.a *.mod $(EXECUTABLES)

distclean: clean 
	$(RM) .depend
	
.depend: $(F90MOD_SRC) $(F90_SRC)
	$(MAKEDEPF90) -b . $(F90MOD_SRC) $(F90_SRC) $(F_SRC) > $@
ifdef C_SRC
	- gcc -MM $(CFLAGS) $(C_SRC)>> $@
endif
ifdef CXX_SRC
	- g++ -MM $(CXXFLAGS) $(CXX_SRC)>> $@
endif

-include .depend

.SUFFIXES:            # Delete the default suffixes
.SUFFIXES: .c .f .f90 .cxx .o .h .hxx   # Define our suffix list

