# CAEBAT_ROOT must be defined before this file is included

ifndef CAEBAT_ROOT
  $(error Error: CAEBAT_ROOT undefined. CAEBAT_ROOT is the root of the CAEBAT components tree)
endif

HOSTNAME = $(shell uname -n)
INSTALL = ~/Documents/modified_dualfoil

CGNS_INCLUDE_DIR = /shared/caebat/tpl/install/cgns/include/
CGNS_INCLUDE = -I$(CGNS_INCLUDE_DIR)
CGNS_LIBS = -L/shared/caebat/tpl/install/cgns/lib -lcgns

F90 = gfortran
F90_FREEFORM_FLAG = -ffree-form
F90_FIXEDFORM_FLAG = -ffixed-form
F90_MOD_INCLUDE_PREFIX = -I
F90_INCLUDE_PREFIX = -I
F90FLAGS = -finit-local-zero $(F90_INCLUDE_PREFIX)$(CGNS_INCLUDE_DIR) $(F90_MOD_INCLUDE_PREFIX)$(HDF5_MOD_INCLUDE)

F77 = gfortran
F77_FREEFORM_FLAG = -ffree-form
F77_FIXEDFORM_FLAG = -ffixed-form
F77_INCLUDE_PREFIX = -I
F77FLAGS = -x f95-cpp-input $(F90_INCLUDE_PREFIX)$(CGNS_INCLUDE_DIR) $(F90_MOD_INCLUDE_PREFIX)$(HDF5_MOD_INCLUDE)

CC = gcc
CFLAGS = -g $(CGNS_INCLUDE) $(HDF5_INCLUDE)  

CXX = g++ 
CXXFLAGS = -g $(CGNS_INCLUDE) $(HDF5_INCLUDE) 


RPATH_FLAG = '-Wl,-rpath='
COMMON_LIBS = $(CGNS_LIBS) $(HDF5_LIBS) -L$(CAEBAT_ROOT)/lib
MAKEDEPF90 = $(CAEBAT_ROOT)/bin/makedepf90

