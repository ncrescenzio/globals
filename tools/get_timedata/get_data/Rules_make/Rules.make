#Gfortran compiler
FC            = gfortran
OPENMP        = -fopenmp
MODEL         = -mcmodel=medium
OFLAGS        = -O5 -ffree-line-length-none
DFLAGS        = -g -C -Wall -fcheck=all -O -ffree-line-length-none
#DFLAGS        = -g -C -Wall -ffree-line-length-none -fcheck=all
PFLAGS        = -pg
CPPFLAGS      = -D_GFORTRAN_COMP
ARFLAGS       =

ODIR          = objs
MDIR          = mods
LDIR          = libs
DDIR          = doxygen

INCLUDE       = -J$(MODDIR)
