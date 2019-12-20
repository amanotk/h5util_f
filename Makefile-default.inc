# -*- Makefile -*-

# include path
HDF5		=

# compilers
FC			= mpif90
FCFLAGS	= -O2 -I$(HDF5)/include
LDFLAGS	= -L$(HDF5)/lib -lhdf5_fortran -lhdf5 -lz

%.o	: %.f90
	$(FC) -c $(FCFLAGS) $<
