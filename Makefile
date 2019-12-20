# -*- Makefile -*-
.PHONY: default clean cleanall

include Makefile.inc

# target
SRCS    = h5util.f90 h5test.f90
OBJS    = $(SRCS:%.f90=%.o)

default: h5test

# build h5test
h5test: $(OBJS)
	$(FC) $(FFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f *.o *.mod

cleanall:
	rm -f *.o *.mod h5test
