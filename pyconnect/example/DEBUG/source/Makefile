F90=gfortran
F90FLAGS = -O -ffixed-line-length-none -ffree-line-length-none

all: disconnectionDPS manipulate

disconnectionDPS: disconnectionDPS.f90
	$(F90) $(F90FLAGS) -o $@ $<
#	cp $@ ddps

manipulate: manipulate.f90
	$(F90) $(F90FLAGS) -o $@ $<
#	cp $@ mnp

clean:
	rm -f disconnectionDPS manipulate *.o *.mod
