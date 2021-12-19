.DEFAULT_GOAL := all  

# Compiler
FC     =  ifort

SOURCES =  Shape/ShapeMod.f90 Cell/CellMod.f90 Arrow/ArrowMod.f90 Box/BoxMod.f90 FORTRANTekz.f90  testFORTRANTekz.f90


compile:
	@echo "Compiling using ifort..."
	$(FC) $(SOURCES)

run:
	./a.out

all: compile run
