# Funtran version 0.0.1

Funtran is a modern fortran library containing some generic useful functions, designed for gfortran versions 2003 and later. The library is constantly being worked on, so be sure to check for updates from time to time!

## Installation

To install funtran, begin by cloning this repository, then build using the makefile.

```bash
git clone https://github.com/WPLavery/Funtran.git

make
```

## Compilation

When compiling your fortran program, use the following syntax to ensure the library is linked.

```bash
gfortran my_program.f90 -L/"path to repository"/lib -lfuntran -o my_program
```

## Usage

To use the library in fortran, select what modules from funtran you would like to use and include them using the following code.

```fortran
using funtran_moduleName
```

Make sure to replace moduleName with the name of the actual module you want to use.
