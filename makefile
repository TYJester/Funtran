# Compiler settings
FC = gfortran
FFLAGS = -Wall -fcheck=all -J$(MOD_DIR) -I$(SRC_DIR)

# Directories
SRC_DIR = src
MOD_DIR = include
LIB_DIR = lib
OBJ_DIR = $(LIB_DIR)/obj
BIN_DIR = bin

# Object files with explicit dependencies
OBJ_FUNTRAN_CONSTS = $(OBJ_DIR)/funtran_consts.o
OBJ_FUNTRAN_TYPES = $(OBJ_DIR)/funtran_types.o
OBJ_FUNTRAN_STANDLIB = $(OBJ_DIR)/funtran_standlib.o

# Library name
LIB_NAME = funtran.a

# Default target
all: $(LIB_NAME)

# Compile the Fortran modules and create the library
$(LIB_NAME): $(OBJ_FUNTRAN_CONSTS) $(OBJ_FUNTRAN_TYPES) $(OBJ_FUNTRAN_STANDLIB)
	ar rcs $(LIB_DIR)/$(LIB_NAME) $(OBJ_FUNTRAN_CONSTS) $(OBJ_FUNTRAN_TYPES) $(OBJ_FUNTRAN_STANDLIB)

# Generic rule for compiling Fortran source to object file
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	@mkdir -p $(OBJ_DIR)
	@mkdir -p $(MOD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Clean up
clean:
	rm -f $(OBJ_DIR)/*.o $(MOD_DIR)/*.mod

# Phony targets
.PHONY: all clean
