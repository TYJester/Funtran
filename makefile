#Compiler Config
FC = gfortran
FFLAGS = -Wall -fcheck=all -J$(MOD_DIR)

#Directories
SRC_DIR = src
MOD_DIR = include
LIB_DIR = lib
OBJ_DIR = $(LIB_DIR)/obj
BIN_DIR = bin

#Sources and Objects
SOURCES = $(wildcard $(SRC_DIR)/*.f90)
OBJECTS = $(patsubst $(SRC_DIR)/%.f90, $(OBJ_DIR)/%.o, $(SOURCES))

#Library Name
LIB_NAME = funtran.a

all: $(LIB_NAME)

#Compile and create library
$(LIB_NAME): $(OBJECTS)
	ar rcs $(LIB_DIR)/$(LIB_NAME) $(OBJECTS)

# Rule to compile Fortran source files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	@mkdir -p $(OBJ_DIR)
	@mkdir -p $(MOD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Clean up
clean:
	rm -f $(OBJ_DIR)/*.o $(MOD_DIR)/*.mod $(LIB_DIR)/$(LIB_NAME)

# Phony targets
.PHONY: all clean
