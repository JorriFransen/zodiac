BUILD_DIR := bin
OBJ_DIR := obj

include Makefile.dyncall_vars.linux.mak

LLVM_LIBS := $(shell llvm-config --libs codegen)
LLVM_CXX_FLAGS := $(shell llvm-config --cxxflags)
LLVM_LD_FLAGS := $(shell llvm-config --ldflags)

BASE_DIR := zodiac_driver
SRC_DIR := $(BASE_DIR)/src
ASSEMBLY := zodiac
EXTENSION :=
COMPILER_FLAGS := -g -MD -MP -Wall -Wvla -Werror -Wno-c99-designator -fdeclspec -fPIC $(LLVM_CXX_FLAGS)
INCLUDE_FLAGS := -Izodiac_lib/src -I$(SRC_DIR) -I$(BASE_DIR)/cxxopts $(DYNCALL_INCLUDE_FLAGS)
LINKER_FLAGS := $(LLVM_LIBS) $(LLVM_LD_FLAGS) -L$(BUILD_DIR) -lzodiac -Wl,-rpath,. -Wl,-rpath,$(BUILD_DIR)
DEFINES := -D_DEBUG -DZIMPORT

SRC_FILES := $(shell find $(SRC_DIR) -name *.cpp)
DIRECTORIES := $(shell find $(SRC_DIR) -type d)
OBJ_FILES := $(SRC_FILES:%=$(OBJ_DIR)/%.o)

FULL_ASSEMBLY_PATH := $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION)

all: scaffold compile link

.PHONY: scaffold
scaffold:
	@mkdir -p bin
	@mkdir -p $(addprefix $(OBJ_DIR)/,$(DIRECTORIES))

.PHONY: compile
compile:
	@echo Compiling $(ASSEMBLY)

$(OBJ_DIR)/%.cpp.o: %.cpp
	@echo "$< -> $@"
	@clang++ $< $(COMPILER_FLAGS) -c -o $@ $(DEFINES) $(INCLUDE_FLAGS)

.PHONY: link
link: $(FULL_ASSEMBLY_PATH)

$(FULL_ASSEMBLY_PATH): $(OBJ_FILES)
	@echo Linking $(ASSEMBLY)
	@clang++ $(COMPILER_FLAGS) $(OBJ_FILES) -o $@ $(LINKER_FLAGS)

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION)
	rm -rf $(OBJ_DIR)/$(BASE_DIR)
	rm -rf *.out *.o

-include $(OBJ_FILES:.o=.d)
