BUILD_DIR := bin
OBJ_DIR := obj

include $(PWD)/3rdparty/Makefile.3rdparty.linux.mak

BASE_DIR := zodiac_driver
SRC_DIR := $(BASE_DIR)
ASSEMBLY := zodiac
EXTENSION :=
COMPILER_FLAGS := -ggdb -O0 -MD -MP -Wall -Wvla -Werror -Wno-c99-designator -fdeclspec
INCLUDE_FLAGS := -Izodiac_lib -I$(SRC_DIR) $(CXXOPTS_INCLUDE_FLAGS) $(DYNCALL_INCLUDE_FLAGS)
LINKER_FLAGS := -ggdb -O0 -L$(BUILD_DIR) -lzodiac -Wl,-rpath,'$$ORIGIN'
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
