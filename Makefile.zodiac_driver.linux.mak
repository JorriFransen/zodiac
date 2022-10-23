BUILD_DIR := bin
OBJ_DIR := obj

BASE_DIR := zodiac_driver
SRC_DIR := $(BASE_DIR)/src
ASSEMBLY := zodiac
EXTENSION :=
COMPILER_FLAGS := -g -MD -MP -Werror=vla -fdeclspec -fPIC
INCLUDE_FLAGS := -Izodiac_lib/src -I$(SRC_DIR)
LINKER_FLAGS := $(BUILD_DIR)/libzodiac.a
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
	clang $< $(COMPILER_FLAGS) -c -o $@ $(DEFINES) $(INCLUDE_FLAGS)

.PHONY: link
link: $(FULL_ASSEMBLY_PATH)
	
$(FULL_ASSEMBLY_PATH): $(OBJ_FILES)
	@echo Linking $(ASSEMBLY)
	clang $(OBJ_FILES) -o $@ $(LINKER_FLAGS)
	
.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION)
	rm -rf $(OBJ_DIR)/$(BASE_DIR)

-include $(OBJ_FILES:.o=.d)