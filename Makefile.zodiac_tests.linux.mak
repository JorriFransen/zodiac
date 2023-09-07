
BUILD_DIR := bin
OBJ_DIR := obj

BASE_DIR := zodiac_tests
SRC_DIR := $(BASE_DIR)/src
ASSEMBLY := zodiac_tests
EXTENSION :=

include $(PWD)/3rdparty/Makefile.3rdparty.linux.mak

COMPILER_FLAGS := -g -MD -MP -Werror=vla -fdeclspec
INCLUDE_FLAGS := -Izodiac_lib/src $(MUNIT_INCLUDE_FLAGS) -I$(SRC_DIR) $(DYNCALL_INCLUDE_FLAGS)
LINKER_FLAGS := -g -L$(BUILD_DIR) -lzodiac -Wl,-rpath,'$$ORIGIN'
DEFINES := -D_DEBUG -DZIMPORT

SRC_FILES := $(shell find $(SRC_DIR) -name *.cpp)
DIRECTORIES := $(shell find $(SRC_DIR) -type d)

# Add munit library
SRC_FILES += $(MUNIT_SOURCE_FILES)
DIRECTORIES += $(MUNIT_SOURCE_DIRS)

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

$(OBJ_DIR)/%.c.o: %.c
	@echo "$< -> $@"
	@clang $< $(COMPILER_FLAGS) -c -o $@ $(DEFINES) $(INCLUDE_FLAGS)

.PHONY: link
link: $(FULL_ASSEMBLY_PATH)

$(FULL_ASSEMBLY_PATH): $(OBJ_FILES)
	@echo Linking $(ASSEMBLY)
	@clang++ $(COMPILER_FLAGS) $(OBJ_FILES) -o $@ $(LINKER_FLAGS)

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION)
	rm -rf $(OBJ_DIR)/$(BASE_DIR)

-include $(OBJ_FILES:.o=.d)
