
BUILD_DIR := bin
OBJ_DIR := obj

BASE_DIR := zodiac_tests
SRC_DIR := $(BASE_DIR)/src
ASSEMBLY := zodiac_tests
EXTENSION :=
COMPILER_FLAGS := -g -MD -Werror=vla -fdeclspec -fPIC
INCLUDE_FLAGS := -Izodiac_lib/src -I$(BASE_DIR)/munit -I$(SRC_DIR)
LINKER_FLAGS :=
DEFINES := -D_DEBUG -DZIMPORT

SRC_FILES := $(shell find $(SRC_DIR) -name *.cpp)
DIRECTORIES := $(shell find $(SRC_DIR) -type d)
OBJ_FILES := $(SRC_FILES:%=$(OBJ_DIR)/%.o)

MUNIT_INCLUDE_FLAGS := -I$(BASE_DIR)/munit/munit
MUNIT_COMPILER_FLAGS := -g -MD -fdeclspec -fPIC
MUNIT_SRC_FILES := $(shell find $(BASE_DIR)/munit -name *.c)
MUNIT_DIRECTORIES := $(shell find $(BASE_DIR)/munit -type d)
MUNIT_OBJ_FILES := $(MUNIT_SRC_FILES:%=$(OBJ_DIR)/%.o)

all: scaffold compile link
	
.PHONY: scaffold
scaffold:
	@mkdir -p bin
	@mkdir -p $(addprefix $(OBJ_DIR)/,$(DIRECTORIES))
	@mkdir -p $(addprefix $(OBJ_DIR)/,$(MUNIT_DIRECTORIES))
	
.PHONY: link
link: scaffold $(OBJ_FILES) $(MUNIT_OBJ_FILES)
	@echo Linking $(ASSEMBLY)
	clang $(BUILD_DIR)/libzodiac.a $(OBJ_FILES) $(MUNIT_OBJ_FILES) -o $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION) $(LINKER_FLAGS)
	
.PHONY: compile
compile:
	@echo Compiling $(ASSEMBLY)
	
$(OBJ_DIR)/%.cpp.o: %.cpp
	clang $< $(COMPILER_FLAGS) -c -o $@ $(DEFINES) $(INCLUDE_FLAGS)
	
$(OBJ_DIR)/%.c.o: %.c
	clang $< $(MUNIT_COMPILER_FLAGS) -c -o $@ $(DEFINES) $(MUNIT_INCLUDE_FLAGS)

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION)
	rm -rf $(OBJ_DIR)/$(BASE_DIR)

-include $(OBJ_FILES:.o=.d)