DIR := $(subst /,\,${CURDIR})

BUILD_DIR := bin
OBJ_DIR := obj

BASE_DIR := zodiac_tests
SRC_DIR := $(BASE_DIR)\src
ASSEMBLY := zodiac_tests
EXTENSION := .exe
COMPILER_FLAGS := -g -MD -MP -Werror=vla -fdeclspec
INCLUDE_FLAGS := -Izodiac_lib/src -I$(BASE_DIR)\munit -I$(SRC_DIR)
LINKER_FLAGS := $(BUILD_DIR)\libzodiac.lib
DEFINES := -D_DEBUG -DZIMPORT

# Make does not offer a recursive wildcard function, so here's one:
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

SRC_FILES := $(call rwildcard,$(SRC_DIR)/,*.cpp)
DIRECTORIES := \$(SRC_DIR) $(subst $(DIR),,$(shell dir $(SRC_DIR) /S /AD /B | findstr /i src)) # Our own source directories

# Add munit library
SRC_FILES += $(call rwildcard,$(BASE_DIR)/munit/,*.c)
DIRECTORIES += \$(SRC_DIR) $(subst $(DIR),,$(shell dir $(BASE_DIR)\munit /S /AD /B | findstr /i munit)) # Munit source directories

OBJ_FILES := $(SRC_FILES:%=$(OBJ_DIR)/%.o)

FULL_ASSEMBLY_PATH := $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION)

all: scaffold compile link
	
.PHONY: scaffold
scaffold:
	-@setlocal enableextensions enabledelayedexpansion && mkdir $(addprefix $(OBJ_DIR), $(DIRECTORIES)) 2>NUL || cd .
	-@setlocal enableextensions enabledelayedexpansion && mkdir $(BUILD_DIR) 2>NUL || cd .
	
.PHONY: compile
compile:
	@echo Compiling $(ASSEMBLY)
	
$(OBJ_DIR)/%.cpp.o: %.cpp
	clang $< $(COMPILER_FLAGS) -c -o $@ $(DEFINES) $(INCLUDE_FLAGS)
	
$(OBJ_DIR)/%.c.o: %.c
	clang $< $(COMPILER_FLAGS) -c -o $@ $(DEFINES) $(INCLUDE_FLAGS)
	
.PHONY: link
link: $(FULL_ASSEMBLY_PATH)

$(FULL_ASSEMBLY_PATH): $(OBJ_FILES)
	@echo Linking $(ASSEMBLY)
	clang $(COMPILER_FLAGS) $(OBJ_FILES) -o $@ $(DEFINES) $(LINKER_FLAGS)

.PHONY: clean
clean:
	if exist $(BUILD_DIR)\$(ASSEMBLY)$(EXTENSION) del $(BUILD_DIR)\$(ASSEMBLY)$(EXTENSION)
	if exist $(BUILD_DIR)\$(ASSEMBLY).pdb del $(BUILD_DIR)\$(ASSEMBLY).pdb
	if exist $(BUILD_DIR)\$(ASSEMBLY).ilk del $(BUILD_DIR)\$(ASSEMBLY).ilk
	rmdir /s /q $(OBJ_DIR)\$(BASE_DIR)

-include $(OBJ_FILES:.o=.d)