DIR := $(subst /,\,${CURDIR})

BUILD_DIR := bin
OBJ_DIR := obj

BASE_DIR := zodiac_lib
SRC_DIR := $(BASE_DIR)\src
ASSEMBLY := libzodiac
EXTENSION := .dll

LLVM_DEBUG_INSTALL_DIR := D:\llvm_install_debug
LLVM_CONFIG := $(LLVM_DEBUG_INSTALL_DIR)\bin\llvm-config.exe
LLVM_LIBS := $(shell $(LLVM_CONFIG) --libnames codegen)
LLVM_CXX_FLAGS := $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_LINK_FLAGS := $(shell $(LLVM_CONFIG) --ldflags)

COMPILER_FLAGS := /MTd /DEBUG -Wall -Wvla -Wno-c++20-designator -Wno-format-nonliteral -Wno-cast-qual -Wno-shadow-field-in-constructor -Wno-old-style-cast -Wno-c++98-compat -Wno-c++98-compat-pedantic $(LLVM_CXX_FLAGS)
INCLUDE_FLAGS := -I$(SRC_DIR)
LINKER_FLAGS := /LDd $(LLVM_LIBS) /link $(LLVM_LINK_FLAGS) /DEBUG
DEFINES := -D_DEBUG -DZEXPORT

# Make does not offer a recursive wildcard function, so here's one:
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

SRC_FILES := $(subst /,\, $(call rwildcard,$(SRC_DIR)/,*.cpp))
DIRECTORIES := \$(SRC_DIR) $(subst $(DIR),,$(shell dir $(SRC_DIR) /S /AD /B | findstr /i src)) #All source directories
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
	clang-cl $< $(COMPILER_FLAGS) -c -o $(subst /,\, $@) $(DEFINES) $(INCLUDE_FLAGS)

.PHONY: link
link: $(FULL_ASSEMBLY_PATH)

$(FULL_ASSEMBLY_PATH): $(OBJ_FILES)
	@echo Linking $(ASSEMBLY)
	clang-cl $(subst /,\, $(OBJ_FILES)) -o $@ $(DEFINES) $(LINKER_FLAGS)

	
.PHONY: clean
clean:
	if exist $(BUILD_DIR)\$(ASSEMBLY)$(EXTENSION) del $(BUILD_DIR)\$(ASSEMBLY)$(EXTENSION)
	if exist $(BUILD_DIR)\$(ASSEMBLY).ilk del $(BUILD_DIR)\$(ASSEMBLY).ilk
	if exist $(BUILD_DIR)\$(ASSEMBLY).pdb del $(BUILD_DIR)\$(ASSEMBLY).pdb
	if exist $(BUILD_DIR)\$(ASSEMBLY).exp del $(BUILD_DIR)\$(ASSEMBLY).exp
	if exist $(BUILD_DIR)\$(ASSEMBLY).lib del $(BUILD_DIR)\$(ASSEMBLY).lib
	rmdir /s /q $(OBJ_DIR)\$(BASE_DIR)

-include $(OBJ_FILES:.o=.d)