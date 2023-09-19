DIR := $(subst /,\,${CURDIR})

BUILD_DIR := bin
OBJ_DIR := obj

BASE_DIR := zodiac_lib
SRC_DIR := $(BASE_DIR)
ASSEMBLY := libzodiac
EXTENSION := .dll

SUPPORT_SRC_DIR := $(BASE_DIR)\support
SUPPORT_ASSEMBLY_DYN := libzrs
SUPPORT_EXTENSION_DYN := .dll
SUPPORT_ASSEMBLY_STAT := libzrs_s
SUPPORT_EXTENSION_STAT := .lib

include $(DIR)/3rdparty/Makefile.3rdparty.windows.mak

COMMON_LINKER_FLAGS := -g -lmsvcrtd -Wl,-nodefaultlib:libcmt

COMPILER_FLAGS := -g -MD -MP -Wall -Werror -Wno-c99-designator -Wvla -fdeclspec $(LLVM_CXX_FLAGS)
INCLUDE_FLAGS := -I$(SRC_DIR) $(DYNCALL_INCLUDE_FLAGS) $(LLVM_INCLUDE_FLAGS)
LINKER_FLAGS := $(COMMON_LINKER_FLAGS) -shared -loleaut32 $(DYNCALL_LINK_FLAGS) $(LLVM_LINKER_FLAGS)
DEFINES := -D_DEBUG -DZEXPORT -D_DLL -D_CRT_SECURE_NO_WARNINGS

# Make does not offer a recursive wildcard function, so here's one:
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

SRC_FILES := $(subst /,\, $(call rwildcard,$(SRC_DIR)/,*.cpp))
DIRECTORIES := \$(SRC_DIR) $(subst $(DIR),,$(shell dir $(SRC_DIR) /S /AD /B | findstr /i /e /v \zodiac_lib\support)) #All source directories except for support
OBJ_FILES := $(SRC_FILES:%=$(OBJ_DIR)\\%.o)

SUPPORT_SRC_FILES := $(subst /,\, $(call rwildcard,$(SUPPORT_SRC_DIR)/,*.cpp))
SUPPORT_DIRECTORIES := \$(SUPPORT_SRC_DIR) $(subst $(DIR),,$(shell dir $(SUPPORT_SRC_DIR) /S /AD /B | findstr /i support))
SUPPORT_OBJ_FILES := $(SUPPORT_SRC_FILES:%=$(OBJ_DIR)\\%.o)

FULL_ASSEMBLY_PATH := $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION)
SUPPORT_ASSEMBLY_DYN_PATH := $(BUILD_DIR)/$(SUPPORT_ASSEMBLY_DYN)$(SUPPORT_EXTENSION_DYN)
SUPPORT_ASSEMBLY_STAT_PATH := $(BUILD_DIR)/$(SUPPORT_ASSEMBLY_STAT)$(SUPPORT_EXTENSION_STAT)

all: scaffold compile_support compile link

.PHONY: scaffold
scaffold:
	-@setlocal enableextensions enabledelayedexpansion && mkdir $(addprefix $(OBJ_DIR), $(DIRECTORIES)) 2>NUL || cd .
	-@setlocal enableextensions enabledelayedexpansion && mkdir $(addprefix $(OBJ_DIR), $(SUPPORT_DIRECTORIES)) 2>NUL || cd .
	-@setlocal enableextensions enabledelayedexpansion && mkdir $(BUILD_DIR) 2>NUL || cd .

.PHONY: compile_support scaffold
compile_support:
	@echo Compiling $(SUPPORT_ASSEMBLY_DYN)

.PHONY: compile
compile:
	@echo Compiling $(ASSEMBLY)

$(OBJ_DIR)\\%.cpp.o: %.cpp scaffold
	@echo $< -^> $@
	@clang $< $(COMPILER_FLAGS) -c -o $(subst /,\, $@) $(DEFINES) $(INCLUDE_FLAGS)

.PHONY: link
link: $(FULL_ASSEMBLY_PATH) $(SUPPORT_ASSEMBLY_DYN_PATH) $(SUPPORT_ASSEMBLY_STAT_PATH)

$(SUPPORT_ASSEMBLY_DYN_PATH): $(SUPPORT_OBJ_FILES)
	@echo Linking $(SUPPORT_ASSEMBLY_DYN_PATH)
	@clang $(SUPPORT_OBJ_FILES) -o $@ $(DEFINES) $(COMMON_LINKER_FLAGS) -shared

$(SUPPORT_ASSEMBLY_STAT_PATH): $(SUPPORT_OBJ_FILES)
	@echo Linking $(SUPPORT_ASSEMBLY_STAT_PATH)
	lib /nologo /out:$@ $(SUPPORT_OBJ_FILES)

$(FULL_ASSEMBLY_PATH): $(OBJ_FILES)
	@echo Linking $(ASSEMBLY)
	@clang $(OBJ_FILES) -o $@ $(DEFINES) $(LINKER_FLAGS)

.PHONY: clean
clean:
	if exist out.exe del out.exe
	if exist out.obj del out.obj
	if exist $(BUILD_DIR)\$(ASSEMBLY)$(EXTENSION) del $(BUILD_DIR)\$(ASSEMBLY)$(EXTENSION)
	if exist $(BUILD_DIR)\$(ASSEMBLY).ilk del $(BUILD_DIR)\$(ASSEMBLY).ilk
	if exist $(BUILD_DIR)\$(ASSEMBLY).pdb del $(BUILD_DIR)\$(ASSEMBLY).pdb
	if exist $(BUILD_DIR)\$(ASSEMBLY).exp del $(BUILD_DIR)\$(ASSEMBLY).exp
	if exist $(BUILD_DIR)\$(ASSEMBLY).lib del $(BUILD_DIR)\$(ASSEMBLY).lib
	if exist $(OBJ_DIR)\$(BASE_DIR) rmdir /s /q $(OBJ_DIR)\$(BASE_DIR)
	if exist $(BUILD_DIR)\$(SUPPORT_ASSEMBLY_DYN)$(SUPPORT_EXTENSION_DYN) del $(BUILD_DIR)\$(SUPPORT_ASSEMBLY_DYN)$(SUPPORT_EXTENSION_DYN)
	if exist $(BUILD_DIR)\$(SUPPORT_ASSEMBLY_DYN).exp del $(BUILD_DIR)\$(SUPPORT_ASSEMBLY_DYN).exp
	if exist $(BUILD_DIR)\$(SUPPORT_ASSEMBLY_DYN).ilk del $(BUILD_DIR)\$(SUPPORT_ASSEMBLY_DYN).ilk
	if exist $(BUILD_DIR)\$(SUPPORT_ASSEMBLY_DYN).lib del $(BUILD_DIR)\$(SUPPORT_ASSEMBLY_DYN).lib
	if exist $(BUILD_DIR)\$(SUPPORT_ASSEMBLY_DYN).pdb del $(BUILD_DIR)\$(SUPPORT_ASSEMBLY_DYN).pdb
	if exist $(BUILD_DIR)\$(SUPPORT_ASSEMBLY_STAT)$(SUPPORT_EXTENSION_STAT) del $(BUILD_DIR)\$(SUPPORT_ASSEMBLY_STAT)$(SUPPORT_EXTENSION_STAT)

-include $(OBJ_FILES:.o=.d)
