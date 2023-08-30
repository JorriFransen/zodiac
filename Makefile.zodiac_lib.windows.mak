DIR := $(subst /,\,${CURDIR})

BUILD_DIR := bin
OBJ_DIR := obj

BASE_DIR := zodiac_lib
SRC_DIR := $(BASE_DIR)\src
ASSEMBLY := libzodiac
EXTENSION := .dll

SUPPORT_SRC_DIR := $(BASE_DIR)\support
SUPPORT_ASSEMBLY_DYN := libzrs
SUPPORT_EXTENSION_DYN := .dll
SUPPORT_ASSEMBLY_STAT := libzrs_s
SUPPORT_EXTENSION_STAT := .lib

LLVM_VERSION := 16.0.6
LLVM_SRC_DIR := $(DIR)\$(BUILD_DIR)\llvm-$(LLVM_VERSION).src
LLVM_DEBUG_BUILD_DIR := "$(DIR)\$(BUILD_DIR)\llvm_build_debug"
LLVM_DEBUG_INSTALL_DIR := "$(DIR)\$(BUILD_DIR)\llvm_install_debug"
LLVM_CXX_FLAGS = -std=c++17

include Makefile.dyncall_vars.windows.mak

COMMON_LINKER_FLAGS := -g -lmsvcrtd -Wl,-nodefaultlib:libcmt

COMPILER_FLAGS := -g -MD -MP -Wall -Werror -Wno-c99-designator -Wvla -fdeclspec $(LLVM_CXX_FLAGS)
INCLUDE_FLAGS := -I$(SRC_DIR) $(DYNCALL_INCLUDE_FLAGS)
LINKER_FLAGS := $(COMMON_LINKER_FLAGS) -shared -loleaut32 $(DYNCALL_LINK_FLAGS)
DEFINES := -D_DEBUG -DZEXPORT -D_DLL -D_CRT_SECURE_NO_WARNINGS

# Make does not offer a recursive wildcard function, so here's one:
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

SRC_FILES := $(subst /,\, $(call rwildcard,$(SRC_DIR)/,*.cpp))
DIRECTORIES := \$(SRC_DIR) $(subst $(DIR),,$(shell dir $(SRC_DIR) /S /AD /B | findstr /i src)) #All source directories
OBJ_FILES := $(SRC_FILES:%=$(OBJ_DIR)\\%.o)

SUPPORT_SRC_FILES := $(subst /,\, $(call rwildcard,$(SUPPORT_SRC_DIR)/,*.cpp))
SUPPORT_DIRECTORIES := \$(SUPPORT_SRC_DIR) $(subst $(DIR),,$(shell dir $(SUPPORT_SRC_DIR) /S /AD /B | findstr /i support))
SUPPORT_OBJ_FILES := $(SUPPORT_SRC_FILES:%=$(OBJ_DIR)\\%.o)

FULL_ASSEMBLY_PATH := $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION)
SUPPORT_ASSEMBLY_DYN_PATH := $(BUILD_DIR)/$(SUPPORT_ASSEMBLY_DYN)$(SUPPORT_EXTENSION_DYN)
SUPPORT_ASSEMBLY_STAT_PATH := $(BUILD_DIR)/$(SUPPORT_ASSEMBLY_STAT)$(SUPPORT_EXTENSION_STAT)

all: scaffold llvm llvm_vars compile_support compile link

.PHONY: scaffold
scaffold:
	-@setlocal enableextensions enabledelayedexpansion && mkdir $(addprefix $(OBJ_DIR), $(DIRECTORIES)) 2>NUL || cd .
	-@setlocal enableextensions enabledelayedexpansion && mkdir $(addprefix $(OBJ_DIR), $(SUPPORT_DIRECTORIES)) 2>NUL || cd .
	-@setlocal enableextensions enabledelayedexpansion && mkdir $(BUILD_DIR) 2>NUL || cd .

.PHONY: compile_support
compile_support:
	@echo Compiling $(SUPPORT_ASSEMBLY_DYN)

.PHONY: compile
compile: llvm_vars
	@echo Compiling $(ASSEMBLY)

$(OBJ_DIR)\\%.cpp.o: %.cpp
	@echo $< -^> $@
	@clang $< $(COMPILER_FLAGS) -c -o $(subst /,\, $@) $(DEFINES) $(INCLUDE_FLAGS)

.PHONY: link
link: llvm_vars $(FULL_ASSEMBLY_PATH) $(SUPPORT_ASSEMBLY_DYN_PATH) $(SUPPORT_ASSEMBLY_STAT_PATH)

$(SUPPORT_ASSEMBLY_DYN_PATH): $(SUPPORT_OBJ_FILES)
	@echo Linking $(SUPPORT_ASSEMBLY_DYN_PATH)
	@clang $(SUPPORT_OBJ_FILES) -o $@ $(DEFINES) $(COMMON_LINKER_FLAGS) -shared

$(SUPPORT_ASSEMBLY_STAT_PATH): $(SUPPORT_OBJ_FILES)
	@echo Linking $(SUPPORT_ASSEMBLY_STAT_PATH)
	lib /nologo /out:$@ $(SUPPORT_OBJ_FILES)

$(FULL_ASSEMBLY_PATH): $(OBJ_FILES)
	@echo Linking $(ASSEMBLY)
	@clang $(OBJ_FILES) -o $@ $(DEFINES) $(LINKER_FLAGS)

.PHONY: llvm
llvm:
	@if not exist $(LLVM_DEBUG_INSTALL_DIR) (\
	     echo Extracting llvm... && \
	    powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('$(BASE_DIR)\llvm\llvm-$(LLVM_VERSION).src.zip', '$(LLVM_SRC_DIR)') }" &&\
	     echo Configuring llvm... &&\
	    mkdir $(LLVM_DEBUG_BUILD_DIR) &&\
	    pushd $(LLVM_DEBUG_BUILD_DIR) &&\
	    cmake.exe $(LLVM_SRC_DIR)\llvm-project-llvmorg-$(LLVM_VERSION)\llvm -DCMAKE_INSTALL_PREFIX=$(LLVM_DEBUG_INSTALL_DIR) -DLLVM_TARGETS_TO_BUILD=X86 -DCMAKE_BUILD_TYPE=Debug -T ClangCl &&\
	     echo Building and installing llvm... &&\
	    mkdir $(LLVM_DEBUG_INSTALL_DIR) &&\
	    pushd $(LLVM_DEBUG_BUILD_DIR) &&\
	    cmake.exe --build . --target install --config Debug &&\
	    popd )
	@if exist $(LLVM_SRC_DIR) (\
	    @echo Removing llvm source dir after instal... &&\
	    rmdir /s /q $(LLVM_SRC_DIR) )
	@if exist $(LLVM_DEBUG_BUILD_DIR) (\
	     echo Removing llvm debug build dir after install &&\
	    rmdir /s /q $(LLVM_DEBUG_BUILD_DIR) )

llvm_vars:
	$(eval LLVM_CONFIG = $(LLVM_DEBUG_INSTALL_DIR)\bin\llvm-config.exe)
	$(eval LLVM_INCLUDE_DIR = $(shell $(LLVM_CONFIG) --includedir))
	$(eval LLVM_LIBS = $(shell $(LLVM_CONFIG) --libs x86))
	$(eval INCLUDE_FLAGS += -I$(LLVM_INCLUDE_DIR))
	$(eval LINKER_FLAGS += $(LLVM_LIBS))

.PHONY: clean_llvm
clean_llvm:
	if exist $(LLVM_SRC_DIR) rmdir /s /q $(LLVM_SRC_DIR)
	if exist $(LLVM_DEBUG_BUILD_DIR) rmdir /s /q $(LLVM_DEBUG_BUILD_DIR)
	REM if exist $(LLVM_DEBUG_INSTALL_DIR) rmdir /s /q $(LLVM_DEBUG_INSTALL_DIR)

.PHONY: clean
clean: clean_llvm
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
