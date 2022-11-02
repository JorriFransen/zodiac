DIR := $(subst /,\,${CURDIR})

BUILD_DIR := bin
OBJ_DIR := obj

BASE_DIR := zodiac_lib
SRC_DIR := $(BASE_DIR)\src
ASSEMBLY := libzodiac
EXTENSION := .dll

LLVM_VERSION := 15.0.2
LLVM_SRC_DIR := $(DIR)\$(BUILD_DIR)\llvm-$(LLVM_VERSION).src
LLVM_DEBUG_BUILD_DIR := "$(DIR)\$(BUILD_DIR)\llvm_build_debug"
LLVM_DEBUG_INSTALL_DIR := "$(DIR)\$(BUILD_DIR)\llvm_install_debug"

COMPILER_FLAGS := -g -MD -MP -Wall -Werror -Wvla -fdeclspec
INCLUDE_FLAGS := -I$(SRC_DIR)
LINKER_FLAGS := -g -shared -Wl,-nodefaultlib:libcmt 
DEFINES := -D_DEBUG -DZEXPORT -D_DLL

# Make does not offer a recursive wildcard function, so here's one:
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

SRC_FILES := $(subst /,\, $(call rwildcard,$(SRC_DIR)/,*.cpp))
DIRECTORIES := \$(SRC_DIR) $(subst $(DIR),,$(shell dir $(SRC_DIR) /S /AD /B | findstr /i src)) #All source directories
OBJ_FILES := $(SRC_FILES:%=$(OBJ_DIR)/%.o)

FULL_ASSEMBLY_PATH := $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION)

all: scaffold llvm llvm_vars compile link
	
.PHONY: scaffold
scaffold:
	-@setlocal enableextensions enabledelayedexpansion && mkdir $(addprefix $(OBJ_DIR), $(DIRECTORIES)) 2>NUL || cd .
	-@setlocal enableextensions enabledelayedexpansion && mkdir $(BUILD_DIR) 2>NUL || cd .
	
.PHONY: compile
compile: llvm_vars
	@echo Compiling $(ASSEMBLY)

$(OBJ_DIR)/%.cpp.o: %.cpp
	clang $< $(COMPILER_FLAGS) -c -o $(subst /,\, $@) $(DEFINES) $(INCLUDE_FLAGS)

.PHONY: link
link: llvm_vars $(FULL_ASSEMBLY_PATH)

$(FULL_ASSEMBLY_PATH): $(OBJ_FILES)
	@echo Linking $(ASSEMBLY)
	clang $(subst /,\, $(OBJ_FILES)) -o $@ $(DEFINES) $(LINKER_FLAGS)

.PHONY: llvm
llvm:
	@if not exist $(LLVM_DEBUG_INSTALL_DIR) (\
	 	echo Extracting llvm... && \
		powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('$(BASE_DIR)\llvm\llvm-$(LLVM_VERSION).src.zip', '$(LLVM_SRC_DIR)'); }" &&\
	 	echo Configuring llvm... &&\
		mkdir $(LLVM_DEBUG_BUILD_DIR) &&\
		pushd $(LLVM_DEBUG_BUILD_DIR) &&\
		cmake.exe $(LLVM_SRC_DIR)\llvm -DCMAKE_INSTALL_PREFIX=$(LLVM_DEBUG_INSTALL_DIR) -DLLVM_TARGETS_TO_BUILD=X86 -DCMAKE_BUILD_TYPE=Debug -T ClangCl &&\
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
	$(eval LLVM_LIBS = $(shell $(LLVM_CONFIG) --libs codegen))
	$(eval INCLUDE_FLAGS += -I$(LLVM_INCLUDE_DIR))
	$(eval LINKER_FLAGS += $(LLVM_LIBS))

.PHONY: clean_llvm
clean_llvm:
	if exist $(LLVM_SRC_DIR) rmdir /s /q $(LLVM_SRC_DIR)
	if exist $(LLVM_DEBUG_BUILD_DIR) rmdir /s /q $(LLVM_DEBUG_BUILD_DIR)
	if exist $(LLVM_DEBUG_INSTALL_DIR) rmdir /s /q $(LLVM_DEBUG_INSTALL_DIR)

.PHONY: clean
clean:
	if exist $(BUILD_DIR)\$(ASSEMBLY)$(EXTENSION) del $(BUILD_DIR)\$(ASSEMBLY)$(EXTENSION)
	if exist $(BUILD_DIR)\$(ASSEMBLY).ilk del $(BUILD_DIR)\$(ASSEMBLY).ilk
	if exist $(BUILD_DIR)\$(ASSEMBLY).pdb del $(BUILD_DIR)\$(ASSEMBLY).pdb
	if exist $(BUILD_DIR)\$(ASSEMBLY).exp del $(BUILD_DIR)\$(ASSEMBLY).exp
	if exist $(BUILD_DIR)\$(ASSEMBLY).lib del $(BUILD_DIR)\$(ASSEMBLY).lib
	rmdir /s /q $(OBJ_DIR)\$(BASE_DIR)

-include $(OBJ_FILES:.o=.d)