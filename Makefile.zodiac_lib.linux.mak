BUILD_DIR := bin
OBJ_DIR := obj

BASE_DIR := zodiac_lib
SRC_DIR := $(BASE_DIR)/src
ASSEMBLY := libzodiac
EXTENSION := .so

SUPPORT_SRC_DIR := $(BASE_DIR)/support
SUPPORT_ASSEMBLY_DYN := libzrs
SUPPORT_EXTENSION_DYN := .so
SUPPORT_ASSEMBLY_STAT := libzrs_s
SUPPORT_EXTENSION_STAT := .a

LLVM_LIBS := $(shell llvm-config --libs x86 --link-static --system-libs)
LLVM_CXX_FLAGS := $(shell llvm-config --cxxflags)
LLVM_LD_FLAGS := $(shell llvm-config --ldflags)

include Makefile.dyncall_vars.linux.mak

COMMON_COMPILER_FLAGS := -g -Og -MD -MP -Wall -Wvla -Werror -Wno-c99-designator -fdeclspec -fPIC
COMMON_LINKER_FLAGS := -g -Og

COMPILER_FLAGS := $(COMMON_COMPILER_FLAGS) $(LLVM_CXX_FLAGS)
INCLUDE_FLAGS := -I$(SRC_DIR) $(DYNCALL_INCLUDE_FLAGS)
LINKER_FLAGS := $(COMMON_LINKER_FLAGS) -shared $(LLVM_LIBS) $(LLVM_LD_FLAGS) $(DYNCALL_LINK_FLAGS)
DEFINES := -D_DEBUG -DZEXPORT

SRC_FILES := $(shell find $(SRC_DIR) -name *.cpp)
DIRECTORIES := $(shell find $(SRC_DIR) -type d)
OBJ_FILES := $(SRC_FILES:%=$(OBJ_DIR)/%.o)

SUPPORT_SRC_FILES := $(shell find $(SUPPORT_SRC_DIR) -name *.cpp)
SUPPORT_DIRECTORIES := $(shell find $(SUPPORT_SRC_DIR) -type d)
SUPPORT_OBJ_FILES := $(SUPPORT_SRC_FILES:%=$(OBJ_DIR)/%.o)

FULL_ASSEMBLY_PATH := $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION)
SUPPORT_ASSEMBLY_DYN_PATH := $(BUILD_DIR)/$(SUPPORT_ASSEMBLY_DYN)$(SUPPORT_EXTENSION_DYN)
SUPPORT_ASSEMBLY_STAT_PATH := $(BUILD_DIR)/$(SUPPORT_ASSEMBLY_STAT)$(SUPPORT_EXTENSION_STAT)

all: scaffold compile_support compile link

.PHONY: scaffold
scaffold:
	@mkdir -p bin
	@mkdir -p $(addprefix $(OBJ_DIR)/,$(DIRECTORIES))
	@mkdir -p $(addprefix $(OBJ_DIR)/,$(SUPPORT_DIRECTORIES))

.PHONY: compile_support
compile_support:
	@echo Compiling $(SUPPORT_ASSEMBLY_DYN)
	@echo Compiling $(SUPPORT_ASSEMBLY_STAT)

.PHONY: compile
compile:
	@echo Compiling $(ASSEMBLY)

$(OBJ_DIR)/%.cpp.o: %.cpp
	@echo "$< -> $@"
	@clang++ $< $(COMPILER_FLAGS) -c -o $@ $(DEFINES) $(INCLUDE_FLAGS)

.PHONY: link
link: $(FULL_ASSEMBLY_PATH) $(SUPPORT_ASSEMBLY_DYN_PATH) $(SUPPORT_ASSEMBLY_STAT_PATH)

$(SUPPORT_ASSEMBLY_DYN_PATH): $(SUPPORT_OBJ_FILES)
	@echo Linking $(SUPPORT_ASSEMBLY_DYN)
	@clang++ $(SUPPORT_OBJ_FILES) $(COMMON_COMPILER_FLAGS) -o $@ $(DEFINES) $(COMMON_LINKER_FLAGS) -shared

$(SUPPORT_ASSEMBLY_STAT_PATH): $(SUPPORT_OBJ_FILES)
	@echo Linking $(SUPPORT_ASSEMBLY_STAT)
	@ar r $@ $(SUPPORT_OBJ_FILES)

$(FULL_ASSEMBLY_PATH): $(OBJ_FILES)
	@echo Linking $(ASSEMBLY)
	@clang++ $(OBJ_FILES) $(COMPILER_FLAGS) -o $@ $(DEFINES) $(LINKER_FLAGS)


.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION)
	rm -rf $(BUILD_DIR)/$(SUPPORT_ASSEMBLY_DYN)$(SUPPORT_EXTENSION_DYN)
	rm -rf $(BUILD_DIR)/$(SUPPORT_ASSEMBLY_STAT)$(SUPPORT_EXTENSION_STAT)
	rm -rf $(OBJ_DIR)/$(BASE_DIR)

-include $(OBJ_FILES:.o=.d)
