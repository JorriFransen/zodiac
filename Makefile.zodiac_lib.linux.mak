BUILD_DIR := bin
OBJ_DIR := obj

BASE_DIR := zodiac_lib
SRC_DIR := $(BASE_DIR)/src
ASSEMBLY := libzodiac
EXTENSION := .a
COMPILER_FLAGS := -g -MD -Wall -Werror -Wvla -fdeclspec -fPIC
INCLUDE_FLAGS := -I$(SRC_DIR)
LINKER_FLAGS := -g -static
DEFINES := -D_DEBUG -DZEXPORT

SRC_FILES := $(shell find $(SRC_DIR) -name *.cpp)
DIRECTORIES := $(shell find $(SRC_DIR) -type d)
OBJ_FILES := $(SRC_FILES:%=$(OBJ_DIR)/%.o)

all: scaffold compile link
	
.PHONY: scaffold
scaffold: #create build directory
	@mkdir -p bin
	@mkdir -p $(addprefix $(OBJ_DIR)/,$(DIRECTORIES))
	
.PHONY: link
link: scaffold $(OBJ_FILES)
	@echo Linking $(ASSEMBLY)
	ar r $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION) $(OBJ_FILES)
	

.PHONY: compile
compile:
	@echo Compiling $(ASSEMBLY)

$(OBJ_DIR)/%.cpp.o: %.cpp
	clang $< $(COMPILER_FLAGS) -c -o $@ $(DEFINES) $(INCLUDE_FLAGS)
	
.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)/$(ASSEMBLY)$(EXTENSION)
	rm -rf $(OBJ_DIR)/$(BASE_DIR)

-include $(OBJ_FILES:.o=.d)