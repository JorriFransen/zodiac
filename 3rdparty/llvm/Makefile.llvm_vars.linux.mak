
LLVM_VERSION := 16.0.6
LLVM_BASE_DIR := $(CWD)/3rdparty/llvm

LLVM_CONFIG := llvm-config
LLVM_INCLUDE_DIR = $(shell $(LLVM_CONFIG) --includedir)
LLVM_INCLUDE_FLAGS = -I$(LLVM_INCLUDE_DIR)
LLVM_LIBS = $(shell $(LLVM_CONFIG) --libs x86 --link-static --system-libs)
LLVM_LINKER_FLAGS = $(LLVM_LIBS) $(shell $(LLVM_CONFIG) --ldflags)
LLVM_CXX_FLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
