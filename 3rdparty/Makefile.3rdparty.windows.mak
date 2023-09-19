
DIR := $(subst /,\,${CURDIR})

include $(DIR)/3rdparty/dyncall/Makefile.dyncall_vars.windows.mak
include $(DIR)/3rdparty/llvm/Makefile.llvm_vars.windows.mak
include $(DIR)/3rdparty/munit/Makefile.munit_vars.windows.mak

CXXOPTS_INCLUDE_FLAGS := -I3rdparty\cxxopts
