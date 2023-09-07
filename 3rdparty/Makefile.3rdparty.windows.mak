
CWD := $(shell cd)
include $(CWD)/3rdparty/dyncall/Makefile.dyncall_vars.windows.mak
include $(CWD)/3rdparty/munit/Makefile.munit_vars.windows.mak

CXXOPTS_INCLUDE_FLAGS := -I3rdparty\cxxopts
