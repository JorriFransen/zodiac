
CWD := $(shell cd)

DYNCALL_VERSION := 1.4
DYNCALL_BASE_DIR := $(CWD)\zodiac_lib\dyncall
DYNCALL_SOURCE_DIR := $(DYNCALL_BASE_DIR)\dyncall-$(DYNCALL_VERSION)
DYNCALL_MAKEFILE := $(DYNCALL_SOURCE_DIR)\Makefile
DYNCALL_LIB := $(CWD)\zodiac_lib\dyncall\dyncall-$(DYNCALL_VERSION)\dyncall\dyncall_s.lib

DYNCALL_INCLUDE_FLAGS := -I$(DYNCALL_SOURCE_DIR)\dyncall -I$(DYNCALL_SOURCE_DIR)\dyncallback -I$(DYNCALL_SOURCE_DIR)\dynload
DYNCALL_LINK_FLAGS := -L$(DYNCALL_SOURCE_DIR)\dyncall -L$(DYNCALL_SOURCE_DIR)\dyncallback -L$(DYNCALL_SOURCE_DIR)\dynload -ldyncall_s -ldyncallback_s -ldynload_s
