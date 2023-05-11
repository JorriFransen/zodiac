
CWD:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

DYNCALL_VERSION := 1.4
DYNCALL_BASE_DIR := $(CWD)/zodiac_lib/dyncall
DYNCALL_INSTALL_DIR := $(DYNCALL_BASE_DIR)/install
DYNCALL_SOURCE_DIR := $(DYNCALL_BASE_DIR)/dyncall-$(DYNCALL_VERSION)
DYNCALL_MAKEFILE := $(DYNCALL_SOURCE_DIR)/Makefile
DYNCALL_LIB := $(CWD)/zodiac_lib/dyncall/install/lib/libdyncall_s.a

DYNCALL_INCLUDE_FLAGS := -I$(DYNCALL_INSTALL_DIR)/include
DYNCALL_LINK_FLAGS := -L$(DYNCALL_INSTALL_DIR)/lib -ldyncall_s -ldyncallback_s -ldynload_s