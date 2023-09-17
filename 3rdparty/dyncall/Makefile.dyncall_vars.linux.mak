
DYNCALL_VERSION := 1.4
DYNCALL_BASE_DIR := $(PWD)/3rdparty/dyncall
DYNCALL_ARCHIVE := $(DYNCALL_BASE_DIR)/dyncall-$(DYNCALL_VERSION).zip
DYNCALL_INSTALL_DIR := $(DYNCALL_BASE_DIR)/install
DYNCALL_SOURCE_DIR := $(DYNCALL_BASE_DIR)/dyncall-$(DYNCALL_VERSION)
DYNCALL_MAKEFILE := $(DYNCALL_SOURCE_DIR)/Makefile
DYNCALL_LIB := $(DYNCALL_INSTALL_DIR)/lib/libdyncall_s.a

DYNCALL_INCLUDE_FLAGS := -I$(DYNCALL_INSTALL_DIR)/include
DYNCALL_LINK_FLAGS := -L$(DYNCALL_INSTALL_DIR)/lib -ldyncall_s -ldyncallback_s -ldynload_s