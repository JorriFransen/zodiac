
CWD:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

DYNCALL_VERSION := 1.4
DYNCALL_BASE_DIR := $(CWD)/zodiac_lib/dyncall
DYNCALL_INSTALL_DIR := $(DYNCALL_BASE_DIR)/install
DYNCALL_SOURCE_DIR := $(DYNCALL_BASE_DIR)/dyncall-$(DYNCALL_VERSION)
DYNCALL_MAKEFILE := $(DYNCALL_SOURCE_DIR)/Makefile
DYNCALL_LIB := $(CWD)/zodiac_lib/dyncall/install/lib/libdyncall_s.a

.PHONY: dyncall
dyncall: $(DYNCALL_LIB)
	@:

$(DYNCALL_INSTALL_DIR):
	@mkdir -p $(DYNCALL_INSTALL_DIR)

$(DYNCALL_MAKEFILE):
	@echo "Configuring dyncall..."
	@cd $(DYNCALL_SOURCE_DIR); \
	./configure --prefix=$(DYNCALL_INSTALL_DIR) > /dev/null

$(DYNCALL_LIB):  $(DYNCALL_INSTALL_DIR) $(DYNCALL_MAKEFILE)
	@echo "Building dyncall..."
	@cd $(DYNCALL_SOURCE_DIR); \
	$(MAKE) $(COMMON_FLAGS) > /dev/null; \
	$(MAKE) install $(COMMON_FLAGS) > /dev/null


.PHONY: clean_dyncall
clean_dyncall:
	rm -rf $(DYNCALL_INSTALL_DIR)
	rm $(DYNCALL_SOURCE_DIR)/Makefile
