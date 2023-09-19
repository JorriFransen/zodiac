
DIR := $(shell pwd)

include $(DIR)/3rdparty/Makefile.3rdparty.linux.mak

COMMON_FLAGS := --no-print-directory

all: dyncall zodiac_lib zodiac_driver zodiac_tests iwyu

.PHONY: zodiac_lib
zodiac_lib: dyncall
	@$(MAKE) -f makefiles/Makefile.zodiac_lib.linux.mak $(COMMON_FLAGS)

.PHONY: zodiac_driver
zodiac_driver: zodiac_lib
	@$(MAKE) -f makefiles/Makefile.zodiac_driver.linux.mak $(COMMON_FLAGS)

.PHONY: zodiac_tests
zodiac_tests: zodiac_lib
	@$(MAKE) -f makefiles/Makefile.zodiac_tests.linux.mak $(COMMON_FLAGS)

.PHONY: iwyu
iwyu: zodiac_lib zodiac_driver zodiac_tests
	./.iwyu.sh

.PHONY: clean
clean: clean_dyncall
	@$(MAKE) -f makefiles/Makefile.zodiac_lib.linux.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f makefiles/Makefile.zodiac_driver.linux.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f makefiles/Makefile.zodiac_tests.linux.mak clean $(COMMON_FLAGS)

.PHONY: dyncall
dyncall: $(DYNCALL_LIB)

$(DYNCALL_SOURCE_DIR):
	@echo "Extracting dyncall..."
	unzip -qq $(DYNCALL_ARCHIVE) 'dyncall-$(DYNCALL_VERSION)/**' -d $(DYNCALL_BASE_DIR)

$(DYNCALL_INSTALL_DIR):
	@mkdir -p $(DYNCALL_INSTALL_DIR)

$(DYNCALL_MAKEFILE): $(DYNCALL_SOURCE_DIR)
	@echo "Configuring dyncall..."
	cd $(DYNCALL_SOURCE_DIR) && ./configure --prefix=$(DYNCALL_INSTALL_DIR)

$(DYNCALL_LIB): $(DYNCALL_SOURCE_DIR) $(DYNCALL_INSTALL_DIR) $(DYNCALL_MAKEFILE)
	@echo "Building dyncall..."
	cd $(DYNCALL_SOURCE_DIR) && $(MAKE) $(COMMON_FLAGS) install > /dev/null


.PHONY: clean_dyncall
clean_dyncall:
	cd $(DYNCALL_SOURCE_DIR) && $(MAKE) $(COMMON_FLAGS) clean > /dev/null 2>&1
	rm -rf $(DYNCALL_INSTALL_DIR)
