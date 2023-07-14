

include Makefile.dyncall_vars.linux.mak

COMMON_FLAGS := --no-print-directory

all: dyncall zodiac_lib zodiac_driver zodiac_tests iwyu

.PHONY: zodiac_lib
zodiac_lib: dyncall
	@$(MAKE) -f Makefile.zodiac_lib.linux.mak $(COMMON_FLAGS)

.PHONY: zodiac_driver
zodiac_driver: zodiac_lib
	@$(MAKE) -f Makefile.zodiac_driver.linux.mak $(COMMON_FLAGS)

.PHONY: zodiac_tests
zodiac_tests: zodiac_lib
	@$(MAKE) -f Makefile.zodiac_tests.linux.mak $(COMMON_FLAGS)

.PHONY: iwyu
iwyu: zodiac_lib zodiac_driver zodiac_tests
	./.iwyu.sh

.PHONY: clean
clean: clean_dyncall
	@$(MAKE) -f Makefile.zodiac_lib.linux.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f Makefile.zodiac_driver.linux.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f Makefile.zodiac_tests.linux.mak clean $(COMMON_FLAGS)

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
	rm -f $(DYNCALL_SOURCE_DIR)/Makefile
