
include Makefile.dyncall_vars.linux.mak

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
