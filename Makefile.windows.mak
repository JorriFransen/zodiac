
CWD := $(shell cd)
include $(CWD)/3rdparty/Makefile.3rdparty.windows.mak

COMMON_FLAGS := --no-print-directory

all: dyncall zodiac_lib zodiac_driver zodiac_tests

.PHONY: zodiac_lib
zodiac_lib: dyncall
	@$(MAKE) -f Makefile.zodiac_lib.windows.mak $(COMMON_FLAGS)

zodiac_driver: zodiac_lib
	@$(MAKE) -f Makefile.zodiac_driver.windows.mak $(COMMON_FLAGS)

zodiac_tests: zodiac_lib
	@$(MAKE) -f Makefile.zodiac_tests.windows.mak $(COMMON_FLAGS)

.PHONY: clean
clean: clean_dyncall
	@$(MAKE) -f Makefile.zodiac_lib.windows.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f Makefile.zodiac_driver.windows.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f Makefile.zodiac_tests.windows.mak clean $(COMMON_FLAGS)

.PHONY: dyncall
dyncall: $(DYNCALL_LIB)

$(DYNCALL_SOURCE_DIR):
	@echo "Extracting dyncall..."
	powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('$(DYNCALL_ARCHIVE)', '$(DYNCALL_BASE_DIR)') }"

$(DYNCALL_MAKEFILE): $(DYNCALL_SOURCE_DIR)
	@echo "Configuring dyncall..."
	cd $(DYNCALL_SOURCE_DIR) && configure.bat

$(DYNCALL_LIB): $(DYNCALL_SOURCE_DIR) $(DYNCALL_MAKEFILE)
	@echo "Building dyncall..."
	cd "$(DYNCALL_SOURCE_DIR)" && set MAKEFLAGS= && nmake -f Nmakefile || cd .


.PHONY: clean_dyncall
clean_dyncall:
	cd $(DYNCALL_SOURCE_DIR) && nmake -f Nmakefile clean
