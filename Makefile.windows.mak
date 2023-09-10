
CWD := $(shell cd)
include $(CWD)/3rdparty/Makefile.3rdparty.windows.mak

COMMON_FLAGS := --no-print-directory

all: dyncall llvm zodiac_lib zodiac_driver zodiac_tests

.PHONY: zodiac_lib
zodiac_lib: dyncall llvm
	@$(MAKE) -f makefiles/Makefile.zodiac_lib.windows.mak $(COMMON_FLAGS)

zodiac_driver: zodiac_lib
	@$(MAKE) -f makefiles/Makefile.zodiac_driver.windows.mak $(COMMON_FLAGS)

zodiac_tests: zodiac_lib
	@$(MAKE) -f makefiles/Makefile.zodiac_tests.windows.mak $(COMMON_FLAGS)

.PHONY: clean
clean: clean_dyncall clean_llvm
	@$(MAKE) -f makefiles/Makefile.zodiac_lib.windows.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f makefiles/Makefile.zodiac_driver.windows.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f makefiles/Makefile.zodiac_tests.windows.mak clean $(COMMON_FLAGS)

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
	cd "$(DYNCALL_SOURCE_DIR)" && set MAKEFLAGS= && nmake -nologo -f Nmakefile || cd .


.PHONY: clean_dyncall
clean_dyncall:
	if exist $(DYNCALL_SOURCE_DIR) cd $(DYNCALL_SOURCE_DIR) && nmake -nologo -f Nmakefile clean


.PHONY: llvm
llvm:
	@if not exist $(LLVM_DEBUG_INSTALL_DIR) (\
		echo Extracting llvm... && \
		powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('3rdparty\llvm\llvm-$(LLVM_VERSION).src.zip', '$(LLVM_SRC_DIR)') }" &&\
		echo Configuring llvm... &&\
		mkdir $(LLVM_DEBUG_BUILD_DIR) &&\
		pushd $(LLVM_DEBUG_BUILD_DIR) &&\
		cmake.exe $(LLVM_SRC_DIR)\llvm-project-llvmorg-$(LLVM_VERSION)\llvm -DCMAKE_INSTALL_PREFIX=$(LLVM_DEBUG_INSTALL_DIR) -DLLVM_TARGETS_TO_BUILD=X86 -DCMAKE_BUILD_TYPE=Debug -T ClangCl &&\
		echo Building and installing llvm... &&\
		mkdir $(LLVM_DEBUG_INSTALL_DIR) &&\
		pushd $(LLVM_DEBUG_BUILD_DIR) &&\
		cmake.exe --build . --target install --config Debug &&\
		popd )
	@if exist $(LLVM_SRC_DIR) (\
		@echo Removing llvm source dir after instal... &&\
		rmdir /s /q $(LLVM_SRC_DIR) )
	@if exist $(LLVM_DEBUG_BUILD_DIR) (\
		echo Removing llvm debug build dir after install &&\
		rmdir /s /q $(LLVM_DEBUG_BUILD_DIR) )

.PHONY: clean_llvm
clean_llvm:
	if exist $(LLVM_SRC_DIR) rmdir /s /q $(LLVM_SRC_DIR)
	if exist $(LLVM_DEBUG_BUILD_DIR) rmdir /s /q $(LLVM_DEBUG_BUILD_DIR)
	REM if exist $(LLVM_DEBUG_INSTALL_DIR) rmdir /s /q $(LLVM_DEBUG_INSTALL_DIR)