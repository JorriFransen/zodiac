
COMMON_FLAGS := --no-print-directory

all: zodiac_lib zodiac_driver zodiac_tests

.PHONY: zodiac_lib
zodiac_lib:
	@$(MAKE) -f Makefile.zodiac_lib.windows.mak $(COMMON_FLAGS)

zodiac_driver: zodiac_lib
	@$(MAKE) -f Makefile.zodiac_driver.windows.mak $(COMMON_FLAGS)

zodiac_tests: zodiac_lib
	@$(MAKE) -f Makefile.zodiac_tests.windows.mak $(COMMON_FLAGS)

.PHONY: clean
clean:
	@$(MAKE) -f Makefile.zodiac_lib.windows.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f Makefile.zodiac_driver.windows.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f Makefile.zodiac_tests.windows.mak clean $(COMMON_FLAGS)