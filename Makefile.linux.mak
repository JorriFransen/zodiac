
COMMON_FLAGS := --no-print-directory

all: zodiac_lib zodiac_driver zodiac_tests

.PHONY: zodiac_lib
zodiac_lib:
	@$(MAKE) -f Makefile.zodiac_lib.linux.mak $(COMMON_FLAGS)

zodiac_driver: zodiac_lib
	@$(MAKE) -f Makefile.zodiac_driver.linux.mak $(COMMON_FLAGS)

zodiac_tests: zodiac_lib
	@$(MAKE) -f Makefile.zodiac_tests.linux.mak $(COMMON_FLAGS)

.PHONY: clean
clean:
	@$(MAKE) -f Makefile.zodiac_lib.linux.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f Makefile.zodiac_driver.linux.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f Makefile.zodiac_tests.linux.mak clean $(COMMON_FLAGS)
