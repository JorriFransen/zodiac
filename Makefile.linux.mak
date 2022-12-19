
COMMON_FLAGS := --no-print-directory

all: zodiac_lib zodiac_driver zodiac_tests

.PHONY: zodiac_lib
zodiac_lib:
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
clean:
	@$(MAKE) -f Makefile.zodiac_lib.linux.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f Makefile.zodiac_driver.linux.mak clean $(COMMON_FLAGS)
	@$(MAKE) -f Makefile.zodiac_tests.linux.mak clean $(COMMON_FLAGS)
