
all: zodiac_lib zodiac_driver zodiac_tests

.PHONY: zodiac_lib
zodiac_lib:
	$(MAKE) -f Makefile.zodiac_lib.windows.mak

zodiac_driver: zodiac_lib
	$(MAKE) -f Makefile.zodiac_driver.windows.mak

zodiac_tests: zodiac_lib
	$(MAKE) -f Makefile.zodiac_tests.windows.mak

.PHONY: clean
clean:
	$(MAKE) -f Makefile.zodiac_lib.windows.mak clean
	$(MAKE) -f Makefile.zodiac_driver.windows.mak clean
	$(MAKE) -f Makefile.zodiac_tests.windows.mak clean
