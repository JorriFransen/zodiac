
all: zodiac_lib zodiac_driver zodiac_tests

.PHONY: zodiac_lib
zodiac_lib:
	$(MAKE) -f Makefile.zodiac_lib.linux.mak

zodiac_driver: zodiac_lib
	$(MAKE) -f Makefile.zodiac_driver.linux.mak

zodiac_tests: zodiac_lib
	$(MAKE) -f Makefile.zodiac_tests.linux.mak

.PHONY: clean
clean:
	$(MAKE) -f Makefile.zodiac_lib.linux.mak clean
	$(MAKE) -f Makefile.zodiac_driver.linux.mak clean
	$(MAKE) -f Makefile.zodiac_tests.linux.mak clean
