
# Make does not offer a recursive wildcard function, so here's one:
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

MUNIT_BASE_DIR := 3rdparty\munit
MUNIT_SOURCE_DIR := $(MUNIT_BASE_DIR)\munit
MUNIT_INCLUDE_FLAGS := -I$(MUNIT_BASE_DIR)
MUNIT_SOURCE_FILES :=  $(subst /,\, $(call rwildcard,$(MUNIT_BASE_DIR)/munit/,*.c))

MUNIT_SOURCE_DIRS := $(MUNIT_BASE_DIR) $(subst $(DIR),,$(shell dir $(MUNIT_BASE_DIR) /S /AD /B | findstr /i munit))
