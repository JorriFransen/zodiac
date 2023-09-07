
MUNIT_BASE_DIR := ./3rdparty/munit
MUNIT_INCLUDE_FLAGS := -I$(MUNIT_BASE_DIR)
MUNIT_SOURCE_FILES := $(shell find $(MUNIT_BASE_DIR)/munit -name *.c)
MUNIT_SOURCE_DIRS := $(shell find $(MUNIT_BASE_DIR)/munit -type d)
