ifeq (, $(shell which zig))
	CC ?= gcc
else
	CC ?= zig cc
endif
CORE_DIR = core
EMACS_DIR = emacs
CLI_DIR = cli

ifeq ($(OS),Windows_NT)
	uname_S := Windows
else
	uname_S := $(shell uname -s)
endif

# On macOS, dynamic modules can also have the suffix .so in addition to .dylib.
SO_FILE = $(EMACS_DIR)/omg-dyn.so
ifeq ($(uname_S), Windows)
	SO_FILE = $(EMACS_DIR)/omg-dyn.dll
endif

CLI = omg-cli
COMMON_HEADERS = $(CORE_DIR)/omg.h $(CORE_DIR)/create_table.h

CLI_OBJECTS = $(CORE_DIR)/omg.o $(CLI_DIR)/cli.o
CLI_HEADERS = $(COMMON_HEADERS) $(CLI_DIR)/help.h
EMACS_OBJECTS = $(CORE_DIR)/omg.o $(EMACS_DIR)/emacs.o
EMACS_HEADERS = $(COMMON_HEADERS)

# Why -fPIC https://stackoverflow.com/a/5311665/2163429
CFLAGS += -g $(shell pkg-config --cflags jansson libcurl sqlite3 libpcre2-posix) -fPIC \
	-std=gnu99 -Wall -Wextra -Werror -Wno-unused-parameter \
	-Wno-gnu -Wimplicit-fallthrough
	# Issue all the warnings demanded by strict ISO C and ISO C++;
	# -Wpedantic

ifeq ($(OMG_TEST), 1)
	CFLAGS += -D OMG_TEST
endif
ifeq ($(OMG_VERBOSE), 1)
	CFLAGS += -D VERBOSE
endif

ifeq ($(uname_S), Darwin)
	# curl/sqlite3 are pre-installed on macOS, so dynamic linking them
	LDFLAGS += $(shell pkg-config --libs libcurl sqlite3) -pthread
	# static link those
	LDFLAGS += $(shell pkg-config --variable=libdir jansson)/libjansson.a
	LDFLAGS += $(shell pkg-config --variable=libdir libpcre2-posix)/libpcre2-posix.a
	LDFLAGS += $(shell pkg-config --variable=libdir libpcre2-posix)/libpcre2-8.a
endif
ifeq ($(uname_S), Linux)
	# TODO: Need to figure out how to static linking on Linux
	LDFLAGS += $(shell pkg-config --libs jansson libcurl sqlite3 libpcre2-posix) -pthread
endif

ifeq ($(OMG_TEST), 1)
	LDFLAGS += -O1 -v
else
	LDFLAGS += -O3
endif

# ASAN cannot be used with valgrind together
ifeq ($(ENABLE_ASAN), 1)
	CFLAGS += -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=address
	LDFLAGS += -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=address
endif

all: $(CLI) emacs-dyn

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(CORE_DIR)/create_table.h: $(CORE_DIR)/create_table.sql
	xxd -i $< | tac | sed '3s/$$/, 0x00/' | tac > $@

$(CLI_DIR)/help.h: $(CLI_DIR)/help.txt
	xxd -i $< > $@

$(CLI): $(CLI_OBJECTS) $(CLI_HEADERS)
	$(CC) $(CLI_OBJECTS) $(LDFLAGS) -o $(CLI)

memcheck:
	valgrind --tool=memcheck --leak-check=full --show-leak-kinds=all ./$(CLI) -f /tmp/test.db

$(SO_FILE): $(EMACS_HEADERS) $(EMACS_OBJECTS)
	$(CC) -shared $(EMACS_OBJECTS) $(LDFLAGS) -o $(SO_FILE)

emacs-dyn: $(SO_FILE)
	@echo "Emacs dynamic module saved to $(SO_FILE)"

install-deps:
ifeq ($(uname_S), Darwin)
	brew install jansson pkg-config pcre2 sqlite
endif
ifeq ($(uname_S), Linux)
	sudo apt install -y libcurl4-openssl-dev pkg-config libjansson-dev libsqlite3-dev valgrind libpcre2-dev xxd
endif

test:
	cd tests && zig build run

clean:
	rm -f $(CLI) $(SO_FILE) $(EMACS_OBJECTS) $(CLI_OBJECTS)
	cd tests && rm -rf zig-cache zig-out
