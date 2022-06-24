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

CFLAGS += -g $(shell pkg-config --cflags jansson libcurl sqlite3 libpcre2-posix)
LDFLAGS += -lcurl $(shell pkg-config --libs jansson libcurl sqlite3 libpcre2-posix) -pthread
# ASAN cannot be used with valgrind together
ifeq ($(ENABLE_ASAN), 1)
	LDFLAGS += -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=address
endif
CC = gcc

.DEFAULT_GOAL := $(CLI)

%.o: %.c
ifeq ($(OMG_TEST), 1)
	$(CC) -D VERBOSE -D OMG_TEST $(CFLAGS) -c $< -o $@
else
	$(CC) $(CFLAGS) -c $< -o $@
endif

$(CORE_DIR)/create_table.h: $(CORE_DIR)/create_table.sql
	xxd -i $< | tac | sed '3s/$$/, 0x00/' | tac > $@

$(CLI_DIR)/help.h: $(CLI_DIR)/help.txt
	xxd -i $< > $@

$(CLI): $(CLI_OBJECTS) $(CLI_HEADERS)
ifeq ($(OMG_TEST), 1)
	$(CC) -O1  $(CLI_OBJECTS) $(LDFLAGS) -o $(CLI)
else
	$(CC) -O3 $(CLI_OBJECTS) $(LDFLAGS) -o $(CLI)
endif

memcheck:
	valgrind --tool=memcheck --leak-check=full --show-leak-kinds=all ./$(CLI) -f /tmp/test.db

$(SO_FILE): $(EMACS_HEADERS) $(EMACS_OBJECTS)
	$(CC) -O3 -dynamiclib $(EMACS_OBJECTS) $(LDFLAGS) -o $(SO_FILE)

emacs-dyn: $(SO_FILE)
	@echo "Emacs dynamic module saved to $(SO_FILE)"

install-deps:
ifeq ($(uname_S), Darwin)
	brew install jansson pkg-config pcre2
endif
ifeq ($(uname_S), Linux)
	sudo apt install -y libcurl4-openssl-dev pkg-config libjansson-dev libsqlite3-dev valgrind libpcre2-dev
endif

clean:
	rm -f $(CLI) $(SO_FILE) $(EMACS_OBJECTS) $(CLI_OBJECTS)
