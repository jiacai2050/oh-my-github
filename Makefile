CORE_DIR = core
EMACS_DIR = emacs
CLI_DIR = cli

ifeq ($(OS),Windows_NT)
	uname_S := Windows
else
	uname_S := $(shell uname -s)
endif

SO_FILE = $(EMACS_DIR)/ghs-dyn.so
ifeq ($(uname_S), Windows)
	SO_FILE = $(EMACS_DIR)/ghs-dyn.dll
endif
ifeq ($(uname_S), Darwin)
	SO_FILE = $(EMACS_DIR)/ghs-dyn.dylib
endif

CLI = ghs-cli
OBJECTS = $(CORE_DIR)/ghs.o $(CLI_DIR)/cli.o $(EMACS_DIR)/emacs.o
HEADERS = $(CORE_DIR)/create_table.h $(CORE_DIR)/ghs.h
CFLAGS += -g $(shell pkg-config --cflags jansson libcurl sqlite3)
LDFLAGS += -lcurl $(shell pkg-config --libs jansson libcurl sqlite3)
CC = gcc

$(CLI): $(OBJECTS)
	@if [ X$(GHS_TEST) = X1 ]; then \
		echo "[Linking] test mode..." && \
		$(CC) -O1 -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=address $(OBJECTS) $(LDFLAGS) -o $(CLI) ;\
	else \
		$(CC) -O3 $(OBJECTS) $(LDFLAGS) -o $(CLI) ;\
	fi

%.o: %.c $(HEADERS)
	@if [ X$(GHS_TEST) = X1 ]; then \
		echo "[Compile] test mode..." && \
		$(CC) -D VERBOSE -D GHS_TEST $(CFLAGS) -c $< -o $@ ;\
	else \
		$(CC) $(CFLAGS) -c $< -o $@ ;\
	fi

core/create_table.h: core/create_table.sql
	xxd -i $< | tac | sed '3s/$$/, 0x00/' | tac > $@

memcheck:
	valgrind --tool=memcheck --leak-check=full --show-leak-kinds=all ./$(CLI) /tmp/test.db

$(SO_FILE): $(OBJECTS)
	$(CC) -O3 -dynamiclib $(OBJECTS) $(LDFLAGS) -o $(SO_FILE)

clean:
	rm -f $(CLI) $(SO_FILE) $(OBJECTS)
