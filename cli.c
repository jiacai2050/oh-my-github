#include "ghs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
  const char *token = getenv("GITHUB_TOKEN");
  if (!token) {
    fprintf(stderr, "token not set\n");
    return 1;
  }
  if (argc < 2) {
    fprintf(stderr, "db path not set\n");
    return 1;
  }
  const char *db_path = argv[1];

  ghs_context ctx;
  ghs_error err = ghs_setup_context(db_path, token, &ctx);
  if (!is_ok(err)) {
    print_error(err);
    return 1;
  }
  err = ghs_sync_stars(ctx);
  if (!is_ok(err)) {
    print_error(err);
    return 1;
  }
}
