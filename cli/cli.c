#include "../core/omg.h"
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

  omg_auto_context ctx = NULL;
  omg_error err = omg_setup_context(db_path, token, &ctx);
  if (!is_ok(err)) {
    print_error(err);
    return 1;
  }
  /*
  err = omg_sync_stars(ctx);
  if (!is_ok(err)) {
    print_error(err);
    return 1;
  }

  err = omg_sync_repos(ctx);
  if (!is_ok(err)) {
    print_error(err);
    return 1;
  }
  */
  omg_auto_repo_list lst = omg_new_repo_list();
  err = omg_query_trending(ctx, "c", "weekly", &lst);
  if (!is_ok(err)) {
    print_error(err);
    return 1;
  }

  /* omg_auto_repo_list lst; */
  /* err = omg_query_repos(ctx, "vagrant", NULL, &lst); */
  /* err = omg_query_repos(ctx, NULL, "Rust", &lst); */
  /* if (!is_ok(err)) { */
  /*   print_error(err); */
  /*   return 1; */
  /* } */
  printf("lst len:%d\n", lst.length);
  for (int i = 0; i < lst.length; i++) {
    printf("name:%s\n", lst.repo_array[i].full_name);
    printf("desc:%s\n", lst.repo_array[i].description);
  }
}
