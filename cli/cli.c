#include "../core/omg.h"
#include "help.h"
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const omg_error NO_ERROR = {.code = OMG_CODE_OK};

omg_error cli_print_help(void) {
  printf("%.*s\n", cli_help_txt_len, cli_help_txt);
  exit(1);
}

void delete_gist(omg_context ctx, char *gist_id) {
  omg_error err = omg_delete_gist(ctx, gist_id);
  if (!is_ok(err)) {
    fprintf(stderr, "delete gist failed");
    print_error(err);
  }
}

omg_error cli_sync(const char *token, const char *db_path) {
  omg_auto_context ctx = NULL;
  omg_error err = omg_setup_context(db_path, token, &ctx);
  if (!is_ok(err)) {
    return err;
  }

  err = omg_sync_starred_repos(ctx);
  if (!is_ok(err)) {
    return err;
  }

  err = omg_sync_created_repos(ctx);
  if (!is_ok(err)) {
    return err;
  }

  err = omg_sync_created_gists(ctx);
  if (!is_ok(err)) {
    return err;
  }

  return omg_sync_starred_gists(ctx);
}

omg_error cli_trendings(const char *token, const char *db_path,
                        // params not used now
                        const char *_params) {
  omg_auto_context ctx = NULL;
  omg_error err = omg_setup_context(db_path, token, &ctx);
  if (!is_ok(err)) {
    return err;
  }
  omg_auto_repo_list lst = {};
  err = omg_query_trending(ctx, NULL, NULL, "daily", &lst);
  if (!is_ok(err)) {
    return err;
  }
  printf("%-12s|%-30s|%-100s\n", "Recent stars", "Repository", "Description");
  for (size_t i = 0; i < lst.length; i++) {
    char recent_stars[10];
    sprintf(recent_stars, "%d", lst.repo_array[i].stargazers_count);
    printf("%-12.*s|%-30.*s|%-100.*s\n",      // format
           12, recent_stars,                  // stars
           25, lst.repo_array[i].full_name,   // name
           100, lst.repo_array[i].description // desc
    );
  }

  return NO_ERROR;
}

int main(int argc, char **argv) {

  const char *token = getenv("GITHUB_TOKEN");
  if (!token) {
    fprintf(stderr, "token not set\n");
    return 1;
  }
  static int help_flag;
  static int sync_flag;
  const char *trend_param = NULL;
  const char *db_path = NULL;

  while (true) {
    static struct option long_options[] = {
        {"help", no_argument, &help_flag, 1},
        {"sync", no_argument, &sync_flag, 1},
        {"trend", required_argument, 0, 't'},
        {"db-file", required_argument, 0, 'f'},
        {0, 0, 0, 0}};
    /* getopt_long stores the option index here. */
    int option_index = 0;

    int c = getopt_long(argc, argv, "ht:sf:", long_options, &option_index);

    /* Detect the end of the options. */
    if (c == -1)
      break;

    switch (c) {
    case 0:
      /* If this option set a flag, do nothing else now. */
      break;

    case 'h':
      help_flag = 1;
      break;

    case 'f':
      db_path = optarg;
      break;

    case 's':
      sync_flag = 1;
      break;

    case 't':
      trend_param = optarg;
      break;

    default:
      exit(1);
    }
  }

  if (help_flag) {
    cli_print_help();
  } else if (sync_flag) {
    omg_error err = cli_sync(token, db_path);
    if (!is_ok(err)) {
      print_error(err);
    }
  } else if (trend_param != NULL) {
    omg_error err = cli_trendings(token, db_path, trend_param);
    if (!is_ok(err)) {
      print_error(err);
    }
  } else {
    cli_print_help();
  }

  /* omg_auto_created_repo_list lst; */
  /* err = omg_query_repos(ctx, "vagrant", NULL, &lst); */
  /* err = omg_query_repos(ctx, NULL, "Rust", &lst); */
  /* if (!is_ok(err)) { */
  /*   print_error(err); */
  /*   return 1; */
  /* } */
  /* printf("lst len:%d\n", lst.length); */
  /* for (int i = 0; i < lst.length; i++) { */
  /*   printf("name:%s\n", lst.repo_array[i].full_name); */
  /*   printf("desc:%s\n", lst.repo_array[i].description); */
  /*   printf("stars:%d\n", lst.repo_array[i].stargazers_count); */
  /* } */
}
