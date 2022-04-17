#ifndef GHS_H
#define GHS_H
#include <stdbool.h>
#include <stdlib.h>

#define GHS_CODE_OK 0
#define GHS_CODE_CURL 1
#define GHS_CODE_JSON 2
#define GHS_CODE_DB 3
#define GHS_CODE_INTERNAL 4

void ghs_free_char(char **buf);
#define ghs_auto_char char *__attribute__((cleanup(ghs_free_char)))

typedef struct ghs_error {
  int code;
  const char *message;
} ghs_error;

void print_error(ghs_error err);
bool is_ok(ghs_error err);

/* Opaque pointer representing ghs core concept: context
   All ghs-related functions require this argument!  */
typedef struct ghs_context *ghs_context;

ghs_error ghs_setup_context(const char *path, const char *github_token,
                            ghs_context *out);

void ghs_free_context(ghs_context *ctx);
#define ghs_auto_context ghs_context __attribute__((cleanup(ghs_free_context)))

// repo
typedef struct ghs_repo {
  int id;
  const char *full_name;
  const char *description;
  bool private;
  const char *created_at;
  const char *updated_at;
  const char *pushed_at;
  int stargazers_count;
  int watchers_count;
  int forks;
  const char *lang;
  const char *homepage;
} ghs_repo;

void ghs_free_repo(ghs_repo *repo);
#define ghs_auto_repo ghs_repo __attribute__((cleanup(ghs_free_repo)))

typedef struct ghs_repo_list {
  ghs_repo *repo_array;
  size_t length;
} ghs_repo_list;

void ghs_free_repo_list(ghs_repo_list *repo_lst);
#define ghs_auto_repo_list                                                     \
  ghs_repo_list __attribute__((cleanup(ghs_free_repo_list)))

// star
typedef struct ghs_star {
  char *starred_at;
  ghs_repo repo;
} ghs_star;

void ghs_free_star(ghs_star *star);
#define ghs_auto_star ghs_star __attribute__((cleanup(ghs_free_star)))

typedef struct ghs_star_list {
  ghs_star *star_array;
  size_t length;
} ghs_star_list;

void ghs_free_star_list(ghs_star_list *);
#define ghs_auto_star_list                                                     \
  ghs_star_list __attribute__((cleanup(ghs_free_star_list)))

ghs_error ghs_query_stars(ghs_context ctx, const char *keyword,
                          const char *language, ghs_star_list *out);
ghs_error ghs_sync_stars(ghs_context ctx);
ghs_error ghs_unstar(ghs_context ctx, size_t repo_id);

#endif
