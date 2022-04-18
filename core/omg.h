#ifndef OMG_H
#define OMG_H
#include <stdbool.h>
#include <stdlib.h>

#define OMG_CODE_OK 0
#define OMG_CODE_CURL 1
#define OMG_CODE_JSON 2
#define OMG_CODE_DB 3
#define OMG_CODE_INTERNAL 4

void omg_free_char(char **buf);
#define omg_auto_char char *__attribute__((cleanup(omg_free_char)))

typedef struct omg_error {
  int code;
  const char *message;
} omg_error;

void print_error(omg_error err);
bool is_ok(omg_error err);

/* Opaque pointer representing omg core concept: context
   All omg-related functions require this argument!  */
typedef struct omg_context *omg_context;

omg_error omg_setup_context(const char *path, const char *github_token,
                            omg_context *out);

void omg_free_context(omg_context *ctx);
#define omg_auto_context omg_context __attribute__((cleanup(omg_free_context)))

// repo
typedef struct omg_repo {
  int id;
  const char *full_name;
  const char *description;
  bool private;
  const char *created_at;
  const char *license;
  const char *pushed_at;
  int stargazers_count;
  int watchers_count;
  int forks_count;
  const char *lang;
  const char *homepage;
  int size;
} omg_repo;

void omg_free_repo(omg_repo *repo);
#define omg_auto_repo omg_repo __attribute__((cleanup(omg_free_repo)))

typedef struct omg_repo_list {
  omg_repo *repo_array;
  size_t length;
} omg_repo_list;

void omg_free_repo_list(omg_repo_list *repo_lst);
#define omg_auto_repo_list                                                     \
  omg_repo_list __attribute__((cleanup(omg_free_repo_list)))

omg_error omg_sync_repos(omg_context ctx);
omg_error omg_query_repos(omg_context ctx, const char *keyword,
                          const char *language, omg_repo_list *out);

// star
typedef struct omg_star {
  char *starred_at;
  omg_repo repo;
} omg_star;

void omg_free_star(omg_star *star);
#define omg_auto_star omg_star __attribute__((cleanup(omg_free_star)))

typedef struct omg_star_list {
  omg_star *star_array;
  size_t length;
} omg_star_list;

void omg_free_star_list(omg_star_list *);
#define omg_auto_star_list                                                     \
  omg_star_list __attribute__((cleanup(omg_free_star_list)))

omg_error omg_query_stars(omg_context ctx, const char *keyword,
                          const char *language, omg_star_list *out);
omg_error omg_sync_stars(omg_context ctx);
omg_error omg_unstar(omg_context ctx, size_t repo_id);

#endif
