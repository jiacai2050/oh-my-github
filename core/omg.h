#ifndef OMG_H
#define OMG_H
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#define OMG_CODE_OK 0
#define OMG_CODE_CURL 1
#define OMG_CODE_JSON 2
#define OMG_CODE_DB 3
#define OMG_CODE_INTERNAL 4
#define OMG_CODE_GITHUB 5

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
                            int32_t timeout, omg_context *out);

void omg_free_context(omg_context *ctx);
#define omg_auto_context omg_context __attribute__((cleanup(omg_free_context)))

typedef struct {
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

typedef struct {
  omg_repo *repo_array;
  size_t length;
} omg_repo_list;

void omg_free_repo_list(omg_repo_list *repo_lst);
#define omg_auto_repo_list                                                     \
  omg_repo_list __attribute__((cleanup(omg_free_repo_list)))

// Created repos
omg_error omg_sync_created_repos(omg_context ctx);
omg_error omg_query_created_repos(omg_context ctx, const char *keyword,
                                  const char *language, omg_repo_list *out);

// Starred repos
typedef struct {
  char *starred_at;
  omg_repo repo;
} omg_starred_repo;

void omg_free_starred_repo(omg_starred_repo *star);
#define omg_auto_starred_repo                                                  \
  omg_starred_repo __attribute__((cleanup(omg_free_starred_repo)))

typedef struct {
  omg_starred_repo *star_array;
  size_t length;
} omg_starred_repo_list;

void omg_free_starred_repo_list(omg_starred_repo_list *);
#define omg_auto_starred_repo_list                                             \
  omg_starred_repo_list __attribute__((cleanup(omg_free_starred_repo_list)))

omg_error omg_sync_starred_repos(omg_context ctx);
omg_error omg_query_starred_repos(omg_context ctx, const char *keyword,
                                  const char *language,
                                  omg_starred_repo_list *out);
omg_error omg_unstar_repo(omg_context ctx, size_t repo_id);

// Users
typedef struct omg_user {
  char *login;
  int id;
  char *name;
  char *company;
  char *blog;
  char *location;
  char *email;
  bool hireable;
  char *bio;
  char *twitter_username;
  int public_repos;
  int public_gists;
  int private_repos;
  int private_gists;
  int followers;
  int following;
  char *created_at;
  int disk_usage;
} omg_user;

void omg_free_user(omg_user *);
#define omg_auto_user omg_user __attribute__((cleanup(omg_free_user)))

omg_error omg_whoami(omg_context ctx, const char *username, omg_user *);

// Commits
typedef struct {
  const char *sha;
  const char *message;
  const char *author;
  const char *email;
  const char *date;
} omg_commit;

void omg_free_commit(omg_commit *commit);
#define omg_auto_repo_commit                                                   \
  omg_repo_commit __attribute__((cleanup(omg_free_commit)))

typedef struct {
  omg_commit *commit_array;
  size_t length;
} omg_commit_list;

void omg_free_commit_list(omg_commit_list *);
#define omg_auto_commit_list                                                   \
  omg_commit_list __attribute__((cleanup(omg_free_commit_list)))

omg_error omg_query_commits(omg_context, const char *full_name, int limit,
                            omg_commit_list *);

// Assets
typedef struct {
  int id;
  char *name;
  int size;
  int download_count;
  char *download_url;
} omg_release_asset;

void omg_free_release_asset(omg_release_asset *release_asset);
#define omg_auto_repo_release_asset                                            \
  omg_repo_release_asset __attribute__((cleanup(omg_free_release_asset)))

// Releases
typedef struct {
  int id;
  char *name;
  char *login;
  char *tag_name;
  char *body;
  bool draft;
  bool prerelease;
  char *published_at;
  omg_release_asset *asset_array;
  int asset_length;
} omg_release;

void omg_free_release(omg_release *release);
#define omg_auto_repo_release                                                  \
  omg_repo_release __attribute__((cleanup(omg_free_release)))

typedef struct {
  omg_release *release_array;
  size_t length;
} omg_release_list;

void omg_free_release_list(omg_release_list *);
#define omg_auto_release_list                                                  \
  omg_release_list __attribute__((cleanup(omg_free_release_list)))

omg_error omg_query_releases(omg_context, const char *full_name, int limit,
                             omg_release_list *);

// Trending
omg_error omg_query_trending(omg_context, const char *spoken_lang,
                             const char *lang, const char *since,
                             omg_repo_list *);

// Gists

typedef struct {
  char *filename;
  char *language;
  char *raw_url;
  size_t size;
} omg_gist_file;

typedef struct {
  char *id;
  char *created_at;
  char *description;
  bool public;
  char *_files_as_json; // private, use file instead
  omg_gist_file file;
} omg_gist;

void omg_free_gist(omg_gist *);
#define omg_auto_gist omg_gist __attribute__((cleanup(omg_free_gist)))

typedef struct {
  omg_gist *gist_array;
  size_t length;
} omg_gist_list;

void omg_free_gist_list(omg_gist_list *);
#define omg_auto_gist_list                                                     \
  omg_gist_list __attribute__((cleanup(omg_free_gist_list)))

omg_error omg_sync_created_gists(omg_context ctx);
omg_error omg_sync_starred_gists(omg_context ctx);
omg_error omg_query_created_gists(omg_context ctx, omg_gist_list *out);
omg_error omg_query_starred_gists(omg_context ctx, omg_gist_list *out);
omg_error omg_delete_gist(omg_context ctx, char *gist_id);
omg_error omg_unstar_gist(omg_context ctx, char *gist_id);

// Pull requests
typedef struct {
  int32_t number;
  int32_t commits;
  int32_t additions;
  int32_t deletions;
} omg_pull;

omg_error omg_create_pull(omg_context ctx, const char *full_name,
                          const char *title, const char *body, const char *head,
                          const char *base, bool draft, omg_pull *out);

// open / close
omg_error omg_toggle_pull(omg_context ctx, const char *full_name,
                          int32_t pull_number, bool close);

// Utils
omg_error omg_download(omg_context ctx, const char *url, const char *filename);

#endif
