#include "../core/omg.h"
#include "emacs-module.h"
#include <pthread.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int plugin_is_GPL_compatible;

typedef struct emacs_runtime *runtime;

emacs_value Qt;
emacs_value Qnil;
/* Core interface used for invoking C API */
omg_context ctx = NULL;
const char *FEATURE_NAME = "omg-dyn";
// Use in multiple threads environment
// 1 means sync is in progress, no other functions can be invoked
static atomic_int IS_SYNC;

#define lisp_symbol(env, symbol)                                               \
  ({                                                                           \
    emacs_env *_env_ = env;                                                    \
    _env_->intern(_env_, (symbol));                                            \
  })

#define lisp_integer(env, integer)                                             \
  ({                                                                           \
    emacs_env *_env_ = env;                                                    \
    _env_->make_integer(_env_, (integer));                                     \
  })

#define lisp_string(env, string)                                               \
  ({                                                                           \
    emacs_env *_env_ = env;                                                    \
    char *_str_ = string;                                                      \
    _str_ ? _env_->make_string(_env_, _str_, strlen(_str_))                    \
          : _env_->make_string(_env_, "", 0);                                  \
  })

#define lisp_funcall(env, fn_name, ...)                                        \
  ({                                                                           \
    emacs_env *_env_ = env;                                                    \
    emacs_value _args_[] = {__VA_ARGS__};                                      \
    int _nargs_ = sizeof(_args_) / sizeof(emacs_value);                        \
    _env_->funcall(_env_, env->intern(env, (fn_name)), _nargs_, _args_);       \
  })

#define lisp_text_button(env, label, ...)                                      \
  ({                                                                           \
    emacs_env *_env_ = env;                                                    \
    lisp_funcall(env, "cons", label,                                           \
                 lisp_funcall(env, "list", lisp_symbol(env, "face"), Qnil,     \
                              __VA_ARGS__));                                   \
  })

#define lisp_integer(env, integer)                                             \
  ({                                                                           \
    emacs_env *_env_ = env;                                                    \
    _env_->make_integer(_env_, (integer));                                     \
  })

#define ENSURE_SETUP(env)                                                      \
  do {                                                                         \
    if (ctx == NULL) {                                                         \
      return lisp_funcall(env, "error",                                        \
                          lisp_string(env, "omg-dyn not setup!"));             \
    }                                                                          \
    if (IS_SYNC) {                                                             \
      return lisp_funcall(                                                     \
          env, "error",                                                        \
          lisp_string(env, "Sync is in progress. Wait a moment..."));          \
    }                                                                          \
  } while (0)

#define ENSURE_NONLOCAL_EXIT(env)                                              \
  do {                                                                         \
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {         \
      return Qnil;                                                             \
    }                                                                          \
  } while (0)

emacs_value eomg_sync_star(emacs_env *env, ptrdiff_t _nargs, emacs_value *_args,
                           void *data) {
  ENSURE_SETUP(env);
  omg_error err = omg_sync_stars(ctx);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }

  ENSURE_NONLOCAL_EXIT(env);

  err = omg_sync_repos(ctx);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }

  ENSURE_NONLOCAL_EXIT(env);

  return Qt;
}

static char *get_string(emacs_env *env, emacs_value symbol) {
  if (env->is_not_nil(env, symbol)) {
    ptrdiff_t len;
    env->copy_string_contents(env, symbol, NULL, &len); // get len
    char *buf = malloc(len);
    env->copy_string_contents(env, symbol, buf, &len); // get data
    return buf;
  }

  return NULL;
}

static char *string_or_empty(const char *s) {
  if (s) {
    return (char *)s;
  }
  return "";
}

static char *human_size(int size) {
  static char *units[] = {"B", "KB", "MB", "GB"};
  static size_t unit_len = 4;
  int order = 0;
  double d = (double)size;
  while (d >= 1024 && order < unit_len) {
    order++;
    d = d / 1024;
  }
  char *buf = malloc(16);
  sprintf(buf, "%.1f%s", d, units[order]);
  return buf;
}

static emacs_value
omg_dyn_query_common(emacs_env *env, const char *first_column, omg_repo repo) {
  char repo_id[64];
  sprintf(repo_id, "%d", repo.id);
  char watchers_count[8];
  sprintf(watchers_count, "%d", repo.watchers_count);
  char stars_count[8];
  sprintf(stars_count, "%d", repo.stargazers_count);
  char forks_count[8];
  sprintf(forks_count, "%d", repo.forks_count);
  char size_str[8];
  sprintf(size_str, "%d", repo.size);
  omg_auto_char readable_size = human_size(repo.size);
  emacs_value size_button =
      lisp_funcall(env, "cons", lisp_string(env, readable_size),
                   (lisp_funcall(env, "list",
                                 // display as plain text
                                 lisp_symbol(env, "face"), Qnil,
                                 // echo full digits
                                 lisp_symbol(env, "help-echo"),
                                 lisp_string(env, size_str), )));

  emacs_value row = lisp_funcall(
      env, "list", lisp_string(env, repo_id),
      lisp_funcall(env, "vector", lisp_string(env, (char *)first_column),
                   lisp_string(env, string_or_empty(repo.full_name)),
                   lisp_string(env, string_or_empty(repo.lang)),
                   lisp_string(env, stars_count), lisp_string(env, forks_count),
                   lisp_string(env, string_or_empty(repo.license)), size_button,
                   lisp_string(env, string_or_empty(repo.description)), ));
  return row;
}

emacs_value omg_dyn_query_repos(emacs_env *env, ptrdiff_t nargs,
                                emacs_value *args, void *data) {
  ENSURE_SETUP(env);
  omg_auto_char keyword = NULL;
  omg_auto_char lang = NULL;
  if (nargs > 0) {
    keyword = get_string(env, args[0]);
    if (nargs > 1) {
      lang = get_string(env, args[1]);
    }
  }

  ENSURE_NONLOCAL_EXIT(env);
  omg_auto_repo_list repo_lst = omg_new_repo_list();
  omg_error err = omg_query_repos(ctx, keyword, lang, &repo_lst);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }
  emacs_value repo_vector = lisp_funcall(
      env, "make-vector", lisp_integer(env, repo_lst.length), Qnil);
  for (int i = 0; i < repo_lst.length; i++) {
    omg_repo repo = repo_lst.repo_array[i];
    emacs_value row = omg_dyn_query_common(env, repo.created_at, repo);
    lisp_funcall(env, "aset", repo_vector, lisp_integer(env, i), row);
  }

  return repo_vector;
}

emacs_value omg_dyn_query_stars(emacs_env *env, ptrdiff_t nargs,
                                emacs_value *args, void *data) {
  ENSURE_SETUP(env);
  omg_auto_char keyword = NULL;
  omg_auto_char lang = NULL;
  if (nargs > 0) {
    keyword = get_string(env, args[0]);
    if (nargs > 1) {
      lang = get_string(env, args[1]);
    }
  }

  ENSURE_NONLOCAL_EXIT(env);

  omg_auto_star_list star_lst = omg_new_star_list();
  omg_error err = omg_query_stars(ctx, keyword, lang, &star_lst);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }

  ENSURE_NONLOCAL_EXIT(env);

  emacs_value star_vector = lisp_funcall(
      env, "make-vector", lisp_integer(env, star_lst.length), Qnil);
  for (int i = 0; i < star_lst.length; i++) {
    omg_repo repo = star_lst.star_array[i].repo;
    emacs_value row =
        omg_dyn_query_common(env, star_lst.star_array[i].starred_at, repo);
    lisp_funcall(env, "aset", star_vector, lisp_integer(env, i), row);
  }

  return star_vector;
}

emacs_value omg_dyn_unstar(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                           void *data) {
  ENSURE_SETUP(env);
  intmax_t repo_id = env->extract_integer(env, args[0]);

  ENSURE_NONLOCAL_EXIT(env);

  omg_error err = omg_unstar(ctx, repo_id);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }

  return Qt;
}

static void *omg_dyn_sync_background(void *ptr) {
  int pipe = *(int *)ptr;
  char *msg = "Start syncing, wait a few seconds...\n";
  write(pipe, msg, strlen(msg));

  omg_error err = omg_sync_stars(ctx);
  if (!is_ok(err)) {
    write(pipe, err.message, strlen(err.message));
  } else {
    msg = "Starred repositories sync finished!\n";
    write(pipe, msg, strlen(msg));
  }

  err = omg_sync_repos(ctx);
  if (!is_ok(err)) {
    write(pipe, err.message, strlen(err.message));
  } else {
    msg = "Owned repositories sync finished!\n";
    write(pipe, msg, strlen(msg));
  }

  msg = "All sync finished, you can safely close this buffer now!\n";
  write(pipe, msg, strlen(msg));

  IS_SYNC = 0;
  close(pipe);
  return NULL;
}

emacs_value omg_dyn_sync(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                         void *data) {
  ENSURE_SETUP(env);
  IS_SYNC = 1;

  emacs_value pipe = args[0];
  pthread_t id;
  int fd = env->open_channel(env, pipe);

  ENSURE_NONLOCAL_EXIT(env);

  int rc = pthread_create(&id, NULL, omg_dyn_sync_background, &fd);
  if (rc) {
    IS_SYNC = 0;
    close(fd);
    return lisp_funcall(env, "error",
                        lisp_string(env, "create sync thread failed"));
  }

  return Qt;
}

emacs_value omg_dyn_whoami(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                           void *data) {
  ENSURE_SETUP(env);
  omg_auto_char username = NULL;
  if (nargs > 0 && env->is_not_nil(env, args[0])) {
    username = get_string(env, args[0]);
  }

  ENSURE_NONLOCAL_EXIT(env);

  omg_auto_user who = omg_new_user();
  omg_error err = omg_whoami(ctx, username, &who);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }

  ENSURE_NONLOCAL_EXIT(env);

  return lisp_funcall(
      env, "list",                                                           //
      lisp_symbol(env, "login"), lisp_string(env, who.login),                //
      lisp_symbol(env, "id"), lisp_integer(env, who.id),                     //
      lisp_symbol(env, "name"), lisp_string(env, who.name),                  //
      lisp_symbol(env, "company"), lisp_string(env, who.company),            //
      lisp_symbol(env, "blog"), lisp_string(env, who.blog),                  //
      lisp_symbol(env, "location"), lisp_string(env, who.location),          //
      lisp_symbol(env, "email"), lisp_string(env, who.email),                //
      lisp_symbol(env, "hireable"), lisp_integer(env, who.hireable),         //
      lisp_symbol(env, "public-repos"), lisp_integer(env, who.public_repos), //
      lisp_symbol(env, "public-gists"), lisp_integer(env, who.public_gists), //
      lisp_symbol(env, "private-repos"),
      lisp_integer(env, who.private_repos), //
      lisp_symbol(env, "private-gists"),
      lisp_integer(env, who.private_gists),                              //
      lisp_symbol(env, "followers"), lisp_integer(env, who.followers),   //
      lisp_symbol(env, "following"), lisp_integer(env, who.following),   //
      lisp_symbol(env, "created-at"), lisp_string(env, who.created_at),  //
      lisp_symbol(env, "disk-usage"), lisp_integer(env, who.disk_usage), //
  );
}

// split string at first newline, and return first part
// if no newline, return entire input
static char *split_newline(const char *input) {
  size_t sep = 0;
  while (input[sep] != '\n' && input[sep] != '\0') {
    sep++;
  }
  char *buf = malloc(sep + 1);
  memcpy(buf, input, sep);
  buf[sep] = '\0';

  return buf;
}

static emacs_value localtime_format(emacs_env *env, emacs_value iso8601) {
  return lisp_funcall(env, "format-time-string",
                      lisp_string(env, "%Y-%m-%d %T"),
                      lisp_funcall(env, "parse-iso8601-time-string", iso8601));
}

static char *shorten_sha(const char *sha) {
  static size_t sha_len = 7;
  char *buf = malloc(sha_len + 1);
  memcpy(buf, sha, sha_len);
  buf[sha_len] = '\0';
  return buf;
}

emacs_value omg_dyn_query_commits(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value *args, void *data) {
  ENSURE_SETUP(env);
  omg_auto_char full_name = get_string(env, args[0]);
  int limit = env->extract_integer(env, args[1]);
  ENSURE_NONLOCAL_EXIT(env);

  omg_auto_commit_list commit_lst = omg_new_commit_list();
  omg_error err = omg_query_commits(ctx, full_name, limit, &commit_lst);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }

  ENSURE_NONLOCAL_EXIT(env);

  emacs_value commit_vector = lisp_funcall(
      env, "make-vector", lisp_integer(env, commit_lst.length), Qnil);
  for (int i = 0; i < commit_lst.length; i++) {
    omg_commit commit = commit_lst.commit_array[i];

    char id[3];
    sprintf(id, "%d", i);

    omg_auto_char short_sha = shorten_sha(commit.sha);

    omg_auto_char short_msg = split_newline(commit.message);
    emacs_value msg_button =
        lisp_text_button(env, lisp_string(env, short_msg),
                         // props
                         lisp_symbol(env, "help-echo"),
                         lisp_string(env, (char *)commit.message));

    emacs_value row = lisp_funcall(
        env, "list", lisp_string(env, id),
        lisp_funcall(
            env, "vector", lisp_string(env, short_sha), msg_button,
            lisp_string(env, (char *)commit.author),
            localtime_format(env, lisp_string(env, (char *)commit.date)), ));
    lisp_funcall(env, "aset", commit_vector, lisp_integer(env, i), row);
  }

  return commit_vector;
}

emacs_value omg_dyn_query_releases(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value *args, void *data) {
  ENSURE_SETUP(env);
  omg_auto_char full_name = get_string(env, args[0]);
  int limit = env->extract_integer(env, args[1]);
  ENSURE_NONLOCAL_EXIT(env);

  omg_auto_release_list release_lst = omg_new_release_list();
  omg_error err = omg_query_releases(ctx, full_name, limit, &release_lst);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }

  ENSURE_NONLOCAL_EXIT(env);

  emacs_value release_vector = lisp_funcall(
      env, "make-vector", lisp_integer(env, release_lst.length), Qnil);
  for (int i = 0; i < release_lst.length; i++) {
    omg_release release = release_lst.release_array[i];

    emacs_value asset_vector = lisp_funcall(
        env, "make-vector", lisp_integer(env, release.asset_length), Qnil);
    for (int j = 0; j < release.asset_length; j++) {
      omg_release_asset asset = release.asset_array[j];
      omg_auto_char readable_size = human_size(asset.size);
      emacs_value row = lisp_funcall(
          env, "list", lisp_string(env, asset.name),
          lisp_funcall(
              env, "vector",
              lisp_text_button(env, lisp_string(env, asset.name),
                               lisp_symbol(env, "raw-url"),
                               lisp_string(env, asset.download_url)),
              lisp_string(env, readable_size),
              lisp_funcall(env, "number-to-string",
                           lisp_integer(env, asset.download_count)), ));
      lisp_funcall(env, "aset", asset_vector, lisp_integer(env, j), row);
    }

    char *name = release.name;
    if (!name || 0 == strcmp(name, "")) {
      name = release.tag_name;
    }
    emacs_value name_button = lisp_text_button(
        env, lisp_string(env, name),
        // props
        lisp_symbol(env, "help-echo"), lisp_string(env, (char *)release.body),
        // len
        lisp_symbol(env, "asset-length"),
        lisp_integer(env, release.asset_length),
        // files
        lisp_symbol(env, "asset-files"), asset_vector, );

    char id[10];
    sprintf(id, "%d", release.id);
    char *draft = release.draft ? "true" : "false";
    char *prerelease = release.prerelease ? "true" : "false";

    emacs_value row = lisp_funcall(
        env, "list", lisp_string(env, id),
        lisp_funcall(env, "vector",
                     localtime_format(
                         env, lisp_string(env, (char *)release.published_at)),
                     name_button, lisp_string(env, (char *)release.login),
                     lisp_string(env, (char *)release.tag_name),
                     lisp_string(env, draft), lisp_string(env, prerelease)));
    lisp_funcall(env, "aset", release_vector, lisp_integer(env, i), row);
  }

  return release_vector;
}

emacs_value omg_dyn_download(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                             void *data) {
  ENSURE_SETUP(env);
  omg_auto_char url = get_string(env, args[0]);
  omg_auto_char filename = get_string(env, args[1]);
  ENSURE_NONLOCAL_EXIT(env);

  omg_error err = omg_download(ctx, url, filename);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }
  return Qt;
}

emacs_value omg_dyn_setup(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                          void *data) {
  if (ctx) {
    return Qt;
  }

  omg_auto_char db_path = get_string(env, args[0]);
  omg_auto_char github_token = get_string(env, args[1]);
#ifdef VERBOSE
  printf("path:%s, token:%s\n", db_path, github_token);
#endif

  ENSURE_NONLOCAL_EXIT(env);

  omg_error err = omg_setup_context(db_path, github_token, &ctx);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }

  return Qt;
}

emacs_value omg_dyn_teardown(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                             void *data) {
  if (ctx) {
    omg_free_context(&ctx);
    ctx = NULL;
    env->free_global_ref(env, Qt);
    env->free_global_ref(env, Qnil);
    return lisp_symbol(env, "t");
  }

  return lisp_symbol(env, "nil");
}

int emacs_module_init(runtime ert) {
  emacs_env *env = ert->get_environment(ert);

  // global emacs values
  Qt = env->make_global_ref(env, lisp_symbol(env, "t"));
  Qnil = env->make_global_ref(env, lisp_symbol(env, "nil"));

  // export functions
  lisp_funcall(
      env, "fset", lisp_symbol(env, "omg-dyn-setup"),
      env->make_function(env, 2, 2, omg_dyn_setup, "Initialize omg-dyn", NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "omg-dyn-teardown"),
               env->make_function(env, 0, 0, omg_dyn_teardown,
                                  "Teardown omg-dyn", NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "omg-dyn-sync"),
               env->make_function(env, 1, 1, omg_dyn_sync,
                                  "Sync Github repositories to local database",
                                  NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "omg-dyn-whoami"),
               env->make_function(
                   env, 0, 1, omg_dyn_whoami,
                   "Return user represented by GitHub personal access token",
                   NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "omg-dyn-query-stars"),
               env->make_function(
                   env, 0, 2, omg_dyn_query_stars,
                   "Query GitHub stars based on keyword or language", NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "omg-dyn-query-repos"),
               env->make_function(
                   env, 0, 2, omg_dyn_query_repos,
                   "Query GitHub repos based on keyword or language", NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "omg-dyn-unstar"),
               env->make_function(env, 1, 1, omg_dyn_unstar,
                                  "Delete GitHub star", NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "omg-dyn-query-commits"),
               env->make_function(env, 2, 2, omg_dyn_query_commits,
                                  "Query commits of a repository", NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "omg-dyn-query-releases"),
               env->make_function(env, 2, 2, omg_dyn_query_releases,
                                  "Query releases of a repository", NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "omg-dyn-download"),
               env->make_function(env, 2, 2, omg_dyn_download,
                                  "Download file given a asset raw-url", NULL));

  lisp_funcall(env, "provide", lisp_symbol(env, FEATURE_NAME));

  return 0;
}
