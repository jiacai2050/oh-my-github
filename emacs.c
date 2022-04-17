#include "emacs-module.h"
#include "ghs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int plugin_is_GPL_compatible;

typedef struct emacs_runtime *runtime;

emacs_value Qt;
emacs_value Qnil;
/* Core interface used for invoking C API */
ghs_context ctx = NULL;
const char *FEATURE_NAME = "ghs-dyn";

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
    _env_->make_string(_env_, _str_, strlen(_str_));                           \
  })

#define lisp_funcall(env, fn_name, ...)                                        \
  ({                                                                           \
    emacs_env *_env_ = env;                                                    \
    emacs_value _args_[] = {__VA_ARGS__};                                      \
    int _nargs_ = sizeof(_args_) / sizeof(emacs_value);                        \
    _env_->funcall(_env_, env->intern(env, (fn_name)), _nargs_, _args_);       \
  })

#define ENSURE_SETUP(env)                                                      \
  do {                                                                         \
    if (ctx == NULL) {                                                         \
      return lisp_funcall(env, "error",                                        \
                          lisp_string(env, "ghs-dyn not setup!"));             \
    }                                                                          \
  } while (0)

#define ENSURE_NONLOCAL_EXIT(env)                                              \
  do {                                                                         \
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {         \
      return Qnil;                                                             \
    }                                                                          \
  } while (0)

emacs_value eghs_sync_star(emacs_env *env, ptrdiff_t _nargs, emacs_value *_args,
                           void *data) {
  ENSURE_SETUP(env);
  ghs_error err = ghs_sync_stars(ctx);
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

emacs_value ghs_dyn_query_stars(emacs_env *env, ptrdiff_t nargs,
                                emacs_value *args, void *data) {
  ENSURE_SETUP(env);
  printf("exit code %d\n", 123);
  ghs_star_list star_lst;
  char *keyword = NULL;
  char *lang = NULL;
  if (nargs > 0) {
    keyword = get_string(env, args[0]);
    if (nargs > 1) {
      lang = get_string(env, args[1]);
    }
  }
  printf("exit code %d\n", env->non_local_exit_check(env));

  ENSURE_NONLOCAL_EXIT(env);

  ghs_error err = ghs_query_stars(ctx, keyword, lang, &star_lst);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }

  ENSURE_NONLOCAL_EXIT(env);

  emacs_value star_vector = lisp_funcall(
      env, "make-vector", lisp_integer(env, star_lst.length), Qnil);
  for (int i = 0; i < star_lst.length; i++) {
    ghs_star s = star_lst.star_array[i];
    char repo_id[64];
    sprintf(repo_id, "%d", s.repo.id);
    char watchers_count[8];
    sprintf(watchers_count, "%d", s.repo.watchers_count);
    char star_count[8];
    sprintf(star_count, "%d", s.repo.stargazers_count);
    char forks[8];
    sprintf(forks, "%d", s.repo.forks);

    emacs_value row = lisp_funcall(
        env, "list", lisp_string(env, repo_id),
        lisp_funcall(env, "vector", lisp_string(env, s.starred_at),
                     lisp_string(env, string_or_empty(s.repo.full_name)),
                     lisp_string(env, string_or_empty(s.repo.lang)),
                     lisp_string(env, star_count),
                     /* lisp_string(env, watchers_count), */
                     lisp_string(env, forks),
                     lisp_string(env, string_or_empty(s.repo.description)), ));

    lisp_funcall(env, "aset", star_vector, lisp_integer(env, i), row);
  }

  return star_vector;
}

emacs_value ghs_dyn_unstar(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                           void *data) {
  ENSURE_SETUP(env);
  intmax_t repo_id = env->extract_integer(env, args[0]);

  ENSURE_NONLOCAL_EXIT(env);

  ghs_error err = ghs_unstar(ctx, repo_id);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }

  return Qt;
}

emacs_value ghs_dyn_sync(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                         void *data) {
  ENSURE_SETUP(env);
  ghs_error err = ghs_sync_stars(ctx);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }

  return Qt;
}

emacs_value ghs_dyn_setup(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                          void *data) {
  if (ctx) {
    return Qt;
  }

  ghs_auto_char db_path = get_string(env, args[0]);
  ghs_auto_char github_token = get_string(env, args[1]);
#ifdef VERBOSE
  printf("path:%s, token:%s\n", db_path, github_token);
#endif

  ENSURE_NONLOCAL_EXIT(env);

  ghs_error err = ghs_setup_context(db_path, github_token, &ctx);
  if (!is_ok(err)) {
    return lisp_funcall(env, "error", lisp_string(env, (char *)err.message));
  }

  return Qt;
}

emacs_value ghs_dyn_teardown(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                             void *data) {
  if (ctx) {
    ghs_free_context(&ctx);
    ctx = NULL;
    return Qt;
  }

  return Qnil;
}

int emacs_module_init(runtime ert) {
  emacs_env *env = ert->get_environment(ert);

  // global emacs values
  Qt = env->make_global_ref(env, lisp_symbol(env, "t"));
  Qnil = env->make_global_ref(env, lisp_symbol(env, "nil"));

  // export functions
  lisp_funcall(
      env, "fset", lisp_symbol(env, "ghs-dyn-setup"),
      env->make_function(env, 2, 2, ghs_dyn_setup, "Initialize ghs-dyn", NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "ghs-dyn-teardown"),
               env->make_function(env, 0, 0, ghs_dyn_teardown,
                                  "Teardown ghs-dyn", NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "ghs-dyn-sync"),
               env->make_function(env, 0, 0, ghs_dyn_sync,
                                  "Sync Github stars to local database", NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "ghs-dyn-query"),
               env->make_function(
                   env, 0, 2, ghs_dyn_query_stars,
                   "Query GitHub stars based on keyword or language", NULL));

  lisp_funcall(env, "fset", lisp_symbol(env, "ghs-dyn-unstar"),
               env->make_function(env, 1, 1, ghs_dyn_unstar,
                                  "Delete GitHub star", NULL));

  lisp_funcall(env, "provide", lisp_symbol(env, FEATURE_NAME));

  return 0;
}
