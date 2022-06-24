CREATE TABLE IF NOT EXISTS omg_repo (
    id integer PRIMARY KEY,
    full_name text NOT NULL,
    description text NULL,
    private integer,
    created_at text NOT NULL,
    license text,
    pushed_at text NOT NULL,
    stargazers_count integer,
    watchers_count integer,
    forks_count integer,
    lang text,
    homepage text,
    `size` integer
);

CREATE INDEX IF NOT EXISTS idx_pushed_at ON omg_repo (pushed_at);
CREATE INDEX IF NOT EXISTS idx_lang ON omg_repo (lang);

CREATE TABLE IF NOT EXISTS omg_starred_repo (
    id integer PRIMARY KEY AUTOINCREMENT,
    starred_at text NOT NULL,
    repo_id integer,
    UNIQUE (repo_id)
);

CREATE TABLE IF NOT EXISTS omg_created_repo (
    id integer PRIMARY KEY AUTOINCREMENT,
    repo_id integer,
    UNIQUE (repo_id)
);

CREATE INDEX IF NOT EXISTS idx_starred ON omg_starred_repo (starred_at);

CREATE VIEW IF NOT EXISTS omg_starred_repo_view AS
SELECT
    ms.starred_at,
    repo.*
FROM
    omg_repo repo,
    omg_starred_repo ms ON repo.id = ms.repo_id;

CREATE VIEW IF NOT EXISTS omg_created_repo_view AS
SELECT
    repo.*
FROM
    omg_repo repo,
    omg_created_repo ON repo.id = omg_created_repo.repo_id;

CREATE TABLE IF NOT EXISTS omg_gist (
    id text PRIMARY KEY,
    created_at text NOT NULL,
    description text,
    files text,
    `public` integer
);

CREATE INDEX IF NOT EXISTS idx_created_at ON omg_gist (created_at);

CREATE TABLE IF NOT EXISTS omg_created_gist (
    id integer PRIMARY KEY AUTOINCREMENT,
    gist_id integer,
    UNIQUE (gist_id)
);

CREATE TABLE IF NOT EXISTS omg_starred_gist (
    id integer PRIMARY KEY AUTOINCREMENT,
    gist_id integer,
    UNIQUE (gist_id)
);

CREATE VIEW IF NOT EXISTS omg_created_gist_view AS
SELECT
    gist.*
FROM
    omg_gist gist,
    omg_created_gist ON gist.id = omg_created_gist.gist_id;

CREATE VIEW IF NOT EXISTS omg_starred_gist_view AS
SELECT
    gist.*
FROM
    omg_gist gist,
    omg_starred_gist ON gist.id = omg_starred_gist.gist_id;
