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

CREATE TABLE IF NOT EXISTS omg_my_star (
    id integer PRIMARY KEY AUTOINCREMENT,
    starred_at text NOT NULL,
    repo_id integer,
    UNIQUE (repo_id)
);

CREATE TABLE IF NOT EXISTS omg_my_repo (
    id integer PRIMARY KEY AUTOINCREMENT,
    repo_id integer,
    UNIQUE (repo_id)
);

CREATE INDEX IF NOT EXISTS idx_starred ON omg_my_star (starred_at);

CREATE VIEW IF NOT EXISTS omg_my_star_view AS
SELECT
    ms.starred_at,
    repo.*
FROM
    omg_repo repo,
    omg_my_star ms ON repo.id = ms.repo_id;

CREATE VIEW IF NOT EXISTS omg_my_repo_view AS
SELECT
    repo.*
FROM
    omg_repo repo,
    omg_my_repo ON repo.id = omg_my_repo.repo_id;
