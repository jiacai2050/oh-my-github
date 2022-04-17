CREATE TABLE IF NOT EXISTS ghs_repo (
    id integer PRIMARY KEY,
    full_name text NOT NULL,
    description text NULL,
    private integer,
    created_at text NOT NULL,
    updated_at text NOT NULL,
    pushed_at text NOT NULL,
    stargazers_count integer,
    watchers_count integer,
    forks integer,
    lang text,
    homepage text
);

CREATE INDEX IF NOT EXISTS idx_full_name ON ghs_repo (full_name);
CREATE INDEX IF NOT EXISTS idx_updated ON ghs_repo (updated_at);
CREATE INDEX IF NOT EXISTS idx_lang ON ghs_repo (lang);

CREATE TABLE IF NOT EXISTS ghs_star (
    id integer PRIMARY KEY AUTOINCREMENT,
    starred_at text NOT NULL,
    repo_id integer,
    UNIQUE (repo_id)
);

CREATE INDEX IF NOT EXISTS idx_starred ON ghs_star (starred_at);

CREATE VIEW IF NOT EXISTS ghs_star_view AS
SELECT
    gs.starred_at,
    gr.*
FROM
    ghs_repo gr,
    ghs_star gs ON gr.id = gs.repo_id;
