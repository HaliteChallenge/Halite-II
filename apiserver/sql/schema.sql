CREATE TABLE alembic_version (
    version_num VARCHAR(32) NOT NULL, 
    CONSTRAINT alembic_version_pkc PRIMARY KEY (version_num)
);

-- Running upgrade  -> 7f0054256cf5

CREATE TABLE badge (
    id MEDIUMINT(8) UNSIGNED NOT NULL, 
    name VARCHAR(256) NOT NULL, 
    PRIMARY KEY (id)
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE UNIQUE INDEX name ON badge (name);

CREATE TABLE game (
    id MEDIUMINT(8) UNSIGNED NOT NULL AUTO_INCREMENT, 
    replay_name VARCHAR(128) NOT NULL, 
    map_width SMALLINT(5) NOT NULL, 
    map_height SMALLINT(5) NOT NULL, 
    map_seed INTEGER(10) UNSIGNED NOT NULL, 
    map_generator VARCHAR(128) NOT NULL, 
    time_played DATETIME DEFAULT CURRENT_TIMESTAMP, 
    replay_bucket SMALLINT(5) NOT NULL DEFAULT '0', 
    PRIMARY KEY (id)
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE INDEX game_time_played ON game (time_played);

CREATE TABLE halite_1_user (
    `userID` MEDIUMINT(8) UNSIGNED NOT NULL AUTO_INCREMENT, 
    `oauthID` INTEGER(12) UNSIGNED NOT NULL, 
    `oauthProvider` TINYINT(1) UNSIGNED NOT NULL, 
    username VARCHAR(32) NOT NULL, 
    email VARCHAR(64), 
    `isRunning` TINYINT(1) UNSIGNED NOT NULL DEFAULT '0', 
    `compileStatus` TINYINT(1) UNSIGNED NOT NULL DEFAULT '0', 
    organization VARCHAR(64) NOT NULL, 
    language VARCHAR(16), 
    mu FLOAT NOT NULL DEFAULT '25', 
    sigma FLOAT UNSIGNED NOT NULL DEFAULT '8.333', 
    rank SMALLINT(5), 
    `numSubmissions` SMALLINT(5) NOT NULL DEFAULT '0', 
    `numGames` SMALLINT(5) NOT NULL DEFAULT '0', 
    `creationTime` DATETIME DEFAULT CURRENT_TIMESTAMP, 
    `updateTime` DATETIME DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP, 
    `onEmailList` TINYINT(1) NOT NULL DEFAULT '1', 
    `githubEmail` VARCHAR(64), 
    `verificationCode` VARCHAR(64), 
    `isEmailGood` TINYINT(1) UNSIGNED NOT NULL DEFAULT '0', 
    `is_gpu_enabled` tinyint(1) NOT NULL DEFAULT '0',
    level ENUM('High School','Undergraduate','Graduate','Professional') NOT NULL DEFAULT 'Professional', 
    PRIMARY KEY (`userID`)
)DEFAULT CHARSET=latin1 AUTO_INCREMENT=5350 ENGINE=InnoDB;

CREATE TABLE organization (
    id INTEGER(11) NOT NULL AUTO_INCREMENT, 
    organization_name VARCHAR(64) NOT NULL, 
    kind ENUM('High School','University','Professional School','Company','Other') NOT NULL DEFAULT 'Other', 
    verification_code VARCHAR(32), 
    PRIMARY KEY (id)
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE UNIQUE INDEX verification_code ON organization (verification_code);

CREATE TABLE game_stat (
    game_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    turns_total INTEGER(10) UNSIGNED NOT NULL, 
    planets_destroyed INTEGER(10) UNSIGNED NOT NULL, 
    ships_produced INTEGER(10) UNSIGNED NOT NULL, 
    ships_destroyed INTEGER(10) UNSIGNED NOT NULL, 
    PRIMARY KEY (game_id), 
    CONSTRAINT game_stat_ibfk_1 FOREIGN KEY(game_id) REFERENCES game (id) ON DELETE CASCADE
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE TABLE game_view_stat (
    game_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    views_total INTEGER(10) UNSIGNED NOT NULL, 
    PRIMARY KEY (game_id), 
    CONSTRAINT game_view_stat_ibfk_1 FOREIGN KEY(game_id) REFERENCES game (id) ON DELETE CASCADE
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE TABLE hackathon (
    id INTEGER(10) UNSIGNED NOT NULL AUTO_INCREMENT, 
    title VARCHAR(256) NOT NULL, 
    description VARCHAR(4096) NOT NULL, 
    start_date DATETIME NOT NULL, 
    end_date DATETIME NOT NULL, 
    verification_code VARCHAR(32) NOT NULL, 
    organization_id INTEGER(11), 
    location VARCHAR(256), 
    thumbnail VARCHAR(512), 
    is_open TINYINT(4) NOT NULL DEFAULT '0', 
    PRIMARY KEY (id), 
    CONSTRAINT hackathon_ibfk_1 FOREIGN KEY(organization_id) REFERENCES organization (id)
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE INDEX organization_id ON hackathon (organization_id);

CREATE UNIQUE INDEX verification_code ON hackathon (verification_code);

CREATE TABLE organization_email_domain (
    organization_id INTEGER(11) NOT NULL, 
    domain VARCHAR(64) NOT NULL, 
    PRIMARY KEY (organization_id, domain), 
    CONSTRAINT organization_email_domain_ibfk_1 FOREIGN KEY(organization_id) REFERENCES organization (id)
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE TABLE user (
    id MEDIUMINT(8) UNSIGNED NOT NULL AUTO_INCREMENT, 
    oauth_id INTEGER(12) UNSIGNED NOT NULL, 
    oauth_provider TINYINT(1) UNSIGNED NOT NULL, 
    username VARCHAR(32) NOT NULL, 
    email VARCHAR(64), 
    github_email VARCHAR(64), 
    verification_code VARCHAR(64), 
    is_active TINYINT(1) NOT NULL DEFAULT '1', 
    on_email_list TINYINT(1) NOT NULL DEFAULT '1', 
    is_email_good TINYINT(1) NOT NULL DEFAULT '0', 
    player_level ENUM('High School','University','Professional') NOT NULL DEFAULT 'Professional', 
    organization_id INTEGER(11), 
    country_code VARCHAR(3), 
    country_subdivision_code VARCHAR(10), 
    creation_time DATETIME DEFAULT CURRENT_TIMESTAMP, 
    update_time DATETIME DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP, 
    api_key_hash VARCHAR(255), 
    is_admin TINYINT(1) DEFAULT '0', 
    PRIMARY KEY (id), 
    CONSTRAINT user_ibfk_1 FOREIGN KEY(organization_id) REFERENCES organization (id)
)DEFAULT CHARSET=utf8 AUTO_INCREMENT=1000 ENGINE=InnoDB;

CREATE INDEX organization_id ON user (organization_id);

CREATE TABLE bot (
    user_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    id MEDIUMINT(8) UNSIGNED NOT NULL, 
    compile_status ENUM('Uploaded','InProgress','Successful','Failed','Disabled') NOT NULL, 
    compile_start DATETIME, 
    language VARCHAR(16), 
    version_number SMALLINT(5) NOT NULL DEFAULT '0', 
    games_played BIGINT(20) NOT NULL DEFAULT '0', 
    mu FLOAT NOT NULL DEFAULT '25', 
    sigma FLOAT UNSIGNED NOT NULL DEFAULT '8.333', 
    score FLOAT NOT NULL DEFAULT '0', 
    creation_time DATETIME DEFAULT CURRENT_TIMESTAMP, 
    update_time DATETIME DEFAULT CURRENT_TIMESTAMP, 
    timeout_sent TINYINT(1) DEFAULT '0', 
    PRIMARY KEY (user_id, id), 
    CONSTRAINT bot_ibfk_2 FOREIGN KEY(user_id) REFERENCES user (id) ON DELETE CASCADE
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE TABLE hackathon_participant (
    hackathon_id INTEGER(10) UNSIGNED NOT NULL, 
    user_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    PRIMARY KEY (hackathon_id, user_id), 
    CONSTRAINT hackathon_participant_ibfk_4 FOREIGN KEY(hackathon_id) REFERENCES hackathon (id) ON DELETE CASCADE, 
    CONSTRAINT hackathon_participant_ibfk_3 FOREIGN KEY(user_id) REFERENCES user (id) ON DELETE CASCADE
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE INDEX user_id ON hackathon_participant (user_id);

CREATE TABLE user_badge (
    user_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    badge_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    is_enabled TINYINT(1) NOT NULL DEFAULT '1', 
    creation_time DATETIME DEFAULT CURRENT_TIMESTAMP, 
    update_time DATETIME DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP, 
    PRIMARY KEY (user_id, badge_id), 
    CONSTRAINT user_badge_ibfk_2 FOREIGN KEY(badge_id) REFERENCES badge (id) ON DELETE CASCADE, 
    CONSTRAINT user_badge_ibfk_1 FOREIGN KEY(user_id) REFERENCES user (id) ON DELETE CASCADE
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE INDEX fk_badge_id_idx ON user_badge (badge_id);

CREATE TABLE user_notification (
    id MEDIUMINT(8) UNSIGNED NOT NULL AUTO_INCREMENT, 
    user_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    title VARCHAR(64) NOT NULL, 
    body VARCHAR(2048) NOT NULL, 
    mood ENUM('error','neutral','success') NOT NULL DEFAULT 'neutral', 
    creation_time DATETIME DEFAULT CURRENT_TIMESTAMP, 
    PRIMARY KEY (id), 
    CONSTRAINT user_notification_ibfk_2 FOREIGN KEY(user_id) REFERENCES user (id) ON DELETE CASCADE
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE INDEX user_id ON user_notification (user_id);

CREATE TABLE user_tier_history (
    user_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    tier VARCHAR(256) NOT NULL, 
    last_in_tier DATETIME DEFAULT CURRENT_TIMESTAMP, 
    total_time_in_tier INTEGER(10) UNSIGNED DEFAULT '0', 
    PRIMARY KEY (user_id, tier), 
    CONSTRAINT user_tier_history_ibfk_2 FOREIGN KEY(user_id) REFERENCES user (id) ON DELETE CASCADE
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE TABLE bot_history (
    user_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    bot_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    version_number SMALLINT(5) NOT NULL, 
    last_rank SMALLINT(5) NOT NULL, 
    last_score FLOAT NOT NULL, 
    last_num_players SMALLINT(5) NOT NULL, 
    last_games_played BIGINT(20), 
    language VARCHAR(16) NOT NULL, 
    when_retired DATETIME DEFAULT CURRENT_TIMESTAMP, 
    PRIMARY KEY (user_id, bot_id, version_number), 
    CONSTRAINT bot_history_ibfk_4 FOREIGN KEY(user_id, bot_id) REFERENCES bot (user_id, id) ON DELETE CASCADE, 
    CONSTRAINT bot_history_ibfk_3 FOREIGN KEY(user_id) REFERENCES user (id) ON DELETE CASCADE
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE TABLE game_bot_stat (
    game_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    user_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    bot_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    planets_controlled INTEGER(10) UNSIGNED NOT NULL, 
    ships_produced INTEGER(10) UNSIGNED NOT NULL, 
    ships_alive INTEGER(10) UNSIGNED NOT NULL, 
    ships_alive_ratio FLOAT NOT NULL, 
    ships_relative_ratio FLOAT NOT NULL, 
    planets_destroyed INTEGER(10) UNSIGNED NOT NULL, 
    attacks_total INTEGER(10) UNSIGNED NOT NULL, 
    PRIMARY KEY (game_id, user_id, bot_id), 
    CONSTRAINT game_bot_stat_ibfk_1 FOREIGN KEY(game_id) REFERENCES game (id) ON DELETE CASCADE, 
    CONSTRAINT fkcompid FOREIGN KEY(user_id, bot_id) REFERENCES bot (user_id, id) ON DELETE NO ACTION ON UPDATE NO ACTION, 
    CONSTRAINT fkuserid FOREIGN KEY(user_id) REFERENCES user (id) ON DELETE NO ACTION ON UPDATE NO ACTION
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE INDEX fkcompid_idx ON game_bot_stat (user_id, bot_id);

CREATE TABLE game_participant (
    game_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    user_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    bot_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    version_number MEDIUMINT(8) UNSIGNED NOT NULL, 
    log_name VARCHAR(64), 
    rank SMALLINT(5) UNSIGNED NOT NULL, 
    player_index SMALLINT(5) UNSIGNED NOT NULL, 
    timed_out TINYINT(1) NOT NULL, 
    PRIMARY KEY (game_id, user_id, bot_id), 
    CONSTRAINT game_participant_ibfk_4 FOREIGN KEY(game_id) REFERENCES game (id) ON DELETE CASCADE, 
    CONSTRAINT game_participant_ibfk_3 FOREIGN KEY(user_id, bot_id) REFERENCES bot (user_id, id), 
    CONSTRAINT game_participant_ibfk_2 FOREIGN KEY(user_id) REFERENCES user (id)
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE INDEX user_id ON game_participant (user_id, bot_id);

CREATE TABLE hackathon_snapshot (
    hackathon_id INTEGER(10) UNSIGNED NOT NULL, 
    user_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    bot_id MEDIUMINT(8) UNSIGNED NOT NULL, 
    games_played INTEGER(10) UNSIGNED DEFAULT '0', 
    score FLOAT NOT NULL, 
    mu FLOAT NOT NULL, 
    sigma FLOAT NOT NULL, 
    version_number INTEGER(11), 
    language VARCHAR(16), 
    PRIMARY KEY (hackathon_id, user_id, bot_id), 
    CONSTRAINT hackathon_snapshot_ibfk_6 FOREIGN KEY(hackathon_id) REFERENCES hackathon (id) ON DELETE CASCADE, 
    CONSTRAINT hackathon_snapshot_ibfk_5 FOREIGN KEY(user_id, bot_id) REFERENCES bot (user_id, id) ON DELETE CASCADE, 
    CONSTRAINT hackathon_snapshot_ibfk_4 FOREIGN KEY(user_id) REFERENCES user (id) ON DELETE CASCADE
)DEFAULT CHARSET=utf8 ENGINE=InnoDB;

CREATE INDEX user_id ON hackathon_snapshot (user_id, bot_id);

INSERT INTO alembic_version (version_num) VALUES ('7f0054256cf5');

-- Running upgrade 7f0054256cf5 -> 33de9025cc63

ALTER TABLE game_stat DROP FOREIGN KEY game_stat_ibfk_1;

ALTER TABLE game_view_stat DROP FOREIGN KEY game_view_stat_ibfk_1;

ALTER TABLE game_bot_stat DROP FOREIGN KEY game_bot_stat_ibfk_1;

ALTER TABLE game_participant DROP FOREIGN KEY game_participant_ibfk_4;

ALTER TABLE game MODIFY id INTEGER(9) UNSIGNED NOT NULL AUTO_INCREMENT;

ALTER TABLE game_stat MODIFY game_id INTEGER(9) UNSIGNED NOT NULL;

ALTER TABLE game_view_stat MODIFY game_id INTEGER(9) UNSIGNED NOT NULL;

ALTER TABLE game_bot_stat MODIFY game_id INTEGER(9) UNSIGNED NOT NULL;

ALTER TABLE game_participant MODIFY game_id INTEGER(9) UNSIGNED NOT NULL;

ALTER TABLE game_stat ADD CONSTRAINT game_stat_ibfk_1 FOREIGN KEY(game_id) REFERENCES game (id) ON DELETE CASCADE;

ALTER TABLE game_view_stat ADD CONSTRAINT game_view_stat_ibfk_1 FOREIGN KEY(game_id) REFERENCES game (id) ON DELETE CASCADE;

ALTER TABLE game_bot_stat ADD CONSTRAINT game_bot_stat_ibfk_1 FOREIGN KEY(game_id) REFERENCES game (id) ON DELETE CASCADE;

ALTER TABLE game_participant ADD CONSTRAINT game_participant_ibfk_4 FOREIGN KEY(game_id) REFERENCES game (id) ON DELETE CASCADE;

UPDATE alembic_version SET version_num='33de9025cc63' WHERE alembic_version.version_num = '7f0054256cf5';

