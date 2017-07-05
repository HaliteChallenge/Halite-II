USE halite2;

CREATE TABLE organization (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  organization_name VARCHAR(64) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE organization_email_domain (
  organization_id INT NOT NULL,
  domain VARCHAR(64) NOT NULL,
  PRIMARY KEY (organization_id, domain),
  FOREIGN KEY (organization_id) REFERENCES organization(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `user` (
  id MEDIUMINT(8) UNSIGNED NOT NULL AUTO_INCREMENT,
  oauth_id INT(12) UNSIGNED NOT NULL,
  oauth_provider TINYINT(1) UNSIGNED NOT NULL,
  username VARCHAR(32) NOT NULL,
  email VARCHAR(64),
  github_email VARCHAR(64),
  verification_code VARCHAR(64),
  is_active BOOL NOT NULL DEFAULT TRUE,
  -- TODO: port this over
  on_email_list BOOL NOT NULL DEFAULT 1,
  is_email_good BOOL NOT NULL DEFAULT 0,
  player_level ENUM('High School','Undergraduate','Graduate','Professional') NOT NULL DEFAULT 'Professional',
  organization_id INT,
  -- ISO-3166 3-letter country code
  country_code VARCHAR(3),
  -- ISO-3166-2 variable length country code
  country_subdivision_code VARCHAR(10),
  -- countryVisible BOOL DEFAULT FALSE,
  creation_time DATETIME DEFAULT CURRENT_TIMESTAMP,
  update_time DATETIME ON UPDATE CURRENT_TIMESTAMP,
  api_key_hash VARCHAR(255),
  FOREIGN KEY (organization_id) REFERENCES organization(id),
  PRIMARY KEY (id)
) ENGINE=InnoDB AUTO_INCREMENT=1000 DEFAULT CHARSET=utf8;

CREATE TABLE bot (
  user_id MEDIUMINT(8) UNSIGNED NOT NULL,
  id MEDIUMINT(8) UNSIGNED NOT NULL,
  compile_status ENUM('Uploaded', 'InProgress', 'Successful', 'Failed') NOT NULL,
  compile_start DATETIME,
  language VARCHAR(16) DEFAULT NULL,
  -- # of times submitted = version number
  version_number SMALLINT(5) NOT NULL DEFAULT 0,
  games_played SMALLINT(5) NOT NULL DEFAULT 0,
  mu FLOAT NOT NULL DEFAULT 25.000,
  sigma FLOAT UNSIGNED NOT NULL DEFAULT 8.333,
  score FLOAT NOT NULL DEFAULT 0,
  FOREIGN KEY (user_id) REFERENCES `user`(id),
  PRIMARY KEY (user_id, id)
);

CREATE TABLE game (
  id MEDIUMINT(8) UNSIGNED NOT NULL AUTO_INCREMENT,
  replay_name VARCHAR(128) NOT NULL,
  map_width SMALLINT(5) NOT NULL,
  map_height SMALLINT(5) NOT NULL,
  map_seed INT UNSIGNED NOT NULL,
  map_generator VARCHAR(128) NOT NULL,
  time_played DATETIME DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE game_participant (
  game_id MEDIUMINT(8) UNSIGNED NOT NULL,
  user_id MEDIUMINT(8) UNSIGNED NOT NULL,
  bot_id MEDIUMINT(8) UNSIGNED NOT NULL,
  version_number MEDIUMINT(8) UNSIGNED NOT NULL,
  log_name VARCHAR(64) DEFAULT NULL,
  rank SMALLINT(5) UNSIGNED NOT NULL,
  player_index SMALLINT(5) UNSIGNED NOT NULL,
  timed_out BOOL NOT NULL,
  FOREIGN KEY (game_id) REFERENCES game(id),
  FOREIGN KEY (user_id) REFERENCES `user`(id),
  FOREIGN KEY (user_id, bot_id) REFERENCES bot(user_id, id),
  PRIMARY KEY (game_id, user_id, bot_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- A history of submitted bots, recorded whenever a new one is submitted.
-- This intends to prevent players from keeping their bots in "stealth", where
-- they submit bots, wait for the rank to stabilize, then take them down.
CREATE TABLE bot_history (
  user_id MEDIUMINT(8) UNSIGNED NOT NULL,
  bot_id MEDIUMINT(8) UNSIGNED NOT NULL,
  version_number SMALLINT(5) NOT NULL,
  last_rank SMALLINT(5) NOT NULL,
  last_score SMALLINT(5) NOT NULL,
  -- The count of active players when this bot was retired.
  last_num_players SMALLINT(5) NOT NULL,
  -- The number of games this bot has played.
  last_games_played SMALLINT(5) DEFAULT NULL,
  language VARCHAR(16) NOT NULL,
  when_retired DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES `user`(id),
  FOREIGN KEY (user_id, bot_id) REFERENCES bot(user_id, id),
  PRIMARY KEY (user_id, bot_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE user_notification (
  id MEDIUMINT(8) UNSIGNED NOT NULL AUTO_INCREMENT,
  user_id mediumint(8) unsigned NOT NULL,
  title varchar(64) NOT NULL,
  body varchar(2048) NOT NULL,
  mood ENUM("error", "neutral", "success") NOT NULL DEFAULT "neutral",
  creation_time DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES `user`(id),
  PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
