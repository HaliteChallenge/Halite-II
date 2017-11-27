-- MySQL dump 10.13  Distrib 5.7.20, for Linux (x86_64)
--
-- Host: 127.0.0.1    Database: halite2
-- ------------------------------------------------------
-- Server version	5.7.14-google-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;
SET @MYSQLDUMP_TEMP_LOG_BIN = @@SESSION.SQL_LOG_BIN;
SET @@SESSION.SQL_LOG_BIN= 0;

--
-- GTID state at the beginning of the backup 
--

SET @@GLOBAL.GTID_PURGED='397d1add-5cc9-11e7-9997-42010a8e0fdd:1-62888918';

--
-- Table structure for table `alembic_version`
--

DROP TABLE IF EXISTS `alembic_version`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `alembic_version` (
  `version_num` varchar(32) NOT NULL,
  PRIMARY KEY (`version_num`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `badge`
--

DROP TABLE IF EXISTS `badge`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `badge` (
  `id` mediumint(8) unsigned NOT NULL,
  `name` varchar(256) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `bot`
--

DROP TABLE IF EXISTS `bot`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `bot` (
  `user_id` mediumint(8) unsigned NOT NULL,
  `id` mediumint(8) unsigned NOT NULL,
  `compile_status` enum('Uploaded','InProgress','Successful','Failed','Disabled') NOT NULL,
  `compile_start` datetime DEFAULT NULL,
  `language` varchar(16) DEFAULT NULL,
  `version_number` smallint(5) NOT NULL DEFAULT '0',
  `games_played` bigint(20) NOT NULL DEFAULT '0',
  `mu` float NOT NULL DEFAULT '25',
  `sigma` float unsigned NOT NULL DEFAULT '8.333',
  `score` float NOT NULL DEFAULT '0',
  `creation_time` datetime DEFAULT CURRENT_TIMESTAMP,
  `update_time` datetime DEFAULT CURRENT_TIMESTAMP,
  `timeout_sent` tinyint(1) DEFAULT '0',
  PRIMARY KEY (`user_id`,`id`),
  CONSTRAINT `bot_ibfk_2` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `bot_history`
--

DROP TABLE IF EXISTS `bot_history`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `bot_history` (
  `user_id` mediumint(8) unsigned NOT NULL,
  `bot_id` mediumint(8) unsigned NOT NULL,
  `version_number` smallint(5) NOT NULL,
  `last_rank` smallint(5) NOT NULL,
  `last_score` float NOT NULL,
  `last_num_players` smallint(5) NOT NULL,
  `last_games_played` bigint(20) DEFAULT NULL,
  `language` varchar(16) NOT NULL,
  `when_retired` datetime DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`user_id`,`bot_id`,`version_number`),
  CONSTRAINT `bot_history_ibfk_3` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE CASCADE,
  CONSTRAINT `bot_history_ibfk_4` FOREIGN KEY (`user_id`, `bot_id`) REFERENCES `bot` (`user_id`, `id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `challenge`
--

DROP TABLE IF EXISTS `challenge`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `challenge` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `finished` datetime DEFAULT NULL,
  `num_games` int(11) NOT NULL,
  `status` enum('created','playing_game','finished') NOT NULL,
  `most_recent_game_task` datetime DEFAULT NULL,
  `issuer` mediumint(8) unsigned NOT NULL,
  `winner` mediumint(8) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `challenge_issuer_fk` (`issuer`),
  KEY `challenge_winner_fk` (`winner`),
  CONSTRAINT `challenge_issuer_fk` FOREIGN KEY (`issuer`) REFERENCES `user` (`id`) ON DELETE CASCADE,
  CONSTRAINT `challenge_winner_fk` FOREIGN KEY (`winner`) REFERENCES `user` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `challenge_participant`
--

DROP TABLE IF EXISTS `challenge_participant`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `challenge_participant` (
  `challenge_id` int(11) NOT NULL,
  `user_id` mediumint(8) unsigned NOT NULL,
  `points` int(11) NOT NULL,
  `ships_produced` int(11) NOT NULL,
  `attacks_made` int(11) NOT NULL,
  PRIMARY KEY (`challenge_id`,`user_id`),
  KEY `challenge_participant_ibfk_2` (`user_id`),
  CONSTRAINT `challenge_participant_fk` FOREIGN KEY (`challenge_id`) REFERENCES `challenge` (`id`) ON DELETE CASCADE,
  CONSTRAINT `challenge_participant_ibfk_2` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `game`
--

DROP TABLE IF EXISTS `game`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `game` (
  `id` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `replay_name` varchar(128) NOT NULL,
  `map_width` smallint(5) NOT NULL,
  `map_height` smallint(5) NOT NULL,
  `map_seed` int(10) unsigned NOT NULL,
  `map_generator` varchar(128) NOT NULL,
  `time_played` datetime DEFAULT CURRENT_TIMESTAMP,
  `replay_bucket` smallint(5) NOT NULL DEFAULT '0',
  `challenge_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `game_time_played` (`time_played`),
  KEY `game_challenge_fk` (`challenge_id`),
  CONSTRAINT `game_challenge_fk` FOREIGN KEY (`challenge_id`) REFERENCES `challenge` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=3492600 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `game_bot_stat`
--

DROP TABLE IF EXISTS `game_bot_stat`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `game_bot_stat` (
  `game_id` mediumint(8) unsigned NOT NULL,
  `user_id` mediumint(8) unsigned NOT NULL,
  `bot_id` mediumint(8) unsigned NOT NULL,
  `planets_controlled` int(10) unsigned NOT NULL,
  `ships_produced` int(10) unsigned NOT NULL,
  `ships_alive` int(10) unsigned NOT NULL,
  `ships_alive_ratio` float NOT NULL,
  `ships_relative_ratio` float NOT NULL,
  `planets_destroyed` int(10) unsigned NOT NULL,
  `attacks_total` int(10) unsigned NOT NULL,
  PRIMARY KEY (`game_id`,`user_id`,`bot_id`),
  KEY `fkcompid_idx` (`user_id`,`bot_id`),
  CONSTRAINT `fkcompid` FOREIGN KEY (`user_id`, `bot_id`) REFERENCES `bot` (`user_id`, `id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fkuserid` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `game_bot_stat_ibfk_1` FOREIGN KEY (`game_id`) REFERENCES `game` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `game_participant`
--

DROP TABLE IF EXISTS `game_participant`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `game_participant` (
  `game_id` mediumint(8) unsigned NOT NULL,
  `user_id` mediumint(8) unsigned NOT NULL,
  `bot_id` mediumint(8) unsigned NOT NULL,
  `version_number` mediumint(8) unsigned NOT NULL,
  `log_name` varchar(256) DEFAULT NULL,
  `rank` smallint(5) unsigned NOT NULL,
  `player_index` smallint(5) unsigned NOT NULL,
  `timed_out` tinyint(1) NOT NULL,
  `mu` float DEFAULT NULL,
  `sigma` float unsigned DEFAULT NULL,
  `leaderboard_rank` smallint(5) DEFAULT NULL,
  PRIMARY KEY (`game_id`,`user_id`,`bot_id`),
  KEY `user_id` (`user_id`,`bot_id`),
  CONSTRAINT `game_participant_ibfk_2` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`),
  CONSTRAINT `game_participant_ibfk_3` FOREIGN KEY (`user_id`, `bot_id`) REFERENCES `bot` (`user_id`, `id`),
  CONSTRAINT `game_participant_ibfk_4` FOREIGN KEY (`game_id`) REFERENCES `game` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `game_stat`
--

DROP TABLE IF EXISTS `game_stat`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `game_stat` (
  `game_id` mediumint(8) unsigned NOT NULL,
  `turns_total` int(10) unsigned NOT NULL,
  `planets_destroyed` int(10) unsigned NOT NULL,
  `ships_produced` int(10) unsigned NOT NULL,
  `ships_destroyed` int(10) unsigned NOT NULL,
  PRIMARY KEY (`game_id`),
  CONSTRAINT `game_stat_ibfk_1` FOREIGN KEY (`game_id`) REFERENCES `game` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `game_view_stat`
--

DROP TABLE IF EXISTS `game_view_stat`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `game_view_stat` (
  `game_id` mediumint(8) unsigned NOT NULL,
  `views_total` int(10) unsigned NOT NULL,
  PRIMARY KEY (`game_id`),
  CONSTRAINT `game_view_stat_ibfk_1` FOREIGN KEY (`game_id`) REFERENCES `game` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `hackathon`
--

DROP TABLE IF EXISTS `hackathon`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `hackathon` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `title` varchar(256) NOT NULL,
  `description` varchar(4096) NOT NULL,
  `start_date` datetime NOT NULL,
  `end_date` datetime NOT NULL,
  `verification_code` varchar(32) NOT NULL,
  `organization_id` int(11) DEFAULT NULL,
  `location` varchar(256) DEFAULT NULL,
  `thumbnail` varchar(512) DEFAULT NULL,
  `is_open` tinyint(4) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `verification_code` (`verification_code`),
  KEY `organization_id` (`organization_id`),
  CONSTRAINT `hackathon_ibfk_1` FOREIGN KEY (`organization_id`) REFERENCES `organization` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=52 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `hackathon_participant`
--

DROP TABLE IF EXISTS `hackathon_participant`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `hackathon_participant` (
  `hackathon_id` int(10) unsigned NOT NULL,
  `user_id` mediumint(8) unsigned NOT NULL,
  PRIMARY KEY (`hackathon_id`,`user_id`),
  KEY `user_id` (`user_id`),
  CONSTRAINT `hackathon_participant_ibfk_3` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE CASCADE,
  CONSTRAINT `hackathon_participant_ibfk_4` FOREIGN KEY (`hackathon_id`) REFERENCES `hackathon` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `hackathon_snapshot`
--

DROP TABLE IF EXISTS `hackathon_snapshot`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `hackathon_snapshot` (
  `hackathon_id` int(10) unsigned NOT NULL,
  `user_id` mediumint(8) unsigned NOT NULL,
  `bot_id` mediumint(8) unsigned NOT NULL,
  `games_played` int(10) unsigned DEFAULT '0',
  `score` float NOT NULL,
  `mu` float NOT NULL,
  `sigma` float NOT NULL,
  `version_number` int(11) DEFAULT NULL,
  `language` varchar(16) DEFAULT NULL,
  PRIMARY KEY (`hackathon_id`,`user_id`,`bot_id`),
  KEY `user_id` (`user_id`,`bot_id`),
  CONSTRAINT `hackathon_snapshot_ibfk_4` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE CASCADE,
  CONSTRAINT `hackathon_snapshot_ibfk_5` FOREIGN KEY (`user_id`, `bot_id`) REFERENCES `bot` (`user_id`, `id`) ON DELETE CASCADE,
  CONSTRAINT `hackathon_snapshot_ibfk_6` FOREIGN KEY (`hackathon_id`) REFERENCES `hackathon` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `halite_1_user`
--

DROP TABLE IF EXISTS `halite_1_user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `halite_1_user` (
  `userID` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `oauthID` int(12) unsigned NOT NULL,
  `oauthProvider` tinyint(1) unsigned NOT NULL,
  `username` varchar(32) NOT NULL,
  `email` varchar(64) DEFAULT NULL,
  `isRunning` tinyint(1) unsigned NOT NULL DEFAULT '0',
  `compileStatus` tinyint(1) unsigned NOT NULL DEFAULT '0',
  `organization` varchar(64) NOT NULL,
  `language` varchar(16) DEFAULT NULL,
  `mu` float NOT NULL DEFAULT '25',
  `sigma` float unsigned NOT NULL DEFAULT '8.333',
  `rank` smallint(5) DEFAULT NULL,
  `numSubmissions` smallint(5) NOT NULL DEFAULT '0',
  `numGames` smallint(5) NOT NULL DEFAULT '0',
  `creationTime` datetime DEFAULT CURRENT_TIMESTAMP,
  `updateTime` datetime DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP,
  `onEmailList` tinyint(1) NOT NULL DEFAULT '1',
  `githubEmail` varchar(64) DEFAULT NULL,
  `verificationCode` varchar(64) DEFAULT NULL,
  `isEmailGood` tinyint(1) unsigned NOT NULL DEFAULT '0',
  `level` enum('High School','Undergraduate','Graduate','Professional') NOT NULL DEFAULT 'Professional',
  PRIMARY KEY (`userID`)
) ENGINE=InnoDB AUTO_INCREMENT=5350 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `leagues`
--

DROP TABLE IF EXISTS `leagues`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `leagues` (
  `id` int(11) NOT NULL,
  `category` varchar(45) NOT NULL,
  `name` varchar(45) NOT NULL,
  `description` varchar(1024) NOT NULL,
  `query` varchar(1024) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `organization`
--

DROP TABLE IF EXISTS `organization`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `organization` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `organization_name` varchar(64) NOT NULL,
  `kind` enum('High School','University','Professional School','Company','Other') NOT NULL DEFAULT 'Other',
  `verification_code` varchar(32) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `verification_code` (`verification_code`)
) ENGINE=InnoDB AUTO_INCREMENT=26472 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `organization_email_domain`
--

DROP TABLE IF EXISTS `organization_email_domain`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `organization_email_domain` (
  `organization_id` int(11) NOT NULL,
  `domain` varchar(64) NOT NULL,
  PRIMARY KEY (`organization_id`,`domain`),
  CONSTRAINT `organization_email_domain_ibfk_1` FOREIGN KEY (`organization_id`) REFERENCES `organization` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user` (
  `id` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `oauth_id` int(12) unsigned NOT NULL,
  `oauth_provider` tinyint(1) unsigned NOT NULL,
  `username` varchar(32) NOT NULL,
  `email` varchar(64) DEFAULT NULL,
  `github_email` varchar(64) DEFAULT NULL,
  `verification_code` varchar(64) DEFAULT NULL,
  `is_active` tinyint(1) NOT NULL DEFAULT '1',
  `on_email_list` tinyint(1) NOT NULL DEFAULT '1',
  `is_email_good` tinyint(1) NOT NULL DEFAULT '0',
  `player_level` enum('High School','University','Professional') NOT NULL DEFAULT 'Professional',
  `organization_id` int(11) DEFAULT NULL,
  `country_code` varchar(3) DEFAULT NULL,
  `country_subdivision_code` varchar(10) DEFAULT NULL,
  `creation_time` datetime DEFAULT CURRENT_TIMESTAMP,
  `update_time` datetime DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP,
  `api_key_hash` varchar(255) DEFAULT NULL,
  `is_admin` tinyint(1) DEFAULT '0',
  `is_gpu_enabled` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `organization_id` (`organization_id`),
  CONSTRAINT `user_ibfk_1` FOREIGN KEY (`organization_id`) REFERENCES `organization` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=4380 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `user_badge`
--

DROP TABLE IF EXISTS `user_badge`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user_badge` (
  `user_id` mediumint(8) unsigned NOT NULL,
  `badge_id` mediumint(8) unsigned NOT NULL,
  `is_enabled` tinyint(1) NOT NULL DEFAULT '1',
  `creation_time` datetime DEFAULT CURRENT_TIMESTAMP,
  `update_time` datetime DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`user_id`,`badge_id`),
  KEY `fk_badge_id_idx` (`badge_id`),
  CONSTRAINT `user_badge_ibfk_1` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE CASCADE,
  CONSTRAINT `user_badge_ibfk_2` FOREIGN KEY (`badge_id`) REFERENCES `badge` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `user_notification`
--

DROP TABLE IF EXISTS `user_notification`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user_notification` (
  `id` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `user_id` mediumint(8) unsigned NOT NULL,
  `title` varchar(64) NOT NULL,
  `body` varchar(2048) NOT NULL,
  `mood` enum('error','neutral','success') NOT NULL DEFAULT 'neutral',
  `creation_time` datetime DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  KEY `user_id` (`user_id`),
  CONSTRAINT `user_notification_ibfk_2` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `user_tier_history`
--

DROP TABLE IF EXISTS `user_tier_history`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user_tier_history` (
  `user_id` mediumint(8) unsigned NOT NULL,
  `tier` varchar(256) NOT NULL,
  `last_in_tier` datetime DEFAULT CURRENT_TIMESTAMP,
  `total_time_in_tier` int(10) unsigned DEFAULT '0',
  PRIMARY KEY (`user_id`,`tier`),
  CONSTRAINT `user_tier_history_ibfk_2` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;
SET @@SESSION.SQL_LOG_BIN = @MYSQLDUMP_TEMP_LOG_BIN;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2017-11-27 16:36:28
-- MySQL dump 10.13  Distrib 5.7.20, for Linux (x86_64)
--
-- Host: 127.0.0.1    Database: halite2
-- ------------------------------------------------------
-- Server version	5.7.14-google-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;
SET @MYSQLDUMP_TEMP_LOG_BIN = @@SESSION.SQL_LOG_BIN;
SET @@SESSION.SQL_LOG_BIN= 0;

--
-- GTID state at the beginning of the backup 
--

SET @@GLOBAL.GTID_PURGED='397d1add-5cc9-11e7-9997-42010a8e0fdd:1-62888958';

--
-- Dumping data for table `alembic_version`
--

LOCK TABLES `alembic_version` WRITE;
/*!40000 ALTER TABLE `alembic_version` DISABLE KEYS */;
INSERT INTO `alembic_version` VALUES ('451d4bb125cb');
/*!40000 ALTER TABLE `alembic_version` ENABLE KEYS */;
UNLOCK TABLES;
SET @@SESSION.SQL_LOG_BIN = @MYSQLDUMP_TEMP_LOG_BIN;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2017-11-27 16:36:30
