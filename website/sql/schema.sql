-- MySQL dump 10.13  Distrib 5.5.49, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: Halite
-- ------------------------------------------------------
-- Server version   5.5.49-0ubuntu0.14.04.1

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

--
-- Table structure for table `Game`
--

CREATE DATABASE IF NOT EXISTS Halite ;
use Halite;

DROP TABLE IF EXISTS `Game`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Game` (
  `gameID` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `replayName` varchar(64) NOT NULL,
  `mapWidth` smallint(5) NOT NULL,
  `mapHeight` smallint(5) NOT NULL,
  `timestamp` datetime DEFAULT CURRENT_TIMESTAMP,
  `workerID` mediumint(8) unsigned DEFAULT NULL,
  PRIMARY KEY (`gameID`)
) ENGINE=InnoDB AUTO_INCREMENT=407227 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `GameUser`
--

DROP TABLE IF EXISTS `GameUser`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `GameUser` (
  `gameID` mediumint(8) unsigned NOT NULL,
  `userID` mediumint(8) unsigned NOT NULL,
  `versionNumber` mediumint(8) unsigned NOT NULL,
  `errorLogName` varchar(64) DEFAULT NULL,
  `rank` smallint(5) unsigned NOT NULL,
  `playerIndex` smallint(5) unsigned NOT NULL,
  `didTimeout` tinyint(1) unsigned NOT NULL,
  PRIMARY KEY (`gameID`,`userID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `User`
--

DROP TABLE IF EXISTS `User`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `User` (
  `userID` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `oauthID` int(12) unsigned NOT NULL,
  `oauthProvider` tinyint(1) unsigned NOT NULL,
  `username` varchar(32) NOT NULL,
  `email` varchar(64),
  `githubEmail` varchar(64),
  `verificationCode` varchar(64),
  `onEmailList` tinyint(1) NOT NULL DEFAULT 1,
  `isEmailGood` tinyint(1) unsigned NOT NULL DEFAULT 0,
  `isRunning` tinyint(1) unsigned NOT NULL DEFAULT 0,
  `compileStatus` tinyint(1) unsigned NOT NULL DEFAULT 0,
  `level` enum('High School','Undergraduate','Graduate','Professional') NOT NULL DEFAULT 'Professional',
  `organization` varchar(64) NOT NULL,
  `language` varchar(16) DEFAULT NULL,
  `mu` float NOT NULL DEFAULT 25.000,
  `sigma` float unsigned NOT NULL DEFAULT 8.333,
  `rank` smallint(5) DEFAULT NULL,
  `numSubmissions` smallint(5) NOT NULL DEFAULT 0,
  `numGames` smallint(5) NOT NULL DEFAULT 0,
  `creationTime` datetime DEFAULT CURRENT_TIMESTAMP,
  `updateTime` datetime ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`userID`)
) ENGINE=InnoDB AUTO_INCREMENT=1000 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `UserHistory`
--

DROP TABLE IF EXISTS `UserHistory`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `UserHistory` (
  `userID` mediumint(8) unsigned NOT NULL,
  `versionNumber` smallint(5) NOT NULL,
  `lastRank` smallint(5) NOT NULL,
  `lastNumPlayers` smallint(5) NOT NULL,
  `lastNumGames` smallint(5) DEFAULT NULL,
  `timestamp` datetime DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `Announcement`
--

DROP TABLE IF EXISTS `Announcement`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Announcement` (
  `announcementID` mediumint(8) unsigned NOT NULL,
  `header` varchar(64) NOT NULL,
  `body` varchar(512) NOT NULL,
  PRIMARY KEY (`announcementID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `DoneWithAnnouncement`
--

DROP TABLE IF EXISTS `DoneWithAnnouncement`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `DoneWithAnnouncement` (
  `announcementID` mediumint(8) unsigned NOT NULL,
  `userID` mediumint(8) unsigned NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `UserNotification`
--

DROP TABLE IF EXISTS `UserNotification`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `UserNotification` (
  `userNotificationID` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `userID` mediumint(8) unsigned NOT NULL,
  `title` varchar(64) NOT NULL,
  `body` varchar(2048) NOT NULL,
  `mood` tinyint(1) NOT NULL,
  `creationTime` datetime DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`userNotificationID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `HighSchool`
--

DROP TABLE IF EXISTS `HighSchool`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `HighSchool` (
  `name` varchar(64) NOT NULL,
  PRIMARY KEY(`name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `Worker`
--

DROP TABLE IF EXISTS `Worker`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Worker` (
  `workerID` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `apiKey` mediumint(8) unsigned NOT NULL,
  `ipAddress` varchar(32) NOT NULL,
  `numGames` mediumint(8) NOT NULL DEFAULT 0,
  `numCompiles` smallint(5) NOT NULL DEFAULT 0,
  `lastRequestTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`workerID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2016-07-22 16:25:27
