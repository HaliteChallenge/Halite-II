CREATE DATABASE IF NOT EXISTS halite2;
use halite2;

DROP TABLE IF EXISTS `Organization`;
CREATE TABLE `Organization` (
  `organizationID` INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `level` enum('High School','Undergraduate','Graduate','Professional') NOT NULL DEFAULT 'Professional',
  `organizationName` VARCHAR(64) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `OrganizationEmailDomain`;
CREATE TABLE `OrganizationEmailDomain` (
    `organizationID` INT NOT NULL,
    `domain` VARCHAR(64) NOT NULL,
    PRIMARY KEY (`organizationID`, `domain`),
    FOREIGN KEY (`organizationID`) REFERENCES Organization(`organizationID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `User`;
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
  `compileStart` datetime,
  `level` enum('High School','Undergraduate','Graduate','Professional') NOT NULL DEFAULT 'Professional',
  `organizationID` INT,
  # ISO-3166 3-letter country code
  `countryCode` VARCHAR(3),
  # ISO-3166-2 variable length country code
  `countrySubdivisionCode` VARCHAR(10),
  `countryVisible` BOOL DEFAULT FALSE,
  `language` varchar(16) DEFAULT NULL,
  `mu` float NOT NULL DEFAULT 25.000,
  `sigma` float unsigned NOT NULL DEFAULT 8.333,
  `rank` smallint(5) DEFAULT NULL,
  `numSubmissions` smallint(5) NOT NULL DEFAULT 0,
  `numGames` smallint(5) NOT NULL DEFAULT 0,
  `creationTime` datetime DEFAULT CURRENT_TIMESTAMP,
  `updateTime` datetime ON UPDATE CURRENT_TIMESTAMP,
  `apiKeyHash` VARCHAR(255),
  FOREIGN KEY (`organizationID`) REFERENCES Organization(`organizationID`),
  PRIMARY KEY (`userID`)
) ENGINE=InnoDB AUTO_INCREMENT=1000 DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `Game`;
CREATE TABLE `Game` (
  `gameID` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `replayName` varchar(64) NOT NULL,
  `mapWidth` smallint(5) NOT NULL,
  `mapHeight` smallint(5) NOT NULL,
  `timestamp` datetime DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`gameID`)
) ENGINE=InnoDB AUTO_INCREMENT=407227 DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `GameUser`;
CREATE TABLE `GameUser` (
  `gameID` mediumint(8) unsigned NOT NULL,
  `userID` mediumint(8) unsigned NOT NULL,
  `versionNumber` mediumint(8) unsigned NOT NULL,
  `errorLogName` varchar(64) DEFAULT NULL,
  `rank` smallint(5) unsigned NOT NULL,
  `playerIndex` smallint(5) unsigned NOT NULL,
  `didTimeout` tinyint(1) unsigned NOT NULL,
  FOREIGN KEY (`gameID`) REFERENCES Game(`gameID`),
  FOREIGN KEY (`userID`) REFERENCES User(`userID`),
  PRIMARY KEY (`gameID`,`userID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `UserHistory`;
CREATE TABLE `UserHistory` (
  `userID` mediumint(8) unsigned NOT NULL,
  `versionNumber` smallint(5) NOT NULL,
  `lastRank` smallint(5) NOT NULL,
  `lastNumPlayers` smallint(5) NOT NULL,
  `lastNumGames` smallint(5) DEFAULT NULL,
  `timestamp` datetime DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (`userID`) REFERENCES User(`userID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `Announcement`;
CREATE TABLE `Announcement` (
  `announcementID` mediumint(8) unsigned NOT NULL,
  `header` varchar(64) NOT NULL,
  `body` varchar(512) NOT NULL,
  PRIMARY KEY (`announcementID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `DoneWithAnnouncement`;
CREATE TABLE `DoneWithAnnouncement` (
  `announcementID` mediumint(8) unsigned NOT NULL,
  `userID` mediumint(8) unsigned NOT NULL,
  FOREIGN KEY (`userID`) REFERENCES User(`userID`),
  FOREIGN KEY (`announcementID`) REFERENCES Announcement(`announcementID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `UserNotification`;
CREATE TABLE `UserNotification` (
  `userNotificationID` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `userID` mediumint(8) unsigned NOT NULL,
  `title` varchar(64) NOT NULL,
  `body` varchar(2048) NOT NULL,
  `mood` tinyint(1) NOT NULL,
  `creationTime` datetime DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (`userID`) REFERENCES User(`userID`),
  PRIMARY KEY (`userNotificationID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
