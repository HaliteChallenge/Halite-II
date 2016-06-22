SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";

--
-- Database: `Halite`
--

use Halite;

--
-- Table structure for table `Game`
--

CREATE TABLE `Game` (
  `gameID` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `replayName` varchar(64) NOT NULL,
  PRIMARY KEY (`gameID`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=27 ;

--
-- Table structure for table `GameUser`
--

CREATE TABLE `GameUser` (
  `gameID` mediumint(8) unsigned NOT NULL,
  `userID` mediumint(8) unsigned NOT NULL,
  `rank` smallint(5) unsigned NOT NULL,
  `score` float unsigned NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Table structure for table `User`
--

CREATE TABLE `User` (
  `userID` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(20) NOT NULL,
  `password` varchar(30) NOT NULL,
  `language` varchar(16) DEFAULT NULL,
  `status` smallint(5) unsigned NOT NULL,
  `mu` float unsigned NOT NULL,
  `sigma` float unsigned NOT NULL,
  PRIMARY KEY (`userID`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=31 ;

--
-- Table structure for table `Worker`
--

CREATE TABLE `Worker` (
  `apiKey` smallint(5) unsigned NOT NULL,
  `ipAddress` varchar(32) NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

