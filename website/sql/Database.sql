-- phpMyAdmin SQL Dump
-- version 2.11.11.3
-- http://www.phpmyadmin.net
--
-- Host: 68.178.217.48
-- Generation Time: Feb 14, 2016 at 09:20 PM
-- Server version: 5.5.43
-- PHP Version: 5.1.6

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";

--
-- Database: `Halite`
--

-- --------------------------------------------------------

--
-- Table structure for table `Game`
--

CREATE TABLE `Game` (
  `gameID` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `replayName` varchar(64) NOT NULL,
  PRIMARY KEY (`gameID`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=27 ;

--
-- Dumping data for table `Game`
--

INSERT INTO `Game` VALUES(15, 'Output_1451516661.hlt');
INSERT INTO `Game` VALUES(14, 'Output_1451516653.hlt');
INSERT INTO `Game` VALUES(13, 'Output_1451516644.hlt');
INSERT INTO `Game` VALUES(12, 'Output_1451516635.hlt');
INSERT INTO `Game` VALUES(11, 'Output_1451516584.hlt');
INSERT INTO `Game` VALUES(10, 'Output_1451516571.hlt');
INSERT INTO `Game` VALUES(9, 'Output_1451512533.hlt');
INSERT INTO `Game` VALUES(16, 'Output_1451516664.hlt');
INSERT INTO `Game` VALUES(17, 'Output_1451516670.hlt');
INSERT INTO `Game` VALUES(18, 'Output_1451516674.hlt');
INSERT INTO `Game` VALUES(19, 'Output_1451516684.hlt');
INSERT INTO `Game` VALUES(20, 'Output_1451516696.hlt');
INSERT INTO `Game` VALUES(21, 'Output_1455427746.hlt');
INSERT INTO `Game` VALUES(22, 'Output_1455427837.hlt');
INSERT INTO `Game` VALUES(23, 'Output_1455427932.hlt');
INSERT INTO `Game` VALUES(24, 'Output_1455427971.hlt');
INSERT INTO `Game` VALUES(25, 'Output_1455428221.hlt');
INSERT INTO `Game` VALUES(26, 'Output_1455428285.hlt');

-- --------------------------------------------------------

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
-- Dumping data for table `GameUser`
--

INSERT INTO `GameUser` VALUES(11, 30, 0, 1);
INSERT INTO `GameUser` VALUES(10, 29, 1, 0.212);
INSERT INTO `GameUser` VALUES(10, 30, 0, 1);
INSERT INTO `GameUser` VALUES(9, 29, 1, 0.178);
INSERT INTO `GameUser` VALUES(9, 30, 0, 1);
INSERT INTO `GameUser` VALUES(11, 29, 1, 0.086);
INSERT INTO `GameUser` VALUES(12, 29, 0, 0.927);
INSERT INTO `GameUser` VALUES(12, 30, 1, 0.5);
INSERT INTO `GameUser` VALUES(13, 29, 0, 0.993);
INSERT INTO `GameUser` VALUES(13, 30, 1, 0.5);
INSERT INTO `GameUser` VALUES(14, 30, 0, 0.617);
INSERT INTO `GameUser` VALUES(14, 29, 1, 0.5);
INSERT INTO `GameUser` VALUES(15, 29, 0, 1);
INSERT INTO `GameUser` VALUES(15, 30, 1, 0.362);
INSERT INTO `GameUser` VALUES(16, 30, 0, 0.859);
INSERT INTO `GameUser` VALUES(16, 29, 1, 0.5);
INSERT INTO `GameUser` VALUES(17, 29, 0, 1);
INSERT INTO `GameUser` VALUES(17, 30, 1, 0.209);
INSERT INTO `GameUser` VALUES(18, 29, 0, 1);
INSERT INTO `GameUser` VALUES(18, 30, 1, 0.168);
INSERT INTO `GameUser` VALUES(19, 29, 0, 1);
INSERT INTO `GameUser` VALUES(19, 30, 1, 0.221);
INSERT INTO `GameUser` VALUES(20, 30, 0, 1);
INSERT INTO `GameUser` VALUES(20, 29, 1, 0.207);
INSERT INTO `GameUser` VALUES(26, 29, 1, 0.5);
INSERT INTO `GameUser` VALUES(26, 30, 0, 0.94);

-- --------------------------------------------------------

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
-- Dumping data for table `User`
--

INSERT INTO `User` VALUES(30, 'john', 'doe', 'Python', 3, 23.2627, 2.63163);
INSERT INTO `User` VALUES(29, 'truell20', '***REMOVED***', 'Python', 3, 26.7373, 2.63163);

-- --------------------------------------------------------

--
-- Table structure for table `Worker`
--

CREATE TABLE `Worker` (
  `apiKey` smallint(5) unsigned NOT NULL,
  `ipAddress` varchar(32) NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `Worker`
--

INSERT INTO `Worker` VALUES(1, '::1');
INSERT INTO `Worker` VALUES(1, '127.0.0.1');
INSERT INTO `Worker` VALUES(1, '67.247.36.53');
INSERT INTO `Worker` VALUES(1, '104.131.205.10');
