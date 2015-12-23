CREATE TABLE `Bot` (
  `botID` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `userID` mediumint(8) unsigned NOT NULL,
  `name` varchar(20) NOT NULL,
  PRIMARY KEY (`botID`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=2 ;

CREATE TABLE `User` (
  `userID` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(20) NOT NULL,
  `password` varchar(30) NOT NULL,
  PRIMARY KEY (`userID`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=31 ;