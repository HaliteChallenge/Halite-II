DELETE FROM User;
DELETE FROM Organization;

INSERT INTO Organization (organizationID, organizationName)
VALUES (1000, 'Cornell University'),
       (1001, 'Basha High School'),
       (1002, 'Two Sigma');

INSERT INTO User (userID, oauthID, oauthProvider, username, email, compileStatus, isRunning, language, organizationID)
VALUES (2609, 0, 0, 'erdman',         'abc@gmail.com', 0, 0, 'Python', 1000),
       (1017, 1, 0, 'djma',           'abc@gmail.com', 0, 0, 'Java',   1001),
       (1018, 1, 0, 'lidavidm',       'abc@gmail.com', 0, 0, 'Java',   1000),
       (1019, 1, 0, 'mmauer',         'abc@gmail.com', 0, 0, 'Java',   1000),
       (1020, 1, 0, 'srastogi',       'abc@gmail.com', 0, 0, 'Java',   1000),
       (1021, 1, 0, 'jclapauch',      'abc@gmail.com', 0, 0, 'Java',   1002),
       (1022, 1, 0, 'azhu',           'abc@gmail.com', 0, 0, 'Java',   1002),
       (3063, 2, 0, 'daniel-shields', 'abc@gmail.com', 0, 0, 'Python', 1002);
