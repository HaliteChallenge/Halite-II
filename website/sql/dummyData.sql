DELETE FROM User;
DELETE FROM Organization;

INSERT INTO Organization (organizationID, organizationName)
VALUES (1000, 'Cornell University'),
       (1001, 'Basha High School'),
       (1002, 'Two Sigma');

INSERT INTO User (userID, oauthID, oauthProvider, username, email, compileStatus, isRunning, language, organizationID)
VALUES (2609, 0, 0, 'erdman',         'abc@gmail.com', 0, 0, 'Python', 1000),
       (1017, 1, 0, 'djma',           'abc@gmail.com', 0, 0, 'Java',   1001),
       (3063, 2, 0, 'daniel-shields', 'abc@gmail.com', 0, 0, 'Python', 1002);
