DELETE FROM User;

INSERT INTO User (userID, username, email, compileStatus, isRunning, language, organization) VALUES (2609, 'erdman', 'abc@gmail.com', 0, 1, 'Python', 'Other'), (1017, 'djma', 'abc@gmail.com', 0, 1, 'Java', 'Other'), (3063, 'daniel-shields', 'abc@gmail.com', 0, 1, 'Python', 'Other');
INSERT INTO Worker (apiKey, ipAddress) VALUES (1, "127.0.0.1");
