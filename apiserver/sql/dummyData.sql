SET sql_mode = 'ANSI_QUOTES';

DELETE FROM "user";
DELETE FROM organization;

INSERT INTO organization (id, organization_name, kind)
VALUES (1000, 'Cornell University', 'University'),
       (1001, 'Massachusetts Institute of Technology', 'University'),
       (1002, 'Two Sigma', 'Company');

INSERT INTO organization_email_domain (organization_id, domain)
VALUES (1000, 'cornell.edu'),
       (1001, 'mit.edu'),
       (1002, 'twosigma.com');

INSERT INTO "user" (id, oauth_id, oauth_provider, username, email, player_level, is_active, organization_id)
VALUES (1016, 0, 0, 'erdman',         'abc@gmail.com', 'Professional',  TRUE, 1000),
       (1017, 1, 0, 'djma',           'abc@gmail.com', 'Undergraduate', TRUE, 1001),
       (1018, 1, 0, 'lidavidm',       'abc@gmail.com', 'Graduate',      TRUE, 1000),
       (1019, 1, 0, 'mmauer',         'abc@gmail.com', 'High School',   TRUE, 1000),
       (1020, 1, 0, 'srastogi',       'abc@gmail.com', 'Professional',  TRUE, 1000),
       (1021, 1, 0, 'jclapauch',      'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1022, 1, 0, 'aaaa',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1023, 1, 0, 'aaab',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1024, 1, 0, 'aaac',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1025, 1, 0, 'aaad',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1026, 1, 0, 'aaae',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1027, 1, 0, 'aaaf',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1028, 1, 0, 'aaag',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1029, 1, 0, 'aaah',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1030, 1, 0, 'aaai',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1031, 1, 0, 'aaaj',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1032, 1, 0, 'aaak',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1033, 1, 0, 'aaal',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1034, 1, 0, 'aaam',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1035, 1, 0, 'aaan',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1036, 1, 0, 'aaao',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1037, 2, 0, 'daniel-shields', 'abc@gmail.com', 'Professional',  TRUE, 1002);

INSERT INTO bot (user_id, id, compile_status, score)
VALUES (1016, 0, 'Successful', 0),
       (1017, 0, 'Successful', 1),
       (1018, 0, 'Successful', 2),
       (1019, 0, 'Successful', 3),
       (1020, 0, 'Successful', 4),
       (1021, 0, 'Successful', 5),
       (1022, 0, 'Successful', 6),
       (1023, 0, 'Successful', 7),
       (1024, 0, 'Successful', 8),
       (1025, 0, 'Successful', 9),
       (1026, 0, 'Successful', 10),
       (1027, 0, 'Successful', 11),
       (1028, 0, 'Successful', 12),
       (1029, 0, 'Successful', 13),
       (1030, 0, 'Successful', 14),
       (1031, 0, 'Successful', 15),
       (1032, 0, 'Successful', 16),
       (1033, 0, 'Successful', 17),
       (1034, 0, 'Successful', 18),
       (1035, 0, 'Successful', 19),
       (1036, 0, 'Successful', 20),
       (1037, 0, 'Successful', 21);