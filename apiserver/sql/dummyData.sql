SET sql_mode = 'ANSI_QUOTES';

DELETE FROM "user";
DELETE FROM organization;

INSERT INTO organization (id, organization_name)
VALUES (1000, 'Cornell University'),
       (1001, 'Basha High School'),
       (1002, 'Two Sigma');

INSERT INTO organization_email_domain (organization_id, domain)
VALUES (1000, 'cornell.edu'),
       (1001, 'cusd80.com'),
       (1001, 'chandlerschools.com'),
       (1002, 'twosigma.com');

INSERT INTO "user" (id, oauth_id, oauth_provider, username, email, player_level, is_active, organization_id)
VALUES (2609, 0, 0, 'erdman',         'abc@gmail.com', 'Professional',  TRUE, 1000),
       (1017, 1, 0, 'djma',           'abc@gmail.com', 'Undergraduate', TRUE, 1001),
       (1018, 1, 0, 'lidavidm',       'abc@gmail.com', 'Graduate',      TRUE, 1000),
       (1019, 1, 0, 'mmauer',         'abc@gmail.com', 'High School',   TRUE, 1000),
       (1020, 1, 0, 'srastogi',       'abc@gmail.com', 'Professional',  TRUE, 1000),
       (1021, 1, 0, 'jclapauch',      'abc@gmail.com', 'Professional',  TRUE, 1002),
       (1022, 1, 0, 'azhu',           'abc@gmail.com', 'Professional',  TRUE, 1002),
       (3063, 2, 0, 'daniel-shields', 'abc@gmail.com', 'Professional',  TRUE, 1002);
