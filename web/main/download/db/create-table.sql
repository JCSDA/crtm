CREATE TABLE crtm_register
(
   uid                 int NOT NULL PRIMARY KEY, 
   date_time           varchar(19) NOT NULL,
   fname               varchar(50) NOT NULL, 
   lname               varchar(50) NOT NULL, 
   email               varchar(60) NOT NULL,
   organization        varchar(100) NOT NULL,
   purpose             mediumtext,
   mailinglist         varchar(3) NOT NULL
);

CREATE TABLE crtm_download
(
   did                 int AUTO_INCREMENT NOT NULL PRIMARY KEY, 
   uid                 int NOT NULL, 
   date_time           varchar(19) NOT NULL,
   IP                  varchar(28) NOT NULL, 
   user_agent          varchar(255)
);

