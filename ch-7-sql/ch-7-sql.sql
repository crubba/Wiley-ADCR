-- --------------------------------------------------------------
-- AUTOMATED DATA COLLECTION WITH R
-- SIMON MUNZERT, CHRISTIAN RUBBA, PETER MEISSNER, DOMINIC NYHUIS
--
-- CODE CHAPTER 7: SQL
-- SQL Code used in chapter 7 (7.3.xxx) 
-- to use with a MySQL database
-- go to http://dev.mysql.com/downloads/ to get community downloads
-- of DBMS and DBMS-management software
-- --------------------------------------------------------------


/* Status? */
SELECT VERSION();

-- ---------------------------------------
/* 7.3.2    Data control language – DCL */
-- ---------------------------------------


-- create DB
CREATE  DATABASE db1 ;

-- create users 
CREATE  USER 'tester'  IDENTIFIED  BY  '123456' ;
CREATE  USER 'tester2' IDENTIFIED  BY  '123456' ;
CREATE  USER 'tester3' IDENTIFIED  BY  '123456' ;
DROP      USER 'tester3' ;

-- grant user rights to specific areas
GRANT  ALL  ON  *.* TO  'tester2' ;
GRANT  ALL  ON  db1.* TO  'tester2' ;
GRANT  ALL  ON  db1.table1 TO  'tester2' ;

-- grant user rights to specific actions
GRANT  SELECT, INSERT  ON  *.* TO  'tester2' ;
GRANT  SELECT, INSERT  ON  *.* TO  'tester2' WITH  GRANT  OPTION ;

-- revoke user rights
REVOKE  ALL  ON  *.* FROM  'tester2' ;

-- clean up
DROP  USER 'tester2' ;



-- ---------------------------------------
/* 7.3.3    Data deﬁnition language – DDL */
-- ---------------------------------------

-- select database to use
USE db1;

-- CREATE TABLE
 CREATE  TABLE  birthdays (
	nameid INTEGER  NOT  NULL  AUTO_INCREMENT  ,
	firstname VARCHAR(100) NOT  NULL  ,
	lastname VARCHAR(100) NOT  NULL  ,
	birthday DATE  ,
	PRIMARY  KEY  (nameid)
) ;

 CREATE  TABLE  foodtypes (
	foodid INT  NOT  NULL  AUTO_INCREMENT,
	foodname VARCHAR(100) NOT  NULL,
	healthy INT,
	kcalp100g float,
	PRIMARY  KEY  (foodid)
);

CREATE  TABLE  foodranking (
	rankid INT  NOT  NULL  AUTO_INCREMENT  ,
	foodid INT  ,
	nameid INT  ,
	rank INT  NULL  ,
	PRIMARY  KEY  (rankid) ,
	FOREIGN  KEY  (foodid) REFERENCES  foodtypes (foodid) ON  UPDATE  CASCADE,
	FOREIGN  KEY  (nameid) REFERENCES  birthdays (nameid) ON  UPDATE  CASCADE    
) ;


-- ALTER TABLE
ALTER  TABLE  foodtypes ADD  COLUMN  dummy INT  ;
ALTER  TABLE  foodtypes MODIFY  COLUMN  dummy FLOAT  ;
ALTER  TABLE  foodtypes DROP  COLUMN  dummy ;


-- DROP TABLE
CREATE  TABLE  dummy (dcolumn INT) ;
DROP  TABLE  dummy ;

-- INSERT INTO 
INSERT  INTO  birthdays (firstname, lastname, birthday)
VALUES    ('Peter', 'Pascal', '1991-02-01'),
          ('Paul',  'Panini', '1992-03-02'),
          ('Mary',  'Meyer',  '1993-04-03') ;

INSERT  INTO  foodtypes (foodname, healthy,kcalp100g)
VALUES  ('spaghetti',    0, 0.158),
        ('hamburger',    0, 0.295),
        ('fruit salad',  1, 0.043),
        ('chocolate',    0, 0.546),
        ('fish fingers', 0, 0.290) ;

INSERT  INTO  foodranking (nameid, foodid, rank)
VALUES  (1, 1, 1),
        (1, 2, 2),
        (2, 3, 1),
        (3, 4, 1),
        (3, 5, 2),
        (3, 2, 3) ;

-- UPDATE
SET  SQL_SAFE_UPDATES = 0 ;
ALTER  TABLE  foodtypes ADD  COLUMN  highenergy INT  ;
UPDATE  foodtypes SET  highenergy=1 WHERE  kcalp100g >  0.2 ;
UPDATE  foodtypes SET  highenergy=0 WHERE  kcalp100g <= 0.2 ;
ALTER  TABLE    foodtypes DROP  COLUMN  highenergy ;

-- DELETE
INSERT  INTO  foodtypes (foodname, healthy, kcalp100g)
VALUES  ("Dominic's incredible pancakes", NULL, NULL) ;
DELETE  FROM  foodtypes WHERE  foodname = "Dominic's incredible pancakes" ;

-- SELECT
SELECT  * FROM  birthdays ;
SELECT  birthday FROM  birthdays ;
SELECT  firstname, birthday FROM  birthdays ;

-- JOIN
INSERT  INTO  birthdays (nameid,firstname,lastname,birthday)
VALUES  (10,"Donald","Docker","1934-06-09") ;

SELECT  * FROM  birthdays ;

SELECT  birthdays.nameid, firstname, lastname, birthday, foodid, rank
	FROM  birthdays
	INNER  JOIN  foodranking
	ON  birthdays.nameid=foodranking.nameid ;

SELECT  birthdays.nameid, firstname, lastname, birthday, foodid, rank
	FROM  birthdays
	LEFT  JOIN  foodranking
	ON  birthdays.nameid=foodranking.nameid ;

SELECT  firstname, rank, foodname FROM  birthdays
	INNER  JOIN  foodranking
	ON  birthdays.nameid = foodranking.nameid
	INNER  JOIN  foodtypes
	ON  foodranking.foodid = foodtypes.foodid  ;

DELETE  FROM  birthdays WHERE  firstname = 'Donald' ;



-- --------------------
/* 7.3.5    Clauses */
-- --------------------

-- WHERE and operators
SELECT  firstname, foodname, rank FROM  birthdays
	INNER  JOIN  foodranking ON  birthdays.nameid = foodranking.nameid
	INNER  JOIN  foodtypes ON  foodranking.foodid = foodtypes.foodid
	WHERE  rank >= 2  AND  firstname = 'Mary' ;


SELECT  firstname, foodname, healthy FROM  birthdays
	INNER  JOIN  foodranking ON  birthdays.nameid = foodranking.nameid
	INNER  JOIN  foodtypes ON  foodranking.foodid = foodtypes.foodid
	WHERE  (healthy = 1 OR  firstname < 'Peter') AND  firstname != 'Mary' ;


SELECT  firstname, lastname FROM  birthdays
	WHERE  firstname IN  ('Peter','Paul','Karl') ;

SELECT  firstname, lastname FROM  birthdays
	WHERE  firstname LIKE  '%er' OR  lastname LIKE  '%e_';

-- ORDER BY
SELECT  firstname FROM  birthdays ORDER  BY  firstname  ;
SELECT  firstname FROM  birthdays
	ORDER  BY  birthday DESC, firstname ASC  ;

-- GROUP BY
SELECT  firstname, COUNT(rank) FROM  birthdays
	INNER  JOIN  foodranking ON  birthdays.nameid = foodranking.nameid
	INNER  JOIN  foodtypes ON  foodranking.foodid = foodtypes.foodid
	GROUP  BY  birthdays.nameid ;

-- HAVING
SELECT  firstname, COUNT(rank) FROM  birthdays
	INNER  JOIN  foodranking ON  birthdays.nameid = foodranking.nameid
	INNER  JOIN  foodtypes ON  foodranking.foodid = foodtypes.foodid
	GROUP  BY  birthdays.nameid
	HAVING  COUNT(rank) > 1 ;



-- ----------------------------------------------
/* 7.3.6    Transaction control language – TCL */
-- ----------------------------------------------
START TRANSACTION  ;

INSERT  INTO  birthdays (firstname, lastname)
	VALUES  ('Simon', 'Sorcerer') ;

SELECT  firstname, lastname FROM  birthdays ;

ROLLBACK  ;

SELECT  firstname, lastname FROM  birthdays ;





