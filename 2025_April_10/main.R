install.packages("odbc")
library(odbc)

# con <- dbConnect(odbc::odbc(),dsn="SCION_DWH")
con <- dbConnect(MySQL(),user="root",password="Password123!",host="localhost",port=3306)

dbGetQuery(con=con,"SHOW databases")
dbGetQuery(con=con,"CREATE database SCION_DWH")
dbGetQuery(con=con,"SHOW databases")
dbGetQuery(con=con,"USE SCION_DWH")
dbGetQuery(con=con,"SHOW tables")
dbGetQuery(con=con,"CREATE TABLE employees (
           id INT PRIMARY KEY,
           first_name VARCHAR(50),
           last_name VARCHAR(50),
           position VARCHAR(50)
           )")
dbGetQuery(con=con,"SHOW tables")

dbGetQuery(con=con,"SELECT * FROM employees")

dbGetQuery(con=con,"INSERT INTO employees (id,first_name,last_name,position)
           VALUES (1,'Andrew','Smith','Genomic Statistician')")

dbGetQuery(con=con,"SELECT * FROM employees")

dbGetQuery(con=con,"INSERT INTO employees (id,first_name,last_name,position)
           VALUES (2,'Julia','Nelson','Genomic Epidemiologist')")

dbGetQuery(con=con,"SELECT * FROM employees")

dbGetQuery(con=con,"INSERT INTO employees (id,first_name,last_name,position)
           VALUES (3,'John','Doe','Genomic Epidemiologist')")

dbGetQuery(con=con,"SELECT * FROM employees WHERE position='Genomic Epidemiologist'")

dbGetQuery(con=con,"INSERT INTO employees (id,first_name,last_name,position)
           VALUES (4,'Sally','Doe','IT Specialist')")

dbGetQuery(con=con,"SELECT * FROM employees WHERE position LIKE 'Genomic%'")

dbGetQuery(con=con,"CREATE TABLE employee_details (
           id INT PRIMARY KEY,
           hire_date DATE,
           email VARCHAR(100),
           position VARCHAR(50),
           FOREIGN KEY (id) REFERENCES employees(id)
           )")

dbGetQuery(con=con,"show tables;")
dbGetQuery(con=con,"SELECT * FROM employee_details")

dbGetQuery(con=con,"INSERT INTO employee_details (id,hire_date,email)
           VALUES
           (2,'2021-12-31','jnelson@dph.sc.gov')
           ")

dbGetQuery(con=con,"SELECT * FROM employee_details")

df <- dbGetQuery(con=con,"SELECT e.id,e.first_name,e.last_name,ed.email FROM employees e INNER JOIN employee_details ed ON e.id = ed.id")

df






