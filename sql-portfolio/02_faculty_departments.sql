-- =========================
-- Faculty + Departments
-- =========================

-- Basic filters

SELECT *
FROM school_faculty
WHERE gender = 'male' AND hire_date > '2014-01-01' AND salary > 10000;

SELECT *
FROM school_faculty
WHERE termination_date IS NULL AND first_name LIKE 'J%';

SELECT first_name, last_name, birth_date
FROM school_faculty
WHERE termination_date IS NULL
ORDER BY birth_date ASC
LIMIT 1;

SELECT DISTINCT position
FROM school_faculty 
WHERE salary > 12000;


-- Aggregations

SELECT MIN(hire_date) AS hiring_date
FROM school_faculty;

SELECT MAX(birth_date) AS birth_date
FROM school_faculty
WHERE termination_date IS NOT NULL;  

SELECT MAX(salary) AS max_salary
FROM school_faculty
WHERE termination_date IS NULL and position ='teacher';

SELECT AVG(salary) AS average_salary
FROM school_faculty
WHERE hire_date > '2015-01-01';

SELECT COUNT(id) AS number_of_staff
FROM school_faculty
WHERE termination_date IS NULL AND position ='teacher' AND salary >= 9000;

SELECT COUNT(id) AS number_of_departments
FROM departments
where name like 'S%';


-- Joins

SELECT school_faculty.first_name, school_faculty.last_name, departments.name AS department_name
FROM school_faculty
JOIN departments
  ON school_faculty.department_id = departments.id
WHERE departments.id in (2,7);

SELECT school_faculty.first_name, school_faculty.last_name, departments.name
FROM school_faculty
JOIN departments
  ON school_faculty.department_id = departments.id
WHERE school_faculty.last_name LIKE '%n%' AND school_faculty.department_id IS NOT NULL;

SELECT departments.name as department_name, COUNT(school_faculty.department_id) AS number_of_employees
FROM school_faculty
JOIN departments
  ON school_faculty.department_id = departments.id
GROUP BY departments.name;

SELECT departments.name AS department_name, MAX(school_faculty.salary) AS max_salary
FROM school_faculty
JOIN departments
  ON school_faculty.department_id = departments.id
GROUP BY departments.name; 

SELECT departments.name AS department_name, AVG(school_faculty.salary) AS average_salary, COUNT(school_faculty.department_id) AS number_of_employees
FROM school_faculty
JOIN departments
  ON school_faculty.department_id  = departments.id 
GROUP BY department_name;


-- Formatting

SELECT EXTRACT(year FROM hire_date) AS year, COUNT(id) AS number_of_staff
FROM school_faculty
GROUP BY year;

SELECT first_name, last_name, EXTRACT(day FROM termination_date) AS Day, EXTRACT(month FROM termination_date) AS Month, EXTRACT(year FROM termination_date) AS Year 
FROM school_faculty
WHERE termination_date IS NOT NULL;
