-- =========================
-- Students + Grades
-- =========================

-- Basic filters

SELECT first_name, last_name, birth_date
FROM students 
WHERE birth_date < '2003-01-01';

SELECT first_name, last_name, birth_date
FROM students
WHERE first_name LIKE 'A%';

SELECT *
FROM students 
WHERE first_name LIKE '_____';

SELECT *
FROM grades
WHERE subject LIKE '%i%';

SELECT first_name, last_name, birth_date, city
FROM students
WHERE first_name IN('David', 'Emily', 'Amanda');

SELECT first_name, last_name, birth_date, city
FROM students 
WHERE birth_date NOT BETWEEN '2005-08-28' and '2007-06-12';

SELECT *
FROM students
ORDER BY birth_date desc
LIMIT 3;

SELECT DISTINCT city
FROM students;

SELECT last_name
FROM students
WHERE city != 'New York'
ORDER BY last_name DESC
LIMIT 1;


-- Aggregations

SELECT COUNT(id) AS number_of_students
FROM students
WHERE city IN ('Buffalo');

SELECT COUNT(id) AS number_of_students
FROM students
WHERE father_phone IS NOT NULL;



-- Joins

SELECT grades.subject AS subject, AVG(grades.grade) AS average_grade
FROM grades
JOIN students
  ON grades.student_id = students.id
WHERE students.city IN ('New York')
GROUP BY grades.subject;

SELECT grades.subject AS subject, AVG(grades.grade) AS average_grade
FROM grades
JOIN students
  ON grades.student_id = students.id
WHERE students.first_name IN ('Sarah') AND students.last_name IN ('Wilson')
GROUP BY grades.subject
ORDER BY average_grade DESC;

SELECT students.first_name AS first_name, students.last_name AS last_name, AVG(grades.grade) AS average_grade
FROM grades
JOIN students
  ON grades.student_id = students.id 
GROUP BY students.id; 

SELECT grades.subject AS subject, grades.year AS year, AVG(grades.grade) AS average_grade
FROM grades
JOIN students
  ON grades.student_id = students.id 
WHERE students.birth_date > '2005-01-01'
GROUP BY grades.subject, grades.year
ORDER BY subject, year DESC, average_grade DESC;


-- Formatting

SELECT COUNT(id) AS number_of_students
FROM students 
  WHERE EXTRACT(month FROM current_date) = EXTRACT(month FROM birth_date);

SELECT CONCAT(s.first_name, ' ', s.last_name,': ', g.subject, ' - ', AVG(g.grade)) AS student_grade 
FROM grades AS g
JOIN students AS s 
  ON g.student_id = s.id 
WHERE s.id = 3
GROUP BY s.first_name, s.last_name, g.subject
ORDER BY g.subject;