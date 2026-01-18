-- =========================
-- SCHEMA: SQL Portfolio Datasets
-- =========================

-- -------------------------
-- DEPARTMENTS
-- -------------------------
CREATE TABLE departments (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

-- -------------------------
-- FACULTY (only termination_date can be NULL)
-- -------------------------
CREATE TABLE school_faculty (
  id INTEGER PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  birth_date DATE NOT NULL,
  gender TEXT NOT NULL,
  phone TEXT NOT NULL,
  position TEXT NOT NULL,
  salary INTEGER NOT NULL,
  hire_date DATE NOT NULL,
  termination_date DATE,                -- the ONLY nullable column
  department_id INTEGER NOT NULL,

  FOREIGN KEY (department_id) REFERENCES departments(id)
);

-- -------------------------
-- STUDENTS (only parent phones can be NULL)
-- -------------------------
CREATE TABLE students (
  id INTEGER PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  birth_date DATE NOT NULL,
  gender TEXT NOT NULL,
  mother_phone TEXT,                    -- nullable allowed
  father_phone TEXT,                    -- nullable allowed
  city TEXT NOT NULL
);

-- -------------------------
-- GRADES (joinable to students)
-- -------------------------
CREATE TABLE grades (
  id INTEGER PRIMARY KEY,
  student_id INTEGER NOT NULL,
  subject TEXT NOT NULL,
  year INTEGER NOT NULL,
  semester TEXT NOT NULL,
  grade INTEGER NOT NULL,

  FOREIGN KEY (student_id) REFERENCES students(id)
);

-- -------------------------
-- CATEGORIES (general food groups)
-- -------------------------
CREATE TABLE category_list (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

-- -------------------------
-- PRODUCTS (joinable to category_list)
-- -------------------------
CREATE TABLE product_list (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  amount INTEGER NOT NULL,
  price INTEGER NOT NULL,
  category_id INTEGER NOT NULL,

  FOREIGN KEY (category_id) REFERENCES category_list(id)
);
