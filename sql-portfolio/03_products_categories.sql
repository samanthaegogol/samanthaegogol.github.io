-- =========================
-- Products + Categories
-- =========================

-- Joins

SELECT SUM(product_list.amount) AS total_amount
FROM product_list
JOIN category_list
  ON product_list.category_id = category_list.id
WHERE category_list.name IN ('Seafood');

SELECT category_list.name
FROM product_list
JOIN category_list
  ON product_list.category_id = category_list.id
WHERE product_list.price < 300
GROUP BY category_list.name;

SELECT category_list.name AS category_name, MIN(product_list.price) AS min_price
FROM product_list
JOIN category_list
  ON product_list.category_id = category_list.id
GROUP BY category_list.name;

SELECT category_list.name AS category_name, COUNT(product_list.category_id) AS number_of_products
FROM product_list
JOIN category_list
  ON product_list.category_id  = category_list.id
WHERE category_list.name IN ('Meat','Grains')
GROUP BY category_list.name;


-- Formatting

SELECT CONCAT(product_list.name, ' - ', product_list.amount, ' units') AS product_information
FROM product_list
JOIN category_list
  ON product_list.category_id = category_list.id
WHERE category_list.name IN('Beverages'); 
