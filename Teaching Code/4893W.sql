/* We use the sakila schema that came with MySQK */
/* I want to find films of actors whose last names start with A or F */
SELECT 
  a.actor_id as 'ID',
  b.last_name as 'Last Name',
  c.title as 'Film'
FROM
  sakila.film_actor a
  inner join sakila.actor b on a.actor_id = b.actor_id
  inner join sakila.film c on a.film_id = c.film_id
  where b.last_name like 'A%' or b.last_name like 'G%'
  order by ID
