CREATE TABLE IF NOT EXISTS customer
  ( id serial
  , name text NOT NULL
  );
  
INSERT INTO customer (name) 
  VALUES ('John Smith');