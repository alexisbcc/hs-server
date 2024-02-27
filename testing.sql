CREATE USER hs_datastore_admin WITH PASSWORD 'jw8s0F4' SUPERUSER;
CREATE DATABASE hsdatastore WITH OWNER=hs_datastore_admin;

CREATE TABLE person (
	uid serial PRIMARY KEY,
	first_name VARCHAR ( 50 ) NOT NULL,
	last_name VARCHAR ( 50 ) NOT NULL,
	email VARCHAR ( 255 ) UNIQUE NOT NULL,
	registration_date TIMESTAMPTZ NOT NULL
);


INSERT INTO person VALUES
(DEFAULT, 'Banana', 'Sanchez', 'bsanchez@gmail.com', current_timestamp),
(DEFAULT, 'Durian', 'Martinez', 'dmartinez@gmail.com', current_timestamp),
(DEFAULT, 'Guineo', 'Castro', 'gcastro@gmail.com', current_timestamp),
(DEFAULT, 'Mango', 'Juarez', 'mjuarez@gmail.com', current_timestamp),
(DEFAULT, 'Papaya', 'Perez', 'pperez@gmail.com', current_timestamp);


INSERT INTO person VALUES (DEFAULT,'Fruta','Magica','fmagica@gmail.com',current_timestamp) RETURNING uid;
