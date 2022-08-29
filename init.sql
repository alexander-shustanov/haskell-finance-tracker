CREATE TABLE Account (
    id bigserial primary key,
    name varchar(255) not null,
    currency varchar(10) not null,
    sum double precision not null
);

INSERT INTO Account(name, currency, sum) VALUES ('Cash USD', 'USD', 100);
INSERT INTO Account(name, currency, sum) VALUES ('Cash RUB', 'RUB', 200);
INSERT INTO Account(name, currency, sum) VALUES ('Coinbase BTC', 'BTC', 0.5);
INSERT INTO Account(name, currency, sum) VALUES ('Binance BTC', 'BTC', 0.5);
INSERT INTO Account(name, currency, sum) VALUES ('Bank Account RUB', 'RUB', 500);

CREATE TABLE Tx (
    id bigserial primary key,
    discriminator char not null,
    description varchar(255) not null,
    -- стоило бы использовать более подходящий тип для даты,
    -- но у меня возникли сложности с конвертацией между стандартными типами дат из haskell и simple postgresql
    ts timestamptz default now() not null,

    fromId bigint,
    fromAmount double precision,
    
    toId bigint,
    toAmount double precision
);

INSERT INTO Tx(discriminator, description, fromId, fromAmount) VALUES ('O', 'Laundery',  2, 200);

INSERT INTO Tx(discriminator, description, toId, toAmount) VALUES ('I', 'Salary',  1, 10000);