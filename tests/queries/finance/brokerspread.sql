CREATE STREAM bids(t FLOAT, id INT, broker_id INT, volume FLOAT, price FLOAT)
  FROM FILE 'examples/data/finance.csv'
  LINE DELIMITED orderbook (book := 'bids', brokers := '10', 
                            deterministic := 'yes');

SELECT x.broker_id, SUM((x.volume * x.price) - (y.volume * y.price)) AS bsp
FROM   bids x, bids y
WHERE  x.broker_id = y.broker_id AND x.t > y.t
GROUP BY x.broker_id;
