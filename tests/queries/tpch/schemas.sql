CREATE STREAM LINEITEM (
        orderkey       INT,
        partkey        INT,
        suppkey        INT,
        linenumber     INT,
        quantity       DECIMAL,
        extendedprice  DECIMAL,
        discount       DECIMAL,
        tax            DECIMAL,
        returnflag     CHAR(1),
        linestatus     CHAR(1),
        shipdate       DATE,
        commitdate     DATE,
        receiptdate    DATE,
        shipinstruct   CHAR(25),
        shipmode       CHAR(10),
        comment        VARCHAR(44)
    )
  FROM FILE 'data/tpch/lineitem.csv'
  LINE DELIMITED CSV (delimiter := '|');

CREATE STREAM ORDERS (
        orderkey       INT,
        custkey        INT,
        orderstatus    CHAR(1),
        totalprice     DECIMAL,
        orderdate      DATE,
        orderpriority  CHAR(15),
        clerk          CHAR(15),
        shippriority   INT,
        comment        VARCHAR(79)
    )
  FROM FILE 'data/tpch/orders.csv'
  LINE DELIMITED CSV (delimiter := '|');

CREATE STREAM PART (
        partkey      INT,
        name         VARCHAR(55),
        mfgr         CHAR(25),
        brand        CHAR(10),
        type         VARCHAR(25),
        size         INT,
        container    CHAR(10),
        retailprice  DECIMAL,
        comment      VARCHAR(23)
    )
  FROM FILE 'data/tpch/part.csv'
  LINE DELIMITED CSV (delimiter := '|');

CREATE STREAM CUSTOMER (
        custkey      INT,
        name         VARCHAR(25),
        address      VARCHAR(40),
        nationkey    INT,
        phone        CHAR(15),
        acctbal      DECIMAL,
        mktsegment   CHAR(10),
        comment      VARCHAR(117)
    )
  FROM FILE 'data/tpch/customer.csv'
  LINE DELIMITED CSV (delimiter := '|');

CREATE STREAM SUPPLIER (
        suppkey      INT,
        name         CHAR(25),
        address      VARCHAR(40),
        nationkey    INT,
        phone        CHAR(15),
        acctbal      DECIMAL,
        comment      VARCHAR(101)
    )
  FROM FILE 'data/tpch/supplier.csv'
  LINE DELIMITED CSV (delimiter := '|');

CREATE STREAM PARTSUPP (
        partkey      INT,
        suppkey      INT,
        availqty     INT,
        supplycost   DECIMAL,
        comment      VARCHAR(199)
    )
  FROM FILE 'data/tpch/partsupp.csv'
  LINE DELIMITED CSV (delimiter := '|');

CREATE TABLE NATION (
        nationkey    INT,
        name         CHAR(25),
        regionkey    INT,
        comment      VARCHAR(152)
    )
  FROM FILE 'data/tpch/nation.csv'
  LINE DELIMITED CSV (delimiter := '|');

CREATE TABLE REGION (
        regionkey    INT,
        name         CHAR(25),
        comment      VARCHAR(152)
    )
  FROM FILE 'data/tpch/region.csv'
  LINE DELIMITED CSV (delimiter := '|');

CREATE STREAM AGENDA (
        src           VARCHAR,
        event         INT,
        acctbal       DECIMAL,
        address       VARCHAR(40),
        availqty      INT,
        brand         CHAR(10),
        clerk         CHAR(15),
        comment       VARCHAR(199),
        commitdate    DATE,
        container     CHAR(10),
        custkey       INT,
        discount      DECIMAL,
        extendedprice DECIMAL,
        linenumber    INT,
        linestatus    CHAR(1),
        mfgr          CHAR(25),
        mktsegment    CHAR(10),
        name          VARCHAR(55),
        nationkey     INT,
        orderdate     DATE,
        orderkey      INT,
        orderpriority CHAR(15),
        orderstatus   CHAR(1),
        partkey       INT,
        phone         CHAR(15),
        quantity      DECIMAL,
        receiptdate   DATE,
        regionkey     INT,
        retailprice   DECIMAL,
        returnflag    CHAR(1),
        shipdate      DATE,
        shipinstruct  CHAR(25),
        shipmode      CHAR(10),
        shippriority  INT,
        size          INT,
        suppkey       INT,
        supplycost    DECIMAL,
        tax           DECIMAL,
        totalprice    DECIMAL,
        type          VARCHAR(25)
    )
  FROM FILE 'data/tpch/agenda.csv'
  LINE DELIMITED AGENDA (delimiter := '|', mux := '0', event := '1', mapping :=
    'CUSTOMER:12,19,5,20,26,4,18,9;
     LINEITEM:22,25,37,15,27,14,13,39,31,16,32,10,28,33,34,9;
     PARTSUPP:25,37,6,38,9;
     PART:25,19,17,7,41,36,11,30,9;
     SUPPLIER:37,19,5,20,26,4,9;
     ORDERS:22,12,24,40,21,23,8,35,9');

