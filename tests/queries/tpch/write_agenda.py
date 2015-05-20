#!/usr/bin/env python
# Parses a unified csv from a TPCH query and rewrites in a wide-schema agenda
# format.

from csv import DictWriter
from itertools import chain

from sys import argv

schemas = {
    'LINEITEM': ['orderkey', 'partkey', 'suppkey', 'linenumber', 'quantity',
                 'extendedprice', 'discount', 'tax', 'returnflag', 'linestatus',
                 'shipdate', 'commitdate', 'receiptdate', 'shipinstruct',
                 'shipmode', 'comment'],

    'ORDERS': ['orderkey', 'custkey', 'orderstatus', 'totalprice', 'orderdate',
               'orderpriority', 'clerk', 'shippriority', 'comment'],

    'CUSTOMER': ['custkey', 'name', 'address', 'nationkey', 'phone', 'acctbal',
                 'mktsegment', 'comment'],

    'NATION': ['nationkey', 'name', 'regionkey', 'comment'],

    'PARTSUPP': ['partkey', 'suppkey', 'availqty', 'supplycost', 'comment'],

    'PART': ['partkey', 'name', 'mfgr', 'brand', 'type', 'size', 'container', 'retailprice',
             'comment'],

    'REGION': ['regionkey', 'name', 'comment'],

    'SUPPLIER': ['suppkey', 'name', 'address', 'nationkey', 'phone', 'acctbal', 'comment'],
}

if __name__ == '__main__':
    main_schema = ['relation', 'event'] + list(sorted(set(chain(*schemas.values()))))
    print main_schema
    print ''

    offset = 2 # for multiplexer and event type

    mapping = {k: [main_schema.index(s) + offset for s in ss] for k, ss in schemas.iteritems()}

    print mapping
