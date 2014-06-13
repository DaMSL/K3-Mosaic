#!/usr/bin/env python3

#
# Unit test k3-mosaic execution.
#
# Usage:
#    ./auto_test.py [options]

import argparse
import os
import sys
import local_test
import dist_test

def check_exists(name, path):
    if not os.path.exists(path):
        raise Exception("No {0} found at {1}".format(name, path))

def run():
    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--path', action='store', dest='test_path',
                        default=None, help="Do all tests in a given directory")
    parser.add_argument('-f', '--file', action='append', dest='test_name',
                        help="Run a specific test file")
    parser.add_argument('-l', '--list', action='store', dest='test_list_name',
                        default=None, help="Execute tests from a list file")
    parser.add_argument('-d', '--dist', action='store_true', dest='distributed',
                        default=False, help="Distributed test")
    parser.add_argument('-n', '--nodes', action='store', dest='num_nodes',
                        default=1, help="Number of data nodes")
    parser.add_argument('-q', '--queue', action='store', dest='queue_type',
                        default="global", help="Queue type: global/trigger/node")
    parser.add_argument('-r', '--force', action='store', dest='force_correctives',
                        default=False, help="Force correctives")
    parser.add_argument('-x', '--shuffle', action='store', dest='shuffle',
                        default=False, help="Shuffle the queues")
    parser.add_argument('-o', '--order', action='store', dest='order_file',
                        default=None, help="Use an order file instead of creating a trace")
    parser.add_argument('-v', '--verbose', action='store_true', dest='verbose',
                        default=False, help="See test results in detail")

    args = parser.parse_args()

    test_list = []

    # handle a test list
    if args.test_list_name:
        check_exists('test list', args.test_list_name)
        with open(args.test_list_name, 'r') as f:
            test_list = f.readlines()

    # take all files in a test_path
    if args.test_path:
        for root, dirs, files in os.walk(args.test_path):
            for f in files:
                if os.path.split(f)[1] == ".sql":
                    test_list += os.path.join(root, f)

    # handle test names from command line
    if args.test_name:
        test_list.extend(args.test_name)

    if len(test_list) == 0:
        parser.print_help()
        sys.exit(1)

    failed = 0
    verbose = args.verbose or len(test_list) == 1
    # run either one test or many tests
    for index, test_file in enumerate(test_list):
        print("[{0}] Testing {1}... ".format(index, test_file), end="", flush=True)
        if verbose:
            print("")
        if not args.distributed:
            res = local_test.run(test_file, verbose)
        else:
            res = dist_test.run(test_file,
                                args.num_nodes,
                                args.queue_type,
                                args.do_shuffle,
                                args.force_correctives,
                                args.order_file,
                                verbose)
        # check if a test failed
        if not res:
            print("[ERROR]")
            failed += 1
        else:
            print("[PASSED]")

    if failed > 0:
        print("Failed {0}/{1} tests".format(failed, len(test_list)))
        sys.exit(1)
    else:
        print("Passed {1} tests".format(len(test_list)))

if __name__ == '__main__':
    run()
