#!/usr/bin/env python3

#
# Unit test k3-mosaic execution.
#
# Usage:
#    ./auto_test.py [options]


import argparse
import os
import sys

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
    parser.add_argument('-n', '--nodes', action='store', type=int, dest='num_nodes',
                        default=1, help="Number of data nodes")
    parser.add_argument('-s', '--switches', action='store', type=int, dest='num_switches',
                        default=1, help="Number of switch nodes")
    parser.add_argument('-o', '--order', action='store', dest='order_file',
                        default=None, help="Use an order file instead of creating a trace")
    parser.add_argument('-v', '--verbose', action='store_true', dest='verbose',
                        default=False, help="See test results in detail")
    parser.add_argument('-m', '--map', action='store', dest='map_type',
                        default='set', help="Use vmap/multi maps")
    parser.add_argument('--no-new', action='store_false', dest='new_k3',
                        default=True, help="Create k3new file")
    parser.add_argument('--no-interp', action='store_false', dest='run_interp',
                        default=True, help="Run the interpreter")
    parser.add_argument('--workdir', action='store', type=str, dest='workdir',
                        default="temp", help="Directory to store work files")
    parser.add_argument('--gc-interval', action='store',
                        default=20000, type=int, help="Change gc interval (ms)")
    parser.add_argument('--msg-interval', action='store',
                        default=2, type=int, help="Change message interval (ms)")
    parser.add_argument('--no-log', action='store_false', dest='logging',
                        default=True, help="Disable logging")
    parser.add_argument('--no-multiidx', action='store_false', dest='multiidx',
                        default=True, help="Disable multi-index maps")
    parser.add_argument('--filemux', action='store_true', dest='filemux',
                        default=False, help="Enable filemux newprint")
    parser.add_argument('--safe-writes', action='store_true', dest='safe_writes',
                        default=False, help="Enable safe array writes")
    parser.add_argument('--overlap-factor', dest='map_overlap_factor',
                        default=None, help="Reduce layout of maps on nodes")
    parser.add_argument('--batch-size', dest='batch_size',
                        default=None, help="Size of batches")
    parser.add_argument('--debug', default=False, action='store_true', help="Debug output")
    parser.add_argument('--no-opt-route', default=True, action='store_false', dest='opt_route', help='disable optimized routing')
    parser.add_argument('--dump-info', default=False, action='store_true', help="Output for route generation")
    parser.add_argument('--print-warmup', dest='print_warmup', default=False, action='store_true', help="Print the warmup program")
    parser.add_argument('--trace', default=False, dest='do_trace', action='store_true', help="Tracing")
    parser.add_argument('--no-gen-deletes', action='store_false', dest='gen_deletes',
                        default=True, help="Don't generate delete triggers")
    parser.add_argument('--no-gen-correctives', action='store_false', dest='gen_correctives',
                        default=True, help="Don't generate correctives")
    parser.add_argument('--no-gen-single-vid', action='store_false', dest='gen_single_vid',
                        default=True, help="Don't generate non-isobatch mode")
    parser.add_argument('--run-correctives', action='store_true', dest='run_correctives',
                        default=False, help="Run with correctives")
    parser.add_argument('--no-run-isobatch', action='store_false', dest='run_isobatch',
                        default=True, help="Don't run in isobatch mode")
    args = parser.parse_args()

    test_list = []

    # handle a test list
    if args.test_list_name:
        check_exists('test list', args.test_list_name)
        # we need to add the file path to each file in the list
        list_path, _ = os.path.split(args.test_list_name)
        with open(args.test_list_name, 'r') as f:
            f_list = f.readlines()
            test_list += list(map(lambda x: os.path.join(list_path, x).replace('\n', ''),
                                  f_list))

    # take all files in a test_path
    if args.test_path:
        for root, dirs, files in os.walk(args.test_path):
            for f in files:
                if os.path.splitext(f)[1] == ".sql":
                    test_list += [os.path.join(root, f)]

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
        print("[{0}] Testing {1}... ".format(index, test_file), end="")
        if verbose:
            print("")
        if not args.distributed:
            res = dist_test.run(test_file, verbose=verbose, distrib=False, logging=args.logging)
        else:
            res = dist_test.run(test_file,
                                num_nodes=args.num_nodes,
                                num_switches=args.num_switches,
                                order_file=args.order_file,
                                debug=args.debug,
                                verbose=verbose,
                                distrib=True,
                                new_k3=args.new_k3,
                                gen_correctives=args.gen_correctives,
                                gen_deletes=args.gen_deletes,
                                gen_single_vid=args.gen_single_vid,
                                run_correctives=args.run_correctives,
                                run_isobatch=args.run_isobatch,
                                map_type=args.map_type,
                                workdir=args.workdir,
                                run_interp=args.run_interp,
                                gc_interval=args.gc_interval,
                                msg_interval=args.msg_interval,
                                logging=args.logging,
                                filemux=args.filemux,
                                safe_writes=args.safe_writes,
                                map_overlap_factor=args.map_overlap_factor,
                                batch_size=args.batch_size,
                                opt_route=args.opt_route,
                                do_trace=args.do_trace,
                                dump_info=args.dump_info,
                                print_warmup=args.print_warmup
                                )
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
        print("Passed {0} test(s)".format(len(test_list)))

if __name__ == '__main__':
    run()
