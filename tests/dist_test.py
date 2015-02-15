#!/usr/bin/env python

#
# Test a distributed SQL query
#

import os
import six
import re
from utils import check_exists, check_error, print_system, concat

def run(target_file,
        num_nodes=1,
        queue_type="global",
        order_file=None,
        verbose=True,
        distrib=False,
        use_idx=False,
        enable_gc=True
        ):

    to_root = ".."
    script_path = os.path.abspath(os.path.dirname(__file__))
    target_file = os.path.abspath(target_file)
    root_path = os.path.join(script_path, to_root)
    dbtoaster_dir = os.path.join(root_path, "external/dbtoaster")
    dbtoaster_name = "bin/dbtoaster_release"
    dbtoaster = os.path.join(dbtoaster_dir, dbtoaster_name)
    k3o = os.path.join(root_path, "bin/k3")
    partmap_tool = os.path.join(root_path, "bin/partmap_tool")
    combine_tool = os.path.join(root_path, "bin/combine_data")

    saved_dir = os.path.abspath(os.path.curdir)
    trace_file = os.path.join(saved_dir, "temp.trace")
    m3_file = os.path.join(saved_dir, "temp.m3")
    k3_file = os.path.join(saved_dir, "temp.k3")
    k3_file2 = os.path.join(saved_dir, "temp2.k3")
    k3_file3 = os.path.join(saved_dir, "temp3.k3")
    k3dist_file = os.path.join(saved_dir, "temp.k3dist")
    data_file = os.path.join(saved_dir, "temp.data")
    error_file = os.path.join(saved_dir, "temp.err")
    part_file = os.path.join(saved_dir, "temp.part")
    output_file = os.path.join(saved_dir, "temp.out")

    check_exists("dbtoaster", dbtoaster)
    check_exists("k3o", k3o)
    if distrib:
        check_exists("partmap_tool", partmap_tool)

    # change to dbtoaster path (dbtoaster needs it)
    if verbose:
        six.print_("cd {0}".format(dbtoaster_dir))
    os.chdir(dbtoaster_dir)

    # run dbtoaster to get interpreted updates
    debug_cmd = ''
    debug_flags = ['PRINT-VERBOSE', 'LOG-INTERPRETER-UPDATES', 'LOG-INTERPRETER-TRIGGERS', 'LOG-M3']
    for f in debug_flags:
        debug_cmd += ' -d ' + f

    cmd = concat([dbtoaster_name, debug_cmd, target_file, ">", trace_file, "2>", error_file])
    print_system(cmd, verbose)
    if check_error(error_file, verbose):
        os.chdir(saved_dir)
        return False

    # run dbtoaster to get m3 file with distributed portion
    if distrib:
        src_lang = "distm3"
    else:
        src_lang = "m3"
    cmd = concat([dbtoaster_name, "-l", src_lang, "-d", "PRINT-VERBOSE", target_file, ">", m3_file, "2>", error_file])
    print_system(cmd, verbose)
    if check_error(error_file, verbose):
        os.chdir(saved_dir)
        return False

    # change directory back
    if verbose:
        six.print_("cd {0}".format(saved_dir))
    os.chdir(saved_dir)

    # create a single-site k3o file
    cmd = concat([k3o, "-p -i m3 -l k3", m3_file, ">", k3_file, "2>", error_file])
    print_system(cmd, verbose)
    if check_error(error_file, verbose) or check_error(k3_file, verbose, True):
        return False

    # get the files we're using
    s = open(k3_file, 'r').read()
    matches = re.findall(r'\("(.+)", csv\)', s)
    if matches is None:
        six.print_("failed to find file references")
        return False

    read_files = []
    for m in matches:
        read_files += [os.path.join(dbtoaster_dir, m)]

    load_path = concat(["--load_path", dbtoaster_dir])

    # execution diverges from here
    if distrib:
        # string for k3 distributed file creation: either use a trace file or an order file
        if not order_file:
            create_cmd = concat(["--trace", trace_file])
        else:
            create_cmd = concat(["--order", order_file])

        idx_cmd = "--use_idx" if use_idx else None
        gc_cmd  = "--gc" if enable_gc else None

        # create a k3 distributed file (without a partition map)
        cmd = concat([k3o, "-p -i m3 -l k3disttest", m3_file, create_cmd, idx_cmd, gc_cmd] +
                ['>', k3dist_file, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3dist_file, verbose, True):
            return False

        # create a partition map
        cmd = concat([partmap_tool, k3dist_file, "-n", num_nodes, ">", part_file])
        print_system(cmd, verbose)
        if check_error(part_file, verbose, True):
            return False

        # combine the data files
        cmd = concat([combine_tool, "--k3", k3dist_file] + read_files + ['>', data_file, '2>', error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose):
            return False

        # create another k3 distributed file (with partition map)
        cmd = concat([k3o, "-p -i m3 -l k3disttest", m3_file, create_cmd, "-m", part_file, idx_cmd, gc_cmd, "--sfile", data_file] + [">", k3dist_file, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3dist_file, verbose, True):
            return False

        # create node list
        node_list = []
        for i in range(num_nodes):
            port = 60000 + (i * 10000)
            node_list += ['localhost:{0}/node'.format(port)]

        # always add the master and timer
        peer_list = ["-n localhost:40000/master", "localhost:50000/timer"] + node_list
        peer_cmd = ','.join(peer_list)

        # run the k3 driver on the input
        cmd = concat([k3o, "--test", peer_cmd, "-q", queue_type, load_path, k3dist_file, ">", output_file, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose, True):
            return False

        # no error. print the file
        with open(error_file, 'r') as f:
            buf = f.read()
            if verbose:
                six.print_(buf)

        return True

    else: # not distrib
        # convert again to check for any loopback malformations
        cmd = concat([k3o, "-p -i k3 -l k3", k3_file, ">", k3_file2, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3_file2, verbose, True):
            return False

        # convert to a test
        cmd = concat([k3o, "-p -i m3 -l k3test --trace", trace_file, m3_file, ">", k3_file3, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3_file, verbose, True):
            return False

        # run the k3 driver on the input to get test results
        cmd = concat([k3o, "--test", k3_file3, load_path, ">", output_file, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose):
            return False
        # no error. print the file
        with open(output_file, 'r') as f:
            buf = f.read()
            if verbose:
                six.print_(buf)
        return True

