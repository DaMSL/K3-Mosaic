#!/usr/bin/env python3

#
# Test a distributed SQL query
#

import os
import re
from utils import check_exists, check_error, print_system, concat

def get_nice_name(path):
    (first, last) = os.path.split(path)
    (last, _) = os.path.splitext(last)
    (first, middle) = os.path.split(first)

    matchobj = re.match('query(.*)', last)
    if matchobj:
        nice_name = middle + matchobj.group(1)
    else:
        nice_name = last
    return nice_name

def run(target_file,
        num_nodes=1,
        order_file=None,
        verbose=True,
        distrib=False,
        use_idx=False,
        enable_gc=True,
        new_k3=True,
        folds_only=True,
        gen_deletes=True,
        gen_correctives=True
        ):

    to_root = ".."
    script_path = os.path.abspath(os.path.dirname(__file__))
    target_file = os.path.abspath(target_file)
    nice_name = get_nice_name(target_file)
    root_path = os.path.join(script_path, to_root)
    dbtoaster = os.path.join(script_path, "dbtoaster_release")
    k3o = os.path.join(root_path, "bin/k3")
    partmap_tool = os.path.join(root_path, "bin/partmap_tool")
    combine_tool = os.path.join(root_path, "bin/combine_data")

    saved_dir = os.path.abspath(os.path.curdir)
    temp_dir = os.path.join(saved_dir, "temp/")
    trace_file = os.path.join(temp_dir, "temp.trace")
    m3_file = os.path.join(temp_dir, nice_name + ".m3")
    k3_file = os.path.join(temp_dir, "temp.k3o")
    k3_file2 = os.path.join(temp_dir, "temp2.k3o")
    k3_file3 = os.path.join(temp_dir, "temp3.k3o")
    k3dist_file = os.path.join(temp_dir, nice_name + ".k3o")
    data_file = os.path.join(temp_dir, nice_name + ".csv")
    k3new_file = os.path.join(temp_dir, nice_name + ".k3")
    k3new_part_file = os.path.join(temp_dir, nice_name + ".part")
    error_file = os.path.join(temp_dir, "temp.err")
    part_file = os.path.join(temp_dir, "temp.part")
    output_file = os.path.join(temp_dir, "temp.out")

    if not os.path.exists(temp_dir):
        os.makedirs(temp_dir)

    check_exists("dbtoaster", dbtoaster)
    check_exists("k3o", k3o)
    if distrib:
        check_exists("partmap_tool", partmap_tool)
        check_exists("combine_data", combine_tool)

    # run dbtoaster to get interpreted updates
    os.chdir(script_path)
    debug_cmd = ''
    debug_flags = ['PRINT-VERBOSE', 'LOG-INTERPRETER-UPDATES', 'LOG-INTERPRETER-TRIGGERS', 'LOG-M3']
    for f in debug_flags:
        debug_cmd += ' -d ' + f

    cmd = concat([dbtoaster, debug_cmd, target_file, ">", trace_file, "2>", error_file])
    print_system(cmd, verbose)
    if check_error(error_file, verbose):
        os.chdir(saved_dir)
        return False

    # run dbtoaster to get m3 file with distributed portion
    if distrib:
        src_lang = "distm3"
    else:
        src_lang = "m3"
    cmd = concat([dbtoaster, "-l", src_lang, "-d", "PRINT-VERBOSE", target_file, ">", m3_file, "2>", error_file])
    print_system(cmd, verbose)
    if check_error(error_file, verbose):
        os.chdir(saved_dir)
        return False

    # create a single-site k3o file
    cmd = concat([k3o, "-p -i m3 -l k3", m3_file, ">", k3_file, "2>", error_file])
    print_system(cmd, verbose)
    if check_error(error_file, verbose) or check_error(k3_file, verbose, True):
        os.chdir(saved_dir)
        return False

    # scan for the files we're using
    s = open(k3_file, 'r').read()
    matches = re.findall(r'\("(.+)", csv\)', s)
    if matches is None:
        print("failed to find file references")
        os.chdir(saved_dir)
        return False

    read_files = []
    for m in matches:
        read_files += [os.path.join(script_path, m)]

    load_path = concat(["--load_path", script_path])

    # execution diverges from here
    if distrib:
        queue_type = "node"

        # string for k3 distributed file creation: either use a trace file or an order file
        if not order_file:
            create_cmd = concat(["--trace", trace_file])
        else:
            create_cmd = concat(["--order", order_file])

        idx_cmd = "--use_idx" if use_idx else None
        gc_cmd  = "--gc" if enable_gc else None
        delete_cmd = "--no-deletes" if not gen_deletes else None
        corrective_cmd = "--no-correctives" if not gen_correctives else None
        options = concat ([create_cmd, idx_cmd, gc_cmd, delete_cmd, corrective_cmd])

        # create a k3 distributed file (without a partition map)
        cmd = concat([k3o, "-p -i m3 -l k3disttest", m3_file, options] +
                ['>', k3dist_file, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3dist_file, verbose, True):
            os.chdir(saved_dir)
            return False

        # create a partition map
        cmd = concat([partmap_tool, k3dist_file, "-n", num_nodes, ">", part_file])
        print_system(cmd, verbose)
        if check_error(part_file, verbose, True):
            os.chdir(saved_dir)
            return False

        # combine the data files
        cmd = concat([combine_tool, "--k3", k3dist_file] + read_files + ['>', data_file, '2>', error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose):
            os.chdir(saved_dir)
            return False

        # create another k3 distributed file (with partition map)
        cmd = concat([k3o, "-p -i m3 -l k3disttest", m3_file, options, "-m", part_file, "--sfile", data_file] + [">", k3dist_file, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3dist_file, verbose, True):
            os.chdir(saved_dir)
            return False

        # create node list
        node_list = []
        for i in range(num_nodes):
            port = 10 + (i * 10)
            node_list += ['localhost:{0}/node'.format(port)]

        # always add the master, switch and timer
        peer_list = ["-n localhost:0/master", "localhost:1/switch", "localhost:3/timer", ] + node_list
        peer_cmd = ','.join(peer_list)

        if new_k3:
            # convert to the new k3 file format
            fold_cmd = ""
            if folds_only:
                fold_cmd = '--k3new_folds'

            print("Converting to new k3 file format...")
            cmd = concat([k3o, "-i k3 -l k3new", fold_cmd, "--datafile", data_file, k3dist_file, ">", k3new_file, "2>", error_file])
            print_system(cmd, verbose)
            if check_error(error_file, verbose, False):
                os.chdir(saved_dir)
                return False

            # create k3_new partmap
            print("Creating k3new partition map...\n")
            cmd = concat([partmap_tool, k3dist_file, "--k3new -n", num_nodes, ">", k3new_part_file])
            print_system(cmd, verbose)

        # run the k3 driver on the input
        cmd = concat([k3o, "--test", peer_cmd, "-q", queue_type, load_path, k3dist_file, ">", output_file, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose, True):
            os.chdir(saved_dir)
            return False

        # no error. print the file
        with open(error_file, 'r') as f:
            buf = f.read()
            if verbose:
                print(buf)

        os.chdir(saved_dir)
        return True

    else: # not distrib
        # convert again to check for any loopback malformations
        queue_type = "global"
        cmd = concat([k3o, "-p -i k3 -l k3", k3_file, ">", k3_file2, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3_file2, verbose, True):
            os.chdir(saved_dir)
            return False

        # convert to a test
        cmd = concat([k3o, "-p -i m3 -l k3test --trace", trace_file, m3_file, ">", k3_file3, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3_file, verbose, True):
            os.chdir(saved_dir)
            return False

        # run the k3 driver on the input to get test results
        cmd = concat([k3o, "--test", k3_file3, load_path, ">", output_file, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose):
            os.chdir(saved_dir)
            return False
        # no error. print the file
        with open(output_file, 'r') as f:
            buf = f.read()
            if verbose:
                print(buf)
        os.chdir(saved_dir)
        return True

