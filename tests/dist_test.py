#!/usr/bin/env python

#
# Test a distributed SQL query
#

import os
import six
from utils import check_exists, check_error, print_system

def run(target_file,
        num_nodes=1,
        queue_type="global",
        do_shuffle=False,
        force_correctives=False,
        order_file="",
        verbose=True,
        distrib=False,
        use_idx=False
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

    saved_dir = os.path.abspath(os.path.curdir)
    trace_file = os.path.join(saved_dir, "temp.trace")
    m3_file = os.path.join(saved_dir, "temp.m3")
    k3_file = os.path.join(saved_dir, "temp.k3")
    k3_file2 = os.path.join(saved_dir, "temp2.k3")
    k3_file3 = os.path.join(saved_dir, "temp3.k3")
    k3dist_file = os.path.join(saved_dir, "temp.k3dist")
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
    cmd = '{dbtoaster_name} {debug_cmd} {target_file} > {trace_file} 2> {error_file}' \
          .format(**locals())
    print_system(cmd, verbose)
    if check_error(error_file, verbose):
        os.chdir(saved_dir)
        return False

    # run dbtoaster to get m3 file with distributed portion
    if distrib:
        src_lang = "distm3"
    else:
        src_lang = "m3"
    cmd = '{dbtoaster_name} -l {src_lang} -d PRINT-VERBOSE {target_file} > {m3_file} 2> {error_file}' \
          .format(**locals())
    print_system(cmd, verbose)
    if check_error(error_file, verbose):
        os.chdir(saved_dir)
        return False

    # change directory back
    if verbose:
        six.print_("cd {0}".format(saved_dir))
    os.chdir(saved_dir)

    # create a single-site k3o file
    cmd = "{k3o} -p -i m3 -l k3 {m3_file} > {k3_file} 2> {error_file}".format(**locals())
    print_system(cmd, verbose)
    if check_error(error_file, verbose) or check_error(k3_file, verbose, True):
        return False

    load_path = "--load_path {0}".format(dbtoaster_dir)
    
    # execution diverges from here
    if distrib:
        # string for k3 distributed file creation: either use a trace file or an order file
        if not order_file:
            create_cmd = "--trace {0}".format(trace_file)
        else:
            create_cmd = "--order {0}".format(order_file)

        force_cmd = "--force" if force_correctives else ""
        idx_cmd = "--use_idx" if use_idx else ""

        # create a k3 distributed file (without a partition map)
        cmd = ('{k3o} -p -i m3 -l k3disttest {m3_file} {create_cmd} {force_cmd} {idx_cmd}'
            + ' > {k3dist_file} 2> {error_file}').format(**locals())
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3dist_file, verbose, True):
            return False

        if num_nodes > 1:
            # create a partition map
            cmd = "{partmap_tool} {k3dist_file} -n {num_nodes} > {part_file}".format(**locals())
            print_system(cmd, verbose)
            if check_error(part_file, verbose, True):
                return False

            # create another k3 distributed file (with partition map)
            cmd = ("{k3o} -p -i m3 -l k3disttest {m3_file} {create_cmd} -m {part_file} {force_cmd} {idx_cmd}"
                + "> {k3dist_file} 2> {error_file}").format(**locals())
            print_system(cmd, verbose)
            if check_error(error_file, verbose) or check_error(k3dist_file, verbose, True):
                return False

        # create node list
        node_list = []
        for i in range(num_nodes):
            port = 50000 + (i * 10000)
            node_list += ['127.0.0.1:{0}/node'.format(port)]

        peer_list = ["-n localhost:40000/switch"] + node_list
        peer_cmd = ','.join(peer_list)
        shuffle_cmd = "--shuffle" if do_shuffle else ""

        # run the k3 driver on the input
        cmd = '{k3o} --test {peer_cmd} -q {queue_type} {shuffle_cmd} {load_path} {k3dist_file} > {output_file} 2> {error_file}' \
            .format(**locals())
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
        cmd = "{k3o} -p -i k3 -l k3 {k3_file} > {k3_file2} 2> {error_file}".format(**locals())
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3_file2, verbose, True):
            return False

        # convert to a test
        cmd = "{k3o} -p -i m3 -l k3test --trace {trace_file} {m3_file} > {k3_file3} 2> {error_file}" \
            .format(**locals())
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3_file, verbose, True):
            return False

        # run the k3 driver on the input to get test results
        cmd = "{k3o} --test {k3_file3} {load_path} >{output_file} 2> {error_file}".format(**locals())
        print_system(cmd, verbose)
        if check_error(error_file, verbose):
            return False
        # no error. print the file
        with open(output_file, 'r') as f:
            buf = f.read()
            if verbose:
                six.print_(buf)
        return True

