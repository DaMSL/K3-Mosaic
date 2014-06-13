#!/usr/bin/env python3

#
# Test a distributed SQL query
#

import os
from test_utils import check_exists, check_error, print_system

def run(target_file,
        num_nodes=1,
        queue_type="global",
        do_shuffle=False,
        force_correctives=False,
        order_file="",
        verbose=True):

    to_root = ".."
    script_path = os.path.dirname(__file__)
    root_path = os.path.join(script_path, to_root)
    dbtoaster_dir = os.path.join(root_path, "./external/dbtoaster")
    dbtoaster_name = "./bin/dbtoaster_release"
    dbtoaster = os.path.join(dbtoaster_dir, dbtoaster_name)
    k3o = os.path.join(root_path, "./bin/k3")
    partmap_tool = os.path.join(root_path, "./bin/partmap_tool")
    saved_dir = os.path.curdir
    trace_file = os.path.join(saved_dir, "temp.trace")
    m3_file = os.path.join(saved_dir, "temp.m3")
    k3_file = os.path.join(saved_dir, "temp.k3")
    k3dist_file = os.path.join(saved_dir, "temp.k3dist")
    error_file = os.path.join(saved_dir, "temp.err")
    part_file = os.path.join(saved_dir, "temp.part")

    check_exists("dbtoaster", dbtoaster)
    check_exists("k3o", k3o)
    check_exists("partmap_tool", partmap_tool)

    # change to dbtoaster path (dbtoaster needs it)
    if verbose:
        print("cd {0}".format(dbtoaster_dir))
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
    cmd = '{dbtoaster_name} -l distm3 -d PRINT-VERBOSE {target_file} > {m3_file} 2> {error_file}' \
          .format(**locals())
    print_system(cmd, verbose)
    if check_error(error_file, verbose):
        os.chdir(saved_dir)
        return False

    # change directory back
    if verbose:
        print("cd {0}".format(saved_dir))
    os.chdir(saved_dir)

    # create a single-site k3o file for comparison's sake
    cmd = "{k3o} -p -i m3 -l k3 {m3_file} > {k3_file} 2> {error_file}".format(**locals())
    print_system(cmd, verbose)

    # string for k3 distributed file creation: either use a trace file or an order file
    create_cmd = "--trace #{trace_file}" if order_file == "" else "--order #{$order_file}"
    force_cmd = "--force" if force_correctives else ""

    # create a k3 distributed file (without a partition map)
    cmd = ('{k3o} -p -i m3 -l k3disttest {m3_file} {create_cmd} {force_cmd}'
           + ' > {k3dist_file} 2> {err_file}').format(**locals())
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
        cmd = ("{k3o} -p -i m3 -l k3disttest {m3_file} {create_cmd} -m {part_file} {force_cmd}"
               + "> {k3dist_file} 2> {err_file}").format(**locals())
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3dist_file, verbose, True):
            return False

    # create node list
    node_list = []
    for i in range(num_nodes):
        port = 50000 + (i * 10000)
        node_list += '127.0.0.1:{0}/node'.format(port)

    peer_cmd = "-n localhost:40000/switch" + ','.join(node_list)
    shuffle_cmd = "--shuffle" if do_shuffle else ""

    # run the k3 driver on the input
    cmd = '{k3o} --test {peer_cmd} -q {queue_type} {shuffle_cmd} {k3dist_file} > {error_file} 2>&1' \
          .format(**locals())
    print_system(cmd, verbose)
    if check_error(error_file, verbose, True):
        return False

    # no error. print the file
    with open(error_file, 'r') as f:
        buf = f.read()
        if verbose:
            print(buf)

    return True
