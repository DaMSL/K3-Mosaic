#!/usr/bin/env python3

#
# Compare the output of an SQL test using dbtoaster to K3 execution
#
# Usage:
#    ./sql_test.py SQL_query
#

import os
from test_utils import check_exists, check_error, print_system

def run(target_file, verbose):
    to_root = ".."
    script_path = os.path.abspath(os.path.dirname(__file__))
    target_file = os.path.join(script_path, target_file)
    root_path = os.path.join(script_path, to_root)
    dbtoaster_dir = os.path.join(root_path, "external/dbtoaster")
    dbtoaster_name = "bin/dbtoaster_release"
    dbtoaster = os.path.join(dbtoaster_dir, dbtoaster_name)
    k3o = os.path.join(root_path, "bin/k3")

    saved_dir = os.path.abspath(os.path.curdir)
    trace_file = os.path.join(saved_dir, "temp.trace")
    m3_file = os.path.join(saved_dir, "temp.m3")
    k3_file = os.path.join(saved_dir, "temp.k3")
    k3_file2 = os.path.join(saved_dir, "temp2.k3")
    k3_file3 = os.path.join(saved_dir, "temp3.k3")
    error_file = os.path.join(saved_dir, "temp.err")
    output_file = os.path.join(saved_dir, "temp.out")

    check_exists("dbtoaster", dbtoaster)
    check_exists("k3o", k3o)

    # change to dbtoaster path (dbtoaster needs it)
    if verbose:
        print("cd {0}".format(dbtoaster_dir))
    os.chdir(dbtoaster_dir)

    # run dbtoaster to get interpreted updates
    debug_cmd = ''
    debug_flags = ['LOG-INTERPRETER-UPDATES', 'LOG-INTERPRETER-TRIGGERS', 'LOG-M3']
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

    # convert to a k3 file to check
    cmd = "{k3o} -p -i m3 -l k3 {m3_file} > {k3_file2} 2> {error_file}".format(**locals())
    print_system(cmd, verbose)
    if check_error(error_file, verbose) or check_error(k3_file2, verbose, True):
        return False

    # convert again to check for any loopback malformations
    cmd = "{k3o} -p -i k3 -l k3 {k3_file2} > {k3_file3} 2> {error_file}".format(**locals())
    print_system(cmd, verbose)
    if check_error(error_file, verbose) or check_error(k3_file3, verbose, True):
        return False

    # convert to a test
    cmd = "{k3o} -p -i m3 -l k3test --trace {trace_file} {m3_file} > {k3_file} 2> {error_file}" \
          .format(**locals())
    print_system(cmd, verbose)
    if check_error(error_file, verbose) or check_error(k3_file, verbose, True):
        return False

    # run the k3 driver on the input to get test results
    cmd = "{k3o} --test {k3_file} >{output_file} 2> {error_file}".format(**locals())
    print_system(cmd, verbose)
    if check_error(error_file, verbose):
        return False
    # no error. print the file
    with open(output_file, 'r') as f:
        buf = f.read()
        if verbose:
            print(buf)
    return True
