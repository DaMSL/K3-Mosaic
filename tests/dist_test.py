#!/usr/bin/env python3

#
# Test a distributed SQL query
#

import os
import re
import platform
import json
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
        num_switches=1,
        order_file=None,
        verbose=True,
        distrib=False,
        new_k3=True,
        folds_only=True,
        map_type="vmap",
        workdir="temp",
        run_interp=True,
        gc_interval=20000,
        msg_interval=2,
        logging=True,
        gen_deletes=True,
        gen_correctives=True,
        gen_single_vid=True,
        run_correctives=False,
        run_isobatch=True,
        filemux=False,
        gen_intmap=False,
        safe_writes=False,
        map_overlap_factor=None,
        batch_size=None,
        debug=False,
        opt_route=True,
        do_trace=False,
        dump_info=False,
        print_warmup=False
        ):

    to_root = ".."
    script_path = os.path.abspath(os.path.dirname(__file__))
    target_file = os.path.abspath(target_file)
    target_path, _ = os.path.split(target_file)
    agenda_file = os.path.join(target_path, "schemas.sql")
    nice_name = get_nice_name(target_file)
    root_path = os.path.join(script_path, to_root)
    if platform.system() == 'Linux':
        dbtoaster = os.path.join(script_path, "dbtoaster_linux")
    elif platform.system() == 'Darwin':
        dbtoaster = os.path.join(script_path, "dbtoaster_osx")
    else:
        print("Failed to determine OS")
        return False
    k3o = os.path.join(root_path, "bin/k3")
    combine_tool = os.path.join(root_path, "bin/combine_data")

    saved_dir = os.path.abspath(os.path.curdir)
    temp_dir = os.path.join(saved_dir, workdir)
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
    json_file = os.path.join(temp_dir, "temp.json")

    if not os.path.exists(temp_dir):
        os.makedirs(temp_dir)

    check_exists("dbtoaster", dbtoaster)
    check_exists("k3o", k3o)
    check_exists("agenda file", agenda_file)
    if distrib:
        check_exists("combine_data", combine_tool)

    # run dbtoaster to get interpreted updates
    os.chdir(script_path)
    debug_cmd = ''
    debug_flags = ['PRINT-VERBOSE',
                   'LOG-INTERPRETER-UPDATES',
                   'LOG-INTERPRETER-TRIGGERS',
                   'LOG-M3']
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
    cmd = concat([dbtoaster, "-l", src_lang, "-d", "PRINT-VERBOSE", target_file,
          ">", m3_file, "2>", error_file])
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

    read_files = [os.path.join(script_path, m) for m in matches]

    load_path = concat(["--load_path", script_path])

    # execution diverges from here for distributed and local
    if distrib:
        queue_type = "node"
        options = []

        # string for k3 distributed file creation: either use a trace file or an order file
        if order_file:
            options += ["--order " + order_file]
        else:
            options += ["--trace " + trace_file]

        if map_type == "vmap":
            options += ["--map-vmap"]
        elif map_type == "multi":
            options += ["--map-multi"]
        if not gen_deletes:
            options += ["--no-deletes"]
        if not gen_correctives:
            options += ["--no-correctives"]
        if not gen_single_vid:
            options += ["--no-single-vid"]
        if not opt_route:
            options += ['--no-opt-route']

        agenda_cmd = "--agenda "+ agenda_file

        # combine the data files
        cmd = concat([combine_tool] + read_files +
                [agenda_cmd, '>', data_file, '2>', error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose):
            os.chdir(saved_dir)
            return False

        # if asking to dump info, do so now and abort
        if dump_info:
            cmd = concat([k3o, "-p -i m3 -l k3disttest", m3_file, "--dump-info"])
            print_system(cmd, True)
            return True

        # if asked to print the warmup, do so now
        if print_warmup:
            cmd = concat([k3o, "-p -i m3 -l k3new --k3new-folds", m3_file, agenda_cmd,
              concat(options), "--sfile", data_file, "--print-warmup", "> dist_warmup.k3 2>", error_file])
            print_system(cmd, True)
            return True


        # create a k3 distributed file

        cmd = concat([k3o, "-p -i m3 -l k3disttest", m3_file, agenda_cmd,
              concat(options), "--sfile", data_file,
              ">", k3dist_file, "2>", error_file])
        print_system(cmd, verbose)
        if check_error(error_file, verbose) or check_error(k3dist_file, verbose, True):
            os.chdir(saved_dir)
            return False

        switch_list = ['localhost:{}/switch_old'.format(2 + i) for i in range(num_switches)]
        node_list = ['localhost:{}/node'.format(20 + (i*10)) for i in range(num_nodes)]

        # always add the master and timer
        peer_list = ['-n localhost:0/master', 'localhost:1/timer'] + node_list + switch_list
        peer_cmd = ','.join(peer_list)

        if new_k3:
            # convert to the new k3 file format
            arg = ""
            if folds_only:
                arg += '--k3new-folds '
            if filemux:
                arg += '--filemux '
            if gen_intmap:
                arg += '--intmap '
            if safe_writes:
                arg += '--safe-writes '

            print("Converting to new k3 file format...")
            cmd = concat([k3o, "-i k3 -l k3new", arg, "--datafile",
                  data_file, k3dist_file, ">", k3new_file, "2>", error_file])
            print_system(cmd, verbose)
            if check_error(error_file, verbose, False):
                os.chdir(saved_dir)
                return False

        if run_interp:
            # create a json file
            j = {}
            j["ms_gc_interval"] = gc_interval
            j["num_switches"] = num_switches
            if map_overlap_factor:
                j["pmap_overlap_factor"] = map_overlap_factor
            if batch_size:
                j["sw_poly_batch_size"] = batch_size
            j["corrective_mode"] = run_correctives
            j["isobatch_mode"] = run_isobatch
            if do_trace:
                j["do_tracing"] = True
            # set csv indices
            for i in range(num_switches):
                node_j = {}
                node_j['sw_csv_index'] = i
                j['localhost:{}'.format(2 + i)] = node_j
            with open(json_file, 'w') as f:
                json.dump(j, f)

            json_cmd = "--interp-args " + json_file
            msg_cmd = "--msg_interval " + str(msg_interval)
            log_cmd = "--no-log" if not logging else ""

            # run the k3 driver on the input
            cmd = concat([k3o, "--test", peer_cmd, "-q", queue_type, load_path, json_cmd,
                  msg_cmd, log_cmd, k3dist_file, ">", output_file, "2>", error_file])
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
        log_cmd = "--no-log" if not logging else ""
        if run_interp:
            cmd = concat([k3o, "--test", k3_file3, log_cmd, load_path, ">", output_file, "2>", error_file])
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

