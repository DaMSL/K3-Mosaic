#!/usr/bin/env python3

#
# Prepare everything the new k3 needs to evaluate
#

import argparse
import os
import re

path = "../../../"

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


def get_file_streams(s):
    file_paths = re.findall(r"CREATE (STREAM|TABLE) (\w+)[^;]+FROM FILE '([^']+)'", s, re.DOTALL)
    return [(f[1], f[2]) for f in file_paths]

def get_data_files(sql_file, sql_file_path):
    sql_path = os.path.split(sql_file_path)[0]
    file_paths = []
    # Get all file streams from include files
    includes = re.findall(r"INCLUDE '([^']+)';", sql_file, re.DOTALL)
    for include in includes:
        with open(os.path.join(sql_path, path, include), 'r') as inc_file:
            s = inc_file.read()
        file_paths += get_file_streams(s)

    # Add file streams from current file
    file_paths += get_file_streams(sql_file)

    # Search for appearance of each stream in the document
    out = []
    for fp in file_paths:
        pat = r'\W{0}\W'.format(fp[0])
        if re.search(pat, sql_file):
            out.append((fp[0], os.path.join(sql_path, path, fp[1])))
    return out

def main():
    parser = argparse.ArgumentParser()

    parser.add_argument("sql_file", type=str, help="Specify path of sql file")
    parser.add_argument("-n", "--nodes", type=int, dest="num_nodes",
                        default=2, help="Number of nodes")
    parser.add_argument("--folds_only", action='store_true', dest="folds_only",
                        default=False, help="Covert map and ext to fold")
    args = parser.parse_args()

    num_nodes = args.num_nodes - 1

    # calculate the proper name we want for this file
    nice_name = get_nice_name(args.sql_file)

    with open(args.sql_file, 'r') as f:
        s = f.read()

    data_files = get_data_files(s, args.sql_file)

    def print_sys(cmd):
        print(cmd)
        os.system(cmd)

    # create the old k3 files necessary for this query
    print("\nCreating old k3 files...")
    cmd = 'time python auto_test.py -d -p 1 -f {file}'.format(file=args.sql_file)
    print_sys(cmd)

    # handle the logs created
    os.system('cp log.out raw_{nice_name}.log'.format(**locals()))
    os.system('../bin/sanitize_log --db old raw_{nice_name}.log > {nice_name}.log'.format(**locals()))

    # convert to the new k3 file format
    fold_cmd = ""
    if args.folds_only:
        fold_cmd = '--k3new_folds'

    print("\nConverting to new k3 file format...")
    cmd = '../bin/k3 -i k3 -l k3new {fold_cmd} --k3new_folds --datafile {nice_name}.csv temp.k3dist > {nice_name}.k3 2> temp.err'.format(**locals())
    print_sys(cmd)

    # create k3 partmap
    print("\nCreating k3 partition map...")
    cmd = '../bin/partmap_tool temp.k3dist --k3new -n {num_nodes} > {nice_name}_part.k3'.format(**locals())
    print_sys(cmd)

    # combine data files
    print("\nCombining data files...")
    files_s = ' '.join([df[1] for df in data_files])
    cmd = '../bin/combine_data --mut {files_s} > {nice_name}.csv'.format(**locals())
    print_sys(cmd)


if __name__ == '__main__':
    main()
