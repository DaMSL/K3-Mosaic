#!/usr/bin/env python3

# Unit test utilities

import os
import re
import sys

err_len = 300

def check_exists(name, path):
    if not os.path.exists(path):
        s = "No {0} executable found at {1}".format(name, path)
        raise Exception(s)

# @param search - only error if we find 'error' with regexp
def check_error(filename, verbose, search=False):
    # Check for error
    size = os.path.getsize(filename)
    if size > 0:
        with open(filename) as f:
            buf = f.read()
            if search:
                mo = re.search(r'(Error|ERROR)', buf)
                if mo:
                    if verbose:
                        print(buf[0:err_len])
                    return True
                return False
            else:
                if verbose:
                    print('ERROR: ' + buf[0:err_len])
                return True
    return False

def print_system(cmd, verbose):
    if verbose:
        print(cmd)
    os.system(cmd)

def get_os():
    l = len("darwin")
    if sys.platform[:l] == "darwin":
        return "osx"
    l = len("linux")
    if sys.platform[:l] == "linux":
        return "ubuntu"
    else:
        return "windows"

def concat(s_list):
    res = ""
    for s in s_list:
        if s is not None:
            res = res + " " + str(s)
    return res
