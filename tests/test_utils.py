#!/usr/bin/env python

# Unit test utilities

import os
import re
import six

def check_exists(name, path):
    if not os.path.exists(path):
        raise Exception("No {0} executable found at {1}".format(name, path))

# @param search - only error if we find 'error' with regexp
def check_error(filename, verbose, search=False):
    # Check for error
    size = os.path.getsize(filename)
    if size > 0:
        with open(filename) as f:
            buf = f.read()
            if search:
                mo = re.search(r'^(Error|ERROR)', buf)
                if mo:
                    if verbose:
                        six.print_(buf)
                    return True
                return False
            else:
                if verbose:
                    six.print_('ERROR: ' + buf)
                return True
    return False

def print_system(cmd, verbose):
    if verbose:
        six.print_(cmd)
    os.system(cmd)
