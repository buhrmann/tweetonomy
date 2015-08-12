#!/usr/bin/env python

import sys
import commands

def substitute_hql(infnm, outfnm, params):

    with open(infnm, "r") as infile:
        cmd = infile.read()

    num_req_params = cmd.count("%s")
    if num_req_params != len(params):
        print "Wrong number of parameters provided! Need %i." % (num_req_params)
        sys.exit(1)

    cmd = cmd % tuple(params)
    cmd = "hive -e '%s'" % (cmd)
    print cmd

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print "Need at least infnm and outfnm!"
        sys.exit(1)

    substitute_hql(sys.argv[1], sys.argv[2], sys.argv[3:len(sys.argv)])