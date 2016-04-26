#!/usr/bin/python
# -*- coding: utf-8 -*-

import argparse
parser = argparse.ArgumentParser(description="Perform an analysis on the " +
    "program trace. The output gives the function name, the call count, and " +
    "the time spent.")
parser.add_argument("prgm", help="program that produced trace")
parser.add_argument("trace", help="trace to read")
parser.add_argument("-o", "--out", help="output file", default=None)
parser.add_argument('-t', '--time', help='sort by time', action="store_true")
parser.add_argument('-c', '--call', help='sort by call', action='store_true')
parser.add_argument('-f', '--force', help='force analysis',action="store_true")
args = parser.parse_args()
# check file existence
tmp = open(args.prgm)
tmp.close()
if not args.out:
    args.out = args.prgm + ".sum"
# do we need to analyze? Look at trace and analysis time stamps
do_analyze = 1
import os
if os.path.isfile(args.out):
    trace_mtime = os.stat(args.trace).st_mtime
    out_mtime = os.stat(args.out)
    # trace is older than the analysis
    if trace_mtime < out_mtime:
        do_analyze = 0
if args.force:
    do_analyze = 1
import subprocess
def analyze_trace():
    print "analyzing {0} for {1}".format(args.trace, args.prgm)
    trace = open(args.trace)
    # runmain stores the runtime address of main
    # time stores the previous time
    _, runmain, kernel, ptime = trace.readline().split(' ')
    # for each function, a pair of
    # (number of function calls, total time in function)
    stat = {}
    # add the first caller (the kernel)
    stat[kernel] = [0, 0.0]
    # whenever a function is called, add it to the stack
    stack = []
    # add main()
    stack.append(runmain)
    ptime = float(ptime)
    trace.seek(0)
    for data in trace:
        ex, callee, _, time = data.split(' ')
        time = float(time)
        if ex == 'e':
            if not callee in stat:
                stat[callee] = [0, 0.0]
            # function entry. Add the call count of callee and time of caller
            stat[callee][0] += 1
            # the caller is the top of the stack
            stat[stack[-1]][1] += time - ptime
            stack.append(callee)
        elif ex == 'x':
            # function exit
            stat[stack[-1]][1] += time - ptime
            stack.pop()
        ptime = time
    trace.close()
    # get the address of main() in the elf binary
    gobjdump = subprocess.Popen("gobjdump -d {0}".format(args.prgm).split(),
        stdout=subprocess.PIPE)
    grep = subprocess.Popen("grep -m 1 _main".split(), stdin=gobjdump.stdout,
        stdout=subprocess.PIPE)
    gobjdump.stdout.close()
    elfmain = int(grep.communicate()[0].split(' ', 1)[0], 16)
    gobjdump.wait()
    # compute the slide offset
    runmain = int(runmain, 16)
    slide = runmain - elfmain
    # begin output
    out = open(args.out, "w+")
    for fn, data in stat.iteritems():
        # symbolicate function address
        atos = subprocess.Popen("atos -d -s {0} -o {1} {2}".format(hex(slide),
         args.prgm,fn).split(' '),stdin=subprocess.PIPE,stdout=subprocess.PIPE)
        symb, err = atos.communicate()
        syms = symb[0:symb.find('(')] + symb[symb.find(')')+2:-1]
        sym, _, src = syms.partition(' ')
        if not src:
            src = "(unknown)"
        sym = sym[:22] + (sym[22:] and '..')
        src = src[:22] + (src[22:] and '..')
        out.write("{0:24} {1:24} {2:>14} {3:>14}\n".format(sym, src, data[0],
            data[1]))
    out.close()
    print "wrote to {0}".format(args.out)

if do_analyze:
    analyze_trace()

# sort and print the trace
if args.time:
    subprocess.call("sort -n -b -r -k 4 {0}".format(args.out).split(' '))
elif args.call:
    subprocess.call("sort -n -b -r -k 3 {0}".format(args.out).split(' '))
