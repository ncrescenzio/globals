#!/usr/bin/python

import re, sys, os, getopt

def usage():
    print """f90_source_deps.py [options] <file ...>

Generates dependencies for the given Fortran 90 source files based on
module and use statements in them. Options are:

  -h, --help     This help output.

  -o, --output <file>
                 Specify the output file. If unspecified then output
                 is to stdout.
"""

# default options
class Opts:
    output = None

def get_mods(filename):
    mods = []
    f = open(filename)
    if not f:
	print "ERROR: unable to open %s%s" % filename
	sys.exit(1)
    within_use_statement = False
    line_with_use = False
    for line in f:
	line = line.lower()
	line = line.strip()
	findkey = line.startswith('module')
	if findkey:
	    words = line.split()
	    mods.append(words[1]+'.mod')
    f.close()
    return (mods)

def write_deps(outf, filename, mods):
    filebase, fileext = os.path.splitext(filename)
    mods = set(mods)
    if("procedure.mod" in mods):
        mods = mods - set(["procedure.mod"])
    if len(mods)>0:
	outf.write("%s: %s.f90\n" % (" ".join(mods), filebase))
	outf.write("\t$(FC) $(FCFLAGS) -c $<\n")

def process_args():
    try:
	opts, args = getopt.getopt(sys.argv[1:], "ho:",
				   ["help", "output="])
    except getopt.GetoptError:
	print "ERROR: invalid commandline options"
	usage()
	sys.exit(1)
    myopts = Opts()
    for o, a in opts:
	if o in ("-h", "--help"):
	    usage()
	    sys.exit()
	if o in ("-o", "--output"):
	    myopts.output = a
    if len(args) < 1:
	usage()
	sys.exit(1)
    return (myopts, args)

def main():
    (opts, filenames) = process_args()
    if opts.output:
	outf = open(opts.output, "w")
    else:
	outf = sys.stdout
    outf.write("# DO NOT EDIT --- auto-generated file\n")
    for filename in filenames:
	mods = get_mods(filename)
	write_deps(outf, filename, mods)
    outf.close()

if __name__ == "__main__":
    main()
