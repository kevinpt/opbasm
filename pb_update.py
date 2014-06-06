#!/usr/bin/python
# -*- coding: utf-8 -*-

'''Picoblaze ROM update script
'''
from __future__ import print_function, division

import sys
import os
import re
from optparse import OptionParser
from subprocess import check_call, CalledProcessError

def parse_command_line():
    progname = os.path.basename(sys.argv[0])
    usage = '{} -m <mem file> -n <NCD file> [-r <RAM inst name>] [-o <output bit file>]'.format(progname)
    parser = OptionParser(usage=usage)

    parser.add_option('-m', '--mem', dest='mem_file', help='mem file')
    parser.add_option('-n', '--ncd', dest='ncd_file', help='NCD file')
    parser.add_option('-r', '--ram_inst', dest='ram_inst', help='RAM instance name')
    parser.add_option('-o', '--output', dest='out_bit_file', help='Output bit file')

    options, args = parser.parse_args()

    if not options.mem_file: parser.error('Missing mem file')
    if not options.ncd_file: parser.error('Missing NCD file')

    return options

def error(*args, **kwargs):
  print('ERROR:', *args, file=sys.stderr)
  if 'exit' in kwargs:
    sys.exit(kwargs['exit'])


def main():
    options = parse_command_line()

    # Set up file names
    design_name = os.path.splitext(os.path.basename(options.ncd_file))[0]
    prog_name = os.path.splitext(os.path.basename(options.mem_file))[0]
    bit_file = os.path.splitext(options.ncd_file)[0] + '.bit'
    xdl_file = design_name + '.xdl'
    bmm_file = prog_name + '.bmm'

    if not options.out_bit_file:
        options.out_bit_file = 'new_' + design_name + '.bit'

    # Check for existence of input files
    if not os.path.exists(options.mem_file):
        error('mem file not found', exit=1)

    if not os.path.exists(options.ncd_file):
        error('NCD file not found', exit=1)

    if not os.path.exists(bit_file):
        error('bit file not found', exit=1)

    # Check timestamp of XDL and NCD to see if we need to update the XDL
    run_xdl = True
    if os.path.exists(xdl_file):
        if os.path.getmtime(xdl_file) > os.path.getmtime(options.ncd_file):
            run_xdl = False

    # Run XDL to get instance information
    if run_xdl:
        print('Running XDL...')
        try:
          check_call(['xdl', '-ncd2xdl', options.ncd_file, xdl_file])
        except CalledProcessError:
          error('XDL failure', exit=1)

    if not os.path.exists(xdl_file):
        error('XDL file not generated', exit=1)

    # Find RAMB16 instances in XDL file
    ram_insts = find_ram_instances(xdl_file)
    inames = ram_insts.keys()

    if len(inames) == 0:
        error('No RAM instances found', exit=1)

    prompt_user = True
    if len(inames) == 1: # Don't bother with prompt if only one RAM is in the netlist
      sel = 0
      prompt_user = False

    # Check if named instance exists
    if options.ram_inst is not None:
        if options.ram_inst in inames:
            sel = inames.index(options.ram_inst)
            prompt_user = False
        else:
            error('Named RAM instance does not exist ({})'.format(options.ram_inst))
            print(' Available RAM instances:', file=sys.stderr)
            for n in inames:
                print('  ', n, file=sys.stderr)
            sys.exit(1)

    # Ask user to select RAMB16 instance
    if prompt_user:
        print('Available RAM instances:')
        for i, n in enumerate(inames):
            print('  {}) {}'.format(i+1, n))
        try:
            sel = int(raw_input('\nSelect RAM instance: ')) - 1
        except ValueError:
            sel = -1

        if sel < 0 or sel >= len(inames):
            error('Invalid selection', exit=1)

    print('\nSelected instance:', inames[sel], 'placed at', ram_insts[inames[sel]])

    # Generate BMM file
    bmm_template = '''ADDRESS_SPACE pb_rom RAMB18 INDEX_ADDRESSING [0x00000000:0x000003FF]
    BUS_BLOCK
        <inst> [17:0] PLACED = <loc>;
    END_BUS_BLOCK;
END_ADDRESS_SPACE;
'''

    bmm_template = bmm_template.replace('<inst>', inames[sel])
    bmm_template = bmm_template.replace('<loc>', ram_insts[inames[sel]])

    with open(bmm_file, 'w') as fh:
        fh.write(bmm_template)

    # Run data2mem
    print('Running data2mem...')
    d2m_cmd = ['data2mem', '-bm', bmm_file, '-bd', options.mem_file, \
               '-bt', bit_file, '-o', 'b', options.out_bit_file]
    print(' ', ' '.join(d2m_cmd))
    try:
      check_call(d2m_cmd)
    except CalledProcessError:
      error('data2mem failure', exit=1)


    print('Generated updated bit file:', options.out_bit_file)
    sys.exit(0)


inst_re = re.compile('inst "([^"]+)" "RAMB16",.+RAMB16_(\w+) *,')

def find_ram_instances(xdl_file):
    with open(xdl_file, 'r') as fh:
        lines = fh.readlines()

    ram_insts = {}
    for l in lines:
        m = inst_re.match(l)
        if m:
            ram_insts[m.group(1)] = m.group(2)

    return ram_insts


if __name__ == '__main__':
    main()
