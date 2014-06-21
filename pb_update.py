#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright © 2014 Kevin Thibedeau
# (kevin 'period' thibedeau 'at' gmail 'punto' com)
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.

'''Picoblaze ROM update script
'''
from __future__ import print_function, division

import sys
import os
import re
from optparse import OptionParser
from subprocess import check_call, CalledProcessError

try:
  from opbasm_lib.color import *
except ImportError:
  # Provide dummy functions if the color module isn't found
  def note(t): return t
  def success(t): return t
  def warn(t): return t
  def error(t): return t


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

def report_error(*args, **kwargs):
  print(error('ERROR:'), *args, file=sys.stderr)
  if 'exit' in kwargs:
    sys.exit(kwargs['exit'])


def main():
    print(note('Picoblaze ROM updater'))
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
        report_error('mem file not found', exit=1)

    if not os.path.exists(options.ncd_file):
        report_error('NCD file not found', exit=1)

    if not os.path.exists(bit_file):
        report_error('bit file not found ({})'.format(bit_file), exit=1)

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
          report_error('XDL failure', exit=1)

    if not os.path.exists(xdl_file):
        report_error('XDL file not generated', exit=1)

    # Find RAMB16 instances in XDL file
    ram_insts = find_ram_instances(xdl_file)
    inames = ram_insts.keys()

    if len(inames) == 0:
        report_error('No RAM instances found', exit=1)

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
            report_error('Named RAM instance does not exist ({})'.format(options.ram_inst))
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
            report_error('Invalid selection', exit=1)

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
      report_error('data2mem failure', exit=1)


    print(success('Generated updated bit file:'), options.out_bit_file)
    sys.exit(0)


inst_re = re.compile('inst "([^"]+)" "RAMB\d+.*",.+RAMB\d+_(\w+) *,')

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
