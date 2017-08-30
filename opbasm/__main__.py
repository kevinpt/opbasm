#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright © 2014, 2015, 2017 Kevin Thibedeau
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

'''Open PicoBlaze Assembler

This is a cross-platform assembler for the PicoBlaze-3 and 
PicoBlaze-6 processors from Xilinx.

USAGE:
PicoBlaze-3 mode (default)

  opbasm.py <source file>.psm [-n <module/entity name>]


PicoBlaze-6 mode

  opbasm.py -6 <source file>.psm [-n <module/entity name>]


You can use all PicoBlaze-6 syntax extensions in PicoBlaze-3 code that don't
depend on PB-6 specific instructions.

For PicoBlaze-3 you can use the following extensions from PicoBlaze-6:
  * Decimal, binary, and character literals (41'd, 01000001'b, "A")
  * Predefined char constants and date/time stamp fields (CR, LF, HT, datestamp_day, etc.)
  * Inverted constants ( ~my_const )
  * Environment variable constants ( constant foo, %my_env_const )
  * INCLUDE, DEFAULT_JUMP, and INST directives
  * Address label constants (my_label'upper  my_label'lower)

For PicoBlaze-3 you CANNOT use the following:
  * STRING and TABLE directives
  * PicoBlaze-6 instructions (CALL@, COMPARECY, HWBUILD, JUMP@, LOAD&RETURN,
                              OUTPUTK, REGBANK, STAR, TESTCY)

Note that you can use the m4 macro package to get enhanced string processing on PB-3.

PicoBlaze-6 enhancements:
  KCPSM6.exe has the -c switch to limit the size of memory. OPBASM provides -m to do the same
  as well as -s to limit the scratchpad memory size to 64 or 128 bytes.
  MEM format files are output by default. HEX format is activated with -x.

Refer to the file "all_kcpsm6_syntax.psm" distributed with KCPSM6 for a detailed
explanation of the new PicoBlaze-6 syntax.
'''

from __future__ import print_function, division, unicode_literals, absolute_import

import os, sys
from optparse import OptionParser

import opbasm.opbasm as opbasm
from .opbasm import *


# Fix broken UTF-8 support on Windows console
if sys.platform == 'win32' and sys.stdout.encoding == 'cp65001':
  try:
    from opbasm.win_console import *
    fix_broken_win_console()
  except ImportError:
    pass


def parse_command_line(argv):
  '''Process command line arguments'''
  progname = 'opbasm'
  usage = _('''{} [-i] <input file> [-n <name>] [-t <template>] [-6|-3] [-m <mem size>] [-s <scratch size>]
              [-d] [-r] [-e <address>]
              [-o <output dir>] [-q] [--m4]
              [-v] [-V] [--debug-preproc <file>]
       {} -g''').format(progname, progname)
  parser = OptionParser(usage=usage)

  parser.add_option('-i', '--input', dest='input_file', help=_('Input file'))
  parser.add_option('-n', '--name', dest='module_name', help=_('Module or entity name (defaults to input file name)'))
  parser.add_option('-t', '--template', dest='template_file', help=_('Template file'))
  parser.add_option('-6', '--pb6', dest='use_pb6', action='store_true', default=False, \
        help=_('Assemble PicoBlaze-6 code'))
  parser.add_option('-3', '--pb3', dest='use_pb3', action='store_true', default=False, \
        help=_('Assemble PicoBlaze-3 code'))
  parser.add_option('-m', '--mem-size', dest='mem_size', \
                    default=0, type=int, help=_('Program memory size'))
  parser.add_option('-s', '--scratch-size', dest='scratch_size', \
                    default=0, type=int, help=_('Scratchpad memory size'))
  parser.add_option('-x', '--hex', dest='hex_output', action='store_true', default=False, \
        help=_('Write HEX in place of MEM file'))
  parser.add_option('--mif', dest='mif_output', action='store_true', default=False, \
        help=_('Write MIF in place of MEM file'))
  parser.add_option('-o', '--outdir', dest='output_dir', default='.', help=_('Output directory'))

  parser.add_option('-O', '--optimize', dest='optimize_level', action='store', \
        default=0, type=int, help=_('Optimization level'))
  parser.add_option('-d', '--report-dead-code', dest='report_dead_code', action='store_true', \
        default=False, help=_('Perform dead code analysis shown in log file'))
  parser.add_option('-r', '--remove-dead-code', dest='remove_dead_code', action='store_true', \
        default=False, help=_('Remove dead code from assembled source'))
  parser.add_option('-e', '--entry-point', dest='entry_point', default=[], action='append', \
        metavar='ADDRESS', help=_('Set address of ISR (or other) entry point'))

  parser.add_option('-c', '--color-log', dest='color_log', action='store_true', default=False, \
        help=_('Colorize log file'))
  parser.add_option('-R', '--reflines', dest='refline_cols', action='store', default=8, type=int, \
        help=_('Set number of columns for reflines in log'))
  parser.add_option('--ascii', dest='ascii', action='store_true', default=False, \
        help=_('Render reflines with ASCII-only characters'))
  parser.add_option('-g', '--get-templates', dest='get_templates', action='store_true', default=False, \
        help=_('Get default template files'))
  parser.add_option('-v', '--version', dest='version', action='store_true', default=False, \
        help=_('Show OPBASM version'))
  parser.add_option('-q', '--quiet', dest='quiet', action='store_true', default=False, \
        help=_('Quiet output'))
  parser.add_option('-V', '--verbose', dest='verbose', action='store_true', default=False, \
        help=_('Verbose output'))
  parser.add_option('--m4', dest='use_m4', action='store_true', default=False, \
        help=_('Use m4 preprocessor on all source files'))
  parser.add_option('-D', '--define', dest='m4_defines', action='append', default=[], \
        metavar='NAME[=VALUE]', help=_('Define m4 macro NAME as having VALUE or empty'))
  parser.add_option('--debug-preproc', dest='debug_preproc', metavar='FILE', \
        help=_('Transformed source file after initial preprocessing'))
  
  options, args = parser.parse_args(argv)

  if options.version:
    print(_('OPBASM version'), __version__)
    sys.exit(0)

  if not options.get_templates:
    if not options.input_file:
      if len(args) > 0:
        options.input_file = args[0]

    if not options.input_file: parser.error(_('Missing input file'))

    if not options.module_name:
      options.module_name = os.path.splitext(os.path.basename(options.input_file))[0]

    if options.use_pb3 and options.use_pb6:
      parser.error(_('Cannot select both PicoBlaze architectures'))

    if options.use_pb6:
      scratch_sizes = (64, 128, 256)
      max_mem_size = 4096
    else: # Default to PB3
      scratch_sizes = (64,)
      max_mem_size = 1024

    if options.scratch_size == 0:
      options.scratch_size = max(scratch_sizes)
    elif options.scratch_size not in scratch_sizes:
      parser.error(_('Invalid scratchpad size'))

    if options.mem_size == 0:
      options.mem_size = max_mem_size
    elif options.mem_size > max_mem_size:
      parser.error(_('Memory size is too large'))


    if len(options.entry_point) == 0:
      options.entry_point.append('0x3FF')

    try:
      options.entry_point = [int(ep,0) for ep in options.entry_point]    
    except ValueError:
        parser.error(_('Invalid entry point address'))

    # Convert m4 definitions into a dict
    options.m4_defines = \
      {t[0]:t[1] if len(t) == 2 else None for t in (d.split('=') for d in options.m4_defines)}

  return options


def find_templates(template_file):
  '''Search for HDL template files'''
  templates = {}
  if template_file is not None:
    if os.path.exists(template_file):
      ext = os.path.splitext(template_file)[1]
      if ext.lower() in ('.vhd', '.vhdl'):
        templates['vhdl'] = template_file
      else:
        templates['verilog'] = template_file
    else:
      raise FatalError(_('Given template file \'{}\' does not exist.').format(template_file))

  else: # Search for standard templates
    vhdl_templates = ('ROM_form.vhd', 'ROM_form.vhdl')
    verilog_templates = ('ROM_form.v',)

    for f in vhdl_templates:
      if os.path.exists(f):
        templates['vhdl'] = f
        break

    for f in verilog_templates:
      if os.path.exists(f):
        templates['verilog'] = f
        break

  return templates


def asm_error(*args, **kwargs):
  '''Print an error message'''
  print(error(_('\nERROR:')), *args, file=sys.stderr)
  if 'statement' in kwargs:
    s = kwargs['statement']
    if s is not None:
      print('  {}:  {}'.format(s.error_line, s.format().lstrip()))
  if 'exit' in kwargs:
    sys.exit(kwargs['exit'])



def main():
  '''Main application code'''

  argv = sys.argv[1:]
  options = parse_command_line(argv)

  # Convert options into assembler config
  config = AssemblerConfig()
  config.mem_size = options.mem_size
  config.scratch_size = options.scratch_size
  config.target_arch = DevicePb6() if options.use_pb6 else DevicePb3()
  config.use_m4 = options.use_m4
  config.m4_defines = options.m4_defines
  config.debug_preproc = options.debug_preproc
  config.optimize_level = options.optimize_level
  config.output_dir = options.output_dir
  config.entry_point = options.entry_point
  config.verbose = options.verbose
  config.quiet = options.quiet

  def printq(*args, **keys):
    if not config.quiet:
      utf_args = [a.encode('utf-8') for a in args]
      print(*utf_args, **keys)

  printq(note(_('OPBASM - Open PicoBlaze Assembler {}').format(opbasm.__version__)))

  # Get the current language from the message catalog
  lang = _('en')

  # Notify user about weak translations
  if lang in ('fr',):
    printq(success(_('''  Translation for <this language> is machine generated.
  You can improve it by editing the message catalogs. See CONTRIBUTING for more info.''')))
  
  if not options.use_pb6 and not options.use_pb3: # We defaulted to PB3
    printq(warn(_('''  WARNING: PicoBlaze-6 will become the default target in version 1.4.
  Convert any PicoBlaze-3 projects to use the -3 option.''')))


  if options.get_templates:
    get_standard_templates()
    sys.exit(0)

  if options.hex_output:
    hex_mem_file = build_path(config.output_dir, options.module_name + '.hex')
  elif options.mif_output:
    hex_mem_file = build_path(config.output_dir, options.module_name + '.mif')
  else:
    hex_mem_file = build_path(config.output_dir, options.module_name + '.mem')

  try:
    templates = find_templates(options.template_file)
  except StatementError, e:
    asm_error(*e.args, exit=1)
  except FatalError, e:
    asm_error(str(e), exit=1)

  # Make sure the extension of the generated VHDL file matches the template extension
  if 'vhdl' in templates:
    vhdl_ext = os.path.splitext(templates['vhdl'])[1]
  else:
    vhdl_ext = '.vhdl'

  # Convert template values into tuples
  vhdl_file = build_path(config.output_dir, options.module_name + vhdl_ext)
  verilog_file = build_path(config.output_dir, options.module_name + '.v')

  for hdl in templates.iterkeys():
    templates[hdl] = (templates[hdl], vhdl_file if hdl == 'vhdl' else verilog_file)

  log_file = build_path(config.output_dir, options.module_name + '.log')


  if options.input_file == '-': # Ensure a module name was provided
    if options.module_name == '-':
      asm_error(_('Module name must be provided when reading from stdin (use -n <name>)'), exit=1)
  else: # Check for existence of input file
    if not os.path.exists(options.input_file):
      asm_error(_('Input file not found'), exit=1)
    
  printq(note(_('Target architecture: ')) + success(config.target_arch.name))

  printq(_('  Device configuration:\n    Memory size: {}, Scratchpad size: {}\n').format(\
    config.mem_size, config.scratch_size))

  # Assemble the code
  asm = Assembler(config)
  asm.command_line_mode = True

  if options.report_dead_code or options.remove_dead_code:
    asm.add_optimizer(StaticAnalyzer())

  if options.remove_dead_code:
    asm.add_optimizer(DeadCodeRemover())


  try:
    assembled_code = asm.assemble_file(options.input_file)
  except StatementError, e:
    asm_error(*e.args, exit=1, statement=e.statement)
  except FatalError, e:
    asm_error(str(e), exit=1)


  # Print summary
  stats = asm.code_stats()

  if not asm.config.quiet:
    printq(_('    {} instructions out of {} ({}%)').format(stats['inst_count'], \
          asm.config.mem_size, int(stats['inst_count'] / asm.config.mem_size * 100)))
    printq(_('    Highest occupied address: {:03X} hex').format(stats['last_addr']))

    if len(templates) > 0:
      printq(_('\n  Found {}:').format(_('templates') if len(templates) > 1 else _('template')))
      for f in templates.itervalues():
        printq('   ', f[0])


  # Write results
  printq(_(_('\n  Writing output')))

  field_size = 19

  if config.debug_preproc:
    printq('{:>{}}'.format(_('preprocessor:'), field_size), config.debug_preproc)

  if options.hex_output:
    asm.write_hex_file(hex_mem_file)
  elif options.mif_output:
    asm.write_mif_file(hex_mem_file)
  else:
    asm.write_mem_file(hex_mem_file)
  
  printq('{:>{}}'.format(_('mem map:'), field_size), hex_mem_file)
  
  asm.write_log_file(log_file, options.color_log, options.refline_cols, not options.ascii)
  printq('{:>{}}'.format(_('log file:'), field_size), log_file)

  try:
    asm.write_template_file(templates)
  except StatementError, e:
    asm_error(*e.args, exit=1, statement=e.statement)
  except FatalError, e:
    asm_error(str(e), exit=1)

  asm.write_formatted_source(config.output_dir)

  sys.exit(0 if asm.valid_asm else 1)


if __name__ == '__main__':
  main()

