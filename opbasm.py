#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright © 2014, 2015 Kevin Thibedeau
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


from __future__ import print_function, division, unicode_literals

import sys
import os
import errno
import subprocess
from optparse import OptionParser
import datetime
import copy
import string
import io
import re
import gettext


# Fix broken UTF-8 support on Windows console
if sys.platform == 'win32' and sys.stdout.encoding == 'cp65001':
  try:
    from opbasm_lib.win_console import *
    fix_broken_win_console()
  except ImportError:
    pass


def find_lib_dir():
  # Look relative to installed library
  try:
    lib_dir = os.path.dirname(sys.modules['opbasm_lib'].__file__)
  except KeyError:
    # Look relative to this script
    lib_dir = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'opbasm_lib')

  return lib_dir

if sys.version_info[0] < 3:
  gettext.install('opbasm', os.path.join(find_lib_dir(), 'lang'), unicode=True)
else:
  gettext.install('opbasm', os.path.join(find_lib_dir(), 'lang'))

try:
  from opbasm_lib.color import *
except ImportError:
  # Provide dummy functions if the color module isn't found
  def note(t): return t
  def success(t): return t
  def warn(t): return t
  def error(t): return t

__version__ = '1.3'


class FatalError(Exception):
  '''General error reporting exception'''
  def __init__(self, statement, *args):
    self.statement = statement
    self.args = args

  def __str__(self):
    return '{}\n{}'.format(self.statment, self.args)


def get_op_info(use_pb6):
  '''Generate a dict of opcode info for each PicoBlaze target'''
  op_info = {}

  if not use_pb6:
    op_info['has_string_table_support'] = False

    op_info['opcodes'] = { \
      'add': 0x18000, 'addcy': 0x1a000, 'and': 0x0a000, 'call': 0x30000, \
      'compare': 0x14000, 'disable': 0x3c000, 'enable': 0x3c001,
      'fetch': 0x06000, 'input': 0x04000, 'jump': 0x34000, \
      'load': 0x00000, 'or': 0x0c000, 'output': 0x2c000, 'return': 0x2a000, \
      'returni': 0x38000, \
      'rl': 0x20002, 'rr': 0x2000c, 'sl0': 0x20006, 'sl1': 0x20007, \
      'slx': 0x20004, 'sla': 0x20000, 'sr0': 0x2000e, 'sr1': 0x2000f, \
      'sra': 0x20008, 'srx': 0x2000a, 'store': 0x2e000, 'sub': 0x1c000, \
      'subcy': 0x1e000, 'test': 0x12000, 'xor': 0x0e000, \
      'inst': 0x00000 \
    }

    op_info['flag_opcodes'] = set(('call', 'jump', 'return'))
    op_info['flag_codes'] = {
      'c' : 0x1800,
      'nc': 0x1c00,
      'z' : 0x1000,
      'nz': 0x1400
    }
    op_info['return_flag_codes'] = op_info['flag_codes']

    op_info['addr_opcodes'] = set(('call', 'jump'))

    op_info['one_reg_opcodes'] = set(('rl', 'rr', 'sl0', 'sl1', 'sla', 'slx', 'sr0', 'sr1', 'sra', 'srx'))

    op_info['two_reg_opcodes'] = set(('add', 'addcy', 'and', 'compare', 'fetch', 'input', \
      'load', 'or', 'output', 'store', 'sub', 'subcy', 'test', 'xor'))
    op_info['two_reg_op_offset'] = 0x1000

    op_info['directives'] = set(('address', 'constant', 'namereg', \
        'include', 'default_jump'))

  else: # PicoBlaze-6
    op_info['has_string_table_support'] = True

    op_info['opcodes'] = { \
      'add': 0x11000, 'addcy': 0x13000, 'and': 0x03000, 'call': 0x20000, \
      'compare': 0x1d000, 'disable': 0x28000, 'enable': 0x28001,
      'fetch': 0x0b000, 'input': 0x09000, 'jump': 0x22000, \
      'load': 0x01000, 'or': 0x05000, 'output': 0x2d000, 'return': 0x25000, \
      'returni': 0x29000, \
      'rl': 0x14002, 'rr': 0x1400c, 'sl0': 0x14006, 'sl1': 0x14007, \
      'slx': 0x14004, 'sla': 0x14000, 'sr0': 0x1400e, 'sr1': 0x1400f, \
      'sra': 0x14008, 'srx': 0x1400a, 'store': 0x2f000, 'sub': 0x19000, \
      'subcy': 0x1b000, 'test': 0x0d000, 'xor': 0x07000, \

      # New PicoBlaze-6 instructions:
      'call@': 0x24000, 'comparecy': 0x1f000, 'hwbuild': 0x14080, 'jump@': 0x26000, \
      'load&return': 0x21000, 'outputk': 0x2b000, 'regbank': 0x37000, 'star': 0x17000, \
      'testcy': 0x0f000, 'inst': 0x00000 \
    }

    op_info['flag_opcodes'] = set(('call', 'jump', 'return'))
    op_info['flag_codes'] = {
      'c' : 0x18000,
      'nc': 0x1c000,
      'z' : 0x10000,
      'nz': 0x14000
    }

    # PicoBlaze-6 uses inconsistent offsets for the conditional return instructions
    op_info['return_flag_codes'] = {
      'c' : 0x14000,
      'nc': 0x18000,
      'z' : 0x0c000,
      'nz': 0x10000
    }

    op_info['addr_opcodes'] = set(('call', 'jump'))

    op_info['one_reg_opcodes'] = set(('rl', 'rr', 'sl0', 'sl1', 'sla', 'slx', 'sr0', 'sr1', 'sra', 'srx', \
      'hwbuild'))

    op_info['two_reg_opcodes'] = set(('add', 'addcy', 'and', 'compare', 'fetch', 'input', \
      'load', 'or', 'output', 'store', 'sub', 'subcy', 'test', 'xor', \
      'comparecy', 'testcy'))
    op_info['two_reg_op_offset'] = -0x1000

    op_info['directives'] = set(('address', 'constant', 'namereg', \
        'include', 'default_jump', 'string', 'table'))

  return op_info



def fail(s, loc, tokens):
  raise ParseFatalException(s, loc, 'Unknown token "{}", s:"{}", loc:"{}"'.format(tokens[0], s, loc))

regex_parser = re.compile(r'''
  (?:
    (?P<label>[.\w]+):\s*
  )?
  (?:
    (?P<cmd>[\w&@]+)\s*
    (?:
        (?:(?P<arg1>[.\w~'#$]+)\s*|(?P<arg1s>".+")\s*)
        (?:,\s*(?P<arg2>[.\w~'%#$]+)|,\s*\(\s*(?P<arg2b>\w+)\s*\)
        |,\s*\[(?P<arg2t>[^\]]+\]('[db])?)|,\s*(?P<arg2s>".+"))?
        |\(\s*(?P<addr1>[.\w~']+)\s*,\s*(?P<addr2>[.\w~']+)\s*\)
    )?\s*
  )?
  (?P<cmnt>;.*)?$
''', re.VERBOSE)

regex_register = re.compile(r'^s[0-9A-F]$', re.IGNORECASE)

class ParseError(ValueError):
  pass

def regex_parse_statement(l):
  '''Regex based parser that performs significantly faster than the original pyparsing
  based recursive descent parser
  '''

  ptree = {}

  m = regex_parser.search(l)
  if m:
    if m.group('label'): ptree['label'] = [m.group('label')]
    if m.group('cmnt'): ptree['comment'] = [m.group('cmnt')[1:]]

    args = []
    if m.group('arg1'): args.append(m.group('arg1'))
    if m.group('arg1s'): args.append(m.group('arg1s'))

    if m.group('arg2'): args.append(m.group('arg2'))
    if m.group('arg2b'):
      args.append([m.group('arg2b')])
      args.append('ireg') # Flag to indicate indirect addressing

    if m.group('arg2t'): # Table definition
      tdata, tend = m.group('arg2t').split(']')
      tdata = [d.strip() for d in tdata.split(',')]
      tdata.append(']' + tend)
      args.append(tdata)

    if m.group('arg2s'): # String definition
      args.append(m.group('arg2s'))
      

    # PicoBlaze-6 *@ instructions
    if m.group('addr1') and m.group('addr2'):
      args.append([m.group('addr1'), m.group('addr2')])
      args.append('iaddr')

    # Convert default register names (s*) to lowercase
    lcargs = []
    for a in args:
      try:
        if regex_register.match(a):
          lcargs.append(a.lower())
        else:
          lcargs.append(a)
      except TypeError: # Argument is a list
        a2 = []
        for sa in a:
          if regex_register.match(sa):
            a2.append(sa.lower())
          else:
            a2.append(sa)
        lcargs.append(a2)
          
    args = lcargs

    if m.group('cmd'):
      cmd = [m.group('cmd').lower()]
      cmd.extend(args)
      ptree['instruction'] = cmd

  # Certain invalid statements will not match the regex or will partially match
  # just the comment. Check that nothing else is present in the line
  non_comment = ''
  if len(ptree) == 0:
    non_comment = l
  elif len(ptree) == 1 and 'comment' in ptree:
    non_comment = l.split(';')[0]

  if not non_comment.isspace() and non_comment != '':
    # There is unparsed text in this line
    raise ParseError

   

  return {'statement': ptree}




class Statement(object):
  '''Low level representation of a statement (instructions, directives, comments)'''
  def __init__(self, ptree, line, source_file, ix_line):
    '''
    ptree : pyparsing style parse tree object for a single statement
    line : source line number
    '''

    self.source_file = source_file
    self.line = line
    self.ix_line = ix_line
    self.label = ptree['label'][0] if 'label' in ptree else None
    self.xlabel = self.label
    self.comment = ptree['comment'][0] if 'comment' in ptree else None

    self.command = None
    self.arg1 = None
    self.arg2 = None
    self.indirect_reg = False
    self.indirect_addr = False
    self.table_def = False

    # Instruction fields
    self.address = -1
    self.opcode = 0
    self.regx = 0
    self.regy = 0
    self.immediate = 0

    # Dead code analysis flags
    self.reachable = False

    self.tags = {}

    if 'instruction' in ptree:
      ifields = ptree['instruction']
      self.command = ifields[0]

      if len(ifields) > 1:
        if 'iaddr' in ifields: # Indirect address (e.g. jump@ (s1, s2))
          self.arg1 = ifields[1][0]
          self.arg2 = ifields[1][1]
          self.indirect_addr = True
          return

        else: # Normal operand
          self.arg1 = ifields[1]

      if len(ifields) > 2:
        if 'table' in ifields:
          self.arg2 = ifields[2]
          self.table_def = True
        elif 'ireg' in ifields:
          self.arg2 = ifields[2][0]
          self.indirect_reg = True
        else:
          self.arg2 = ifields[2]

  def machine_word(self):
    '''Returns the numeric value of the assembled instruction'''
    return self.opcode + (self.regx << 8) + (self.regy << 4) + self.immediate

  def is_instruction(self):
    '''Identify if this statement is an instruction (vs. directive)'''
    if self.command is None: return False
    if self.command in ('address', 'constant', 'namereg', 'include', 'default_jump', \
        'string', 'table'): return False
    return True

  def removable(self):
    '''Identify if this statement is eligible for dead code removal'''
    return self.is_instruction() and self.reachable == False and 'keep' not in self.tags \
      and 'keep_auto' not in self.tags

  re_ansi_strip = re.compile(r'\x1b[^m]*m')

  def format(self, upper=True, show_addr=False, show_dead=False, colorize=False):
    '''Generate a formatted string for the statement
    upper : Upper case instructions and directives
    show_addr : Include assembled memory address and machine word
    '''
    label = self.label + ':' if self.label is not None else ''
    comment = ';' + self.comment if self.comment is not None else ''
    inst = ''
    if self.command is not None:
      inst = self.command
      if upper: inst = inst.upper()

      if self.indirect_addr:
        inst += ' ({}, {})'.format(self.arg1, self.arg2)
      else:
        if self.arg1 is not None:
          inst += ' ' + self.arg1
        if self.arg2 is not None:
          if self.indirect_reg:
            inst += ', (' + self.arg2 + ')'
          elif self.table_def or not isinstance(self.arg2, basestring):
            inst += ', [' + ', '.join(self.arg2[:-1]) + self.arg2[-1]
          else:
            inst += ', ' + str(self.arg2)

    if show_addr:
      if self.is_instruction():
        op = '{:05X}'.format(self.machine_word())
        op = note(op) if colorize else op # blue text
        addr = '{:03X}  {}'.format(self.address, op)
      elif self.address >= 0:
        addr = '{:03X}       '.format(self.address)
      else:
        addr = '          '

      if show_dead:
        if self.is_instruction() and not self.reachable:
          if 'keep_auto' in self.tags:
            addr += success(' KEEP') if colorize else ' KEEP'
            #addr += success(' AUTO') if colorize else ' AUTO'
          elif 'keep' in self.tags:
            addr += success(' KEEP') if colorize else ' KEEP'
          else:
            addr += error(' DEAD') if colorize else ' DEAD'
        else:
          addr += '     '
    else:
      addr = ''

    if colorize:
      if len(comment) > 0:
        comment = success(comment) # green text
      if len(label) > 0:
        label = error(label) # red text

    # The ANSI color codes interfere with field width format specifiers
    # Detect them and add an adjustment
    label_width = 20 + len(label) - len(self.re_ansi_strip.sub('', label))

    if len(inst) > 0:
      return '{} {:>{}} {:30} {}'.format(addr, label, label_width, inst, comment).rstrip()
    else:
      return '{} {:>{}} {}'.format(addr, label, label_width, comment).rstrip()

  @property
  def error_line(self):
    '''Generate line number summary for error messages'''
    if self.ix_line is not None:
      return _('{} line {} (expanded line {})').format(self.ix_line[2], self.ix_line[1], self.line)
    else:
      return _('{} line {}').format(self.source_file if self.source_file else 'UNKNOWN', self.line)


class Symbol(object):
  '''Entry object for symbol tables (labels, constants, registers, strings, tables)'''
  def __init__(self, name, value, val_text=None, source_file=None, source_line=-1):
    self.name = name
    self.value = value
    self._val_text = val_text
    self.source_file = source_file
    self.source_line = source_line
    self.in_use = False # Track use of labels, constants, tables, and strings

  @property
  def val_text(self):
    '''Returns string value for the symbol'''
    if self._val_text is None:
      try:
        return '{:02X}'.format(self.value)
      except ValueError:
        return str(self.value)
    else:
      return self._val_text



def parse_lines(lines, op_info, source_file, index):
  '''Parse a list of text lines into Statement objects'''

  statements = []
  for i, l in enumerate(lines):
    try:
      ptree = regex_parse_statement(l)
    except ParseError:
      if index is not None:
        ix_line = index[i]
        error_line = _('{} line {} (expanded line {})').format(ix_line[2], ix_line[1], i+1)
      else:
        error_line = _('{} line {}').format(source_file, i+1)
      print(error(_('PARSE ERROR:')) + _(' Bad statement in {}:\n  {}').format(error_line, l))


      sys.exit(1)

    ix_line = index[i] if index is not None else None
    statements.append(Statement(ptree['statement'], i+1, source_file, ix_line))
    #print('### ptree:', i+1, ptree['statement'])

  return statements


def find_m4():
  m4_cmd = ''
  if sys.platform == 'win32': # Use included m4 binary in opbasm_lib on Windows (except within Cygwin)
    m4_cmd = os.path.join(find_lib_dir(), 'm4', 'm4.exe')
  
  if not os.path.exists(m4_cmd): # Use system path to find m4
    m4_cmd = 'm4'

  return m4_cmd


def find_standard_m4_macros():
  macro_file = os.path.join(find_lib_dir(), 'picoblaze.m4')
  if not os.path.exists(macro_file):
    print(_('  No m4 macro directory found'), macro_file)
    sys.exit(1)

  return macro_file

  
class Assembler(object):
  '''Main object for running assembler and tracking symbol information'''
  def __init__(self, top_source_file, timestamp, options, upper_env_names):
    self.top_source_file = top_source_file
    self.mem_size = options.mem_size
    self.scratch_size = options.scratch_size
    self.use_pb6 = options.use_pb6
    self.use_m4 = options.use_m4
    self.m4_defines = options.m4_defines
    self.debug_preproc = options.debug_preproc
    self.use_static_analysis = options.report_dead_code or options.remove_dead_code
    self.m4_file_num = 0
    self.output_dir = options.output_dir
    self.timestamp = timestamp

    self.constants = self._init_constants()
    self.labels = {}
    self.removed_labels = set()
    self.cur_context = ''

    self.registers = None
    self.init_registers()
    self.strings = self._init_strings()
    self.tables = {}

    self.sources = {}
    self.default_jump = None

    self.op_info = get_op_info(options.use_pb6)
    self.upper_env_names = upper_env_names
    
    self.line_index = {}

  def init_registers(self):
    '''Initialize table of register names'''
    hex_digits = [hex(d)[-1] for d in xrange(16)]
    self.registers = dict(('s' + h, i) for i, h in enumerate(hex_digits))

  def _init_constants(self):
    '''Initialize the constant symbol table with the
       PicoBlaze-6 automatic timestamp and datestamp values'''

    constants = {
      'timestamp_hours'   : Symbol('timestamp_hours', self.timestamp.hour),
      'timestamp_minutes' : Symbol('timestamp_minutes', self.timestamp.minute),
      'timestamp_seconds' : Symbol('timestamp_seconds', self.timestamp.second),
      # Leave some work for someone to fix in Y3K
      'datestamp_year'    : Symbol('timestamp_year', self.timestamp.year - 2000),
      'datestamp_month'   : Symbol('timestamp_month', self.timestamp.month),
      'datestamp_day'     : Symbol('timestamp_day', self.timestamp.day),

      'NUL' : Symbol('NUL', 0x00),
      'BEL' : Symbol('BEL', 0x07),
      'BS'  : Symbol('BS', 0x08),
      'HT'  : Symbol('HT', 0x09),
      'LF'  : Symbol('LF', 0x0a),
      'VT'  : Symbol('VT', 0x0b),
      'CR'  : Symbol('CR', 0x0d),
      'ESC' : Symbol('ESC', 0x1b),
      'DEL' : Symbol('DEL', 0x7f),
      'DCS' : Symbol('DCS', 0x90),
      'ST'  : Symbol('ST', 0x9c)
    }
    
    for c in constants.itervalues():
      c.in_use = True

    return constants


  def _init_strings(self):
    '''Initialize predefined strings'''
    ts = self.timestamp.strftime('%H:%M:%S')
    ds = self.timestamp.strftime('%d %b %Y')
    strings = {
      'timestamp$' : Symbol('timestamp$', ts, '"{}"'.format(ts)),
      'datestamp$' : Symbol('datestamp$', ds, '"{}"'.format(ds)),
    }
    
    for s in strings.itervalues():
      s.in_use = True

    return strings
    

  def preprocess_with_m4(self, source_file):
    '''Determine if file should be preprocessed for m4 macros and generate
    expanded file with ".gen.psm" extension
    '''
    if (not self.use_m4) and os.path.splitext(source_file)[1] not in ('.psm4', '.m4'): # No change
      return source_file

    pp_source_file = os.path.splitext(source_file)[0] + '.gen.psm'
    pp_source_file = add_output_dir(self.output_dir, pp_source_file)

    # Look for common picoblaze.m4 macro definitions
    macro_defs = find_standard_m4_macros()
    proc_mode = 'PB6' if self.use_pb6 else 'PB3' # Definition for active processor type

    # Preprocess C-style syntax
    pure_m4 = self.preprocess_c_style(source_file)
    if self.debug_preproc:
      with io.open(self.debug_preproc, 'w', encoding='utf-8') as fh:
        print(pure_m4, file=fh)

    self.m4_file_num += 1

    m4_options = ['-s'] # Activate synclines so we can track original line numbers

    source_dir = os.path.dirname(source_file)
    if len(source_dir) > 0:
      # Add source file directory to m4 search path for include() macros
      m4_options.append('-I ' + os.path.dirname(source_file))

    m4_options = ' '.join(m4_options)
    
    m4_defines = self.m4_defines.copy()
    # Set default defines for m4, overriding any attempt to redefine them by the user
    m4_defines.update({proc_mode:None,
                  'M4_FILE_NUM':self.m4_file_num,
                  'DATE_STAMP':self.strings['datestamp$'].val_text,
                  'TIME_STAMP':self.strings['timestamp$'].val_text
                  })

    # Build argument string for defines fed into m4
    m4_def_args = ' '.join('-D{}={}'.format(k,v) if v else '-D{}'.format(k) for k,v in m4_defines.iteritems())

    m4_cmd = find_m4()
    cmd = '{} {} {} {} -'.format(m4_cmd, m4_options, m4_def_args, macro_defs)
    p = subprocess.Popen(cmd, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    m4_result, m4_err = p.communicate(input=pure_m4.encode('utf-8'))
    if p.returncode:
      asm_error(_('m4 failure on file'), source_file, exit=1)
      
    # On Windows the pipe output is raw bytes containing '\r\n' for line terminators.
    # We need to manually convert these back to \n to prevent the \r from being preserved
    # and ending up with files containing \r\r\n which then results in unwanted empty lines
    # When Python reads back the file with expanded macros.
    if sys.platform == 'win32':
      m4_result = m4_result.decode('utf-8').replace('\r\n', '\n')
    else:
      m4_result = m4_result.decode('utf-8')

    # Use synclines to build index tracking how original source lines were expanded
    self.index_expanded_line_numbers(m4_result, pp_source_file, source_file)
      
    return pp_source_file


  def preprocess_c_style(self, source_file):
    '''Perform an initial transformation to convert C-style syntax into valid m4 macros'''
    lines = []
    if source_file == '-':
      lines = [l.decode('utf-8') for l in sys.stdin.readlines()]
    else:
      with io.open(source_file, 'r', encoding='utf-8') as fh:
        lines = fh.readlines()
      
    # Add a trailing empty line so that m4 doesn't complain about files without them
    # or ending with a comment
    lines.append('\n')

    elines = []
    # We need to first protect '}' chars inside strings to prevent them from being
    # turned into m4 syntax.
    curly_string = re.compile(r'^.*"(.*}.*)".*$')
    # The signed() macro needs its right paren escaped to prevent pattern matching failures
    signed_macro = re.compile(r'(signed\([^)]+\))', re.IGNORECASE)

    # Convert constant directives into const() macros
    const_def = re.compile(r'^([^;]*)constant\s+([^,]+)\s*,\s*("."|[^;]+)(;.*)?', re.IGNORECASE)
    
    # Escape quoted strings to ensure no words in them are expanded by m4
    string_def = re.compile(r'string(.*)("[^"]*")', re.IGNORECASE)

    for l in lines:
      m = curly_string.match(l)
      if m:
        escaped = m.group(1).replace('}', "`'_rcurly`'")
        l = re.sub(r'^(.*").*(".*)$',r'\1{}\2'.format(escaped),l)

      m = signed_macro.search(l)
      if m:
        escaped = m.group(1).replace(')', "`'_rparen`'")
        l = re.sub(r'signed\([^)]+\)', escaped, l)

      m = const_def.search(l)
      if m:
        if m.group(3) not in ('";"', '","', '"`"', '"\'"'):
          # Wrap quoted chars in m4 quotes to protect ",". This doesn't work for ";" so we leave it in pb syntax
          arg2 = m.group(3).strip()
          if arg2[0] == '"':
            arg2 = "`{}'".format(arg2)
          else:
            arg2 = arg2.replace("'", "!") # m4 doesn't play nice with strings that have embedded ' chars
          l = '{}const({}, {}) {}\n'.format(m.group(1), m.group(2).strip(), arg2, m.group(4) if m.group(4) else '')

      m = string_def.search(l)
      if m: # Protect quoted strings with extra m4 quotes
        l = re.sub(r'"[^"]*"', "`{}'".format(m.group(2)), l)

      elines.append(l)

    # Now we can run pattern substitutions to convert the C-style syntax into m4
    ecode = ''.join(elines)
    # else-if clause
    ecode = re.sub(r'}(?:\s*;.*\n)*\s*else(?:\s*;.*\n)*\s*if\s*\(([^)]*)\)(?:\s*;.*\n)*\s*{', r"', \1,`", ecode)
    ecode = re.sub(r'if\s*\(([^)]*)\)(?:\s*;.*\n)*\s*{', r'if(\1, `', ecode) # if statement
    ecode = re.sub(r'}(?:\s*;.*\n)*\s*else(?:\s*;.*\n)*\s*{', r"', `", ecode) # else clause
    ecode = re.sub(r'do(?:\s*;.*\n)*\s*{', r'_dowhile2(`', ecode) # start of do-while

    # proc def
    ecode = re.sub(r'proc\s+(\w+)\s*\(([^)]*)\)(?:\s*;.*\n)*\s*{', r"proc(\1,`\2',`", ecode)
    # func def
    ecode = re.sub(r'func\s+(\w+)\s*\(([^)]*)\)\s*:?\s*(\d*)(?:\s*;.*\n)*\s*{', r"func(\1,`\2',\3,`", ecode)
    # isr def
    ecode = re.sub(r'isr\s+(\w+)\s*\(([^)]*)\)\s*:?\s*(\w*)(?:\s*;.*\n)*\s*{', r"isr(\1,`\2',`\3',`", ecode)

    # while following a block
    ecode = re.sub(r'}(?:\s*;.*\n)*\s*while\s*\(([^)]*)\)(?:\s*;.*\n)*\s*{', r"')\nwhile(\1, `", ecode)
    ecode = re.sub(r'}(?:\s*;.*\n)*\s*while\s*\(([^)]*)\)', r"',\1)", ecode) # end of do-while
    ecode = re.sub(r'while\s*\(([^)]*)\)(?:\s*;.*\n)*\s*{', r'while(\1, `', ecode) # plain while
    ecode = re.sub(r'for\s*\(([^)]*)\)(?:\s*;.*\n)*\s*{', r'for(\1, `', ecode) # for loop
    ecode = re.sub(r'}', r"')", ecode) # end of a block
    return ecode

  def index_expanded_line_numbers(self, m4_result, pp_source_file, source_file):
    '''Strip out inserted m4 synclines comments and build an index relating original source lines to the
       macro expanded code'''
    syncline = re.compile(r'^#line (\d+) *(".*")?')

    index = []
    elines = []    
    cur_line = 1
    active_file = None
    source_lines = {}

    for l in m4_result.splitlines(True):
      m = syncline.match(l)
      if m: # This is a syncline
        if m.group(2): # This is start of a new file
          active_file = m.group(2).strip('"')
          if active_file == 'stdin':
            active_file = source_file
          source_lines[active_file] = int(m.group(1))
        else: # Expanded macro
          source_lines[active_file] = int(m.group(1))
      else: # Ordinary source line
        elines.append(l)
        index.append((cur_line, source_lines[active_file], active_file))
        cur_line += 1
        source_lines[active_file] = source_lines[active_file] + 1
        
    # Write preprocessed file with synclines removed
    with io.open(pp_source_file, 'w', encoding='utf-8') as fh:
      fh.writelines(elines)
      
    self.line_index[source_file] = index

  def process_includes(self, source_file=None):
    '''Scan a list of statements for INCLUDE directives and recursively
    read each included source file. Constant, string, and table definitions
    are also processed to keep track of where they are defined.
    This is a generator function that yields the name of each included file'''
    if source_file is None: source_file = self.top_source_file
    if source_file in self.sources: return

    pp_source_file = self.preprocess_with_m4(source_file)
    
    source_file_display = source_file if source_file != '-' else '<stdin>'

    if pp_source_file != source_file: # Preprocessor was run on source
      yield source_file_display + ' (m4)'
    else:
      yield source_file_display


    try:
      if pp_source_file == '-':
        source = [s.decode('utf-8').rstrip() for s in sys.stdin.readlines()]
      else:
        with io.open(pp_source_file, 'r', encoding='utf-8') as fh:
          source = [s.rstrip() for s in fh.readlines()]
    except UnicodeDecodeError:
      # Fall back to Latin-1 if UTF-8 fails
      if pp_source_file == '-':
        source = [s.decode('latin-1').rstrip() for s in sys.stdin.readlines()]
      else:
        with io.open(pp_source_file, 'r', encoding='latin-1') as fh:
          source = [s.rstrip() for s in fh.readlines()]


    index = self.line_index[source_file] if source_file in self.line_index else None
    slist = parse_lines(source, self.op_info, source_file, index)
    self.sources[source_file] = slist

    # Scan for include directives
    for s in slist:
      # Track label sources
      if s.label is not None:
        if s.label.startswith('.'): # Local label
          xlabel = self.expand_label(s.label)
          s.xlabel = xlabel
        else: # Global label
          self.cur_context = s.label
          xlabel = s.label

        if xlabel in self.labels:
          raise FatalError(s, _('Redefinition of label:'), xlabel)

        #print('### LABEL DEF:', xlabel)
        self.labels[xlabel] = Symbol(s.label, -1, source_file=source_file, source_line=s.line)

      # Recursively include additional sources
      if s.command == 'include':
        if s.arg1 is not None and s.arg1[0] == '"' and s.arg1[-1] == '"':
          # Convert Windows backslashes to forward to get a path that works on all platforms
          include_file = s.arg1[1:-1].replace('\\', '/')

          # If the included file has a relative path build it off of the current path
          if not os.path.isabs(include_file):
            include_file = os.path.join(os.path.dirname(source_file), include_file)

          if not os.path.exists(include_file):
            raise FatalError(s, _('Include file not found:'), include_file)

          for inc_file in self.process_includes(include_file):
            yield inc_file
        else:
          raise FatalError(s, _('Invalid include parameter'), s.arg1)

      # Handle constant definitions before flattening
      elif s.command == 'constant':
        if s.arg1 is None or s.arg2 is None:
          raise FatalError(s, _('Missing argument to constant directive'))

        if s.arg1 in self.constants:
          raise FatalError(s, _('Redefinition of constant:'), s.arg1)

        # Prevent the use of 3 or less character constants that are valid hex literals
        if len(s.arg1) <= 3 and hex_to_int(s.arg1) is not None:
          raise FatalError(s, _('Invalid constant. Conflicts with hex literal'), s.arg1)

        if s.arg2[0] == '%': # Look up env variable
          ename = s.arg2[1:]
          if ename.upper() not in self.upper_env_names:
            raise FatalError(s, _('Unknown environment variable:'), ename)
          ename = self.upper_env_names[ename.upper()]
          cval = convert_literal(os.getenv(ename))
          self.constants[s.arg1] = Symbol(s.arg1, cval, val_text=s.arg2, \
              source_file=source_file, source_line=s.line)

        else: # Normal literal constant
          cval = convert_literal(s.arg2)
          self.constants[s.arg1] = Symbol(s.arg1, cval, val_text=s.arg2, \
              source_file=source_file, source_line=s.line)

      elif s.command == 'string':
        if s.arg1 is None or s.arg2 is None:
          raise FatalError(s, _('Missing argument to STRING directive'))
        if s.arg1[-1] != '$':
          raise FatalError(s, _('Invalid string name (missing $):'), s.arg1)
        if s.arg1 in self.strings:
          raise FatalError(s, _('Redefinition of string:'), s.arg1)
        if s.arg2[0] != '"' or s.arg2[-1] != '"':
          raise FatalError(s, _('Not a valid string:'), s.arg2)

        self.strings[s.arg1] = Symbol(s.arg1, s.arg2[1:-1], s.arg2, \
            source_file=source_file, source_line=s.line)

      elif s.command == 'table':
        if s.arg1 is None or s.arg2 is None:
          raise FatalError(s, _('Missing argument to TABLE directive'))
        if s.arg1[-1] != '#':
          raise FatalError(s, _('Invalid table name (missing #):'), s.arg1)
        if s.arg1 in self.strings:
          raise FatalError(s, _('Redefinition of table:'), s.arg1)
        if s.table_def is False:
          raise FatalError(s, _('Missing table definition'))

        # Determine the radix of the elements
        if s.arg2[-1].endswith('d'):
          radix = 10
        elif s.arg2[-1].endswith('b'):
          radix = 2
        else:
          radix = 16

        # Convert the table elements to integers
        try:
          tbl = [int(e, radix) for e in s.arg2[:-1]]
        except ValueError:
          raise FatalError(s, _('Invalid table element (radix {})').format(radix))

        # Ensure table values are within range
        for e in tbl:
          if not (0 <= e < 256):
            raise FatalError(s, _('Table value out of range:'), e)

        val_text = '[' + ', '.join(s.arg2[:-1]) + s.arg2[-1]
        self.tables[s.arg1] = Symbol(s.arg1, tbl, val_text, \
          source_file=source_file, source_line=s.line)


  def flatten_includes(self, slist, include_stack=None):
    '''Generator function that produces a flattened list of statements
    after evaluating INCLUDE directives.

    slist         : List of statements from top-level source file
    include_stack : Stack of parent source files used to detect recursive includes
    '''
    if include_stack is None: include_stack = [self.top_source_file]

    for s in slist:
      if s.command == 'include':
        include_file = s.arg1[1:-1].replace('\\', '/')

        # If the included file has a relative path build it off of the current path
        if not os.path.isabs(include_file):
          include_file = os.path.join(os.path.dirname(include_stack[-1]), include_file)

        if include_file in include_stack:
          raise FatalError(s, _('Recursive include:'), s.arg1)
        else:
          include_stack.append(include_file)
          islist = list(self.flatten_includes(self.sources[include_file], include_stack))
          include_stack.pop()
          for i in islist:
            yield copy.copy(i)
      else:
        yield s

  def expand_label(self, addr_label):
    if addr_label.startswith('.'):
      return self.cur_context + addr_label
    else:
      return addr_label


  def get_address(self, addr_label):
    '''Lookup the address assigned to addr_label'''

    xlabel = self.expand_label(addr_label)
    #print('## GA:', addr_label, xlabel)
    if xlabel in self.labels:
      self.labels[xlabel].in_use = True
      return self.labels[xlabel].value
    else:
      return convert_literal(addr_label)

  def get_constant(self, arg):
    '''Lookup the constant or literal value associated with arg'''
    invert_value = False
    if arg[0] == '~':
      invert_value = True
      arg = arg[1:]

    if arg.endswith("'upper") or arg.endswith("'lower"):
      # Address constant
      label, portion = arg.split("'")
      addr = self.get_address(label)
      if addr is None:
        return None

      if portion == 'lower':
        value = addr & 0xFF
      else:
        value = (addr >> 8) & 0xF

    elif arg in self.constants: # Normal constant
      self.constants[arg].in_use = True
      value = self.constants[arg].value
    else: # Attempt to convert a constant literal
      value = convert_literal(arg)

    if invert_value and value is not None:
      value = (~value) & 0xFF

    return value

  def get_register(self, arg):
    '''Lookup the register named in arg'''
    if arg in self.registers:
      return self.registers[arg]
    return None

  def get_string(self, name):
    '''Lookup the string associated with name'''
    if name in self.strings:
      self.strings[name].in_use = True
      return self.strings[name].value
    return None

  def get_table(self, name):
    '''Lookup the table associated with name'''
    if name in self.tables:
      self.tables[name].in_use = True
      return self.tables[name].value
    return None


  def statement_words(self, s):
    '''Determine the number of words generated for each instruction

    Normally this is 1 but the OUTPUTK and LOAD&RETURN instructions are
    replicated if a string or table is passed as an operand.
    s : Statement object
    '''
    if s.is_instruction():
      num_words = 1

      array_name = None
      if s.command == 'outputk':
        if s.arg1 is not None and s.arg1[-1] in ('$', '#'):
          array_name = s.arg1

      elif s.command == 'load&return':
        if s.arg2 is not None and s.arg2[-1] in ('$', '#'):
          array_name = s.arg2

      if array_name is not None:
        if array_name[-1] == '$':
          if array_name not in self.strings:
            raise FatalError(s, _('Unknown string:'), array_name)
          num_words = len(self.strings[array_name].value)
    
        else: # Table
          if array_name not in self.tables:
            raise FatalError(s, _('Unknown table:'), array_name)
          num_words = len(self.tables[array_name].value)

      return num_words
    else:
      return 0


  def assemble(self, bounds_check=True):
    '''Generate assembled instructions
    Returns a list of Statement objects with filled in instruction fields
    '''
    # Pass 1: Flatten includes
    slist = list(self.flatten_includes(self.sources[self.top_source_file]))

    # Scan for pragma meta-comments
    annotate_pragmas(slist)

    return self.raw_assemble(slist, 0, bounds_check)


  def raw_assemble(self, slist, start_address=0, bounds_check=True):
    '''Generate assembled instructions from a raw statement list'''
    cur_addr = start_address
    self.default_jump = None

    # Pass 2: Set instruction and label addresses
    for s in slist:
      if s.label is not None:
        if s.label.startswith('.'): # Local label
          xlabel = s.xlabel
        else: # Global label
          #print('### GLOBAL:', s.label)
          self.cur_context = s.label
          xlabel = s.label

        #print('### SET LABEL:', xlabel)
        if xlabel not in self.labels:
          raise FatalError(s, _('Label not found "{}". Possibly removed.').format(xlabel))

        self.labels[xlabel].value = cur_addr
        s.address = cur_addr

      if s.is_instruction():
        if bounds_check and cur_addr >= self.mem_size:
          raise FatalError(s, _('Address exceeds memory bounds: {:03X} (limit {:03X})').format( \
             cur_addr, self.mem_size-1))

        s.address = cur_addr
        cur_addr += self.statement_words(s) # Move to next address. Could be > 1 if a string or table operand

      elif s.command == 'address':
        cur_addr = self.get_address(s.arg1)
        if cur_addr is None:
          raise FatalError(s, _('Invalid address:'), s.arg1)

        if bounds_check and cur_addr >= self.mem_size:
          raise FatalError(s, _('Address exceeds memory bounds: {:03X} (limit {:03X})').format(\
            cur_addr, self.mem_size-1))

    # Assign phony addresses to non-instruction lines with comment or label
    for s in reversed(slist):
      if s.is_instruction():
        cur_addr = s.address
        continue
      elif s.comment or s.command:
        s.address = cur_addr

    

    # Pass 3: Validate and assemble instructions
    instructions = []
    self.cur_context = ''
    for s in slist:
      addr_label = None

      if s.label is not None and not s.label.startswith('.'):
        self.cur_context = s.label

      if s.is_instruction():
        # Verify instruction is valid
        if s.command not in self.op_info['opcodes']:
          raise FatalError(s, _('Invalid PicoBlaze-{} instruction:').format( \
            6 if self.use_pb6 else 3), s.command)

        s.opcode = self.op_info['opcodes'][s.command] # Set base opcode

        if s.command in self.op_info['flag_opcodes']:
          # Check if first argument is a flag
          addr_label = s.arg1
          if s.arg1 is not None:
            if s.command == 'return':
              flag_codes = self.op_info['return_flag_codes']
            else:
              flag_codes = self.op_info['flag_codes']

            if s.arg1.lower() in flag_codes:
              s.opcode += flag_codes[s.arg1.lower()]
              addr_label = s.arg2

          if s.command in self.op_info['addr_opcodes']: # Include address for call and jump
            if addr_label is None:
              raise FatalError(s, _('Missing address'))

            s.immediate = self.get_address(addr_label)
            if s.immediate is None:
              raise FatalError(s, _('Invalid address:'), addr_label)
            if bounds_check and s.immediate >= self.mem_size:
              raise FatalError(s, _('Out of range address'))

        elif s.command in self.op_info['one_reg_opcodes']:
          if s.arg1 is None:
            raise FatalError(s, _('Missing operand'))
          if s.arg2 is not None:
            raise FatalError(s, _('Illegal operand:'), s.arg2)

          s.regx = self.get_register(s.arg1)
          if s.regx is None:
            raise FatalError(s, _('Invalid register:'), s.arg1)

        elif s.command in self.op_info['two_reg_opcodes']:
          if s.arg1 is None or s.arg2 is None:
            raise FatalError(s, _('Missing operand'))

          s.regx = self.get_register(s.arg1)
          if s.regx is None:
            raise FatalError(s, _('Invalid register:'), s.arg1)

          s.regy = self.get_register(s.arg2)
          if s.regy is not None: # Using y register opcode
            s.opcode += self.op_info['two_reg_op_offset'] # Adjust opcode

          else: # The second arg was not a register
            s.regy = 0
            s.immediate = self.get_constant(s.arg2)

            if s.immediate is None:
              if s.arg2.endswith("'upper") or s.arg2.endswith("'lower"):
                xlabel = self.expand_label(s.arg2.split("'")[0])
                if xlabel in self.removed_labels:
                  raise FatalError(s, _('Label has been removed: {}\n       Add ";PRAGMA keep [on,off]" to preserve this label').format(s.arg2))
                else:
                  raise FatalError(s, _('Unknown label:'), s.arg2)
              else:
                raise FatalError(s, _('Invalid operand:'), s.arg2)
            if not (0 <= s.immediate < 256):
              raise FatalError(s, _('Immediate value out of range:'), s.immediate)

            if s.command in ('fetch', 'store'):
              if s.immediate >= self.scratch_size:
                raise FatalError(s, _('Scratchpad address out of range:'), hex(s.immediate))

        elif s.command in ('enable', 'disable'):
          if s.arg1 is None:
            raise FatalError(s, _('Missing operand'))
          if s.arg1.lower() != 'interrupt' or s.arg2 is not None:
            raise FatalError(s, _('Invalid operand to {}').format(s.command.upper()))

        elif s.command == 'returni':
          if s.arg1 is None:
            raise FatalError(s, _('Missing operand'))
          if s.arg1.lower() not in ('enable', 'disable'):
            raise FatalError(s, _('Invalid operand to RETURNI'))

          if s.arg1.lower() == 'enable':
            s.opcode += 1

        # Irregular PicoBlaze-6 instructions
        elif s.command in ('call@', 'jump@'):
          if s.arg1 is None or s.arg2 is None:
            raise FatalError(s, _('Missing operand'))
          s.regx = self.get_register(s.arg1)
          if s.regx is None:
            raise FatalError(s, _('Invalid register:'), s.arg1)

          s.regy = self.get_register(s.arg2)
          if s.regy is None:
            raise FatalError(s, _('Invalid register:'), s.arg2)

        elif s.command == 'load&return':
          if s.arg1 is None or s.arg2 is None:
            raise FatalError(s, _('Missing operand'))

          s.regx = self.get_register(s.arg1)
          if s.regx is None:
            raise FatalError(s, _('Invalid register:'), s.arg1)

          elems = []
          if s.arg2.endswith('$'):
            string_data = self.get_string(s.arg2)
            if string_data is not None:
              elems = [(ord(e), '"{}"'.format(e)) for e in string_data]

          elif s.arg2.endswith('#'):
            table_data = self.get_table(s.arg2)
            if table_data is not None:
              elems = [(e, '{:02X}'.format(e)) for e in table_data]

          if len(elems) > 0:
            for i, (e, e_text) in enumerate(elems):
              new_s = copy.deepcopy(s)
              new_s.immediate = e
              new_s.arg2 = e_text
              new_s.address += i
              if i > 0:
                new_s.comment = None
                new_s.label = None
              instructions.append(new_s)
            continue

          else:
            s.immediate = self.get_constant(s.arg2)
            if s.immediate is None:
              raise FatalError(s, _('Invalid operand:'), s.arg2)


        elif s.command == 'outputk':
          if s.arg1 is None or s.arg2 is None:
            raise FatalError(s, _('Missing operand'))

          port = self.get_constant(s.arg2)
          if port is None:
            raise FatalError(s, _('Invalid operand:'), s.arg2)
          if not 0 <= port < 16:
            port_name = '{:02X}'.format(port) if port == convert_literal(s.arg2) else '{} ({:02X})'.format(s.arg2, port)
            raise FatalError(s, _('Invalid port for OUTPUTK <value>, <port>:  {}\n       Port must range from 0 to F').format(port_name))

          elems = []
          if s.arg1.endswith('$'):
            elems = [(ord(e), '"{}"'.format(e)) for e in self.strings[s.arg1].value]
          elif s.arg1.endswith('#'):
            elems = [(e, '{:02X}'.format(e)) for e in self.tables[s.arg1].value]

          if len(elems) > 0: # Table or string argument
            # Create expanded load&return instructions for each element in string or table
            for i, (e, e_text) in enumerate(elems):
              new_s = copy.deepcopy(s)
              new_s.immediate = (e << 4) + port
              new_s.arg1 = e_text
              new_s.address += i
              if i > 0:
                new_s.comment = None
                new_s.label = None
              instructions.append(new_s)
            continue

          else: # Single constant argument
            const = self.get_constant(s.arg1)
            if const is None:
              raise FatalError(s, _('Invalid operand:'), s.arg1)
            if not (0 <= const < 256):
              raise FatalError(s, _('Immediate value out of range:'), const)

          s.immediate = (const << 4) + port

        elif s.command == 'regbank':
          if s.arg1 is None:
            raise FatalError(s, _('Missing operand'))
          if s.arg1.lower() not in ('a', 'b'):
            raise FatalError(s, _('Invalid operand to REGBANK'))

          if s.arg1.lower() == 'b':
            s.opcode += 1

        elif s.command == 'star':
          if s.arg1 is None or s.arg2 is None:
            raise FatalError(s, _('Missing operand'))

          # The STAR instruction has special rules for RegX (arg1)
          # It MUST be in the form: s[hexdigit]
          # We skip the usual table lookup for arg1
          if len(s.arg1) == 2 and s.arg1[0].lower() == 's' and s.arg1[1] in string.hexdigits:
            s.regx = int(s.arg1[1], 16)
          else:
            raise FatalError(s, _('Invalid register:'), s.arg1)

          # PB6 has the STAR sX, kk variant so we will accept a constant
          # as the second argument as well as a register.
          # http://forums.xilinx.com/t5/PicoBlaze/obscure-undocumented-property-of-REGBANK-instruction/m-p/489774#M2375

          s.regy = self.get_register(s.arg2)
          if s.regy is not None: # Using y register opcode
            s.opcode += self.op_info['two_reg_op_offset'] # Adjust opcode

          else: # The second arg was not a register
            s.regy = 0
            s.immediate = self.get_constant(s.arg2)

            if s.immediate is None:
              raise FatalError(s, _('Invalid operand:'), s.arg2)
            if not (0 <= s.immediate < 256):
              raise FatalError(s, _('Immediate value out of range:'), s.immediate)


        elif s.command == 'inst':
          # NOTE: INST is really a directive but we need to reserve a space in the
          # address map for its value so we treat it as an instruction with 0x00 opcode.

          if s.arg1 is None:
            raise FatalError(s, _('Missing operand'))
          s.immediate = convert_literal(s.arg1)
          if s.immediate is None or not 0 <= s.immediate < 2**18:
            raise FatalError(s, _('Invalid INST value:'), s.arg1)

        else:
          raise FatalError(s, _('Unknown instruction:'), s.command)


      else: # Not an instruction
        if s.command == 'namereg':
          if s.arg1 is None or s.arg2 is None:
            raise FatalError(s, _('Missing argument to NAMEREG directive'))
          if s.arg1 not in self.registers:
            raise FatalError(s, _('Unknown register name:'), s.arg1)

          self.registers[s.arg2] = self.get_register(s.arg1)
          del self.registers[s.arg1]


        elif s.command == 'default_jump':
          if self.default_jump is not None:
            raise FatalError(s, _('Redefinition of default jump'))
          if s.arg2 is not None:
            raise FatalError(s, _('Too many arguments to DEFAULT_JUMP'))

          self.default_jump = self.get_address(s.arg1)
          if self.default_jump is None:
            raise FatalError(s, _('Invalid address:'), s.arg1)

      instructions.append(s)


    # Pass 4: Find continuous blocks of labeled load&return instructions
    if self.use_pb6 and self.use_static_analysis:
      cur_label = None
      prev_jump = None
      for s in instructions:
        if s.label is not None:
          cur_label = s.xlabel
          prev_jump = None
        
        unconditional_jump = s.command == 'jump' and s.arg2 is None

        # Mark l&r for preservation if its associated label is referenced by other code
        # Mark two or more consecutive unconditional jumps for preservation as part of a jump table
        if s.command == 'load&return' or (unconditional_jump and prev_jump):
          if cur_label is not None and self.labels[cur_label].in_use:
            if 'keep' not in s.tags:
              s.tags['keep_auto'] = (True,)
              
              # Mark this as the (possible) end of a jump table and flag it for preservation
              if unconditional_jump: # and s.arg1 in self.labels:
                s.tags['jump_table_end'] = (True,)
                s.tags['keep'] = (True,) # For jump table instructions we need to tag with 'keep'
                del s.tags['keep_auto']

                # Mark previous jump as part of a jump table and flag it for preservation
                if 'jump_table_end' in prev_jump.tags: del prev_jump.tags['jump_table_end']
                prev_jump.tags['jump_table'] = (True,)
                prev_jump.tags['keep'] = (True,) # For jump table instructions we need to tag with 'keep'

        elif s.is_instruction() and not unconditional_jump: cur_label = None
        
        # Remember previous jump instruction to identify jump tables
        if unconditional_jump:
          prev_jump = s
        elif s.is_instruction():
          prev_jump = None

    # Apply keep_auto to INST directives
    if self.use_static_analysis:
      for s in instructions:
        if s.command == 'inst' and 'keep' not in s.tags:
          s.tags['keep_auto'] = (True,)


    # Create default jump instruction
    if self.default_jump is None:
      self.default_jump = 0
    else:
      self.default_jump = self.op_info['opcodes']['jump'] + self.default_jump

    return instructions


  def remove_dead_code(self, assembled_code):
    '''Mark unreachable code for removal'''
    for s in assembled_code:
      if s.removable():
        s.comment = _('REMOVED: ') + s.format().lstrip()
        s.command = None
        if s.label is not None:
          if s.xlabel in self.labels:
            #print('### Deleting label:', s.xlabel)
            del self.labels[s.xlabel]
            self.removed_labels.add(s.xlabel)
          s.label = None
          s.xlabel = None



def hex_to_int(s):
  '''Convert a hex string literal into an integer. Returns None on failure.'''
  try:
    return int(s, 16)
  except ValueError:
    return None

def convert_literal(arg):
  '''Convert a string literal into an integer. Returns None on failure.'''
  if "'d" in arg: # Decimal literal
    try:
      return int(arg[:-2])
    except ValueError:
      return None
  elif "'b" in arg: # Binary literal
    try:
      return int(arg[:-2], 2)
    except ValueError:
      return None
  elif '"' in arg and len(arg) == 3: # Character literal
    return ord(arg[1])
  else: # Assume it is a hex literal
    return hex_to_int(arg)


def asm_error(*args, **kwargs):
  '''Print an error message'''
  print(error(_('\nERROR:')), *args, file=sys.stderr)
  if 'statement' in kwargs:
    s = kwargs['statement']
    print('  {}:  {}'.format(s.error_line, s.format().lstrip()))
  if 'exit' in kwargs:
    sys.exit(kwargs['exit'])



def parse_command_line():
  '''Process command line arguments'''
  progname = os.path.basename(sys.argv[0])
  usage = _('''{} [-i] <input file> [-n <name>] [-t <template>] [-6|-3] [-m <mem size>] [-s <scratch size>]
              [-d] [-r] [-e <address>]
              [-o <output dir>] [-q] [--m4]
              [--debug-preproc <file>]
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


  parser.add_option('-d', '--report-dead-code', dest='report_dead_code', action='store_true', \
        default=False, help=_('Perform dead code analysis shown in log file'))
  parser.add_option('-r', '--remove-dead-code', dest='remove_dead_code', action='store_true', \
        default=False, help=_('Remove dead code from assembled source'))
  parser.add_option('-e', '--entry-point', dest='entry_point', default=[], action='append', \
        metavar='ADDRESS', help=_('Set address of ISR (or other) entry point'))

  parser.add_option('-c', '--color-log', dest='color_log', action='store_true', default=False, \
        help=_('Colorize log file'))
  parser.add_option('-g', '--get-templates', dest='get_templates', action='store_true', default=False, \
        help=_('Get default template files'))
  parser.add_option('-v', '--version', dest='version', action='store_true', default=False, \
        help=_('Show OPBASM version'))
  parser.add_option('-q', '--quiet', dest='quiet', action='store_true', default=False, \
        help=_('Quiet output'))
  parser.add_option('--m4', dest='use_m4', action='store_true', default=False, \
        help=_('Use m4 preprocessor on all source files'))
  parser.add_option('-D', '--define', dest='m4_defines', action='append', default=[], \
        metavar='NAME[=VALUE]', help=_('Define m4 macro NAME as having VALUE or empty'))
  parser.add_option('--debug-preproc', dest='debug_preproc', metavar='FILE', \
        help=_('Transformed source file after initial preprocessing'))
  
  options, args = parser.parse_args()

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
    options.m4_defines = {t[0]:t[1] if len(t) == 2 else None for t in (d.split('=') for d in options.m4_defines)}
  return options


def build_memmap(slist, mem_size, default_jump):
  '''Insert assembled instructions into a memory array'''
  # Ensure mem size is a power of 2
  addr_bits = (mem_size-1).bit_length()
  adj_mem_size = 2 ** addr_bits

  mmap = [default_jump] * adj_mem_size
  for s in slist:
    if s.is_instruction():
      mmap[s.address] = s.machine_word()
      
  if 0:
    # Compute CRC
    # Convert to 9-bit words
    mmap9 = []
    for w in mmap:
      mmap9.append( ((w >> 8) & 0x100) + (w & 0xFF) )
      mmap9.append( ((w >> 9) & 0x100) + ((w >> 8) & 0xFF) )
      
    for xor in (0xFFFF, 0x0000):
      for poly in (0x8005, 0x1021, 0x8007):
        for reflect in (True, False):
          crc = gen_crc(16, 9, poly, 0x0000, xor, reflect, reflect, mmap9)
          print('### CRC: {:04X} {:04X} {:5s} = {:04X}'.format(xor, poly, str(reflect), crc))
  return mmap


def find_dead_code(assembled_code, entry_points):
  '''Perform dead code analysis'''
  itable = build_instruction_table(assembled_code)
  analyze_code_reachability(assembled_code, itable, entry_points)
  analyze_recursive_keeps(assembled_code, itable)


def build_instruction_table(slist):
  '''Build index of all instruction statements by address'''
  itable = {}
  for s in slist:
    if s.is_instruction():
      itable[s.address] = s

  return itable


def analyze_code_reachability(slist, itable, entry_points):
  '''Scan assembled statements for reachability'''

  addresses = set(entry_points)
  addresses.add(0)

  find_reachability(addresses, itable)


def analyze_recursive_keeps(slist, itable):
  '''Scan assembled statements for reachability'''

  for s in slist:
    if s.is_instruction() and 'keep' in s.tags:
      find_reachability((s.address,), itable, follow_keeps=True)


def find_reachability(addresses, itable, follow_keeps=False):
  '''Recursive function that follows graph of executable statements to determine
     reachability'''
  for a in addresses:
    while a in itable:
      s = itable[a]
      if s.reachable: break # Skip statements already visited
      if follow_keeps and 'keep_auto' in s.tags: break

      if s.is_instruction():
        if not follow_keeps:
          s.reachable = True
        elif 'keep' not in s.tags:
          s.tags['keep_auto'] = (True,)
          
        # Stop on unconditional return, returni, load&return, and jump@ instructions
        if s.command in ('returni', 'load&return', 'jump@') or \
           (s.command == 'return' and s.arg1 is None):
          break

        # Follow branch address for jump and call
        if s.command in ('jump', 'call'):
          if not follow_keeps or (s.immediate in itable and 'keep' not in itable[s.immediate].tags):
            find_reachability((s.immediate,), itable, follow_keeps)

          # Stop on unconditional jump
          if s.command == 'jump' and s.arg2 is None and 'jump_table' not in s.tags: # Only 1 argument -> unconditional
            break

      # Continue with next instruction if it exists
      a += 1



def parse_pragma(comment):
  '''Extract fields from pragma meta-comments'''
  if comment is not None and comment.lower().lstrip().startswith('pragma '):
    args = comment.split()[1:]
    pragma = args[0]
    args = args[1:]
    return (pragma.lower(), args)
  else:
    return (None, None)


def annotate_pragmas(slist):
  '''Look for pragmas marking blocks of code and add annotations to the
     affected instructions'''
  active_tags = {}
  for s in slist:
    pragma, args = parse_pragma(s.comment)
    del_pragma = False
    if pragma is not None:
      op = args[-1]
      if op.lower() in ('on', 'start', 'begin'):
        active_tags[pragma] = args[:-1] if len(args) > 1 else (True,)
      elif op.lower() in ('off', 'stop', 'end'):
        if pragma in active_tags:
          del_pragma = True # Schedule this pragma for removal from active_tags
      else:
        print('\n' + warn(_('WARNING:')) + _(' Unrecognized pragma at {}').format(s.error_line))

    if s.is_instruction():
      for p, a in active_tags.iteritems():
        s.tags[p] = a

    if del_pragma:
      del active_tags[pragma]

class Block(object):
  '''Track info for extracted pragma blocks'''
  def __init__(self, name, args, start, end=-1):
    self.name = name
    self.args = args
    self.start = start
    self.end = end if end >= 0 else start
    self.has_inst = False

  def __str__(self):
    return '{} {} ({:03X} - {:03X})'.format(self.name, self.args, self.start, self.end)

def extract_pragma_blocks(slist):
  '''Build set of pragma blocks from assembled code'''
  all_blocks = []
  open_blocks = {}

  for s in slist:
    if s.tags:
      for p, a in s.tags.iteritems():
        if p in open_blocks:
          if s.tags[p] == open_blocks[p].args:
            open_blocks[p].end = s.address
        else: # New block
          open_blocks[p] = Block(p, a, s.address)

        if s.is_instruction():
          open_blocks[p].has_inst = True

    # Check if blocks have closed (no tag present on this instruction or different value)
    if s.is_instruction():
      for p in open_blocks.keys():
        if p not in s.tags: # Block has ended
          #print('### finish:', p, open_blocks[p].args)
          all_blocks.append(open_blocks[p])
          del open_blocks[p]
        elif s.tags[p] != open_blocks[p].args: # Block restarted with new args
          #print('### restart:', p, s.tags[p], open_blocks[p].args)
          all_blocks.append(open_blocks[p])
          del open_blocks[p]
          open_blocks[p] = Block(p, s.tags[p], s.address)
          if s.is_instruction():
            open_blocks[p].has_inst = True


  # Move any remaining unclosed blocks to the list
  all_blocks.extend(open_blocks.itervalues())

  # Remove empty blocks containing no instructions
  all_blocks = [b for b in all_blocks if b.has_inst]

  #print('### BLOCKS:')
  #for b in all_blocks:
  #  print(b)

  return all_blocks      



def write_hex_file(fname, mmap):
  '''Write a memory map as a hex format file'''
  with open(fname, 'w') as fh:
    for m in mmap:
      print('{:05X}'.format(m), file=fh)

def write_mem_file(fname, mmap):
  '''Write a memory map as a mem format file'''
  with open(fname, 'w') as fh:
    # write MEM file header (one start address)
    print('@00000000', file=fh)
    # write data lines 
    for m in mmap:
      print('{:05X}'.format(m), file=fh)

def write_mif_file(fname, mmap):
  '''Write a memory map in Altera's MIF format.'''
  with open(fname, 'w') as fh:
    # write MIF file header
    print('DEPTH = {};         -- memory words'.format(len(mmap)), file=fh)
    print('WIDTH = 18;           -- bits per word', file=fh)
    print('ADDRESS_RADIX = HEX;  -- Address radix (BIN, DEC, HEX, OCT or UNS)', file=fh)
    print('DATA_RADIX = HEX;     -- Data radix', file=fh)
    print('CONTENT', file=fh)
    print('BEGIN', file=fh)
    # write data lines -> 'Address : Content' in multiples of 8
    for a in xrange(len(mmap) // 8):
      d = ' '.join('{:05X}'.format(w) for w in mmap[a*8:(a+1)*8])
      print('{:04X} : {};'.format(a*8, d), file=fh)

    # write MIF footer
    print('END;', file=fh)

def get_timestamp():
  '''Get a current datestamp'''
  return datetime.datetime.now().replace(microsecond=0)

def code_stats(assembled_code):
  '''Analyze assembled code to determine addressing parameters'''
  inst_count = 0
  last_inst = None
  for s in assembled_code:
    if s.is_instruction():
      inst_count += 1
      last_inst = s

  if last_inst is not None:
    addr_bits = max(10, last_inst.address.bit_length())
    last_addr = last_inst.address
  else:
    addr_bits = 10
    last_addr = 0

  nom_size = 2 ** addr_bits

  stats = {
    'inst_count': inst_count,
    'addr_bits': addr_bits,
    'last_addr': last_addr,
    'nom_size': nom_size
  }

  return stats

def instruction_usage(assembled_code, asm):
  '''Analyse assembled code to determine instruction histogram'''
  stats = dict((k, 0) for k in asm.op_info['opcodes'].iterkeys())
  del stats['inst'] # Not a real opcode

  for s in assembled_code:
    if s.is_instruction() and s.command in stats:
      stats[s.command] += 1

  return stats


def underline(s, char='-'):
  '''Insert an underline aligned to a text string'''
  vis_len = len(s.strip())
  return '{}\n{}'.format(s, ' ' * (len(s) - vis_len) + char * vis_len)


def format_table(rows, col_names, indent=0):
  '''Format tabular data with variable width columns
  Returns a list of strings
  '''
  cols = zip(col_names, *rows)
  col_size = [max(len(unicode(i)) for i in col) for col in cols]

  fmt = ' ' * indent + '  '.join('{{:{}}}'.format(w) for w in col_size)
  tbl = [fmt.format(*col_names),
    fmt.format(*['-'*len(c) for c in col_names])
  ]
  for r in rows:
    tbl.append(fmt.format(*['' if i is None else unicode(i) for i in r]).rstrip())

  return tbl


def write_log_file(log_file, assembled_code, stats, asm, colorize, show_dead):
  '''Write a log file with details of assembled code'''
  with io.open(log_file, 'w', encoding='utf-8') as fh:
    def printf(*args):
      # For Python 2.x we have to ensure all args are unicode strings
      # before passing them to print().
      return print(*[unicode(a) for a in args], file=fh)

    printf(_('Open PicoBlaze Assembler log for program "{}"').format(asm.top_source_file))
    printf(_('Generated by opbasm v{}').format(__version__))
    printf(_('  Assembled on {}').format(asm.timestamp.isoformat()))
    printf(_('  PicoBlaze-{} mode\n').format(6 if asm.use_pb6 else 3))

    printf(_('  Last occupied address: {:03X} hex').format(stats['last_addr']))
    printf(_('  Nominal program memory size: {}K ({})  address({}:0)').format( \
                stats['nom_size'] // 1024, stats['nom_size'], stats['addr_bits']-1))
    printf(_('  Actual memory size:'), asm.mem_size)
    printf(_('  Occupied memory locations:'), stats['inst_count'])
    printf(_('  Memory locations available:'), asm.mem_size - stats['inst_count'])
    printf(_('  Scratchpad size:'), asm.scratch_size)

    if stats['dead_inst'] is not None:
      printf('\n\n' + underline(_('Optimizations')))
      printf(_('  Static analysis:\n    Dead instructions {}: {}').format( \
        _('removed') if stats['dead_removed'] else _('found'), stats['dead_inst']))
      printf(_('    Analyzed entry points:'), ', '.join(['0x{:03X}'.format(e) for e in \
        sorted(stats['entry_points'])]))


    printf('\n\n' + underline(_('Assembly listing')))
    for s in assembled_code:
      printf(s.format(show_addr=True, show_dead=show_dead, colorize=colorize))

    if asm.default_jump is None or asm.default_jump == 0:
      # Decoding of "all zeros" instruction varies between processors
      zero_instr = 'LOAD s0, s0' if asm.use_pb6 else 'LOAD s0, 00'
      printf(_('\nAll unused memory locations contain zero (equivalent to "{}")'.format(zero_instr)))
    else:
      printf(_('\nAll unused memory locations contain a DEFAULT_JUMP to {:03X}').format(asm.default_jump & 0xFFF))

    printf('\n\n' + underline(_('PSM files that have been assembled')))
    for f in asm.sources.iterkeys():
      printf('   ', os.path.abspath(f))

    printf('\n\n' + underline(_('List of defined constants')))
    headings = [_('   CONSTANT name'), _('Value'), _('Source PSM file')]
    rows = [(('   ' if asm.constants[c].in_use else '*  ') + c, \
      asm.constants[c].val_text, asm.constants[c].source_file) \
      for c in sorted(asm.constants.iterkeys())]
    for r in format_table(rows, headings, indent=1):
      printf(r)

    if not all(c.in_use for c in asm.constants.itervalues()): # Show caption
      printf(_('\n       * Unreferenced constant(s)'))
      

    if len(asm.tables) == 0:
      printf(_('\n\n  No tables defined'))
    else:
      printf('\n\n' + underline(_('List of defined tables')))
      headings = [_('   TABLE name'), _('Value'), _('Source PSM file')]
      rows = [(('   ' if asm.tables[t].in_use else '*  ') + t, \
        asm.tables[t].val_text, asm.tables[t].source_file) \
        for t in sorted(asm.tables.iterkeys())]
      for r in format_table(rows, headings, indent=1):
        printf(r)

    if not all(t.in_use for t in asm.tables.itervalues()): # Show caption
      printf(_('\n       * Unreferenced table(s)'))


    printf('\n\n' + underline(_('List of text strings')))
    headings = [_('   STRING name'), _('Value'), _('Source PSM file')]
    rows = [(('   ' if asm.strings[s].in_use else '*  ') + s, \
      asm.strings[s].val_text, asm.strings[s].source_file) \
      for s in sorted(asm.strings.iterkeys())]
    for r in format_table(rows, headings, indent=1):
      printf(r)

    if not all(s.in_use for s in asm.strings.itervalues()): # Show caption
      printf(_('\n       * Unreferenced string(s)'))


    printf('\n\n' + underline(_('List of line labels')))
    headings = [_('   Label'), _('Addr'), _('Source PSM file')]
    rows = [(('   ' if asm.labels[l].in_use else '*  ') + l, \
      '{:03X}'.format(asm.labels[l].value), asm.labels[l].source_file) \
      for l in sorted(asm.labels.iterkeys())]
    for r in format_table(rows, headings, indent=1):
      printf(r)

    if not all(l.in_use for l in asm.labels.itervalues()): # Show caption
      printf(_('\n       * Unreferenced label(s)'))


    printf('\n\n' + underline(_('List of pragma blocks')))
    all_blocks = extract_pragma_blocks(assembled_code)
    headings = [_('Name'), _('Addr range'), _('Value')]
    rows = [(b.name, '({:03X} - {:03X})'.format(b.start, b.end), \
      ' '.join([unicode(a) for a in b.args])) for b in all_blocks]
    for r in format_table(rows, headings, indent=3):
      printf(r)

    printf('\n\n' + underline(_('Instruction usage statistics')))
    inst_usage = instruction_usage(assembled_code, asm)
    headings = [_('Instruction'), _('Instances')]
    rows = [(i.upper(), inst_usage[i] if inst_usage[i] > 0 else '-') \
      for i in sorted(inst_usage.iterkeys())]
    for r in format_table(rows, headings, indent=3):
      printf(r)

def build_xilinx_mem_init(mmap, split_data=False):
  '''Create a dict of Xilinx BRAM INIT and INITP strings'''
  minit = {}

  if not split_data: # Map to 18-bit BRAMs
    # Lower 16-bits are put in INIT
    for a in xrange(len(mmap) // 16):
      mline = mmap[a*16:(a+1)*16]
      init = ''.join('{:04X}'.format(w & 0xFFFF) for w in reversed(mline))
      minit['INIT_{:02X}'.format(a)] = init

    # Upper 2-bits are put in INITP
    for a in xrange(len(mmap) // 128):
      mline = mmap[a*128:(a+1)*128]
      nibbles = []
      for i in xrange(0, len(mline), 2):
        nibbles.append((mline[i] >> 16) + ((mline[i+1] >> 14) & 0xC))

      init = ''.join('{:01X}'.format(n & 0xF) for n in reversed(nibbles))
      minit['INITP_{:02X}'.format(a)] = init

  else: # Map to 9-bit BRAMs
    # Split the mem-map into 9-bit halves
    mmap_l = []
    mmap_h = []
    for m in mmap:
      l = m & 0x1FF
      h = (m >> 9) & 0x1FF
      mmap_l.append(l)
      mmap_h.append(h)

    build_9_bit_mem_init(mmap_l, minit, '8:0')
    build_9_bit_mem_init(mmap_h, minit, '17:9')

  return minit


def build_9_bit_mem_init(mmap, minit, bit_range):
  '''Compute Xilinx BRAM INIT and INITP strings for 9-bit split memories'''
  # Lower 8-bits are put in INIT
  for a in xrange(len(mmap) // 32):
    mline = mmap[a*32:(a+1)*32]
    init = ''.join('{:02X}'.format(w & 0xFF) for w in reversed(mline))
    minit['[{}]_INIT_{:02X}'.format(bit_range, a)] = init

  # Upper 1-bits are put in INITP
  for a in xrange(len(mmap) // 256):
    mline = mmap[a*256:(a+1)*256]
    nibbles = []
    for i in xrange(0, len(mline), 4):
      nibbles.append((mline[i] >> 8) + ((mline[i+1] >> 7) & 0x02) + \
        ((mline[i+2] >> 6) & 0x04) + ((mline[i+3] >> 5) & 0x08))

    init = ''.join('{:01X}'.format(n & 0xF) for n in reversed(nibbles))
    minit['[{}]_INITP_{:02X}'.format(bit_range, a)] = init


try:
  from  opbasm_lib.hamming import secded_encode_num
  
  def build_xilinx_ecc_mem_init(mmap):
    '''Create a dict of Xilinx BRAM INIT and INITP strings for the 7-series ECC template'''
    minit = {}

    # ECC memory is 1.5K long. We must first fold three 500-byte blocks into 64-bit words
    folded = [(z << 40) + (y << 20) + x for x,y,z in zip(mmap[0:0x200], mmap[0x200:0x400], mmap[0x400:0x600])]

    # Merge every 4 folded words into an init word
    for a in xrange(len(folded) // 4):
      mline = folded[a*4:(a+1)*4]
      init = ''.join('{:016X}'.format(w) for w in reversed(mline))
      minit['ECC_7S_1K5_INIT_{:02X}'.format(a)] = init

    # Compute Hamming ECC (72,64) and store in INITPs
    hamming = [secded_encode_num(w, 64) for w in folded]

    for a in xrange(len(folded) // 32):
      mline = hamming[a*32:(a+1)*32]
      init = ''.join('{:02X}'.format(w) for w in reversed(mline))
      minit['ECC_7S_1K5_INITP_{:02X}'.format(a)] = init

    return minit

except ImportError:
  def build_xilinx_ecc_mem_init(mmap):
    return {}



def build_default_jump_inits(default_jump):
  '''Create a dummy memory map that contains enough default jump instructions to fill
     out an INIT and INITP string for both 18-bit and 9-bit memories'''
  dj_mmap = [default_jump] * 256
  
  dj_minit_18 = build_xilinx_mem_init(dj_mmap)
  dj_minit_9 = build_xilinx_mem_init(dj_mmap, split_data=True)
  
  dj_inits = { 'INIT':dj_minit_18['INIT_00'], 'INITP':dj_minit_18['INITP_00'],
                r'\[17:9\]_INIT':dj_minit_9['[17:9]_INIT_00'], r'\[17:9\]_INITP':dj_minit_9['[17:9]_INITP_00'],
                r'\[8:0\]_INIT':dj_minit_9['[8:0]_INIT_00'], r'\[8:0\]_INITP':dj_minit_9['[8:0]_INITP_00']
  }

  return dj_inits

  
def write_hdl_file(input_file, hdl_file, hdl_template, minit, timestamp, default_jump=0):
  '''Insert INIT strings and other fields into an HDL template.
     Returns False if the template contains additional INIT fields
     not covered by the minit map.'''
  # Read template
  with io.open(hdl_template, 'r', encoding='latin1') as fh:
    template = fh.readlines()

  # Remove template header
  found_header = -1
  for i, l in enumerate(template):
    if '{begin template}' in l:
      found_header = i
      break

  if found_header >= 0:
    hdl = ''.join(template[found_header+1:])
  else:
    hdl = ''.join(template)

  # Substitute template tags
  for k, v in minit.iteritems():
    hdl = hdl.replace('{{{}}}'.format(k), v)
    
  # When using the -m <size> option with the KCPSM6 ROM_form.vhd/v files there will
  # be additional {*INIT_nn} fields remaining for the larger memories that shouldn't
  # be in use. We will insert DEFAULT_JUMP instructions for these extra init strings.

  all_inits_replaced = True  
  init_re = re.compile(r'{(\[\d+:\d+\]_)?INIT_..}')
  m = init_re.search(hdl)
  if m: # Additional INIT fields were found
    all_inits_replaced = False
    # Substitute default jump instructions into the unused INIT fields
    dj_inits = build_default_jump_inits(default_jump)
    for k, v in dj_inits.iteritems():
      hdl = re.sub(r'{{{}_..}}'.format(k), v, hdl)

  hdl = hdl.replace('{source file}', input_file)  # Extension not used by KCPSM3.exe
  # We don't support {psmname} because it is followed by a hard-coded extension in the templates which may be invalid
  hdl = hdl.replace('{name}', os.path.splitext(hdl_file)[0])
  hdl = hdl.replace('{timestamp}', timestamp)
  # Used in ROM_form_7S_1K5_with_ecc
  hdl = hdl.replace('{default_jump}', '{:018b}'.format(default_jump))
  # {CRC_2K}       # Used in ROM_form_7S_2K_with_error_detection

  with io.open(hdl_file, 'w', encoding='latin1') as fh:
    fh.write(hdl)
    
  return all_inits_replaced

# Compute CRC of data
def gen_crc(size, data_size, poly, xor_in, xor_out, reflect_in, reflect_out, data):
  crc = xor_in
  for w in data:
    crc = next_crc(size, data_size, crc, poly, reflect_in, w)
  return end_crc(size, crc, reflect_out, xor_out)

def next_crc(size, data_size, crc, poly, reflect_in, data):
  sreg = crc
  bits = '{:0{}b}'.format(data, data_size)
  
  if reflect_in:
    #print('#### {} {}'.format(bits, bits[::-1]))
    bits = bits[::-1]

  for b in bits:
    leftbit = 1 if sreg & (2**(size-1)) > 0 else 0
    sreg = (sreg << 1) & (2**size-1)
    if int(b) != leftbit:
      sreg = sreg ^ poly

  return sreg

def end_crc(size, crc, reflect_out, xor_out):
  if reflect_out:
    b = '{:0{}b}'.format(crc, size)
    crc = int(b[::-1],2)
  
  return (crc ^ xor_out) & (2**size-1)


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
      print(_('Given template file \'{}\' does not exist.').format(template_file))
      sys.exit(1)

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

class TemplateDataFormat(object):
  ROM9    = 9
  ROM18   = 18
  ROMBoth = 19
  ROMECC  = 20  # Special ECC template using 7-series BRAM ECC hardware

def template_data_size(template_file):
  ''''Determine the data bus width used by an HDL template

  On Spartan-6 The PicoBlaze-6 2K and 4K memories must be split across
  2Kx9 BRAMs and on Virtex-6 and 7-series the PicoBlaze-6 4K memories
  must be split across 4Kx9 BRAMs.

  This scans a template file to see what format of init placeholder is
  used for address 00. This breaks for the KCPSM6 ROM_form.vhd/v files
  using the JTAG uploader because they contain both 18-bit and 9-bit
  memory declarations so we scan for both types separately and return
  -1 if both are found.
  '''
  init_9 = False
  init_18 = False

  signatures = {'init_9': '{[8:0]_INIT_00}', 'init_18': '{INIT_00}',
              'init_ecc': '{ECC_7S_1K5_INIT_00}'}

  found_sigs = set()

  # Search for a line containing each possible signature
  with io.open(template_file, 'r', encoding='latin1') as fh:
    for l in fh:
      for name, sig in signatures.iteritems():
        if sig in l:
          found_sigs.add(name)
          break

  data_format = None

  # Determine which format is in use
  if 'init_9' in found_sigs or 'init_18' in found_sigs:
    if 'init_9' in found_sigs and 'init_18' in found_sigs:
      data_format = TemplateDataFormat.ROMBoth
    elif 'init_9' in found_sigs:
      data_format = TemplateDataFormat.ROM9
    else:
      data_format = TemplateDataFormat.ROM18
  else:
    if 'init_ecc' in found_sigs:
      data_format = TemplateDataFormat.ROMECC

  if data_format is None:
    asm_error(_('Cannot determine template data format'), exit=1)

  return data_format


import shutil

def get_standard_templates():
  '''Create copies of standard templates from the installed package'''
  print(_('Retrieving default templates...'))

  lib_dir = find_lib_dir()
  tpl_dir = os.path.normpath(os.path.join(lib_dir, '../templates'))

  if not os.path.exists(tpl_dir):
    print(_('  No template directory found'))
    sys.exit(1)

  files = os.listdir(tpl_dir)
  print(','.join(files))
  for f in files:
    p = os.path.join(tpl_dir, f)
    if not os.path.isfile(p): continue # Skip anything that isn't a file
    if os.path.isfile(f): continue # Skip files that already exist in the current directory

    print(_('  COPYING: '), p)
    shutil.copyfile(p, f)


def add_output_dir(output_dir, fname):
  return os.path.normpath(os.path.join(output_dir, os.path.split(fname)[1]))


def main():
  '''Main application code'''
  options = parse_command_line()
  
  def printq(*args, **keys):
    if not options.quiet: print(*args, **keys)

  printq(note(_('OPBASM - Open PicoBlaze Assembler {}').format(__version__)))

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
    hex_mem_file = add_output_dir(options.output_dir, options.module_name + '.hex')
  elif options.mif_output:
    hex_mem_file = add_output_dir(options.output_dir, options.module_name + '.mif')
  else:
    hex_mem_file = add_output_dir(options.output_dir, options.module_name + '.mem')

  templates = find_templates(options.template_file)

  # Make sure the extension of the generated VHDL file matches the template extension
  if 'vhdl' in templates:
    vhdl_ext = os.path.splitext(templates['vhdl'])[1]
  else:
    vhdl_ext = '.vhdl'

  vhdl_file = add_output_dir(options.output_dir, options.module_name + vhdl_ext)
  verilog_file = add_output_dir(options.output_dir, options.module_name + '.v')
  log_file = add_output_dir(options.output_dir, options.module_name + '.log')

  
  if options.input_file == '-': # Ensure a module name was provided
    if options.module_name == '-':
      asm_error(_('Module name must be provided when reading from stdin (use -n <name>)'), exit=1)
  else: # Check for existence of input file
    if not os.path.exists(options.input_file):
      asm_error(_('Input file not found'), exit=1)
    
  mode = _('Running in PicoBlaze-{} mode').format(6 if options.use_pb6 else 3)
  m = re.match('(^.*)(PicoBlaze-[0-9])(.*$)', mode)
  printq(note(m.group(1)) + success(m.group(2)) + note(m.group(3)))

  printq(_('  Device configuration:\n    Memory size: {}, Scratchpad size: {}\n').format(\
    options.mem_size, options.scratch_size))

  timestamp = get_timestamp()
  upper_env_names = dict(((k.upper(), k) for k in os.environ.iterkeys()))
  asm = Assembler(options.input_file, timestamp, options, upper_env_names)

  # Create output directory(ies) if it doesn't exist
  try:
    os.makedirs(options.output_dir)
  except OSError as e:
    if e.errno != errno.EEXIST:
      raise  # Some other OS error


  try:
    # Read input source
    for fname in asm.process_includes():
      printq(_('  Reading source:'), fname)

    # Assemble program
    printq(_('\n  Assembling code... '))
    sys.stdout.flush()

    assembled_code = asm.assemble(bounds_check=not options.remove_dead_code)

  except FatalError, e:
    asm_error(*e.args, exit=1, statement=e.statement)

  dead_instructions = None
  entry_points = None
  if options.remove_dead_code or options.report_dead_code:
    printq(_('\n  Beginning optimizations...\n'))
    # Run static analysis
    printq(_('  Static analysis: searching for dead code... '), end='')
    entry_points = set((asm.default_jump & 0xFFF, 0))
    entry_points |= set(options.entry_point)
    find_dead_code(assembled_code, entry_points)
    printq(success(_('COMPLETE')))

    # Summarize analysis
    printq(_('    Entry points:'), ', '.join(['0x{:03X}'.format(e) for e in \
      sorted(entry_points)]))
    dead_instructions = len([s for s in assembled_code if s.removable()])
    printq(_('    {} dead instructions found').format(dead_instructions))


  if options.remove_dead_code:
    asm.remove_dead_code(assembled_code)

    # Reinitialize registers to default names
    asm.init_registers()

    printq(_('  Removing dead code... '), end='')
    # Reassemble code with dead code removed
    try:
      assembled_code = asm.raw_assemble(assembled_code)
    except FatalError, e:
      asm_error(*e.args, exit=1, statement=e.statement)

    printq(success(_('COMPLETE')))

    
  # Verify that no instructions have been assigned to the same address
  instructions = [s for s in assembled_code if s.is_instruction()]
  prev_addresses = {}
  valid_asm = True
  for i in instructions:
    if i.address in prev_addresses:
      asm_error(_('Two instructions are assigned to address {:03X}\n   First: {}\n  Second: {}\n').format(i.address, \
        prev_addresses[i.address].error_line, i.error_line))
      valid_asm = False
      break
    else:
      prev_addresses[i.address] = i

  if valid_asm: printq('  ' + success(_('SUCCESS') + '\n'))
      
  # Print summary
  stats = code_stats(assembled_code)
  stats['dead_inst'] = dead_instructions
  stats['dead_removed'] = options.remove_dead_code
  stats['entry_points'] = entry_points
  if not options.quiet:
    print(_('    {} instructions out of {} ({}%)').format(stats['inst_count'], \
          options.mem_size, int(stats['inst_count'] / options.mem_size * 100)))
    print(_('    Highest occupied address: {:03X} hex').format(stats['last_addr']))

    if len(templates) > 0:
      print(_('\n  Found {}:').format(_('templates') if len(templates) > 1 else _('template')))
      for f in templates.itervalues():
        print('   ', f)


  # Write results
  printq(_(_('\n  Writing output')))

  field_size = 19

  if options.debug_preproc:
    printq('{:>{}}'.format(_('preprocessor:'), field_size), options.debug_preproc)

  mmap = build_memmap(assembled_code, options.mem_size, asm.default_jump)
  if options.hex_output:
    write_hex_file(hex_mem_file, mmap)
  elif options.mif_output:
    write_mif_file(hex_mem_file, mmap)
  else:
    write_mem_file(hex_mem_file, mmap)
  
  printq('{:>{}}'.format(_('mem map:'), field_size), hex_mem_file)
  
  show_dead = True if options.report_dead_code or options.remove_dead_code else False
  write_log_file(log_file, assembled_code, stats, asm, options.color_log, show_dead)
  printq('{:>{}}'.format(_('log file:'), field_size), log_file)

  minit_18 = build_xilinx_mem_init(mmap)
  minit_9 = build_xilinx_mem_init(mmap, split_data=True)

  # Find longest template file name so we can align the warning messages
  # about unmapped INIT fields.
  longest_template_name = 0
  if 'vhdl' in templates:
    longest_template_name = len(vhdl_file)
  if 'verilog' in templates:
    longest_template_name = max(len(verilog_file), longest_template_name)

  for hdl_name, template_file in templates.iteritems():
    # Prepare INIT strings for the memory width found in the template
    data_format = template_data_size(templates[hdl_name])
    if data_format == TemplateDataFormat.ROMBoth: # KCPSM6 JTAG loader ROM_form contains both 18 and 9-bit memories
      minit = minit_9.copy()
      minit.update(minit_18) # Merge the init strings together for both types
    elif data_format == TemplateDataFormat.ROM9:
      minit = minit_9
    elif data_format == TemplateDataFormat.ROM18:
      minit = minit_18
    elif data_format == TemplateDataFormat.ROMECC:
      minit = build_xilinx_ecc_mem_init(mmap)
    else:
      asm_error(_('Unknown template data format'), exit=1)

    target_file = vhdl_file if hdl_name == 'vhdl' else verilog_file
    file_type = _('VHDL file:') if hdl_name == 'vhdl' else _('Verilog file:')
    all_inits_replaced = write_hdl_file(options.input_file, target_file, template_file, minit, timestamp.isoformat(), asm.default_jump)

    unmapped_warn = warn(_('WARNING:')) + _(' Unmapped INIT fields found in template') if not all_inits_replaced else ''
    printq('{:>{}} {:<{}}   {}'.format(file_type, field_size, target_file, longest_template_name, unmapped_warn))
    

  printq(_('\n  Formatted source:'))
  for fname, source in asm.sources.iteritems():
    if fname == '-': fname = 'stdin'
    fname = os.path.splitext(os.path.basename(fname))[0] + '.fmt'
    fname = add_output_dir(options.output_dir, fname)
    printq('   ', fname)
    with io.open(fname, 'w', encoding='utf-8') as fh:
      for s in source:
        print(s.format(), file=fh)

  printq('')

  sys.exit(0 if valid_asm else 1)


if __name__ == '__main__':
  main()

