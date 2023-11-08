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

'''Open PicoBlaze Assembler main library'''



import sys
import os
import errno
import subprocess
import datetime
import copy
import string
import io
import re
import gettext
import hashlib
import uuid

import  opbasm.optimize as optimize

from opbasm.common import *
from opbasm.color import *
from opbasm.devices import *
from opbasm.hamming import secded_encode_num


def find_lib_dir():
  '''Get the Opbasm library path'''
  # Look relative to installed library
  try:
    lib_dir = os.path.dirname(sys.modules['opbasm'].__file__)
  except KeyError:
    # Look relative to this module
    lib_dir = os.path.dirname(os.path.realpath(__file__))

  return lib_dir

# Configure multilingual strings
gettext.install('opbasm', os.path.join(find_lib_dir(), 'lang'))


__version__ = '1.3.10'



regex_parser = re.compile(r'''
  (?:
    (?P<label>[.&\w]+):\s*
  )?
  (?:
    (?P<cmd>[\w&@]+)\s*
    (?:
        (?:(?P<arg1>[.&\w~'#$]+)\s*|(?P<arg1s>".+")\s*)
        (?:,\s*(?P<arg2>[.&\w~'%#$]+)|,\s*\(\s*(?P<arg2b>\w+)\s*\)
        |,\s*\[(?P<arg2t>[^\]]+\]('[db])?)|,\s*(?P<arg2s>".+"))?
        |\(\s*(?P<addr1>[.&\w~']+)\s*,\s*(?P<addr2>[.&\w~']+)\s*\)
    )?\s*
  )?
  (?P<cmnt>;.*)?$
''', re.VERBOSE)

regex_register = re.compile(r'^s[0-9A-F]$', re.IGNORECASE)


def parse_statement(l):
  '''Parse a line of code'''

  # Regex parser that performs significantly faster than the original pyparsing
  # based recursive descent parser

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

   

  #return {'statement': ptree}
  return ptree


def parse_lines(lines, source_file, index=None):
  '''Parse a list of text lines into Statement objects'''

  statements = []
  for i, l in enumerate(lines):
    try:
      ptree = parse_statement(l)
    except ParseError:
      if index is not None:
        ix_line = index[i]
        error_line = _('{} line {} (expanded line {})').format(ix_line[2], ix_line[1], i+1)
      else:
        error_line = _('{} line {}').format(source_file, i+1)

      raise ParseError(_(' Bad statement in {}:\n  {}').format(error_line, l))

    ix_line = index[i] if index is not None else None
    statements.append(Statement(ptree, i+1, source_file, ix_line))
    #print('### ptree:', i+1, ptree)

  return statements


class Statement(object):
  '''Low level representation of a statement (instructions, directives, comments)'''
  def __init__(self, ptree, line, source_file, ix_line):
    '''
    ptree : dict parse tree object for a single statement
    line  : source line number
    '''

    self.source_file = source_file
    self.line = line
    self.ix_line = ix_line
    self.label = ptree['label'][0] if 'label' in ptree else None
    self.xlabel = self.label
    self.comment = ptree['comment'][0] if 'comment' in ptree else None

    # Parsed statement fields
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
    self.reachable = False  # FIXME: Make more general

    self.refline = None
    self.tags = {}
    self._is_instruction = None

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

  def __str__(self):
    inst = self.format_inst()

    if self.label is not None and len(self.label) > 0:
      code = '{}: {}'.format(self.label, inst)
    else:
      code = inst

    return code

  def format_inst(self):
    '''Convert instruction and arguments to a string'''
    if self.command is not None:
      inst = self.command

      if self.indirect_addr:
        inst += ' ({}, {})'.format(self.arg1, self.arg2)
      else:
        if self.arg1 is not None:
          inst += ' ' + self.arg1
        if self.arg2 is not None:
          if self.indirect_reg:
            inst += ', (' + self.arg2 + ')'
          elif self.table_def or not isinstance(self.arg2, str):
            inst += ', [' + ', '.join(self.arg2[:-1]) + self.arg2[-1]
          else:
            inst += ', ' + str(self.arg2)
      return inst
    else:
      return ''

  re_ansi_strip = re.compile(r'\x1b[^m]*m') # Match ANSI escape codes

  def format(self, upper=True, show_addr=False, show_dead=False, show_reflines=False, colorize=False):
    '''Generate a formatted string for the statement
    upper : Upper case instructions and directives
    show_addr : Include assembled memory address and machine word
    '''
    label = self.label + ':' if self.label is not None else ''
    comment = ';' + self.comment if self.comment is not None else ''
    inst = self.format_inst()

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

    refline = self.refline if show_reflines else ''

    if colorize:
      if len(comment) > 0:
        comment = success(comment) # green text
      if len(label) > 0:
        label = error(label) # red text

    # The ANSI color codes interfere with field width format specifiers
    # Detect them and add an adjustment
    label_width = 20 + len(label) - len(self.re_ansi_strip.sub('', label))

    if len(inst) > 0:
      return '{} {}{:>{}} {:30} {}'.format(addr, refline, label, label_width, inst, comment).rstrip()
    else:
      return '{} {}{:>{}} {}'.format(addr, refline, label, label_width, comment).rstrip()



  @staticmethod
  def from_line(line):
    return parse_lines([line], '-', )[0]

  def machine_word(self):
    '''Returns the numeric value of the assembled instruction'''
    return self.opcode + (self.regx << 8) + (self.regy << 4) + self.immediate

  _directives = set(('address', 'constant', 'namereg', 'include', 'default_jump', \
        'string', 'table'))

  def is_instruction(self):
    '''Identify if this statement is an instruction (vs. directive)'''

    if self._is_instruction is None:
      self._is_instruction = False if self.command is None or self.command in self._directives else True

    return self._is_instruction


  def is_removable(self): # FIXME: Move to optimizer?
    '''Identify if this statement is eligible for dead code removal'''
    return self.is_instruction() and self.reachable == False \
      and not any(t in self.tags for t in ('keep', 'keep_auto'))


  def comment_out(self, prefix=_('REMOVED')):
    if self.command is not None:
      self.comment = '{}: {}'.format(prefix, self.format().lstrip())
      self.command = None
      self._is_instruction = False

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



def get_m4_path():
  '''Get the path to the m4 executable'''
  m4_cmd = ''
  if sys.platform == 'win32': # Use included m4 binary in opbasm lib on Windows (except within Cygwin)
    m4_cmd = os.path.join(find_lib_dir(), 'm4', 'm4.exe')
  
  if not os.path.exists(m4_cmd): # Use system path to find m4
    m4_cmd = 'm4'

  return m4_cmd


def find_standard_m4_macros():
  '''Get the path to the Opbasm m4 macros'''
  macro_file = os.path.join(find_lib_dir(), 'picoblaze.m4')
  if not os.path.exists(macro_file):
    raise FatalError(_('  No m4 macro directory found') + ' ' + macro_file)

  return macro_file


class AssemblerConfig(object):
  '''Configuration settings for the Assembler class'''
  def __init__(self, config=None):
    # Set defaults
    self.mem_size = 1024
    self.scratch_size = 64
    self.target_arch = DevicePb6()
    self.use_m4 = False
    self.m4_defines = {}
    self.debug_preproc = None
    self.optimize_level = 0
    self.output_dir = '.'
    self.entry_point = [0x3FF]
    self.verbose = False
    self.quiet = True

    if config is not None:
      self.apply_config(config)
  
  def apply_config(self, config):
    '''Copy settings from a dict or other AssemblerConfig object'''
    self.mem_size = config.mem_size
    self.scratch_size = config.scratch_size
    self.target_arch = config.target_arch
    self.use_m4 = config.use_m4
    self.m4_defines = config.m4_defines
    self.debug_preproc = config.debug_preproc
    self.optimize_level = config.optimize_level
    self.output_dir = config.output_dir
    self.entry_point = config.entry_point
    self.verbose = config.verbose
    self.quiet = config.quiet


class Assembler(object):
  '''Main object for running assembler and tracking symbol information'''

  def __init__(self, config=None, timestamp=None):
    # Force creation of config object
    self.config = AssemblerConfig(config)

    self.reset(None, timestamp)


  def reset(self, config=None, timestamp=None):
    '''Initialize state to default values'''

    if config is not None: # Build new configuration options
      self.config = AssemblerConfig(config)

    self.top_source_file = None
    self.m4_file_num = 0
    self.timestamp = timestamp if timestamp is not None else get_timestamp()

    self.constants = self._init_constants()
    self.labels = {}
    self.removed_labels = set() # Labels eliminated by an optimizer
    self.cur_context = ''

    self.registers = self._init_registers()
    self.strings = self._init_strings()
    self.tables = {}
    self.sources = {}
    self.default_jump = None
    
    self.upper_env_names = dict(((k.upper(), k) for k in os.environ.keys()))

    self.line_index = {}

    # Results
    self.assembled_code = None
    self.valid_asm = False
    self._mmap = None
    self._stats = None
    self._minit_18 = None
    self._minit_9 = None


    self.optimizers = {}

    # Configure optimizers implied by optimization level
    for opt_class in optimize._all_optimizers:
      opt = opt_class()
      if opt.priority < self.config.optimize_level * 100:
        self.add_optimizer(opt)


  @property
  def mmap(self):
    '''Memoize memory map'''
    if self._mmap is None:
      self.build_memmap()
    return self._mmap

  @property
  def minit_18(self):
    '''Memoize 18-bit template init'''
    if self._minit_18 is None:
      self._minit_18 = build_xilinx_mem_init(self.mmap)
    return self._minit_18

  @property
  def minit_9(self):
    '''Memoize 9-bit template init'''
    if self._minit_9 is None:
      self._minit_9 = build_xilinx_mem_init(self.mmap, split_data=True)
    return self._minit_9


  @property
  def stats(self):
    '''Memoize code stats'''
    if self._stats is None:
      self._stats = self.code_stats()
    return self._stats

  @property
  def optimizer_sequence(self):
    '''Return list of active optimizers sorted by priority'''
    return sorted(iter(self.optimizers.values()), key=lambda o: o.priority)

  def add_optimizer(self, opt):
    '''Register a new optimizer'''
    opt.register(self)


  def _init_registers(self):
    '''Initialize table of register names'''
    hex_digits = [hex(d)[-1] for d in range(16)]
    return dict(('s' + h, i) for i, h in enumerate(hex_digits))

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
    
    for c in constants.values():
      c.in_use = True

    return constants


  def _init_strings(self):
    '''Initialize predefined strings'''
    ts = self.timestamp.strftime('%H:%M:%S')
    ds = self.timestamp.strftime('%d %b %Y')
    ver = __version__
    strings = {
      'timestamp$' : Symbol('timestamp$', ts, '"{}"'.format(ts)),
      'datestamp$' : Symbol('datestamp$', ds, '"{}"'.format(ds)),
      'Opbasm_version$' : Symbol('Opbasm_version$', ver, '"{}"'.format(ver)),
    }

    for s in strings.values():
      s.in_use = True

    return strings


  def _preprocess_with_m4(self, source_file, source_code):
    '''Preproces source for m4 macros and generate expanded file
    with ".gen.psm" extension
    '''
    self.create_output_dir() # Make sure directory exists for generated source

    pp_source_file = os.path.splitext(source_file)[0] + uuid.uuid4().hex + '.gen.psm'
    pp_source_file = build_path(self.config.output_dir, pp_source_file)

    # Look for common picoblaze.m4 macro definitions
    macro_defs = find_standard_m4_macros()

    proc_mode = self.config.target_arch.short_name.upper() # Definition for active processor type
    
    # Convert comments with backticks and single quotes to avoid m4 errors
    requoted = []
    for l in source_code:
      stmt_parts = l.split(';', 1)
      if len(stmt_parts) > 1:
        # Replace ASCII backtick and single quote with Unicode equivalents
        rq = stmt_parts[1].replace('`', '\u2018').replace("'", '\u2019')
        requoted.append(';'.join([stmt_parts[0], rq]))
      else:
        requoted.append(l)

    # Preprocess C-style syntax
    pure_m4 = self._preprocess_c_style(source_file, requoted)
    if self.config.debug_preproc:
      with io.open(self.config.debug_preproc, 'w', encoding='utf-8') as fh:
        print(pure_m4, file=fh)

    self.m4_file_num += 1 # Increment counter for unique macro labels

    m4_options = ['-s'] # Activate synclines so we can track original line numbers

    source_dir = os.path.dirname(source_file)
    if len(source_dir) > 0:
      # Add source file directory to m4 search path for include() macros
      m4_options.append('-I ' + os.path.dirname(source_file))

    m4_options = ' '.join(m4_options)
    
    m4_defines = self.config.m4_defines.copy()
    # Set default defines for m4, overriding any attempt to redefine them by the user
    m4_defines.update({proc_mode:None,
                  'M4_FILE_NUM':self.m4_file_num,
                  'DATE_STAMP':self.strings['datestamp$'].val_text,
                  'TIME_STAMP':self.strings['timestamp$'].val_text
                  })

    # Build argument string for defines fed into m4
    m4_def_args = \
      ' '.join('-D{}={}'.format(k,v) if v else '-D{}'.format(k) for k,v in m4_defines.items())

    m4_cmd = get_m4_path()
    cmd = '"{}" {} {} "{}" -'.format(m4_cmd, m4_options, m4_def_args, macro_defs)
    if self.config.verbose:
      print('  Running m4 on file "{}":\n\t{}'.format(source_file, cmd))

    p = subprocess.Popen(cmd, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    m4_result, m4_err = p.communicate(input=pure_m4.encode('utf-8'))
    if p.returncode:
      raise FatalError(_('m4 failure on file {}').format(source_file))

    # On Windows the pipe output is raw bytes containing '\r\n' for line terminators.
    # We need to manually convert these back to \n to prevent the \r from being preserved
    # and ending up with files containing \r\r\n which then results in unwanted empty lines
    # When Python reads back the file with expanded macros.
    if sys.platform == 'win32':
      m4_result = m4_result.decode('utf-8').replace('\r\n', '\n')
    else:
      m4_result = m4_result.decode('utf-8')

    # Use synclines to build index tracking how original source lines were expanded
    self._index_expanded_line_numbers(m4_result, pp_source_file, source_file)

    return pp_source_file


  def _preprocess_c_style(self, source_file, source_code=None):
    '''Perform an initial transformation to convert C-style syntax into valid m4 macros'''

    if source_code is None:
      lines = []
      if source_file == '-':
        lines = [l.decode('utf-8') for l in sys.stdin.readlines()]
      else:
        with io.open(source_file, 'r', encoding='utf-8') as fh:
          lines = fh.readlines()
    else: # Contents are in a string
      lines = source_code

    # Add a trailing empty line so that m4 doesn't complain about files without them
    # or ending with a comment
    lines.append('\n')

    elines = []
    # We need to first protect '}' chars inside strings to prevent them from being
    # turned into m4 syntax.
    curly_string = re.compile(r'^.*"(.*}.*)".*$')

    # Convert constant directives into const() macros
    const_def = re.compile(r'^([^;]*)constant\s+([^,]+)\s*,\s*("."|[^;]+)(;.*)?', re.IGNORECASE)

    # Escape quoted strings to ensure no words in them are expanded by m4
    string_def = re.compile(r'string(.*)("[^"]*")', re.IGNORECASE)

    for l in lines:
      m = curly_string.match(l)
      if m:
        escaped = m.group(1).replace('}', "`'_rcurly`'")
        l = re.sub(r'^(.*").*(".*)$',r'\1{}\2'.format(escaped),l)

      m = const_def.search(l)
      if m:
        if m.group(3) not in ('";"', '","', '"`"', '"\'"'):
          # Wrap quoted chars in m4 quotes to protect ",".
          # This doesn't work for ";" so we leave it in pb syntax.
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
    ecode = '\n'.join(elines)
    # else-if clause
    ecode = re.sub(r'}(?:\s*;.*\n)*\s*else(?:\s*;.*\n)*\s*if\s*\((.*?)\)(?:\s*;.*\n)*\s*{', r"', \1,`", ecode)
    ecode = re.sub(r'if\s*\((.*?)\)(?:\s*;.*\n)*\s*{', r'if(\1, `', ecode) # if statement
    ecode = re.sub(r'}(?:\s*;.*\n)*\s*else(?:\s*;.*\n)*\s*{', r"', `", ecode) # else clause
    ecode = re.sub(r'do(?:\s*;.*\n)*\s*{', r'_dowhile2(`', ecode) # start of do-while

    # proc def
    ecode = re.sub(r'proc\s+(\w+)\s*\((.*?)\)(?:\s*;.*\n)*\s*{', r"proc(\1,`\2',`", ecode)
    # func def
    ecode = re.sub(r'func\s+(\w+)\s*\((.*?)\)\s*:?\s*(\d*)(?:\s*;.*\n)*\s*{', r"func(\1,`\2',\3,`", ecode)
    # isr def
    ecode = re.sub(r'isr\s+(\w+)\s*\((.*?)\)\s*:?\s*(\w*)(?:\s*;.*\n)*\s*{', r"isr(\1,`\2',`\3',`", ecode)

    # while following a block
    ecode = re.sub(r'}(?:\s*;.*\n)*\s*while\s*\((.*?)\)(?:\s*;.*\n)*\s*{', r"')\nwhile(\1, `", ecode)
    ecode = re.sub(r'}(?:\s*;.*\n)*\s*while\s*\((.*?)\)', r"',\1)", ecode) # end of do-while
    ecode = re.sub(r'while\s*\((.*?)\)(?:\s*;.*\n)*\s*{', r'while(\1, `', ecode) # plain while
    ecode = re.sub(r'for\s*\((.*?)\)(?:\s*;.*\n)*\s*{', r'for(\1, `', ecode) # for loop
    ecode = re.sub(r'}', r"')", ecode) # end of a block
    return ecode

  def _index_expanded_line_numbers(self, m4_result, pp_source_file, source_file):
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
          if active_file is None: # This shouldn't happen
            raise FatalError(' Missing file name from expanded m4 source "{}"'.format(l))

          source_lines[active_file] = int(m.group(1))
      else: # Ordinary source line
        if active_file is None: # This shouldn't happen
          raise FatalError(' Missing file name from expanded m4 source "{}"'.format(l))

        elines.append(l)
        index.append((cur_line, source_lines[active_file], active_file))
        cur_line += 1
        source_lines[active_file] = source_lines[active_file] + 1
        
    # Write preprocessed file with synclines removed
    with io.open(pp_source_file, 'w', encoding='utf-8') as fh:
      fh.writelines(elines)
      
    self.line_index[source_file] = index


  def _process_includes(self, source_file=None, source_code=None):
    '''Scan a list of statements for INCLUDE directives and recursively
    read each included source file.

    Constant, string, and table definitions are also processed to keep
    track of where they are defined. This is a generator function that
    yields the name of each included file'''

    from_stdin = source_file == '-'

    if source_code is None: # Read file contents
      if source_file in self.sources: return # Already processed

      try:
        if from_stdin:
          source_code = [s.decode('utf-8').rstrip() for s in sys.stdin.readlines()]
        else:
          with io.open(source_file, 'r', encoding='utf-8') as fh:
            source_code = [s.rstrip() for s in fh.readlines()]
      except UnicodeDecodeError:
        # Fall back to Latin-1 if UTF-8 fails
        if from_stdin:
          source_code = [s.decode('latin-1').rstrip() for s in sys.stdin.readlines()]
        else:
          with io.open(source_file, 'r', encoding='latin-1') as fh:
            source_code = [s.rstrip() for s in fh.readlines()]
    else: # Take code from string
      source_code = source_code.split('\n')

    if (source_file is None) or from_stdin: # Make temp name
      prefix = 'STDIN' if from_stdin else 'CODE'
      # Build hash to identify this code
      hash_data = hashlib.md5()
      for s in source_code:
        hash_data.update(s)

      source_file = '{}_{}.psm'.format(prefix, hash_data.hexdigest()[-8:])

      if from_stdin:
        self.top_source_file = source_file

    used_m4 = False
    if (self.config.use_m4) or (os.path.splitext(source_file)[1] in ('.psm4', '.m4')):
      pp_source_file = self._preprocess_with_m4(source_file, source_code)
      used_m4 = True
      # Get expanded source code
      try:
        with io.open(pp_source_file, 'r', encoding='utf-8') as fh:
          source_code = [s.rstrip() for s in fh.readlines()]
      except UnicodeDecodeError:
        # Fall back to Latin-1 if UTF-8 fails
        with io.open(pp_source_file, 'r', encoding='latin-1') as fh:
          source_code = [s.rstrip() for s in fh.readlines()]

    yield (source_file, used_m4)

    index = self.line_index[source_file] if source_file in self.line_index else None
    slist = parse_lines(source_code, source_file, index)
    self.sources[source_file] = slist

    # Scan for include directives
    for s in slist:
      # Track label sources
      if s.label is not None:
        if s.label.startswith('.'): # Local label
          xlabel = self._expand_label(s.label)
          s.xlabel = xlabel
        elif s.label.startswith('~~'): # Macro label
          # "~~" prefix is ignored for context changes so that macro generated
          # labels won't interfere with user expectations for local label references.
          xlabel = s.label
        else: # Global label created by user
          self.cur_context = s.label # Change to new block context
          xlabel = s.label


        if xlabel in self.labels:
          raise StatementError(s, _('Redefinition of label:'), xlabel)

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
            raise StatementError(s, _('Include file not found:'), include_file)

          for inc_file in self._process_includes(include_file):
            yield inc_file
        else:
          raise StatementError(s, _('Invalid include parameter'), s.arg1)

      # Handle constant definitions before flattening
      elif s.command == 'constant':
        if s.arg1 is None or s.arg2 is None:
          raise StatementError(s, _('Missing argument to constant directive'))

        if s.arg1 in self.constants:
          raise StatementError(s, _('Redefinition of constant:'), s.arg1)

        # Prevent the use of 3 or less character constants that are valid hex literals
        if len(s.arg1) <= 3 and hex_to_int(s.arg1) is not None:
          raise StatementError(s, _('Invalid constant. Conflicts with hex literal'), s.arg1)

        if s.arg2[0] == '%': # Look up env variable
          ename = s.arg2[1:]
          if ename.upper() not in self.upper_env_names:
            raise StatementError(s, _('Unknown environment variable:'), ename)
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
          raise StatementError(s, _('Missing argument to STRING directive'))
        if s.arg1[-1] != '$':
          raise StatementError(s, _('Invalid string name (missing $):'), s.arg1)
        if s.arg1 in self.strings:
          raise StatementError(s, _('Redefinition of string:'), s.arg1)
        if s.arg2[0] != '"' or s.arg2[-1] != '"':
          raise StatementError(s, _('Not a valid string:'), s.arg2)

        self.strings[s.arg1] = Symbol(s.arg1, s.arg2[1:-1], s.arg2, \
            source_file=source_file, source_line=s.line)

      elif s.command == 'table':
        if s.arg1 is None or s.arg2 is None:
          raise StatementError(s, _('Missing argument to TABLE directive'))
        if s.arg1[-1] != '#':
          raise StatementError(s, _('Invalid table name (missing #):'), s.arg1)
        if s.arg1 in self.strings:
          raise StatementError(s, _('Redefinition of table:'), s.arg1)
        if s.table_def is False:
          raise StatementError(s, _('Missing table definition'))

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
          raise StatementError(s, _('Invalid table element (radix {})').format(radix))

        # Ensure table values are within range
        for e in tbl:
          if not (0 <= e < 256):
            raise StatementError(s, _('Table value out of range:'), e)

        val_text = '[' + ', '.join(s.arg2[:-1]) + s.arg2[-1]
        self.tables[s.arg1] = Symbol(s.arg1, tbl, val_text, \
          source_file=source_file, source_line=s.line)


  def _flatten_includes(self, slist, include_stack):
    '''Generator function that produces a flattened list of statements
    after evaluating INCLUDE directives.

    slist         : List of statements from source file
    include_stack : Stack of parent source files used to detect recursive includes
    '''
    #if include_stack is None: include_stack = [self.top_source_file]

    for s in slist:
      if s.command == 'include':
        include_file = s.arg1[1:-1].replace('\\', '/')

        # If the included file has a relative path build it off of the current path
        if not os.path.isabs(include_file):
          include_file = os.path.join(os.path.dirname(include_stack[-1]), include_file)

        if include_file in include_stack:
          raise StatementError(s, _('Recursive include:'), s.arg1)
        else:
          include_stack.append(include_file)
          islist = list(self._flatten_includes(self.sources[include_file], include_stack))
          include_stack.pop()
          for i in islist:
            yield copy.copy(i)
      else:
        yield s

  def _expand_label(self, addr_label):
    '''Expand local labels to their full name'''
    if addr_label.startswith('.'):
      return self.cur_context + addr_label
    else:
      return addr_label


  def label_address(self, addr_label, track_usage=True):
    '''Lookup the address assigned to addr_label'''

    xlabel = self._expand_label(addr_label)

    if xlabel in self.labels and track_usage:
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
      addr = self.label_address(label)
      if addr is None:
        return None

      if portion == 'lower':
        value = addr & 0xFF
      else: # Upper 4-bits
        value = (addr >> 8) & 0xF

    elif arg in self.constants: # Normal constant
      self.constants[arg].in_use = True
      value = self.constants[arg].value
    else: # Attempt to convert a constant literal
      value = convert_literal(arg)

    if invert_value and value is not None:
      value = (~value) & 0xFF

    return value

  def register_index(self, arg):
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


  def _raw_assemble(self, slist, start_address=0, bounds_check=True):
    '''Generate assembled instructions from a raw statement list'''
    cur_addr = start_address
    self.default_jump = None

    target_arch = self.config.target_arch
    macro_label_prefix = '&&'

    # Pass 2: Set instruction and label addresses
    for s in slist:
      if s.label is not None:
        if s.label.startswith('.'): # Local label
          xlabel = s.xlabel
        elif s.label.startswith(macro_label_prefix): # Macro label
          xlabel = s.label
        else: # Global label
          self.cur_context = s.label
          xlabel = s.label

        #print('### SET LABEL:', xlabel)
        if xlabel not in self.labels:
          raise StatementError(s, _('Label not found "{}". Possibly removed.').format(xlabel))

        self.labels[xlabel].value = cur_addr
        s.address = cur_addr

      if s.is_instruction():
        if bounds_check and cur_addr >= self.config.mem_size:
          raise StatementError(s, _('Address exceeds memory bounds: {:03X} (limit {:03X})').format( \
             cur_addr, self.config.mem_size-1))

        s.address = cur_addr
        # Move to next address. Could be > 1 if a string or table operand
        #cur_addr += self.statement_words(s)
        cur_addr += target_arch.instruction_words(self, s)

      elif s.command == 'address':
        cur_addr = self.label_address(s.arg1)
        if cur_addr is None:
          raise StatementError(s, _('Invalid address:'), s.arg1)

        if bounds_check and cur_addr >= self.config.mem_size:
          raise StatementError(s, _('Address exceeds memory bounds: {:03X} (limit {:03X})').format(\
            cur_addr, self.config.mem_size-1))

    # Assign phony addresses to non-instruction lines with comment or label
    for s in reversed(slist):
      if s.is_instruction():
        cur_addr = s.address
      elif s.comment or s.command:
        s.address = cur_addr


    # Pass 3: Validate and assemble instructions
    instructions = []
    self.cur_context = ''
    for s in slist:
      addr_label = None

      if s.label is not None and not s.label.startswith('.') \
        and not s.label.startswith(macro_label_prefix):
        self.cur_context = s.label

      if s.is_instruction():
        # Verify instruction is valid
        if s.command not in target_arch.opcodes:
          raise StatementError(s, _('Invalid {} instruction:').format( \
            target_arch.name), s.command)

        s.opcode = target_arch.opcodes[s.command] # Set base opcode

        if s.command in target_arch.flag_opcodes:
          # Check if first argument is a flag
          addr_label = s.arg1
          if s.arg1 is not None:
            if s.command == 'return':
              flag_codes = target_arch.return_flag_codes
            else:
              flag_codes = target_arch.flag_codes

            if s.arg1.lower() in flag_codes:
              s.opcode += flag_codes[s.arg1.lower()]
              addr_label = s.arg2

          if s.command in target_arch.addr_opcodes: # Include address for call and jump
            if addr_label is None:
              raise StatementError(s, _('Missing address'))

            s.immediate = self.label_address(addr_label)
            if s.immediate is None:
              raise StatementError(s, _('Invalid address:'), addr_label)
            if bounds_check and s.immediate >= self.config.mem_size:
              raise StatementError(s, _('Out of range address'))

        elif s.command in target_arch.one_reg_opcodes:
          if s.arg1 is None:
            raise StatementError(s, _('Missing operand'))
          if s.arg2 is not None:
            raise StatementError(s, _('Illegal operand:'), s.arg2)

          s.regx = self.register_index(s.arg1)
          if s.regx is None:
            raise StatementError(s, _('Invalid register:'), s.arg1)

        elif s.command in target_arch.two_reg_opcodes:
          if s.arg1 is None or s.arg2 is None:
            raise StatementError(s, _('Missing operand'))

          s.regx = self.register_index(s.arg1)
          if s.regx is None:
            raise StatementError(s, _('Invalid register:'), s.arg1)

          s.regy = self.register_index(s.arg2)
          if s.regy is not None: # Using y register opcode
            s.opcode += target_arch.two_reg_op_offset # Adjust opcode

          else: # The second arg was not a register
            s.regy = 0
            s.immediate = self.get_constant(s.arg2)

            if s.immediate is None:
              if s.arg2.endswith("'upper") or s.arg2.endswith("'lower"):
                xlabel = self._expand_label(s.arg2.split("'")[0])
                if xlabel in self.removed_labels:
                  raise StatementError(s, _('Label has been removed: {}\n       Add ";PRAGMA keep [on,off]" to preserve this label').format(s.arg2))
                else:
                  raise StatementError(s, _('Unknown label:'), s.arg2)
              else:
                raise StatementError(s, _('Invalid operand:'), s.arg2)
            if not (0 <= s.immediate < 256):
              raise StatementError(s, _('Immediate value out of range:'), s.immediate)

            if s.command in ('fetch', 'store'):
              if s.immediate >= self.config.scratch_size:
                raise StatementError(s, _('Scratchpad address out of range:'), hex(s.immediate))

        elif s.command in ('enable', 'disable'):
          if s.arg1 is None:
            raise StatementError(s, _('Missing operand'))
          if s.arg1.lower() != 'interrupt' or s.arg2 is not None:
            raise StatementError(s, _('Invalid operand to {}').format(s.command.upper()))

        elif s.command == 'returni':
          if s.arg1 is None:
            raise StatementError(s, _('Missing operand'))
          if s.arg1.lower() not in ('enable', 'disable'):
            raise StatementError(s, _('Invalid operand to RETURNI'))

          if s.arg1.lower() == 'enable':
            s.opcode += 1

        # Irregular PicoBlaze-6 instructions
        elif s.command in ('call@', 'jump@'):
          if s.arg1 is None or s.arg2 is None:
            raise StatementError(s, _('Missing operand'))
          s.regx = self.register_index(s.arg1)
          if s.regx is None:
            raise StatementError(s, _('Invalid register:'), s.arg1)

          s.regy = self.register_index(s.arg2)
          if s.regy is None:
            raise StatementError(s, _('Invalid register:'), s.arg2)

        elif s.command == 'load&return':
          if s.arg1 is None or s.arg2 is None:
            raise StatementError(s, _('Missing operand'))

          s.regx = self.register_index(s.arg1)
          if s.regx is None:
            raise StatementError(s, _('Invalid register:'), s.arg1)

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
              raise StatementError(s, _('Invalid operand:'), s.arg2)


        elif s.command == 'outputk':
          if s.arg1 is None or s.arg2 is None:
            raise StatementError(s, _('Missing operand'))

          port = self.get_constant(s.arg2)
          if port is None:
            raise StatementError(s, _('Invalid operand:'), s.arg2)
          if not 0 <= port < 16:
            port_name = '{:02X}'.format(port) if port == convert_literal(s.arg2) else '{} ({:02X})'.format(s.arg2, port)
            raise StatementError(s, _('Invalid port for OUTPUTK <value>, <port>:  {}\n       Port must range from 0 to F').format(port_name))

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
              raise StatementError(s, _('Invalid operand:'), s.arg1)
            if not (0 <= const < 256):
              raise StatementError(s, _('Immediate value out of range:'), const)

          s.immediate = (const << 4) + port

        elif s.command == 'regbank':
          if s.arg1 is None:
            raise StatementError(s, _('Missing operand'))
          if s.arg1.lower() not in ('a', 'b'):
            raise StatementError(s, _('Invalid operand to REGBANK'))

          if s.arg1.lower() == 'b':
            s.opcode += 1

        elif s.command == 'star':
          if s.arg1 is None or s.arg2 is None:
            raise StatementError(s, _('Missing operand'))

          # The STAR instruction has special rules for RegX (arg1)
          # It MUST be in the form: s[hexdigit]
          # We skip the usual table lookup for arg1
          if len(s.arg1) == 2 and s.arg1[0].lower() == 's' and s.arg1[1] in string.hexdigits:
            s.regx = int(s.arg1[1], 16)
          else:
            raise StatementError(s, _('Invalid register:'), s.arg1)

          # PB6 has the STAR sX, kk variant so we will accept a constant
          # as the second argument as well as a register.
          # http://forums.xilinx.com/t5/PicoBlaze/obscure-undocumented-property-of-REGBANK-instruction/m-p/489774#M2375

          s.regy = self.register_index(s.arg2)
          if s.regy is not None: # Using y register opcode
            s.opcode += target_arch.two_reg_op_offset # Adjust opcode

          else: # The second arg was not a register
            s.regy = 0
            s.immediate = self.get_constant(s.arg2)

            if s.immediate is None:
              raise StatementError(s, _('Invalid operand:'), s.arg2)
            if not (0 <= s.immediate < 256):
              raise StatementError(s, _('Immediate value out of range:'), s.immediate)


        elif s.command == 'inst':
          # NOTE: INST is really a directive but we need to reserve a space in the
          # address map for its value so we treat it as an instruction with 0x00 opcode.

          if s.arg1 is None:
            raise StatementError(s, _('Missing operand'))
          s.immediate = convert_literal(s.arg1)
          if s.immediate is None or not 0 <= s.immediate < 2**18:
            raise StatementError(s, _('Invalid INST value:'), s.arg1)

        else:
          raise StatementError(s, _('Unknown instruction:'), s.command)


      else: # Not an instruction
        if s.command == 'namereg':
          if s.arg1 is None or s.arg2 is None:
            raise StatementError(s, _('Missing argument to NAMEREG directive'))
          if s.arg1 not in self.registers:
            raise StatementError(s, _('Unknown register name:'), s.arg1)

          self.registers[s.arg2] = self.register_index(s.arg1)
          del self.registers[s.arg1]


        elif s.command == 'default_jump':
          if self.default_jump is not None:
            raise StatementError(s, _('Redefinition of default jump'))
          if s.arg2 is not None:
            raise StatementError(s, _('Too many arguments to DEFAULT_JUMP'))

          self.default_jump = self.label_address(s.arg1)
          if self.default_jump is None:
            raise StatementError(s, _('Invalid address:'), s.arg1)

      instructions.append(s)

    # Create default jump instruction
    if self.default_jump is None: # Fill with 0's by default
      self.default_jump = 0
    else: # Fill with jump instructions
      self.default_jump = target_arch.opcodes['jump'] + self.default_jump

    return instructions


  def _print(self, *args, **keys):
    '''Print message to console'''

    if not self.config.quiet:
      flush = False
      if 'flush' in keys:
        flush = keys['flush']
        del keys['flush']

      print(*args, **keys)
      if flush:
        sys.stdout.flush()

  def create_output_dir(self, output_dir=None):
    '''Create output directory(ies) if it doesn't exist'''

    if output_dir is None:
      output_dir = self.config.output_dir

    try:
      os.makedirs(output_dir)
    except OSError as e:
      if e.errno != errno.EEXIST:
        raise  # Some other OS error

  def assemble_file(self, top_source_file):
    '''Assemble source from a file'''
    self.top_source_file = top_source_file

    # Read input sources
    for fname, used_m4 in self._process_includes(self.top_source_file):
      self._print(_('  Reading source:'), fname, '(m4)' if used_m4 else '')

    # Assemble program
    self._print(_('\n  Assembling code... '), flush=True)

    self._assemble(self.top_source_file)

  def assemble_statements(self, statements):
    '''Assemble source from a list of statement objects'''
    source_code = '\n'.join(str(s) for s in statements)
    self.assemble_text(source_code)

  def assemble_text(self, source_code):
    '''Assemble source from a string'''
    fnames = list(self._process_includes(source_code=source_code))

    for fname, used_m4 in fnames:
      self._print(_('  Reading source:'), fname, '(m4)' if used_m4 else '')

    self._assemble(fnames[0][0])


  def _assemble(self, top_source_file):
    '''Common assembler routine'''

    # Pass 1: Flatten includes
    slist = list(self._flatten_includes(self.sources[top_source_file], [top_source_file]))

    # Scan for pragma meta-comments
    _annotate_pragmas(slist)

    # If an optimizer might remove code we skip the initial bounds check
    may_remove_code = any(o.removes_code for o in self.optimizer_sequence)
    assembled_code = self._raw_assemble(slist, 0, bounds_check=not may_remove_code)

    # Run optimizers
    if len(self.optimizers) > 0:
      self._print(_('\n  Beginning optimizations...\n'))
      for opt in self.optimizer_sequence:
        assembled_code = opt.apply(self, assembled_code)


    # Verify that no instructions have been assigned to the same address
    instructions = [s for s in assembled_code if s.is_instruction()]
    prev_addresses = {}

    for i in instructions:
      if i.address in prev_addresses:
        raise FatalError(_('Two instructions are assigned to address {:03X}\n   First: {}\n  Second: {}\n').format(i.address, \
          prev_addresses[i.address].error_line, i.error_line))
        break
      else:
        prev_addresses[i.address] = i

    self._print('  ' + success(_('SUCCESS') + '\n'))

    self.assembled_code = assembled_code
    self.valid_asm = True

  def code_stats(self):
    '''Analyze assembled code to determine addressing parameters'''
    inst_count = 0
    last_inst = None
    for s in self.assembled_code:
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


  def instruction_usage(self):
    '''Analyze assembled code to determine instruction histogram'''
    usage = dict((k, 0) for k in self.config.target_arch.opcodes.keys())
    del usage['inst'] # Not a real opcode

    for s in self.assembled_code:
      if s.is_instruction() and s.command in usage:
        usage[s.command] += 1

    return usage


  def build_memmap(self):
    '''Insert assembled instructions into a memory array'''
    # Ensure mem size is a power of 2
    addr_bits = (self.config.mem_size-1).bit_length()
    adj_mem_size = 2 ** addr_bits

    mmap = [self.default_jump] * adj_mem_size
    for s in self.assembled_code:
      if s.is_instruction():
        mmap[s.address] = s.machine_word()

    if 0: # FIXME: Implement CRC support
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

    self._mmap = mmap
    return mmap

  def write_hex_file(self, fname):
    '''Write a memory map as a hex format file'''
    self.create_output_dir()
    with open(fname, 'w') as fh:
      for m in self.mmap:
        print('{:05X}'.format(m), file=fh)

  def write_mem_file(self, fname):
    '''Write a memory map as a mem format file'''
    self.create_output_dir()
    with open(fname, 'w') as fh:
      # Write MEM file header (one start address)
      print('@00000000', file=fh)
      # Write data lines
      for m in self.mmap:
        print('{:05X}'.format(m), file=fh)

  def write_mif_file(self, fname):
    '''Write a memory map in Altera's MIF format.'''
    self.create_output_dir()
    with open(fname, 'w') as fh:
      # Write MIF file header
      print('DEPTH = {};         -- memory words'.format(len(self.mmap)), file=fh)
      print('WIDTH = 18;           -- bits per word', file=fh)
      print('ADDRESS_RADIX = HEX;  -- Address radix (BIN, DEC, HEX, OCT or UNS)', file=fh)
      print('DATA_RADIX = HEX;     -- Data radix', file=fh)
      print('CONTENT', file=fh)
      print('BEGIN', file=fh)
      # Write data lines -> 'Address : Content' in multiples of 8
      for a in range(len(self.mmap) // 8):
        d = ' '.join('{:05X}'.format(w) for w in self.mmap[a*8:(a+1)*8])
        print('{:04X} : {};'.format(a*8, d), file=fh)

      # Write MIF footer
      print('END;', file=fh)

  def top_source_name(self):
    if self.top_source_file is None:
      return '<unknown>'
    elif self.top_source_file == '-':
      return '<stdin>'
    else:
      return self.top_source_file

  def write_log_file(self, log_file, colorize=False, refline_cols=8, use_unicode=True):
    '''Write a log file with details of assembled code'''
    self.create_output_dir()
    with io.open(log_file, 'w', encoding='utf-8') as fh:

      def printf(*args):
        # For Python 2.x we have to ensure all args are unicode strings
        # before passing them to print().
        return print(*[str(a) for a in args], file=fh)

      printf(_('Open PicoBlaze Assembler log for program "{}"').format(self.top_source_name()))
      printf(_('Generated by opbasm v{}').format(__version__))
      printf(_('  Assembled on {}').format(self.timestamp.isoformat()))
      printf(_('  Target architecture: {}\n').format(self.config.target_arch.name))

      printf(_('  Last occupied address: {:03X} hex').format(self.stats['last_addr']))
      printf(_('  Nominal program memory size: {}K ({})  address({}:0)').format( \
                  self.stats['nom_size'] // 1024, self.stats['nom_size'], self.stats['addr_bits']-1))
      printf(_('  Actual memory size:'), self.config.mem_size)
      printf(_('  Occupied memory locations:'), self.stats['inst_count'])
      printf(_('  Memory locations available:'), self.config.mem_size - self.stats['inst_count'])
      printf(_('  Scratchpad size:'), self.config.scratch_size)

      if len(self.optimizers) > 0:
        printf('\n\n' + underline(_('Optimizations')))

      for opt in self.optimizer_sequence:
          opt.summary(printf)

      printf('\n\n' + underline(_('Assembly listing')))
      use_static_analysis = 'static' in self.optimizers

      show_reflines = False
      if refline_cols > 0:
        # Generate call/jump graph
        # Restrict columns to range 2-50
        max_cols = max(min(refline_cols, 50), 2)
        min_cols = max_cols if max_cols >= 8 else 8
        show_reflines = True
        self.generate_refline_graph(min_cols, max_cols, use_unicode)

      for s in self.assembled_code:
        printf(s.format(show_addr=True, show_dead=use_static_analysis, \
          show_reflines=show_reflines, colorize=colorize))

      if self.default_jump is None or self.default_jump == 0:
        # Decoding of "all zeros" instruction varies between processors
        printf(_('\nAll unused memory locations contain zero (equivalent to "{}")'.format(\
        self.config.target_arch.zero_instr)))
      else:
        printf(_('\nAll unused memory locations contain a DEFAULT_JUMP to {:03X}').format(self.default_jump & 0xFFF))

      printf('\n\n' + underline(_('PSM files that have been assembled')))
      for f in self.sources.keys():
        printf('   ', os.path.abspath(f))

      printf('\n\n' + underline(_('List of defined constants')))
      headings = [_('   CONSTANT name'), _('Value'), _('Source PSM file')]
      rows = [(('   ' if self.constants[c].in_use else '*  ') + c, \
        self.constants[c].val_text, self.constants[c].source_file) \
        for c in sorted(self.constants.keys())]
      for r in format_table(rows, headings, indent=1):
        printf(r)

      if not all(c.in_use for c in self.constants.values()): # Show caption
        printf(_('\n       * Unreferenced constant(s)'))


      if len(self.tables) == 0:
        printf(_('\n\n  No tables defined'))
      else:
        printf('\n\n' + underline(_('List of defined tables')))
        headings = [_('   TABLE name'), _('Value'), _('Source PSM file')]
        rows = [(('   ' if self.tables[t].in_use else '*  ') + t, \
          self.tables[t].val_text, self.tables[t].source_file) \
          for t in sorted(self.tables.keys())]
        for r in format_table(rows, headings, indent=1):
          printf(r)

      if not all(t.in_use for t in self.tables.values()): # Show caption
        printf(_('\n       * Unreferenced table(s)'))


      printf('\n\n' + underline(_('List of text strings')))
      headings = [_('   STRING name'), _('Value'), _('Source PSM file')]
      rows = [(('   ' if self.strings[s].in_use else '*  ') + s, \
        self.strings[s].val_text, self.strings[s].source_file) \
        for s in sorted(self.strings.keys())]
      for r in format_table(rows, headings, indent=1):
        printf(r)

      if not all(s.in_use for s in self.strings.values()): # Show caption
        printf(_('\n       * Unreferenced string(s)'))


      printf('\n\n' + underline(_('List of line labels')))
      headings = [_('   Label'), _('Addr'), _('Source PSM file')]
      rows = [(('   ' if self.labels[l].in_use else '*  ') + l, \
        '{:03X}'.format(self.labels[l].value), self.labels[l].source_file) \
        for l in sorted(self.labels.keys())]
      for r in format_table(rows, headings, indent=1):
        printf(r)

      if not all(l.in_use for l in self.labels.values()): # Show caption
        printf(_('\n       * Unreferenced label(s)'))


      printf('\n\n' + underline(_('List of pragma blocks')))
      all_blocks = extract_pragma_blocks(self.assembled_code)
      headings = [_('Name'), _('Addr range'), _('Value')]
      rows = [(b.name, '({:03X} - {:03X})'.format(b.start, b.end), \
        ' '.join([str(a) for a in b.args])) for b in all_blocks]
      for r in format_table(rows, headings, indent=3):
        printf(r)

      printf('\n\n' + underline(_('Instruction usage statistics')))
      inst_usage = self.instruction_usage()
      headings = [_('Instruction'), _('Instances')]
      rows = [(i.upper(), inst_usage[i] if inst_usage[i] > 0 else '-') \
        for i in sorted(inst_usage.keys())]
      for r in format_table(rows, headings, indent=3):
        printf(r)


  def write_template_file(self, template_file, target_file, longest_name=0):
    # Cache init strings when writing multiple templates

    # Prepare INIT strings for the memory width found in the template
    data_format = template_data_size(template_file)
    if data_format == TemplateDataFormat.ROMBoth:
      # KCPSM6 JTAG loader ROM_form contains both 18 and 9-bit memories
      minit = self.minit_9.copy()
      minit.update(self.minit_18) # Merge the init strings together for both types
    elif data_format == TemplateDataFormat.ROM9:
      minit = self.minit_9
    elif data_format == TemplateDataFormat.ROM18:
      minit = self.minit_18
    elif data_format == TemplateDataFormat.ROMECC:
      minit = build_xilinx_ecc_mem_init(mmap)
    else:
      raise FatalError(_('Unknown template data format'))

    target_ext = os.path.splitext(target_file)[1].lower()
    file_type = _('VHDL file:') if target_ext in ('.vhdl', '.vhd') else _('Verilog file:')
    self.create_output_dir()
    all_inits_replaced = write_hdl_file(self.top_source_file, target_file, template_file, \
                                      minit, self.timestamp.isoformat(), self.default_jump)

    field_size = 19
    unmapped_warn = warn(_('WARNING:')) + _(' Unmapped INIT fields found in template') if not all_inits_replaced else ''
    self._print('{:>{}} {:<{}}   {}'.format(file_type, field_size, target_file, longest_name, unmapped_warn))


  def write_formatted_source(self, output_dir):
    '''Write source for all included files'''
    self._print(_('\n  Formatted source:'))
    self.create_output_dir(output_dir)
    for fname, source in self.sources.items():
      if fname == '-': fname = 'stdin'
      fname = os.path.splitext(os.path.basename(fname))[0] + '.fmt'
      fname = build_path(output_dir, fname)
      self._print('   ', fname)
      with io.open(fname, 'w', encoding='utf-8') as fh:
        for s in source:
          print(s.format(), file=fh)

    self._print('')


  def generate_refline_graph(self, min_cols=8, max_cols=8, use_unicode=True):
    '''Draw a radare2 style refline graph for code listings'''

    class ReflineArc(object):
      def __init__(self, statement, index):
        self.statement = statement
        self.start = index
        self._target = -1
        self.min = 0
        self.max = 0
        self.column = -1

      @property
      def target(self):
        return self._target

      @target.setter
      def target(self, t):
        self._target = t
        # Update min and max attributes
        self.min = min(self.start, self._target)
        self.max = max(self.start, self._target)

      def __repr__(self):
        return '{} -> {} ({}) col: {}  {}'.format(self.start, self.target, \
          abs(self.start - self.target), self.column, self.statement.format())

      def overlaps(self, j):
        '''Check if another arc overlaps with this one'''
        return not((self.max < j.min) or (j.max < self.min))

    # Find all jump/call instructions
    jumps = []
    for ix, s in enumerate(self.assembled_code):
      if s.command in ('jump', 'call'):
        jumps.append(ReflineArc(s, ix))

    # Build index to map target addresses into output lines
    target_index = {}
    for ix, s in enumerate(self.assembled_code):
      target_index[s.address] = ix

    # Assign target indices for arcs
    for j in jumps:
      try:
        j.target = target_index[j.statement.immediate]
      except KeyError: # Address isn't in index
        j.column = -100 # Mark this as a bad arc

    # Remove bad arcs
    jumps = [j for j in jumps if j.column != -100]


    # Sort by span length and lowest address
    # We want to prioritize the shortest spans by putting them in the rightmost column
    # possible. This doesn't necessarily achieve the optimum packing but it is more visually
    # pleasing and minimizes line crossings.
    jumps = sorted(jumps, key=lambda j: (abs(j.start - j.target), j.min))

    # Build map of occupied columns
    columns = [[]]

    # Assign each jump to a column
    for j in jumps:
      j_max = j.max
      # Search for empty space for this jump
      assigned = False
      for i, col in enumerate(columns):
        clear = True
        # Check for any overlaps with existing arcs in this column
        for arc in col:
          if j_max < arc.min: # No possibility of more overlaps
            break
          if j.overlaps(arc):
            clear = False
            break

        if clear: # Add this jump to the column
          j.column = i
          col.insert(0, j)
          col.sort(key=lambda r: r.min)
          assigned = True
          break
        # No space. Check next column...
      if not assigned: # No space found. Make a new column
        # If we have exceeded max_cols then there is no need to continue searching.
        # All remaining arcs will be packed into the leftmost column anyway
        if len(columns) >= max_cols-1:
          # Stick remaining unassigned arcs into last column
          columns[-1].extend([q for q in jumps if q.column == -1])
          for q in columns[-1]: # Set their column numbers
            q.column = max_cols - 2
          break
        else: # New column
          columns.append([j])
          j.column = len(columns) - 1


    # Drawing characters to use
    u_chars = {
      'top_corner': '\u256d', # ╭
      'bot_corner': '\u2570', # ╰
      'hline': '\u2500',      # ─
      'vline': '\u2502',      # │
      'sarrow': '\u2919',     # ⤙
      'earrow': '\u25b6',     # ▶
      'top_tee': '\u252c',    # ┬
      'bot_tee': '\u2534'     # ┴
    }

    a_chars = {
      'top_corner': '.',
      'bot_corner': '`',
      'hline': '-',
      'vline': '|',
      'sarrow': '<',
      'earrow': '>',
      'top_tee': '+',
      'bot_tee': '+'
    }

    chars = u_chars if use_unicode else a_chars

    # Build char matrix for ASCII arcs
    total_cols = min(max(min_cols, len(columns)), max_cols)

    graph = [[' ']*total_cols for x in range(len(self.assembled_code))]
    for j in jumps:
      j_min = j.min
      j_max = j.max
      # Draw corners
      graph[j_min][total_cols - 2 - j.column] = chars['top_corner']
      graph[j_max][total_cols - 2 - j.column] = chars['bot_corner']

      # Draw horizontal lines
      for c in range(total_cols - 1 - j.column, total_cols):
        min_cell = graph[j_min][c]
        if min_cell == chars['top_corner']:
          graph[j_min][c] = chars['top_tee'] # Add tee to merge with existing arc
        elif min_cell == chars['bot_corner']:
          graph[j_min][c] = chars['bot_tee'] # Add tee to merge with existing arc
        elif min_cell in (' ', chars['vline']):
          graph[j_min][c] = chars['hline']

        max_cell = graph[j_max][c]
        if max_cell == chars['bot_corner']:
          graph[j_max][c] = chars['bot_tee'] # Add tee to merge with existing arc
        elif max_cell == chars['top_corner']:
          graph[j_max][c] = chars['top_tee'] # Add tee to merge with existing arc
        elif max_cell in (' ', chars['vline']):
          graph[j_max][c] = chars['hline']

      # Draw direction arrows on right side
      graph[j.start][total_cols-1] = chars['sarrow']
      graph[j.target][total_cols-1] = chars['earrow']

      # Draw vertical connections within a column
      for v in range(j_min+1, j_max):
        if graph[v][total_cols - 2 - j.column] == ' ':
          graph[v][total_cols - 2 - j.column] = chars['vline']

    # Collapse rows into single strings
    graph = [''.join(l) for l in graph]

    # Add refline strings into statement objects
    for s, r in zip(self.assembled_code, graph):
      s.refline = r


  def _basic_blocks(self):
    '''Extract basic blocks from assembled code'''
    blocks = []
    start = 0
    in_block = False
    label_pending = None
    
    def end_block(i):
      end = i;
      if end >= start:
        blocks.append((start, end))
      #print('## END BLOCK', start, end)
    
    for i, s in enumerate(self.assembled_code):
      #print('## STMT:', i, str(s))    
      if not s.is_instruction():
        if s.command is not None and in_block: # Terminate previous block
          end_block(i-1)
          in_block = False
        if s.label is not None and label_pending is None:
          label_pending = i
      else: # Got an instruction
        if not in_block:
          #print('## NEW BLOCK:', i)
          start = i

        in_block = True

        if (s.label is not None or label_pending is not None) and i > start:
          # Labels can only be at the start of a block
          # This guarantees a single entry point
          if label_pending is None:
            end_block(i-1)
            start = i
          else:
            end_block(label_pending-1)
            start = label_pending
        elif s.command in ('jump', 'jump@', 'load&return', 'return', 'returni'):
          # Jumps can't only be at the end of a block
          # Guarantees a single exit point
          end_block(i)
          start = i+1
          
        label_pending = None

    if in_block:
      blocks.append((start, i))

    return blocks


##############################################



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


def _parse_pragma(comment):
  '''Extract fields from pragma meta-comments'''
  if comment is not None and comment.lower().lstrip().startswith('pragma '):
    args = comment.split()[1:]
    pragma = args[0]
    args = args[1:]
    return (pragma.lower(), args)
  else:
    return (None, None)


def _annotate_pragmas(slist):
  '''Look for pragmas marking blocks of code and add annotations to the
     affected instructions'''
  active_tags = {}
  for s in slist:
    pragma, args = _parse_pragma(s.comment)
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
      for p, a in active_tags.items():
        s.tags[p] = a

    if del_pragma:
      del active_tags[pragma]

class PragmaBlock(object):
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
      for p, a in s.tags.items():
        if p in open_blocks:
          if s.tags[p] == open_blocks[p].args:
            open_blocks[p].end = s.address
        else: # New block
          open_blocks[p] = PragmaBlock(p, a, s.address)

        if s.is_instruction():
          open_blocks[p].has_inst = True

    # Check if blocks have closed (no tag present on this instruction or different value)
    if s.is_instruction():
      for p in list(open_blocks.keys()):
        if p not in s.tags: # Block has ended
          #print('### finish:', p, open_blocks[p].args)
          all_blocks.append(open_blocks[p])
          del open_blocks[p]
        elif s.tags[p] != open_blocks[p].args: # Block restarted with new args
          #print('### restart:', p, s.tags[p], open_blocks[p].args)
          all_blocks.append(open_blocks[p])
          del open_blocks[p]
          open_blocks[p] = PragmaBlock(p, s.tags[p], s.address)
          if s.is_instruction():
            open_blocks[p].has_inst = True


  # Move any remaining unclosed blocks to the list
  all_blocks.extend(iter(open_blocks.values()))

  # Remove empty blocks containing no instructions
  all_blocks = [b for b in all_blocks if b.has_inst]

  #print('### BLOCKS:')
  #for b in all_blocks:
  #  print(b)

  return all_blocks      


def get_timestamp():
  '''Get a current datestamp'''
  return datetime.datetime.now().replace(microsecond=0)


def underline(s, char='-'):
  '''Insert an underline aligned to a text string'''
  vis_len = len(s.strip())
  return '{}\n{}'.format(s, ' ' * (len(s) - vis_len) + char * vis_len)


def format_table(rows, col_names, indent=0):
  '''Format tabular data with variable width columns
  Returns a list of strings
  '''
  cols = list(zip(col_names, *rows))
  col_size = [max(len(str(i)) for i in col) for col in cols]

  fmt = ' ' * indent + '  '.join('{{:{}}}'.format(w) for w in col_size)
  tbl = [fmt.format(*col_names),
    fmt.format(*['-'*len(c) for c in col_names])
  ]
  for r in rows:
    tbl.append(fmt.format(*['' if i is None else str(i) for i in r]).rstrip())

  return tbl


def build_xilinx_mem_init(mmap, split_data=False):
  '''Create a dict of Xilinx BRAM INIT and INITP strings'''
  minit = {}

  if not split_data: # Map to 18-bit BRAMs
    # Lower 16-bits are put in INIT
    for a in range(len(mmap) // 16):
      mline = mmap[a*16:(a+1)*16]
      init = ''.join('{:04X}'.format(w & 0xFFFF) for w in reversed(mline))
      minit['INIT_{:02X}'.format(a)] = init

    # Upper 2-bits are put in INITP
    for a in range(len(mmap) // 128):
      mline = mmap[a*128:(a+1)*128]
      nibbles = []
      for i in range(0, len(mline), 2):
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
  for a in range(len(mmap) // 32):
    mline = mmap[a*32:(a+1)*32]
    init = ''.join('{:02X}'.format(w & 0xFF) for w in reversed(mline))
    minit['[{}]_INIT_{:02X}'.format(bit_range, a)] = init

  # Upper 1-bits are put in INITP
  for a in range(len(mmap) // 256):
    mline = mmap[a*256:(a+1)*256]
    nibbles = []
    for i in range(0, len(mline), 4):
      nibbles.append((mline[i] >> 8) + ((mline[i+1] >> 7) & 0x02) + \
        ((mline[i+2] >> 6) & 0x04) + ((mline[i+3] >> 5) & 0x08))

    init = ''.join('{:01X}'.format(n & 0xF) for n in reversed(nibbles))
    minit['[{}]_INITP_{:02X}'.format(bit_range, a)] = init



  def build_xilinx_ecc_mem_init(mmap):
    '''Create a dict of Xilinx BRAM INIT and INITP strings for the 7-series ECC template'''
    minit = {}

    # ECC memory is 1.5K long. We must first fold three 500-byte blocks into 64-bit words
    folded = [(z << 40) + (y << 20) + x for x,y,z in zip(mmap[0:0x200], mmap[0x200:0x400], mmap[0x400:0x600])]

    # Merge every 4 folded words into an init word
    for a in range(len(folded) // 4):
      mline = folded[a*4:(a+1)*4]
      init = ''.join('{:016X}'.format(w) for w in reversed(mline))
      minit['ECC_7S_1K5_INIT_{:02X}'.format(a)] = init

    # Compute Hamming ECC (72,64) and store in INITPs
    hamming = [secded_encode_num(w, 64) for w in folded]

    for a in range(len(folded) // 32):
      mline = hamming[a*32:(a+1)*32]
      init = ''.join('{:02X}'.format(w) for w in reversed(mline))
      minit['ECC_7S_1K5_INITP_{:02X}'.format(a)] = init

    return minit


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
  for k, v in minit.items():
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
    for k, v in dj_inits.items():
      hdl = re.sub(r'{{{}_..}}'.format(k), v, hdl)

  hdl = hdl.replace('{source file}', input_file)  # Extension not used by KCPSM3.exe
  # We don't support {psmname} because it is followed by a hard-coded extension in the
  # templates which may be invalid
  hdl = hdl.replace('{name}', os.path.splitext(os.path.basename(hdl_file))[0])
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
      for name, sig in signatures.items():
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
    raise FatalError(_('Cannot determine template data format'))

  return data_format


import shutil

def copy_standard_templates():
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



def build_path(output_dir, fname):
  '''Build a file path located in the output directory'''
  return os.path.normpath(os.path.join(output_dir, os.path.split(fname)[1]))

