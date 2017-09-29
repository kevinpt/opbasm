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

from __future__ import print_function, division, unicode_literals, absolute_import

class DeviceArch(object):
  def __init__(self):
    self.name = ''
    self.short_name = ''

  def instruction_words(self, asm, stmt):
    '''Determine the number of words generated for each instruction

    Normally this is 1 but the OUTPUTK and LOAD&RETURN instructions are
    replicated if a string or table is passed as an operand.
    s : Statement object
    '''

    return 1 if stmt.is_instruction() else 0



class DevicePb3(DeviceArch):
  def __init__(self):
    self.name = 'PicoBlaze-3'
    self.short_name = 'pb3'

    self.has_string_table_support = False

    self.zero_instr = 'LOAD s0, 00'

    self.opcodes = { \
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

    self.flag_opcodes = set(('call', 'jump', 'return'))
    self.flag_codes = {
      'c' : 0x1800,
      'nc': 0x1c00,
      'z' : 0x1000,
      'nz': 0x1400
    }
    self.return_flag_codes = self.flag_codes

    self.addr_opcodes = set(('call', 'jump'))

    self.one_reg_opcodes = set(('rl', 'rr', 'sl0', 'sl1', 'sla', 'slx', 'sr0', 'sr1', 'sra', 'srx'))

    self.two_reg_opcodes = set(('add', 'addcy', 'and', 'compare', 'fetch', 'input', \
      'load', 'or', 'output', 'store', 'sub', 'subcy', 'test', 'xor'))
    self.two_reg_op_offset = 0x1000

    self.directives = set(('address', 'constant', 'namereg', \
        'include', 'default_jump'))

class DevicePb6(DeviceArch):
  def __init__(self):
    self.name = 'PicoBlaze-6'
    self.short_name = 'pb6'

    self.has_string_table_support = True

    self.zero_instr = 'LOAD s0, s0'

    self.opcodes = { \
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

    self.flag_opcodes = set(('call', 'jump', 'return'))
    self.flag_codes = {
      'c' : 0x18000,
      'nc': 0x1c000,
      'z' : 0x10000,
      'nz': 0x14000
    }

    # PicoBlaze-6 uses inconsistent offsets for the conditional return instructions
    self.return_flag_codes = {
      'c' : 0x14000,
      'nc': 0x18000,
      'z' : 0x0c000,
      'nz': 0x10000
    }

    self.addr_opcodes = set(('call', 'jump'))

    self.one_reg_opcodes = set(('rl', 'rr', 'sl0', 'sl1', 'sla', 'slx', 'sr0', 'sr1', 'sra', 'srx', \
      'hwbuild'))

    self.two_reg_opcodes = set(('add', 'addcy', 'and', 'compare', 'fetch', 'input', \
      'load', 'or', 'output', 'store', 'sub', 'subcy', 'test', 'xor', \
      'comparecy', 'testcy'))
    self.two_reg_op_offset = -0x1000

    self.directives = set(('address', 'constant', 'namereg', \
        'include', 'default_jump', 'string', 'table'))


  def instruction_words(self, asm, stmt):
    '''Determine the number of words generated for each instruction

    Normally this is 1 but the OUTPUTK and LOAD&RETURN instructions are
    replicated if a string or table is passed as an operand.
    s : Statement object
    '''
    if stmt.is_instruction():
      num_words = 1

      array_name = None
      if stmt.command == 'outputk':
        if stmt.arg1 is not None and stmt.arg1[-1] in ('$', '#'):
          array_name = stmt.arg1

      elif stmt.command == 'load&return':
        if stmt.arg2 is not None and stmt.arg2[-1] in ('$', '#'):
          array_name = stmt.arg2

      if array_name is not None:
        if array_name[-1] == '$':
          if array_name not in asm.strings:
            raise StatementError(stmt, _('Unknown string:'), array_name)
          num_words = len(asm.strings[array_name].value)

        else: # Table
          if array_name not in asm.tables:
            raise StatementError(stmt, _('Unknown table:'), array_name)
          num_words = len(asm.tables[array_name].value)

      return num_words
    else:
      return 0

