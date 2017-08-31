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

from opbasm.color import *

class Optimizer(object):
  name = ''
  requires = []

  def __init__(self):
    self.priority = 10

  def apply(self, asm, assembled_code):
    return []

  def summary(self, printf):
    pass

  def register(self, asm):
    # Register this optimizer
    asm.optimizers[self.name] = self

    # Register any other required optimizers recursively
    for opt_class in self.requires:
      opt = opt_class()
      if opt.name not in asm.optimizers:
        opt.register(asm)


class StaticAnalyzer(Optimizer):
  '''Analyzes code for reachability by statically tracing execution paths'''
  name = 'static'

  def __init__(self):
    self.priority = 50
    self.dead_instructions = None
    self.entry_points = None

  def apply(self, asm, assembled_code):

    self.keep_instructions(asm, assembled_code)

    self.dead_instructions = None
    self.entry_points = None

    # Run static analysis
    asm._print(_('  Static analysis: searching for dead code... '), end='')
    self.entry_points = set((asm.default_jump & 0xFFF, 0))
    self.entry_points |= set(asm.config.entry_point)
    self.find_dead_code(assembled_code, self.entry_points)
    asm._print(success(_('COMPLETE')))

    if not asm.config.quiet:
      # Summarize analysis
      asm._print(_('    Entry points:'), ', '.join(['0x{:03X}'.format(e) for e in \
        sorted(self.entry_points)]))
      self.dead_instructions = len([s for s in assembled_code if s.removable()])
      asm._print(_('    {} dead instructions found').format(self.dead_instructions))

    return assembled_code


  def keep_instructions(self, asm, assembled_code):
    '''Mark instructions we want to automatically keep'''
    # Find continuous blocks of labeled load&return instructions
    if asm.config.target_arch.has_string_table_support: # PB6 load&return depends on string/table
      cur_label = None
      prev_jump = None
      for s in assembled_code:
        if s.label is not None:
          cur_label = s.xlabel
          prev_jump = None

        unconditional_jump = s.command == 'jump' and s.arg2 is None

        # Mark l&r for preservation if its associated label is referenced by other code
        # Mark two or more consecutive unconditional jumps for preservation as part of a jump table
        if s.command == 'load&return' or (unconditional_jump and prev_jump):
          if cur_label is not None and asm.labels[cur_label].in_use:
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
    for s in assembled_code:
      if s.command == 'inst' and 'keep' not in s.tags:
        s.tags['keep_auto'] = (True,)


  def find_dead_code(self, assembled_code, entry_points):
    '''Perform dead code analysis'''
    itable = self.build_instruction_table(assembled_code)
    self.analyze_code_reachability(assembled_code, itable, entry_points)
    self.analyze_recursive_keeps(assembled_code, itable)


  def build_instruction_table(self, slist):
    '''Build index of all instruction statements by address'''
    itable = {}
    for s in slist:
      if s.is_instruction():
        itable[s.address] = s

    return itable


  def analyze_code_reachability(self, slist, itable, entry_points):
    '''Scan assembled statements for reachability'''

    addresses = set(entry_points)
    addresses.add(0)

    self.find_reachability(addresses, itable)


  def analyze_recursive_keeps(self, slist, itable):
    '''Scan assembled statements for reachability'''

    for s in slist:
      if s.is_instruction() and 'keep' in s.tags:
        self.find_reachability((s.address,), itable, follow_keeps=True)


  def find_reachability(self, addresses, itable, follow_keeps=False):
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
              self.find_reachability((s.immediate,), itable, follow_keeps)

            # Stop on unconditional jump
            # Only 1 argument -> unconditional
            if s.command == 'jump' and s.arg2 is None and 'jump_table' not in s.tags:
              break

        # Continue with next instruction if it exists
        a += 1


  def summary(self, printf):
    printf(_('  Static analysis:\n    Dead instructions {}: {}').format( \
      _('found'), self.dead_instructions))
    printf(_('    Analyzed entry points:'), ', '.join(['0x{:03X}'.format(e) for e in \
      sorted(self.entry_points)]))


class DeadCodeRemover(Optimizer):
  '''Removes instructions marked as dead'''
  name = 'dead_code'
  requires = [StaticAnalyzer]

  def __init__(self):
    self.priority = 60
    self.removed = 0

  def apply(self, asm, assembled_code):
    self.removed = 0
    self.remove_dead_code(asm, assembled_code)

    if self.removed > 0:
      # Reinitialize registers to default names
      asm._init_registers()

      asm._print(_('  Dead code removal: '), end='')
      # Reassemble code with dead code removed
      assembled_code = asm.raw_assemble(assembled_code)

      asm._print(success(_('COMPLETE')))

    return assembled_code


  def remove_dead_code(self, asm, assembled_code):
    '''Mark unreachable code for removal'''
    for s in assembled_code:
      if s.removable():
        # Convert the old instruction into a comment
        s.comment = _('REMOVED: ') + s.format().lstrip()
        s.command = None
        self.removed += 1

        # Track any removed labels
        if s.label is not None:
          if s.xlabel in self.labels:
            del asm.labels[s.xlabel]
            asm.removed_labels.add(s.xlabel)
          s.label = None
          s.xlabel = None

  def summary(self, printf):
    printf(_('  Dead code removal: {}'.format(_('Applied') if self.removed > 0 else _('None'))))


_all_optimizers = set([StaticAnalyzer, DeadCodeRemover])

