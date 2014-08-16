#!/usr/bin/python
# -*- coding: utf-8 -*-

'''Generate random Picoblaze instructions'''

from __future__ import print_function

import os,sys,inspect
currentdir = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))
parentdir = os.path.dirname(currentdir)
sys.path.insert(0,parentdir)

from opbasm import Statement

import random, string


def random_bool():
  return random.choice((True, False))

def weighted_choice(choices):
  r = random.random() * sum(w for c, w in choices)
  for c, w in choices:
    r -= w
    if r < 0.0:
      return c

class random_pb3(object):

  def __init__(self):
    self.label_ix = 0
    self.const_ix = 0

    self.labels = set()
    self.constants = {}
    self.one_reg_opcodes = set(('rl', 'rr', 'sl0', 'sl1', 'sla', 'slx', 'sr0', 'sr1', 'sra', 'srx'))
    self.two_reg_opcodes = set(('add', 'addcy', 'and', 'compare', 'fetch', 'input', \
       'load', 'or', 'output', 'store', 'sub', 'subcy', 'test', 'xor'))

    self.storage_io_opcodes = set(('store', 'fetch', 'input', 'output'))
    self.registers = set(['s{}'.format(n) for n in xrange(10)] + ['sa', 'sb', 'sc', 'sd', 'se', 'sf'])

  def write_file(self, fname, n):
    asm = [s.format() + '\n' for s in self.statements(n)]
    with open(fname, 'w') as fh:
      fh.writelines(asm)

  def statements(self, n):
    for _ in xrange(n):
      yield(self.gen_statement())

  def gen_statement(self):

    ptree = {}

    if weighted_choice([(True, 0.3), (False, 1)]):
      ptree['label'] = ['L_{:03}'.format(self.label_ix)]
      self.label_ix += 1
      self.labels.add(ptree['label'][0])

    if random_bool():
      ptree['comment'] = [''.join([random.choice(string.ascii_letters) for _ in xrange(10)])]

    if random.random() > 0.1:
      # Generate command
      ptree['instruction'] = self.random_command()
      #print('### ptree:', ptree)
    
    #ptree = {'label':['lbl_1'], 'comment':[' comment 1'], 'instruction':['command']}
    return Statement(ptree, 1)

  def random_command(self):
    f = weighted_choice([(self.interrupt_command, 0.1), (self.control_command, 0.1), \
      (self.one_arg_command, 1), (self.two_arg_command, 1), (self.constant_def, 0.2)])

    return f()


  def one_arg_command(self):
    return [random.sample(self.one_reg_opcodes, 1)[0], self.random_register()]

  def two_arg_command(self):
    cmd = [random.sample(self.two_reg_opcodes, 1)[0], self.random_register()]

    if cmd[0] in self.storage_io_opcodes:
      if random_bool():
        cmd.append([self.random_register()])
        cmd.append('ireg')
      else:
        cmd.append('{:02X}'.format(random.randint(0, 0x3F)))
    else: # arithmetic or logical
      if random_bool():
        cmd.append(self.random_register())
      else:
        cmd.append(self.random_constant())

    return cmd

  def control_command(self):
    cmd = [random.choice(('jump', 'call', 'return'))]

    flag = random.choice(('', 'z', 'nz', 'c', 'nc'))

    if len(flag) > 0: cmd.append(flag)

    if cmd[0] != 'return': cmd.append(self.random_address())

    return cmd

  def interrupt_command(self):
    cmd = [random.choice(('returni', 'enable', 'disable'))]

    if cmd[0] == 'returni':
      cmd.append(random.choice(('enable', 'disable')))
    else:
      cmd.append('interrupt')

    return cmd

  def constant_def(self):
    const = ['constant', 'C_{:03}'.format(self.const_ix), '{:02X}'.format(random.randint(0, 0x3f))]
    self.const_ix += 1
    self.constants[const[1]] = const[2]

    return const
    

  def random_register(self):
    return random.sample(self.registers, 1)[0]

  def random_constant(self):
    const = '{:02X}'.format(random.randint(0, 255))

    if len(self.constants) > 0 and random_bool():
      const = random.sample(self.constants.keys(), 1)[0]
    
    return const


  def random_address(self):
    addr = '{:03X}'.format(random.randint(0, 1023))

    if len(self.labels) > 0 and random_bool():
      addr = random.sample(self.labels, 1)[0]
    
    return addr



class random_pb6(random_pb3):
  def __init__(self):
    random_pb3.__init__(self)
    self.string_ix = 0
    self.strings = {}
    self.prev_string = None
    self.two_reg_opcodes.add('comparecy')
    self.two_reg_opcodes.add('testcy')

  def random_command(self):
    if self.prev_string is not None:
      cmd = ['load&return', self.random_register(), self.prev_string]
      self.prev_string = None
      return cmd

    f = weighted_choice([(random_pb3.random_command.__get__(self), 1),
      (self.random_regbank, 0.03), (self.random_star, 0.03),
      (self.random_load_return, 0.03), (self.random_string, 0.03), (self.random_outputk, 0.03)])
    #f = random_pb3.random_command.__get__(self)

    return f()


  def control_command(self):
    cmd = [random.choice(('jump', 'call', 'return', 'jump@', 'call@'))]

    if cmd[0][-1] != '@':
      flag = random.choice(('', 'z', 'nz', 'c', 'nc'))

      if len(flag) > 0: cmd.append(flag)

      if cmd[0] != 'return': cmd.append(self.random_address())

    else: # PB6 *@ command
      cmd.append([self.random_register(), self.random_register()])
      #cmd.append(self.random_register())
      cmd.append('iaddr')

    return cmd


  def random_constant(self):
    c = random.randint(0, 255)
    choices = ['{:02X}'.format(c), "{}'d".format(c), "{:08b}'b".format(c),
      '"{}"'.format(random.choice(string.ascii_letters))]

    if len(self.constants) > 0:
      choices.append(random.sample(self.constants.keys(), 1)[0])
      choices.append('~' + choices[-1])

    return weighted_choice(zip(choices, [1]*len(choices)))

  def random_regbank(self):
    return ['regbank', '{}'.format(random.choice(('a', 'b')))]


  def random_star(self):
    cmd = ['star', self.random_register()]
    if True: #random_bool(): # FIXME: Allow random constant for arg2 once kcpsm6 is updated
      cmd.append(self.random_register())
    else:
      cmd.append(self.random_constant())
    return cmd

  def random_load_return(self):
    return ['load&return', self.random_register(), self.random_constant()]

  def random_string(self):
    sn = 'S_{:03}$'.format(self.string_ix)
    self.string_ix += 1
    sv = ''.join([random.choice(string.ascii_letters) for _ in xrange(random.randint(2, 10))])
    self.strings[sn] = sv

    self.prev_string = sn
    return ['string', sn, '"{}"'.format(sv)]

  def random_outputk(self):
    return ['outputk', self.random_constant(), '{:01X}'.format(random.randint(0, 15))]

if __name__ == '__main__':
  ra = random_pb6()

  for s in ra.statements(int(sys.argv[1])):
    print(s.format())
  #ra.write_file('foobar.psm', 40)

