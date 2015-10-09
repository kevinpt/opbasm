#!/usr/bin/python
# -*- coding: utf-8 -*-

from __future__ import print_function, division

import unittest
import os
import random
import sys
import psm_gen
import subprocess as subp
import filecmp
import string
import shutil

class RandomSeededTestCase(unittest.TestCase):
    def __init__(self, methodName='runTest', seedVarName='TEST_SEED'):
        unittest.TestCase.__init__(self, methodName=methodName)
        self.seed_var_name = seedVarName
        self.test_name = 'Unnamed test'
        self.trial = 0
        self.trial_count = 0

    def setUp(self):
        # In sub classes use the following to call this setUp() from an overrided setUp()
        # super(<sub-class>, self).setUp()
        
        # Use seed from enviroment if it is set
        try:
            seed = long(os.environ[self.seed_var_name])
        except KeyError:
            random.seed()
            seed = long(random.random() * 1e9)

        print('\n * Random seed: {} *'.format(seed))
        random.seed(seed)

    def update_progress(self, cur_trial, dotted=True):
        self.trial = cur_trial
        if not dotted:
            print('\r  {} {} / {}  '.format(self.test_name, self.trial, self.trial_count), end='')
        else:
            if self.trial == 1:
                print('  {} '.format(self.test_name), end='')
            endc = '' if self.trial % 100 else '\n'
            print('.', end=endc)

        sys.stdout.flush()


    def assertRelativelyEqual(self, a, b, epsilon, msg=None):
        if not relativelyEqual(a, b, epsilon):
            if msg is None:
                msg = '{} != {}'.format(a, b)
            raise self.failureException(msg)




def dos_to_unix(fname):
  with open(fname, 'r') as fh:
    lines = fh.readlines()

  with open(fname, 'w') as fh:
    fh.writelines(string.replace(l, '\r\n', '\n') for l in lines)



class TestPicoblaze(RandomSeededTestCase):

  def setUp(self):
    RandomSeededTestCase.setUp(self)
    test_output = 'test/testo'
    if not os.path.exists(test_output):
      os.makedirs(test_output)

    # KCPSM3 requires templates to be present in same directory as psm file
    kcpsm3_files = ['ROM_form.vhd', 'ROM_form.v', 'ROM_form.coe']

    for f in kcpsm3_files:
      shutil.copyfile(os.path.join('test/kcpsm3/Assembler', f), os.path.join(test_output, f))

  
  @unittest.skip('not in use')
  def test_pb3(self):
    self.test_name = 'Picoblaze 3 test'
    self.trial_count = 10

    os.chdir('test/testo')

    for i in xrange(self.trial_count):
      self.update_progress(i+1)

      basename = 'pb3_{:02}'.format(i)

      # Generate random source code
      ra = psm_gen.random_pb3()
      psm_file = basename + '.psm'
      ra.write_file(psm_file, 800)

      # Run opbasm on the source
      opbasm = subp.Popen(['opbasm', '-n', basename + '.opbasm', psm_file], stdin=subp.PIPE,
        stderr=subp.PIPE, stdout=subp.PIPE)
      opbasm.communicate()
      rval = opbasm.returncode

      self.assertEqual(rval, 0, 'Opbasm bad return value: {}'.format(rval))
      opbasm_mem = basename + '.opbasm.mem'

      # Generate dosemu batch file
      batch_file = 'run_pb3.bat'
      with open(batch_file, 'w') as fh:
        print(r'lredir x: linux\fs\lstorage\Projects\Devel\Python\opbasm', file=fh)
        print(r'set path=%path%;x:\test\kcpsm3\Assembler', file=fh)
        print(r'kcpsm3 X:\test\testo\{}'.format(psm_file), file=fh)

      # Run kcpsm3 on the source
      kcpsm3 = subp.Popen(['dosemu', '-dumb', batch_file], stdin=subp.PIPE,
        stderr=subp.PIPE, stdout=subp.PIPE)
      kcpsm3.communicate()
      rval = kcpsm3.returncode

      kcpsm_mem = basename + '.mem'

      # Convert DOS to unix line endings
      dos_to_unix(kcpsm_mem)

      # Compare mem files
      self.assertTrue(filecmp.cmp(opbasm_mem, kcpsm_mem, False),
        'Mem file mismatch {}'.format(kcpsm_mem))


  @unittest.skip('not in use')
  def test_pb6(self):
    self.test_name = 'Picoblaze 6 test'
    self.trial_count = 10

    os.chdir('test/testo')

    for i in xrange(self.trial_count):
      self.update_progress(i+1)

      basename = 'pb6_{:02}'.format(i)

      # Generate random source code
      ra = psm_gen.random_pb6()
      psm_file = basename + '.psm'
      ra.write_file(psm_file, 800)

      # Run opbasm on the source
      opbasm = subp.Popen(['opbasm', '-6', '-n', basename + '.opbasm', '-x', psm_file], stdin=subp.PIPE,
        stderr=subp.PIPE, stdout=subp.PIPE)
      opbasm.communicate()
      rval = opbasm.returncode

      self.assertEqual(rval, 0, 'Opbasm bad return value: {}'.format(rval))
      opbasm_mem = basename + '.opbasm.hex'

      # Generate dosemu batch file
      #batch_file = 'run_pb6.bat'
      #with open(batch_file, 'w') as fh:
      #  print(r'lredir x: linux\fs\lstorage\Projects\Devel\Python\opbasm', file=fh)
      #  print(r'set path=%path%;x:\test\kcpsm3\Assembler', file=fh)
      #  print(r'kcpsm6 X:\test\testo\{}'.format(psm_file), file=fh)

      # Run kcpsm6 on the source
      kcpsm6 = subp.Popen(['wine', '../kcpsm6/kcpsm6.exe', psm_file], stdin=subp.PIPE,
        stderr=subp.PIPE, stdout=subp.PIPE)
      kcpsm6.communicate()
      rval = kcpsm6.returncode

      kcpsm_mem = basename + '.hex'

      # Convert DOS to unix line endings
      dos_to_unix(kcpsm_mem)

      # Compare mem files
      self.assertTrue(filecmp.cmp(opbasm_mem, kcpsm_mem, False),
        'Mem file mismatch {}'.format(kcpsm_mem))


