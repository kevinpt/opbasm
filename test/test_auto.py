#!/usr/bin/python

from __future__ import print_function, division

import unittest, types
import os, re, subprocess, json


def FindOpbsim():
  '''Look for Opbsim under the local sim directory first then give default if it isn't found'''
  for p in ['sim/opbsim', 'sim/opbsim.exe']:
    if os.path.isfile(p): return p

  # Let the OS search for it
  return 'opbsim'


class GeneratedTestCase(unittest.TestCase):
    def __init__(self, methodName, method, **kwargs):
      setattr(self, methodName, types.MethodType(method, self))
      
      unittest.TestCase.__init__(self, methodName=methodName)

      for k, v in kwargs.iteritems():
        assert not hasattr(self, k), 'Attribute "{}" already exists'.format(k)
        setattr(self, k, v)

def do_test(self):
  
  # Compile PSM if MEM doesn't exist or is older than source
  path, psm_base = os.path.split(self.psm_file)
  out_dir = os.path.join(path, 'build', self.target)
  mem_file = os.path.join(out_dir, re.sub(r'\.psm4?$', '.mem', psm_base))
  
  
  if (not os.path.exists(mem_file)) or (os.path.getmtime(mem_file) < os.path.getmtime(self.psm_file)):
    # Assemble source
    #print('#####', os.path.getmtime(mem_file), os.path.getmtime(self.psm_file))

    cmd = 'opbasm -{} -q -o {} {}'.format(6 if self.target == 'pb6' else 3, out_dir, self.psm_file)
    #print('Running opbasm:', cmd)
    print('ASSEMBLING: {} ({})'.format(psm_base, self.target))
    r = subprocess.call(cmd, shell=True)
    #print('Ran opbasm:', r)
    self.assertTrue(r == 0, 'Opbasm failed')
    
  self.assertTrue(os.path.exists(mem_file), 'Cannot generate mem file')
  
  # Run the test
  opbsim = FindOpbsim()
  cmd = '{} -m:{} -j --{}'.format(opbsim, mem_file, self.target)
  #print('Running opbsim:', cmd)
  try:
    json_out = subprocess.check_output(cmd, shell=True)
  except subprocess.CalledProcessError as e:
    print('\n>>> '.join(('\n' + e.output).splitlines()).lstrip())
    try:
      json_out = json.loads(e.output)
      error_port = 0xFF  
      error_count = json_out['ports_out'][error_port]
    except ValueError:
      error_count = 1
      print('## JSON not found')
      
    self.assertTrue((e.returncode == 0) and (error_count == 0), 'Simulation failed with {} errors on {} target'.format(error_count, self.target))

  # Check results
  json_out = json.loads(json_out)
  self.assertTrue(json_out['termination'] == 'termNormal', 'Simulation failed with ' + json_out['termination'])


 
def load_tests(loader, tests, pattern):

    # Get all PSM files to use in tests
    psm_dir = 'test/asm'
    psm_files = [os.path.join(psm_dir,f) for f in next(os.walk(psm_dir))[2] if re.search(r'\.psm4?$', f)]
    
    for f in psm_files:
      # Check which processors to target
      with open(f, 'r') as fh:
        m = re.match(r';TARGET\s+(\w+)', fh.readline())
        if m:
          targets = [m.group(1)]
        else:
          targets = ['pb3', 'pb6']

      for target in targets:
        methodName = 'test_{}_{}'.format(os.path.basename(f).replace('.', '_'), target)
        tests.addTest(GeneratedTestCase(methodName, do_test, psm_file=f, target=target))
    
#    print('### PSM:', psm_files)
#    
#    for mem_file in ['foo/a.psm4', 'foo/b', 'foo/c']:
#        methodName = 'test_' + os.path.basename(mem_file).replace('.', '_')
#        tests.addTest(GeneratedTestCase(methodName, do_test, mem_file=mem_file))
        
    return tests

if __name__ == '__main__':
    unittest.main()

