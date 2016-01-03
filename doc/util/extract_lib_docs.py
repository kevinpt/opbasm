#!/usr/bin/python
'''
Extract documentation from the picoblaze macro library.
This uses a modified Docstring parser from sphinxcontrib-napoleon
'''

from sphinxcontrib.napoleon import *
import re

class PicoBlazeGoogleDocstring(GoogleDocstring):
  def __init__(self, docstring, config=None, app=None, what='', name='',
               obj=None, options=None):
               
    super(PicoBlazeGoogleDocstring, self).__init__(docstring, config, app, what,
                                         name, obj, options)


  def _parse_examples_section(self, section):
    '''Create an examples section with a PicoBlaze code block'''
    header = ['', '.. rubric:: Example:', '']
    block = ['.. code-block:: picoblaze', '']
    lines = self._consume_usage_section()
    lines = self._indent(lines, 3)
    
    return header + block + lines + ['']
    

  def _parse_parameters_section(self, section):
    '''Need to add blank line before parameter list'''
    fields = self._consume_fields()
    if self._config.napoleon_use_param:
      lines = []
      for _name, _type, _desc in fields:
        field = ':param %s: ' % _name
        lines.extend(self._format_block(field, _desc))
        if _type:
          lines.append(':type %s: %s' % (_name, _type))
      return [''] + lines + ['']
    else:
      return [''] + self._format_fields('Parameters', fields)


def extract_docstrings(fname):
  '''Pull out docstrings from picoblaze.m4'''
  funcs = {}
  state = None
  ds = []
  with open(fname) as fh:
    for l in fh:
      if l.startswith(';--------'):
        state = 'docstring'
        ds = []
        continue

      if state == 'docstring':
        if l.startswith(';'):
          ds.append(l[2:].rstrip())
        elif l.startswith('define('):
          # Get macro name
          m = re.match(r"^\s*define\(\s*`([^_][\w]+)'", l)
          if not m:
            m = re.match(r"^\s*define\(\s*<!([^_][\w]+)!>", l)
          
          if m:          
            funcs[m.group(1)] = ds
            state = None

            
  return funcs
  
def indent(text, spaces=2):
  prefix = ' '*spaces
  return prefix + ('\n'+prefix).join(text.split('\n'))

def build_signature(macro, body):
  '''Generate a macro signature from the parameter list'''
  params = []
  
  for l in body.split('\n'):
    m = re.search(r':param\s+([^:]*)\s*:', l)
    if m:
      p = m.group(1)
      if 'optional' in l.lower():
        p = '[{}]'.format(p)
      params.append(p)
  
  sig = '{}({})'.format(macro, ', '.join(params))
  return sig



  
  
funcs = extract_docstrings(sys.argv[1])


print '''
Opbasm PicoBlaze macro library reference
========================================
'''

for k in sorted(funcs.iterkeys()):
  body = indent(str(PicoBlazeGoogleDocstring(funcs[k])))
  
  signature = build_signature(k, body)
  
  print '.. pb:macro:: {}\n\n{}\n'.format(signature, body)

