# -*- coding: utf-8 -*-
"""
    Sphinx picoblaze domain
    ~~~~~~~~~~~~~~~~~~~~~~~

    The PicoBlaze language domain. Adapted from the C domain definition.

    :copyright: Copyright 2015 Kevin Thibedeau.
    :license: MIT, see LICENSE for details.
"""

import re
import string

from docutils import nodes

from sphinx import addnodes
from sphinx.roles import XRefRole
from sphinx.locale import l_, _
from sphinx.domains import Domain, ObjType
from sphinx.directives import ObjectDescription
from sphinx.util.nodes import make_refnode
from sphinx.util.docfields import Field, TypedField


# RE to split at word boundaries
wsplit_re = re.compile(r'(\W+)')

# REs for PicoBlaze signatures

pb_sig_re = re.compile(
  r'''^([\w]+)  \s* # Thing name
      (?: \((.*)\)  # Optional arguments
      )?  $         # Nothing else
  ''', re.VERBOSE)


def _parse_arglist(signode, arglist):
    """"Parse" a list of arguments separated by commas.

    Arguments can have "optional" annotations given by enclosing them in
    brackets.  Currently, this will split at any comma, even if it's inside a
    string literal (e.g. default argument value).
    """
    paramlist = addnodes.desc_parameterlist()
    stack = [paramlist]
    try:
        for argument in arglist.split(','):
            argument = argument.strip()
            ends_open = ends_close = 0
            while argument.startswith('['):
                stack.append(addnodes.desc_optional())
                stack[-2] += stack[-1]
                argument = argument[1:].strip()
            while argument.startswith(']'):
                stack.pop()
                argument = argument[1:].strip()
            while argument.endswith(']') and not argument.endswith('[]'):
                ends_close += 1
                argument = argument[:-1].strip()
            while argument.endswith('['):
                ends_open += 1
                argument = argument[:-1].strip()
            if argument:
                stack[-1] += addnodes.desc_parameter(argument, argument)
            while ends_open:
                stack.append(addnodes.desc_optional())
                stack[-2] += stack[-1]
                ends_open -= 1
            while ends_close:
                stack.pop()
                ends_close -= 1
        if len(stack) != 1:
            raise IndexError
    except IndexError:
        # if there are too few or too many elements on the stack, just give up
        # and treat the whole argument list as one argument, discarding the
        # already partially populated paramlist node
        signode += addnodes.desc_parameterlist()
        signode[-1] += addnodes.desc_parameter(arglist, arglist)
    else:
        signode += paramlist


class PBObject(ObjectDescription):
    """
    Description of a PicoBlaze language object.
    """

    doc_field_types = [
        TypedField('parameter', label=l_('Parameters'),
                   names=('param', 'parameter', 'arg', 'argument'),
                   typerolename='type', typenames=('type',)),
        Field('returnvalue', label=l_('Returns'), has_arg=False,
              names=('returns', 'return')),
        Field('returntype', label=l_('Return type'), has_arg=False,
              names=('rtype',)),
    ]


    def handle_signature(self, sig, signode):
        """Transform a PicoBlaze signature into RST nodes."""
        
        m = pb_sig_re.match(sig)
        if m is None:
            raise ValueError('no match')
        name, arglist = m.groups()
        
        signode += addnodes.desc_name(name, name)

        if not arglist:
            if self.objtype in ('function', 'macro'):
                # for functions and macros, add an empty parameter list
                signode += addnodes.desc_parameterlist()
                self.env.domains['pb'].data['has_params'][name] = False
            return name

        _parse_arglist(signode, arglist)
        
        self.env.domains['pb'].data['has_params'][name] = True
        
        return name

    def get_index_text(self, name):
        if self.objtype == 'function':
            return _('%s') % name
        elif self.objtype == 'macro':
            return _('%s') % name
        elif self.objtype == 'var':
            return _('%s (PicoBlaze variable)') % name
        else:
            return ''

    def add_target_and_index(self, name, sig, signode):
        # for PicoBlaze API items we add a prefix since names are usually not qualified
        # by a module name and so easily clash with e.g. section titles
        targetname = 'pb.' + name
        if targetname not in self.state.document.ids:
            signode['names'].append(targetname)
            signode['ids'].append(targetname)
            signode['first'] = (not self.names)
            self.state.document.note_explicit_target(signode)
            inv = self.env.domaindata['pb']['objects']
            if name in inv:
                self.state_machine.reporter.warning(
                    'duplicate PicoBlaze object description of %s, ' % name +
                    'other instance in ' + self.env.doc2path(inv[name][0]),
                    line=self.lineno)
            inv[name] = (self.env.docname, self.objtype)

        indextext = self.get_index_text(name)
        if indextext:
            self.indexnode['entries'].append(('single', indextext,
                                              targetname, '', None))


class PBXRefRole(XRefRole):
    def process_link(self, env, refnode, has_explicit_title, title, target):
        if not has_explicit_title:
            target = target.lstrip('~') # only has a meaning for the title
            # if the first character is a tilde, don't display the module/class
            # parts of the contents
            if title[0:1] == '~':
                title = title[1:]
                dot = title.rfind('.')
                if dot != -1:
                    title = title[dot+1:]
        return title, target
        
    def _has_parameters(self, env, title):
      if title in env.domains['pb'].data['has_params']:
        return env.domains['pb'].data['has_params'][title]
      else:
        return True

    def _fix_parens(self, env, has_explicit_title, title, target):
        '''Only add parens to macros with a parameter list'''
        if not has_explicit_title:
            if title.endswith('()'):
                # remove parentheses
                title = title[:-2]
            if env.config.add_function_parentheses and self._has_parameters(env, title):
                # add them back to all occurrences if configured
                title += '()'
        # remove parentheses from the target too
        if target.endswith('()'):
            target = target[:-2]
        return title, target

class PicoBlazeDomain(Domain):
    """PicoBlaze language domain."""
    name = 'pb'
    label = 'PicoBlaze'
    object_types = {
        'function': ObjType(l_('function'), 'func'),
        'macro':    ObjType(l_('macro'),    'macro'),
        'var':      ObjType(l_('variable'), 'data'),
    }

    directives = {
        'function': PBObject,
        'macro':    PBObject,
        'var':      PBObject,
    }
    roles = {
        'func' :  PBXRefRole(),
        'macro':  PBXRefRole(fix_parens=True),
        'type':   PBXRefRole(),
    }
    initial_data = {
        'objects': {},  # fullname -> docname, objtype
    }
    
    def __init__(self, env):
      super(PicoBlazeDomain, self).__init__(env)
      # Keep track of macros that do and do not have a parameter list
      self.data['has_params'] = {}
    

    def clear_doc(self, docname):
        for fullname, (fn, _) in self.data['objects'].items():
            if fn == docname:
                del self.data['objects'][fullname]

    def resolve_xref(self, env, fromdocname, builder,
                     typ, target, node, contnode):

        if target not in self.data['objects']:
            return None
        obj = self.data['objects'][target]
        return make_refnode(builder, fromdocname, obj[0], 'pb.' + target,
                            contnode, target)

    def get_objects(self):
        for refname, (docname, type) in self.data['objects'].iteritems():
            yield (refname, refname, type, docname, 'pb.' + refname, 1)
            
def setup(app):
    app.add_domain(PicoBlazeDomain)
