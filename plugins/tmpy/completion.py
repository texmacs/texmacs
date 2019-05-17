import sys
import os
import keyword

py_ver = sys.version_info[0]

def do_module_hierarchy(mod, attr):
    """Explore an object's hierarchy.
    
    Go through the object hierarchy looking for
    attributes/methods to provide as autocompletion options.
    """
    dot = attr.find('.')
    if dot>0:
       if hasattr(mod, attr[:dot]):
          next = getattr(mod, attr[:dot])
          return do_module_hierarchy(next, attr[dot+1:])
    if isinstance(mod, dict):
       return dir(mod)
    else:
       return dir(mod)
 

def find_completion_candidates(cmpl_str, my_globals):
    """Harvest candidates to provide as autocompletion options."""
    
    if py_ver == 3:
        haystack = list(my_globals.keys()) + \
                   dir(my_globals['__builtins__']) + keyword.kwlist
    else:
        haystack = my_globals.keys() + \
                   dir(my_globals['__builtins__']) + keyword.kwlist

    dot = cmpl_str.rfind('.')
    offset = None
    if dot >= 0:
        offset = len(cmpl_str[dot+1:])
        first_dot = cmpl_str[:dot].find('.')
        if first_dot < 0:
            mod_name = cmpl_str[:dot]
            r_str = cmpl_str[dot+1:]
        else:
            mod_name = cmpl_str[:first_dot]
            r_str = cmpl_str[first_dot+1:]
        if mod_name in keyword.kwlist:
            return None, []
        if py_ver == 3:    
            if mod_name in os.sys.modules:
                haystack = do_module_hierarchy(os.sys.modules[mod_name], r_str)
            elif mod_name in list(my_globals.keys()):
                haystack = do_module_hierarchy(my_globals[mod_name], r_str)
            else:
                haystack = do_module_hierarchy(type(mod_name), r_str)
        else:
            if os.sys.modules.has_key(mod_name):
                haystack = do_module_hierarchy(os.sys.modules[mod_name], r_str)
            elif mod_name in my_globals.keys():
                haystack = do_module_hierarchy(my_globals[mod_name], r_str)
            else:
                haystack = do_module_hierarchy(type(mod_name), r_str)
            
    if py_ver == 3:
        return offset, [x for x in haystack if x.find(cmpl_str[dot+1:])  ==  0]
    else:
        return offset, filter(lambda x:x.find(cmpl_str[dot+1:])  ==  0, haystack)


def name_char(c):
    """Check whether a character is a valid identifier/keyword."""
    return c not in '+-*/%<>&|^~=!,:()[]{} \n\t'


def complete(s, pos, my_globals):
    """Process autocomplete command. """
    
    try:
        s = s[:pos]
        if not s:
            return '(tuple "" "")'
    except Exception as e:
        return '(tuple "" "")'
    # We get the string after the last space character.
    # No completion is done for strings containing spaces.
    i = len(s) - 1
    while i > 0:
        if not name_char(s[i]):
            i += 1
            break
        i -= 1
    s = s[i:]
    pos = len(s)
    # no string after last space? return empty completion
    if not s:
        return '(tuple "" "")'
        
    # Find completion candidates and form a suitable answer to Texmacs
    offset, cand = find_completion_candidates (s, my_globals)
    if not cand:
        res = '""'
    else:
        res = ''
    for c in cand:
        if offset is not None:
            pos = offset
        res += '"%s" ' % c[pos:]
    return '(tuple "' + s + '" ' + res + ')'

def from_scm_string(s):
    if len(s) > 2 and s[0] == '"' and s[-1] == '"':
        return s[1:-1]
    return s


def parse_complete_command(s):
    """HACK"""
    t1 = s.strip().strip('()').split(' ', 1)
    t2 = t1[1].rsplit(' ', 1)
    # Don't use strip('"') in case there are several double quotes
    return [t1[0], from_scm_string(t2[0]), int(t2[1])]
