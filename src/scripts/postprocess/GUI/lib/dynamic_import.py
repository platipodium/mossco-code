# -----------------------------------------------------------------------------
# > @brief NCINFO - Visualization tool for NOSSCO-output files
#
#   This computer program is part of MOSSCO.
# > @copyright Copyright (C) 2013, 2014, 2015 Bundesanstalt fuer Wasserbau
# > @author Nikolai Chernikov, <nikolai.chernikov.ru@gmail.com>
#
#  MOSSCO is free software: you can redistribute it and/or modify it under the
#  terms of the GNU General Public License v3+.  MOSSCO is distributed in the
#  hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
#  LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
# ------------------------------------------------------------------------------

# Importing a dynamically generated module


def importCode(code, name, add_to_sys_modules=False):
    """
    Import dynamically generated code as a module. code is the
    object containing the code (a string, a file handle or an
    actual compiled code object, same types as accepted by an
    exec statement). The name is the name to give to the module,
    and the final argument says wheter to add it to sys.modules
    or not. If it is added, a subsequent import statement using
    name will return this module. If it is not added to sys.modules
    import will try to load it in the normal fashion.

    import foo

    is equivalent to

    foofile = open("/path/to/foo.py")
    foo = importCode(foofile, "foo", True)

    Returns a newly generated module.
    """
    import sys, imp

    module = imp.new_module(name)
    
    exec code in module.__dict__
    if add_to_sys_modules:
        sys.modules[name] = module

    return module


if __name__ == '__main__':
    # Example
    code = str(
        'def testFunc():\n' +
        '   print "spam!"\n' +
        '\n' +
        'class testClass:\n' +
        '   def testMethod(self):\n' +
        '       print "eggs!"')

    m = importCode(code, "test")
    m.testFunc()
    o = m.testClass()
    o.testMethod()
