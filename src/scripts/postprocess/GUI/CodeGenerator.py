#
# a Python code generator backend
#
# fredrik lundh, march 1998
#
# fredrik@pythonware.com
# http://www.pythonware.com
#
# Code taken from http://effbot.org/zone/python-code-generator.htm

import sys, string

class CodeGeneratorBackend:

    def begin(self, tab="    "):
        self.code = []
        self.tab = tab
        self.level = 0

    def end(self):
        return unicode(string.join(self.code, ""))

    def write(self, string):
        self.code.append(string)

    def writeln(self, string):
        self.code.append(self.tab * self.level + string + '\n')

    def indent(self):
        self.level = self.level + 1

    def dedent(self):
        if self.level == 0:
            raise SyntaxError, "internal error in code generator"
        self.level = self.level - 1

class Point():
    """Defines a Point. Has x and y."""
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def dump_self(self, filename):
        self.c = CodeGeneratorBackend()
        self.c.begin(tab="    ")
        self.c.write("class {0}{1}Point()\n".format(self.x,self.y))
        self.c.indent()
        self.c.write('"""Defines a Point. Has x and y"""\n')
        self.c.write('def __init__(self, x={0}, y={1}):\n'.format(self.x, self.y))
        self.c.indent()
        self.c.write('self.x = {0}\n'.format(self.x))
        self.c.write('self.y = {0}\n'.format(self.y))
        self.c.dedent()
        self.c.dedent()
        f = open(filename,'w')
        f.write(self.c.end())
        f.close()

if __name__ == "__main__":
    p = Point(3,4)
    p.dump_self('demo.py')