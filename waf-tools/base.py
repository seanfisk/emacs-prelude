"""Emacs configuration installation helpers"""

from os import path

from waflib.Configure import conf

@conf
def install_node(self, node):
    self.install_as(path.join(self.env.PREFIX, node.relpath()), node)
