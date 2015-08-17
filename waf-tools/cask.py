"""Detect and configure Cask."""

import os
from os import path

import waflib

@waflib.Configure.conf
def run_cask(self, subcommand):
    env = os.environ.copy()
    # Manually specify Emacs executable for Cask.
    env['EMACS'] = self.env.EMACS[0]
    self.exec_command(self.env.CASK + subcommand, cwd=self.env.USER_EMACS_DIR)

def configure(ctx):
    cmd = ctx.find_program('cask')
    prefix = None
    if ctx.env.BREW:
        try:
            # If Cask is installed using Homebrew, use the opt/ directory
            # instead of a directory with a specific version in the path. This
            # allows our file to survive upgrades of the tool.
            out = ctx.cmd_and_log(ctx.env.BREW + ['--prefix', 'cask'],
                                  quiet=waflib.Context.STDOUT)
        except waflib.Errors.WafError:
            pass
        else:
            prefix = out.rstrip()
    if prefix is None:
        # Otherwise just find the prefix based upon the location of the 'cask'
        # executable.
        prefix = path.dirname(path.dirname(path.realpath(cmd[0])))

    el_name = 'cask.el'
    el_path = path.join(prefix, el_name)
    el_node = ctx.root.find_node(el_path)
    ctx.msg("Checking for '{}'".format(el_name),
            el_node.abspath() if el_node else False)
    if el_node is None:
        ctx.fatal("Cask file not found: {}".format(el_path))
    ctx.env.CASK_EL_PATH = el_path

def build(ctx):
    in_node = ctx.path.find_resource([
        'personal', 'preload', 'personal-cask.el.in'])
    out_node = in_node.change_ext('')
    ctx(features='subst', target=out_node, source=in_node)
    ctx.install_node(out_node)

    ctx.install_node(ctx.path.find_resource('Cask'))
