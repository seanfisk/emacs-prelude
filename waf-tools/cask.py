"""Detect and configure Cask."""

import os
from pathlib import Path

import waflib

@waflib.Configure.conf
def run_cask(self, subcommand):
    env = os.environ.copy()
    # Manually specify Emacs executable for Cask.
    env['EMACS'] = self.env.EMACS[0]
    self.exec_command(self.env.CASK + subcommand, cwd=self.env.USER_EMACS_DIR)

def configure(ctx):
    cmd = ctx.find_program('cask')
    if ctx.env.BREW and ctx.exec_command(
            ctx.env.BREW + ['list', '--versions', 'cask']) == 0:
        ctx.env.CASK_REQUIRE = '''\
; Installed with Homebrew, and already on the `load-path'.
(require 'cask)'''
    else:
        # Otherwise just find the prefix based upon the location of the 'cask'
        # executable.
        el_path = Path(cmd[0]).resolve().parents[1] / 'cask.el'
        ctx.env.CASK_REQUIRE = '(require \'cask "{}")'.format(el_path)

def build(ctx):
    in_node = ctx.path.find_resource([
        'personal', 'preload', 'personal-cask.el.in'])
    out_node = in_node.change_ext('')
    ctx(features='subst', target=out_node, source=in_node)
    ctx.install_node(out_node)

    ctx.install_node(ctx.path.find_resource('Cask'))
