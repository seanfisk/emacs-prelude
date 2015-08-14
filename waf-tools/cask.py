"""Detect and configure Cask."""

from os.path import dirname, realpath, join

import waflib

def configure(ctx):
    path = ctx.find_program('cask')
    try:
        # If Cask is installed using Homebrew, use the opt/ directory
        # instead of a directory with a specific version in the path. This
        # allows our file to survive upgrades of the tool.
        out = ctx.cmd_and_log(['brew', '--prefix', 'cask'],
                              quiet=waflib.Context.STDOUT)
    except waflib.Errors.WafError:
        # Otherwise just find the prefix based upon the location of the
        # 'cask' executable.
        prefix = dirname(dirname(realpath(path[0])))
    else:
        prefix = out.rstrip()
    el_name = 'cask.el'
    el_path = join(prefix, el_name)
    el_node = ctx.root.find_node(el_path)
    ctx.msg("Checking for '{}'".format(el_name),
            el_node.abspath() if el_node else False)
    if el_node is None:
        ctx.fatal("Cask file not found: {}".format(el_path))
    ctx.env.CASK_EL_PATH = el_path

def build(ctx):
    init_node = ctx.path.find_or_declare('personal-cask-init.el')
    @ctx.rule(target=init_node, vars=['CASK_EL_PATH'])
    def _make_cask_init(tsk):
        tsk.outputs[0].write('''(require 'cask "{}")
(cask-initialize)'''.format(tsk.env.CASK_EL_PATH))
    ctx.install_files('${PREFIX}/personal/preload', init_node)
    ctx.install_files(ctx.env.PREFIX, 'Cask')
