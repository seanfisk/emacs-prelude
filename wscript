# -*- mode: python -*-

"""Waf build file"""

import os

import waflib
from waflib.Errors import WafError

# Avoid having unnecessary public attributes in this file, else they will be
# picked up as Waf commands.

# Waf constants
APPNAME = 'emacs-config'
VERSION = '0.1'
top = '.' # pylint: disable=invalid-name
out = 'build' # pylint: disable=invalid-name
# Override the default prefix.
default_prefix = os.path.expanduser('~/.emacs.d') # pylint: disable=invalid-name

WAF_TOOLDIR = 'waf-tools'

def configure(ctx):
    ctx.load(['python', 'emacs'], tooldir=WAF_TOOLDIR)
    ctx.check_python_version(version=('3', '4'))
    ctx.check_emacs_version(('24',))
    ctx.load('cask', tooldir=WAF_TOOLDIR)
    ctx.env.REPO_DIR = ctx.srcnode.abspath()


def build(ctx):
    # Install *.el files.
    ctx.install_files(
        ctx.env.PREFIX, ctx.path.ant_glob(incl='**/*.el', excl='sample/*'),
        # Preserve the directory hierarchy.
        relative_trick=True)

    # Create and install cask init file.
    ctx.load('cask', tooldir=WAF_TOOLDIR)

    # Install e-sink script.
    ctx.install_as(os.path.expanduser('~/bin/e-sink'),
                   'vendor/e-sink/e-sink.pl', chmod=waflib.Utils.O755)

    # Write this repository's path to a file in the configuration directory so
    # that the repository can be opened directly from Emacs.
    repo_path_node = ctx.path.find_or_declare('emacs-repo-path')
    @ctx.rule(target=repo_path_node, vars='REPO_DIR')
    def _make_repo_path(tsk):
        tsk.outputs[0].write(tsk.env.REPO_DIR)
    ctx.install_files(ctx.env.PREFIX, repo_path_node)
