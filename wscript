# -*- mode: python -*-

"""Waf build file"""

import sys
import os
from tempfile import TemporaryDirectory
from pathlib import Path
import platform

import waflib

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

class TryContext(waflib.Build.InstallContext):
    """try it before you buy it"""
    cmd = 'try'

def configure(ctx):
    ctx.msg('Setting prefix to', ctx.env.PREFIX)
    ctx.load(['python', 'emacs'], tooldir=WAF_TOOLDIR)
    ctx.check_python_version(version=('3', '5'))
    # Emacs 25 adds support for file notifications use kqueue. Using glib for
    # file notifications was possible since 24.4, but did not actually work on
    # OS X.
    ctx.check_emacs_version(('25', '0'))
    ctx.load(['brew', 'cask', 'dired', 'git', 'markdown'], tooldir=WAF_TOOLDIR)
    ctx.env.REPO_DIR = ctx.srcnode.abspath()

def build(ctx):
    # Run 'cask install' after installing the configuration. Add this post_fun
    # before adding the one for the 'try' command.
    if ctx.cmd in ('install', 'try'):
        ctx.add_post_fun(lambda ctx: ctx.run_cask(['install']))

    if ctx.cmd == 'try':
        # Create temporary directory.
        temp_dir = TemporaryDirectory(prefix='emacs-config-')
        temp_path = Path(temp_dir.name)
        ctx.to_log("Writing files to '{}'\n".format(temp_path))

        full_screen_terminal = platform.system() != 'Darwin'
        # There's no point in waiting for the user to see the temp dir path if
        # the user can't answer the prompt or we're not full-screening the
        # terminal.
        if full_screen_terminal and sys.stdin.isatty():
            input('Press Enter to continue.')

        emacs_path = temp_path / '.emacs.d'
        emacs_path.mkdir()
        # Symlink the real '.cask' directory to the fake one.
        ctx.symlink_as(str(emacs_path / '.cask'),
                       os.path.join(ctx.env.PREFIX, '.cask'))
        # Reset prefix.
        ctx.env.PREFIX = str(emacs_path)

        def run(ctx):
            env = os.environ.copy()
            # Remapping HOME is the easiest way to load an alternate init file.
            # http://stackoverflow.com/a/17149070/879885
            env['HOME'] = str(temp_path)
            emacs_args = ['--debug-init']
            if full_screen_terminal:
                ctx.exec_command(
                    ctx.env.EMACS_EXE + emacs_args,
                    # Don't buffer the program's output.
                    stdout=None, stderr=None,
                    env=env)
            else:
                # XXX This ignores the Emacs set by ctx.env.EMACS_EXE
                ctx.exec_command(
                    [
                        'open', '-a', 'Emacs',
                        '-n', # Start a new instance
                        '-W', # Wait (block) until closed
                        '--args'
                    ] + emacs_args,
                    env=env)
            ctx.to_log("Removing '{}'\n".format(str(temp_path)))
            temp_dir.cleanup()

        ctx.add_post_fun(run)

    ctx.load('base', tooldir=WAF_TOOLDIR)

    # Install *.el files.
    el_nodes = ctx.path.ant_glob(
        incl='**/*.el',
        # Make sure to exclude the build directory.
        excl=['sample', out])
    for node in el_nodes:
        ctx.install_node(node)

    # Create and install generated files.
    ctx.load(['emacs_repr', 'brew', 'cask', 'dired', 'git', 'markdown'],
             tooldir=WAF_TOOLDIR)

    # Install e-sink script.
    ctx.install_as(os.path.expanduser('~/bin/e-sink'),
                   'vendor/e-sink/e-sink.pl', chmod=waflib.Utils.O755)

    # Write this repository's path to a file in the configuration directory so
    # that the repository can be opened directly from Emacs.
    repo_path_node = ctx.path.find_or_declare('emacs-repo-path')
    @ctx.rule(target=repo_path_node, vars='REPO_DIR')
    def _make_repo_path(tsk):
        tsk.outputs[0].write(tsk.env.REPO_DIR)
    ctx.install_node(repo_path_node)

class CaskupContext(waflib.Build.BuildContext):
    """updates Cask packages"""
    cmd = 'caskup'
    fun = 'caskup'

def caskup(ctx):
    ctx.run_cask(['update'])
