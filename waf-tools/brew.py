"""Detect and configure Homebrew Emacs packages."""

from os import path

import waflib

def configure(ctx):
    cmd = ctx.find_program('brew', mandatory=False)
    if not cmd:
        return

    prefix = ctx.cmd_and_log(
        cmd + ['--prefix'], quiet=waflib.Context.STDOUT).rstrip()
    site_lisp_path = path.join(prefix, 'share', 'emacs', 'site-lisp')
    if path.isdir(site_lisp_path):
        ctx.env.BREW_SITE_LISP_PATH = site_lisp_path

    # Check for gettext, which isn't for some reason included in Homebrew's
    # 'site-lisp' directory.
    try:
        out = ctx.cmd_and_log(ctx.env.BREW + ['--prefix', 'gettext'],
                              quiet=waflib.Context.STDOUT)
    except waflib.Errors.WafError:
        gettext_prefix = False
    else:
        gettext_prefix = path.join(out.rstrip(), 'share', 'emacs', 'site-lisp')

    ctx.msg('Checking for Homebrew gettext', gettext_prefix)
    if gettext_prefix:
        ctx.env.BREW_GETTEXT_PREFIX = gettext_prefix

def build(ctx):
    if not ctx.env.BREW:
        return

    in_node = ctx.path.find_resource([
        'personal', 'preload', 'personal-brew.el.in'])
    out_node = in_node.change_ext('')
    ctx(features='subst',
        target=out_node,
        source=in_node,
        GETTEXT_COMMANDS=('''
;; We need to add this to the `load-path' instead of just requiring it, as
;; other files in this directory are needed.
(add-to-list 'load-path "{}")
;; We can't use `require' here as `start-po' has no `provides' and is therefore
;; not a feature.
(load-library "start-po")
'''.format(ctx.env.BREW_GETTEXT_PREFIX)
                          if ctx.env.BREW_GETTEXT_PREFIX else ''))
    ctx.install_node(out_node)
