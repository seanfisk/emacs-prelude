"""Configure Emacs dired.

Find an ``ls`` compatible with dired.
"""

def configure(ctx):
    # Don't display find_program's messages
    ctx.in_msg = 1

    # Prefer 'gls', as it is convention to name custom-installed GNU coreutils
    # 'ls' as 'gls'.
    for name in ['gls', 'ls']:
        prog = ctx.find_program(name, var='_' + name.upper(), mandatory=False)
        # This is how dired itself checks for a compatible 'ls'.
        if prog and ctx.exec_command(prog + ['--dired']) == 0:
            ctx.env.LS = prog
            break

    ctx.in_msg = 0
    ctx.msg("Checking for dired-compatible 'ls'",
            ctx.env.LS[0] if ctx.env.LS else False)

def build(ctx):
    in_node = ctx.path.find_resource([
        'personal', 'personal-dired.el.in'])
    out_node = in_node.change_ext('')
    use_ls = bool(ctx.env.LS)
    ctx(features='subst', target=out_node, source=in_node,
        USE_LS=ctx.emacs_repr(use_ls),
        SET_LS='(setq insert-directory-program {})'.format(ctx.emacs_repr(
            ctx.env.LS[0])) if use_ls else '')
    ctx.install_node(out_node)
