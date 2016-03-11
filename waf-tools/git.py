"""Detect and configure Git and Hub for Magit."""

def configure(ctx):
    ctx.find_program('git')
    ctx.find_program('hub', mandatory=False)

def build(ctx):
    in_node = ctx.path.find_resource(['personal', 'personal-magit.el.in'])
    out_node = in_node.change_ext('')
    ctx(features='subst', target=out_node, source=in_node,
        GIT=ctx.emacs_repr((ctx.env.HUB if ctx.env.HUB else ctx.env.GIT)[0]))
    ctx.install_node(out_node)
