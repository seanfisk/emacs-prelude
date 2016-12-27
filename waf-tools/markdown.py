"""Detect and configure Pandoc for markdown-mode."""

def configure(ctx):
    ctx.find_program('pandoc', mandatory=False)

def build(ctx):
    if not ctx.env.PANDOC:
        return
    in_node = ctx.path.find_resource(['personal', 'personal-markdown.el.in'])
    out_node = in_node.change_ext('')
    ctx(features='subst', target=out_node, source=in_node,
        MARKDOWN_COMMAND='(setq markdown-command {})'.format(
            ctx.emacs_repr('{} --from=markdown_github --to=html5'.format(
                ctx.env.PANDOC[0]))))
    ctx.install_node(out_node)
