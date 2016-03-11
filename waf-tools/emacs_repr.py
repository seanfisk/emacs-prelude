from waflib.Configure import conf

@conf
def emacs_repr(self, obj):
    """Convert a Python object to an Emacs source representation.

    Supports None, bool, and str."""
    if obj is None:
        return 'nil'
    if isinstance(obj, bool):
        return 't' if obj else 'nil'
    if isinstance(obj, str):
        # XXX Presumptuous quoting
        return '"{}"'.format(obj.replace('"', r'\"'))
    raise ValueError('Unsupported type {}'.format(type(obj)))
