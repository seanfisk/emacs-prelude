"""Check for a minimum Emacs version."""

from waflib.Configure import conf

import shlex
import re

VERSION_RE = re.compile(r'GNU Emacs (\d+)\.(\d+)\.(\d+)')

def _format_version(version_tuple):
    return 'Emacs ' + '.'.join(version_tuple)

@conf
def check_emacs_version(self, version):
    # When running a compilation from within Emacs, Emacs set the EMACS
    # environment variable to 't', which causes it to be picked up here if
    # using the default environment variable. Change it to EMACS_EXE to avoid
    # this problem.
    emacs_path = self.find_program('emacs', var='EMACS_EXE')
    cmd = emacs_path + ['--version']
    out = self.cmd_and_log(cmd)
    match = VERSION_RE.match(out)
    if not match:
        self.fatal("Unexpected output format from {}".format(
            shlex.quote(' '.join(shlex.quote(arg) for arg in cmd))))
    found_version = match.groups()
    required_name = _format_version(version) + '+'
    found_name = _format_version(found_version)
    meets_requirements = found_version >= version
    self.msg('Checking for ' + required_name,
             found_name if meets_requirements else False)
    if not meets_requirements:
        self.fatal('Expecting {}, found {}'.format(required_name, found_name))
