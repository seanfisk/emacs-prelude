* Press `C-;` to toggle whether Option key means Meta or Option. This
  can be used to get rid of the annoying insertion of symbols when I
  tap it accidentally.
* If Aquamacs dies and you get the "Send Error Report" dialog, this
  can cause nightly builds (as of 2012-12-08) to crash when the dialog
  buttons are clicked. Directly run the executable in the app bundle
  to see the errors. To fix it, I put Aquamacs-2.4 back into
  /Applications, opened Aquamacs, closed the dialogs, then put nightly
  Aquamacs back into /Applications (overwriting 2.4). Ugly, but it
  works.
* Use `M-x finder-list-keywords` to get a list of keywords that can be
  used in Emacs Lisp file headers.
* Open multiple info buffers by giving a prefix argument to info,
  e.g., `C-u 5 C-h i`.
* Use directory variables by creating a `.dir-locals.el` file.
* Use registers and bookmarks
* When rebuilding the config for Aquamacs, you can...
    * Check progress by looking at the **Window** menu. There should be a bunch of `*-autoloads.el` open. As long as those keep opening, it's going good. When a backtrace buffer appears, it started failing.
    * If it looks like nothing is happening, clicking **About Aquamacs** can be used to bring up a frame.
* `M-x fixup-whitespace` collapses all but one space.
* Use keyboard macros (`start-kb-macro`, `end-kbd-macro`, `call-last-kbd-macro`, etc. `C-h a kbd-macro` too see all)
* Use more snippets (and include Emacs Lisp transforms)
* Check out expand region
* AFAIK, it's not really possible to automatically autoload vendorized code.
* Press `C-h` after a key prefix to see all the keys defined for that
  prefix. For example, `C-x C-h` shows all keys bound with a prefix of
  `C-x`. Ditto for `C-c C-h`.
* zsh adds a bunch of entries to the MANPATH that it somehow
  finds. Starting Emacs from zsh on my Mac OS X allows man and WoMan
  to find a bunch more manuals than starting from bash.
