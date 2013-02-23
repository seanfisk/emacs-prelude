I use Aquamacs on Mac OS X with Prelude. Aquamacs is configured by default to use Mac OS X's spell checker, i.e., `ispell-program-name` is `"NSSpellChecker"`. This is what I want. However, Prelude overrides this and assumes I want to use Aspell.

While I would definitely agree that Ispell → Aspell is an upgrade, I would disagree that Hunspell → Aspell is necessarily best. I believe Hunspell is used internally by Mac OS X's spell checker as well.

Emacs 24.2.1 already contains this code in `ispell.el` for selecting a proper spell checker:

```lisp
(defcustom ispell-program-name
  (or (locate-file "aspell"   exec-path exec-suffixes 'file-executable-p)
      (locate-file "ispell"   exec-path exec-suffixes 'file-executable-p)
      (locate-file "hunspell" exec-path exec-suffixes 'file-executable-p)
      "ispell")
  "Program invoked by \\[ispell-word] and \\[ispell-region] commands."
  :type 'string
  :group 'ispell)
```

This means that Emacs is already preferring Aspell to Ispell (but not to Hunspell). I do like the addition of `"--sug-mode=ultra"` *if* Aspell is being used.
