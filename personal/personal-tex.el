;;; personal-tex.el --- LaTeX and AUCTeX customizations
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: bib, docs, tex, local
;; Compatibility: GNU Emacs: 24.x, Aquamacs: 3.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; If previews for preview-latex don't work, see this post on the TeX
;; Stack Exchange:
;;
;; <http://tex.stackexchange.com/questions/28458/preview-latex-in-emacs-auctex-empty-boxes>

;; Recommended by the AUCTeX manual.
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Make PDFs.
(setq TeX-PDF-mode t)

(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-command-list
                  ;; `%o' expands to the output filename. See
                  ;; `TeX-expand-list' for more details.

                  ;; The first `nil' disables giving the user a chance
                  ;; to modify the command.

                  ;; The second `t' enables for all TeX modes.
                  '("SCons" "scons %o" TeX-run-TeX nil t
                    :help "Run Scons in the current directory") t)
     (add-to-list 'TeX-command-list '("SCons-Clean" "scons --clean"
                                      TeX-run-command nil t) t)
     (add-to-list 'TeX-command-list
                  '("DocView" "(find-file \"%o\")" TeX-run-function
                    nil t :help "Open document in Emacs DocView") t)
     ;; This was compiled from the following sources:
     ;; /Applications/Aquamacs.app/Contents/Resources/lisp/aquamacs/auctex-config.el
     ;; <http://www.cs.berkeley.edu/~prmohan/emacs/latex.html>
     ;;
     ;; In the file mentioned above, Aquamacs uses AppleScript to talk
     ;; to Skim and has some other operations. However, there is a bug
     ;; in the implementation. Adding the function call to
     ;; `aquamacs-call-viewer' to the viewers list does not actually
     ;; call the function, because `TeX-run-discard-or-function' is
     ;; looking for a function _name_, not an S-Expression. Using
     ;; `TeX-run-function' works just fine, but that would prevent
     ;; other viewers from being used. Argh. This is just simpler.
     (when (eq system-type 'darwin)
       ;; Named Skim-displayline so as not to conflict with Skim in Aquamacs.
       (add-to-list 'TeX-view-program-list '("Skim-displayline" "/Applications/Skim.app/Contents/SharedSupport/displayline -readingbar %n %o %b"))
       ;; Now we want Skim-displayline to be the default viewer for
       ;; PDFs. Both Aquamacs and Prelude overwrite
       ;; `TeX-view-program-selection'. I want to replace the cons
       ;; cell for `output-pdf'. Let the games begin.
       (setq TeX-view-program-selection
             (cons '(output-pdf "Skim-displayline")
                   (assq-delete-all 'output-pdf TeX-view-program-selection))))))

(provide 'personal-tex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-tex.el ends here
