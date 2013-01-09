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

(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-command-list
                  ;; `%o' expands to the output filename. See
                  ;; `TeX-expand-list' for more details.

                  ;; The first `nil' disables giving the user a chance
                  ;; to modify the command.

                  ;; The second `t' enables for all TeX modes.
                  '("SCons" "scons %o" TeX-run-command nil t
                    :help "Run Scons in the current directory") t)
     (add-to-list 'TeX-command-list '("SCons-Clean" "scons --clean"
                                      TeX-run-command nil t) t)))

(provide 'personal-tex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-tex.el ends here
