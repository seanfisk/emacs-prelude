;;; personal-misc-fn.el --- Miscellaneous functions
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: convenience, local, tools
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

(defun sort-buffer (reverse)
  "Sort lines in buffer alphabetically; argument means descending order."
  (interactive "P")
  (sort-lines reverse (point-min) (point-max)))

(when (eq system-type 'darwin)
  ;; Latest flex installed using homebrew.
  (defun read-latest-flex-info ()
    "Open up the Info manual for the latest version of flex installed using Homebrew."
    (interactive)
    ;; Using `*flex-info*' as the buffer name allows it not to
    ;; conflict with default `*info*' buffers, and have both open at
    ;; the same time.
    (info "/usr/local/opt/flex/share/info/flex.info" "*flex-info*")))

;; This is copied and modified from `beginning-or-indentation' from
;; `misc-cmds.el' by Drew Adams.
(defun toggle-beginning-or-indentation ()
  "Move cursor to beginning of this line or to its indentation.
If at or within the indentation, move to beginning of line.

If at the beginning of the line or after the indentation, move to
the indentation."
  (interactive)
  (cond ((bolp)
         ;; We are at the beginning of the line.
         (back-to-indentation))
        ((save-excursion (skip-chars-backward " \t") (bolp))
         ;; We are at or within the indentation.
         (forward-line 0))
        ;; Otherwise, go to indentation.
        (t (back-to-indentation))))

(provide 'personal-misc-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-misc-fn.el ends here
