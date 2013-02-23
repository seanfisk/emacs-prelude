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

(provide 'personal-misc-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-misc-fn.el ends here
