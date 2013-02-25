;;; personal-python.el --- Python Customizations
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: languages
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

(defun personal-python-mode-setup ()
  ;; Follow PEP8 conventions.
  (let ((max-line-length 79))
    (setq fill-column max-line-length)
    ;; Whitespace-mode refuses to inherit whatever the value of
    ;; `fill-column` is. However, `fill-column-indicator' does it
    ;; nicely.
    (setq whitespace-line-column max-line-length))
  ;; Turn off flycheck-mode.
  ;; TODO: Use flycheck-mode and not flymake-mode with Elpy.
  (flycheck-mode -1)
  )

(add-hook 'python-mode-hook 'personal-python-mode-setup t)

(provide 'personal-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-python.el ends here
