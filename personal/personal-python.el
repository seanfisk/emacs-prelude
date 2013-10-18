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

(require 'personal-misc-fn)

(defun personal-python-mode-setup ()
  ;; Follow PEP 8 conventions.
  ;; <http://www.python.org/dev/peps/pep-0008/#maximum-line-length>
  (personal-set-max-line-length 79)
  ;; Turn off flycheck-mode.
  ;; TODO: Use flycheck-mode and not flymake-mode with Elpy.
  (flycheck-mode -1)
  )

(add-to-list 'auto-mode-alist `(,(concat (regexp-opt '("SConstruct" "SConscript")) "\\'") . python-mode))

(add-hook 'python-mode-hook 'personal-python-mode-setup t)

(provide 'personal-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-python.el ends here
