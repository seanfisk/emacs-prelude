;;; personal-c-common.el --- C and C++ customizations
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: c, languages, local
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

(defun personal-c-common-hook ()
  (setq c-default-style "bsd")
  (setq c-basic-offset 2)
  (setq indent-tabs-mode t)
  (setq tab-width 2)
  ;; correct for a bug in `whitespace.el'
  ;; shouldn't have to do this, but make sure we use tabs even in
  ;; `whitespace-cleanup'
  (setq whitespace-indent-tabs-mode indent-tabs-mode))

(add-hook 'c-mode-common-hook 'personal-c-common-hook t)

(provide 'personal-c-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-c-common.el ends here
