;;; personal-shell.el --- Shell-mode customizations
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: local, processes, terminals, unix
;; Compatibility: GNU Emacs: 24.x; Aquamacs: 3.x
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

(defun personal-sh-hook ()
  (setq sh-basic-offset 2)
  (setq tab-width 2)
  (setq indent-tabs-mode t)
  ;; this is necessary to refresh local `whitespace-mode' variables
  (whitespace-mode t))

(add-hook 'sh-mode-hook 'personal-sh-hook t)

(provide 'personal-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-shell.el ends here
