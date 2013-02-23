;;; personal-global-keybindings.el --- Personal keybindings
;;
;; Filename: personal-global-keybindings.el
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: convenience, local
;; Compatibility:
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

(require 'bind-key)

;; The default key to switch buffer is C-x b, but that's not easy
;; enough.
(bind-key "C-x b" 'ido-switch-buffer)
(bind-key "C-x C-c" 'ido-switch-buffer)
(bind-key "C-x B" 'ibuffer)
;; An easy shortcut is needed for this common task.
(bind-key "C-x j" 'kill-this-buffer)
(bind-key "C-c r" 'rename-buffer)

(bind-key "RET" 'newline-and-indent)

;; Now that we've clobbered `kill-emacs', give a shortcut back.
(bind-key "C-x q" 'kill-emacs)

(provide 'personal-global-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-global-keybindings.el ends here
