;;; personal-server.el --- Emacs server configuration
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: convenience, local, tools
;; Compatibility: GNU Emacs: 24.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Configure the Emacs server.
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

;; `server-running-p' and `server-buffer-clients' are not autoloaded,
;; so require `server' feature before using it.
(require 'server)

(defun personal-server-hook ()
  "Configure the Emacs server."
  (when (current-local-map)
    (use-local-map (copy-keymap (current-local-map))))
  (when server-buffer-clients
    ;; Use the same shortcut as kill-this-buffer
    (local-set-key [remap kill-this-buffer] 'server-edit)))

(add-hook 'server-switch-hook 'personal-server-hook t)

(when (not (server-running-p))
  ;; Start the Emacs server for use with emacsclient, but only if one
  ;; doesn't already exist. Useful for running two Emacsen, for
  ;; example, when editing the Emacs configuration.
  (server-start))

(provide 'personal-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-server.el ends here
