;;; max-line-length.el --- Set the maximum line length
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: languages
;; Compatibility: GNU Emacs: 24.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Set the maximum line length.
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


(require 'whitespace)
(require 'fill-column-indicator)

(defun max-line-length-set (max-line-length)
  "Set the maximum allowed line length (without warnings) in a buffer to MAX-LINE-LENGTH."
  ;; Specifically *don't* set `fci-fill-column' (the column at which the
  ;; line is shown). `fci-mode' will then default to using the value of
  ;; `fill-column'.
  (setq fill-column max-line-length)
  (turn-on-fci-mode)
  ;; Whitespace-mode refuses to inherit whatever the value of
  ;; `fill-column` is. However, `fill-column-indicator' does it
  ;; nicely.
  (setq whitespace-line-column max-line-length))

(provide 'max-line-length)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; max-line-length.el ends here
