;;; personal-ruby.el --- Ruby configuration
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
;; Configure ruby-mode.
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

(require 'max-line-length)

(defun personal-ruby-mode-setup ()
  "Configure `ruby-mode'."
  ;; Attempt to follow Ruby Style Guide conventions (enforced by Rubocop).
  ;; <https://github.com/bbatsov/ruby-style-guide#source-code-layout>
  ;;
  ;; In the Ruby Style Guide, it specifies the max length as 80, but
  ;; Rubocop defaults to 79. It doesn't really matter, so we'll set it
  ;; at 79 to comply with default Rubocop.
  (max-line-length-set 79))

(add-to-list 'auto-mode-alist '("Buildfile" . ruby-mode))

(add-hook 'ruby-mode-hook 'personal-ruby-mode-setup t)

(provide 'personal-ruby)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-ruby.el ends here
