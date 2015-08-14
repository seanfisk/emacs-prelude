;;; personal-misc.el --- Miscellaneous configuration
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: convenience
;; Compatibility: GNU Emacs: 24.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Configure things that don't fit anywhere else.
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

(defvar compilation-scroll-output)
(setq compilation-scroll-output 'first-error)

;; I end sentences with a single space. Sue me.
(setq sentence-end-double-space nil)

(defun bootstrap-Info-mode ()
  "Open info files in \"real\" Info-mode."
  (interactive)
  ;; Credit: http://stackoverflow.com/a/1921156
  ;; TODO: This works, but it usually dumps this to the terminal from emacsclient:
  ;;
  ;;     *ERROR*: Wrong type argument: stringp, nil
  ;;
  ;; Will have to work on this.
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))

(add-to-list 'auto-mode-alist '("\\.info\\'" . bootstrap-Info-mode))

(provide 'personal-misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-misc.el ends here
