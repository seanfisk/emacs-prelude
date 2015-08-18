;;; personal-misc-fn.el --- Miscellaneous functions
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Package-Requires: ((f "0.17.2"))
;; Keywords: convenience, local, tools
;; Compatibility: GNU Emacs: 24.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Assorted functions that are generally useful.
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

(defun personal-set-fill-column (value)
  "Set `fill-column' for a mode to VALUE.

This also turns on `fci-mode'."
  (setq fill-column value)
  ;; Specifically *don't* set `fci-rule-column' (the column at which
  ;; the line is shown). `fci-mode' will then default to using the
  ;; value of `fill-column'.
  (turn-on-fci-mode))

(defun personal-sort-buffer (reverse)
  "Sort lines in buffer alphabetically; REVERSE means descending order."
  (interactive "P")
  (sort-lines reverse (point-min) (point-max)))

;; This is copied and modified from `beginning-or-indentation' from
;; `misc-cmds.el' by Drew Adams.
(defun personal-toggle-beginning-or-indentation ()
  "Move cursor to beginning of this line or to its indentation.
If at or within the indentation, move to beginning of line.

If at the beginning of the line or after the indentation, move to
the indentation."
  (interactive)
  (cond ((bolp)
         ;; We are at the beginning of the line.
         (back-to-indentation))
        ((save-excursion (skip-chars-backward " \t") (bolp))
         ;; We are at or within the indentation.
         (forward-line 0))
        ;; Otherwise, go to indentation.
        (t (back-to-indentation))))

(require 'f)

(defun personal-emacs-config-open ()
  "Open my Emacs config for editing."
  (interactive)
  (magit-status (f-read (f-expand "emacs-repo-path" user-emacs-directory))))

(defun personal-git-merge-squash-commit-message-cleanup ()
  "Clean up a git merge squash commit buffer.
This assumes that each of your commit messages is one line only!
Don't use it otherwise!"
  (interactive)
  (save-excursion
    ;; Jump to beginning of file.
    (goto-char (point-min))
    ;; Delete the header.
    (delete-lines 2)
    ;; Delete the extra junk.
    (flush-lines "^\\(?:commit\\|Author:\\|Date:\\|$\\)")
    ;; Replace the leading spaces with bullets.
    (while (re-search-forward "^ \\{4\\}" nil t)
      (replace-match "- "))
    ;; Now, we'll be on the last line of the list. Search for the
    ;; comment start.
    (re-search-forward "^#")
    ;; Reverse the list so it is chronological.
    (reverse-region (point-min) (point))))

(provide 'personal-misc-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-misc-fn.el ends here
