;;; personal-org.el --- Orgmode customizations
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

(eval-after-load 'org
  '(defun personal-org-todo-sort ()
    (interactive)
    ;; The `save-excursion' honestly doesn't do much, since org resets
    ;; the point to the beginning of the list it just sorted. But it
    ;; seems right to have it there.
    (save-excursion
      ;; Go to the beginning of the buffer in an attempt to sort
      ;; top-level.
      (goto-char (point-min))
      ;; First, priority sort. Order does matter here.
      (org-sort-entries nil ?p)
      ;; The point is now at the beginning of the list that just got
      ;; sorted. So if we run sort without this next call, it will just
      ;; sort one entry. Not what we want. Go to the beginning again.
      (goto-char (point-min))
      ;; Now, todo sort.
      (org-sort-entries nil ?o))))

(provide 'personal-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-org.el ends here
