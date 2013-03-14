;;; toggle-plural.el --- Toggle between singular and plural forms
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; URL: http://github.com/seanfisk/emacs
;; Keywords: abbrev, convenience, wp
;; Compatibility: GNU Emacs 24.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Toggle between the singular and plural forms of a word at the
;; point.  The implementation uses `inflections.el' and is far from
;; perfect.  It does, however, work surprisingly well in many cases.
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

(require 'inflections)

(defun toggle-plural-string (str)
  "Toggle word contained in STR between singular and plural forms."
  ;; This function is not technically correct because it's impossible
  ;; to tell, given the layout of the `inflections' package, whether a
  ;; word is a singular or plural form. It is really just best-effort.
  (when (stringp str)
    ;; None of the pluralizations should change the first character of
    ;; the string, so just save it to preserve the case of the first
    ;; letter. It's not perfect, but it's better than downcasing it
    ;; every time.
    (let ((lower-str (downcase str)))
      (concat (list (aref str 0))
              (substring
               (or
                ;; In inflections, this does not pass a `downcase'd
                ;; word. Not sure why, since that's more correct.
                (car (member lower-str inflection-uncountables))
                ;; Try to singularize with irregulars.
                (caar (member* lower-str inflection-irregulars :key 'cadr :test 'equal))
                ;; Try to pluralize with irregulars.
                (cadar (member* lower-str inflection-irregulars :key 'car :test 'equal))
                ;; Try to singularize with regex.
                (loop for (from to) in inflection-singulars
                      for singular = (string=~ from str (sub to))
                      when singular do (return singular))
                ;; Try to pluralize with regex.
                (loop for (from to) in inflection-plurals
                      for plurals = (or (string=~ from str (sub to)))
                      when plurals do (return plurals))
                ;; Failed to do all of these.
                str) 1)))))

(defun toggle-plural-at-point ()
  "Toggle between the singular and plural forms of word at the point."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds))
         (word (buffer-substring start end))
         (new-word (toggle-plural-string word)))
    ;; If the transformed word is the same, don't bother changing it.
    (when (not (equal word new-word))
      (delete-region start end)
      (insert new-word)
      ;; At the end, the point will be at the end of the word. With
      ;; `save-excursion', it would go back to the beginning.
      )))

(provide 'toggle-plural)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; toggle-plural.el ends here
