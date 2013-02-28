(require 'inflections)

;; Most correct.
(defun toggle-pluralize-string (str)
  ;; This is still not technically correct because it's impossible to
  ;; tell, given the layout of the inflections package, whether a word
  ;; is a singular or plural form. It is really just best-effort.
  (when (stringp str)
    ;; None of the pluralizations should change the first
    ;; character of the string, so just save it to preserve
    ;; the case of the first letter. It's not perfect but
    ;; it's better than downcasing it every time.
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

;; Not technically correct.
(defun toggle-pluralize-string (str)
  (car (delete-dups
        (list
         (singularize-string str)
         (pluralize-string str)))))

;; Not technically correct.
(defun toggle-pluralize-string (word)
  (or
   ;; It's important that `singularize-string' come before
   ;; `pluralize-string' here. That's because some words can be
   ;; pluralized nonsensically, e.g. "man" -> "men", "men" ->
   ;; "mens". In this case, we prefer to singularize the word rather
   ;; than pluralize it.
   (dolist (func '(singularize-string pluralize-string))
     (let ((modified-word (funcall func word)))
          (when (not (equal word modified-word))
            (return modified-word))))
   word))

(defun toggle-plural-at-point ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds))
         (word (buffer-substring start end))
         (new-word (toggle-pluralize-string word)))
    ;; If it's the same, don't bother.
    (when (not (equal word new-word))
      (delete-region start end)
      (insert new-word)
      ;; At the end, the point will be at the end of the word. With
      ;; `save-excursion', it would go back to the beginning.
      )))
