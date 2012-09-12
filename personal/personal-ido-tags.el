;; Find file in tags file using ido
;; Stolen straight from the EmacsWiki <http://emacswiki.org/emacs/InteractivelyDoThings#toc11>

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(define-key global-map (kbd "C-x p") 'ido-find-file-in-tag-files)
