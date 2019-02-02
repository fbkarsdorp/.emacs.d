
(defun count-items (items)
  (let (counts)
    (dolist (elt items counts)
      (let ((found (assoc elt counts)))
        (if found
            (setcdr found (1+ (cdr found)))
          (push (cons elt 1) counts))))
    counts))

(defun org-get-category-archive (&optional pos)
  (let ((org-marker (get-text-property (or pos (point)) 'org-marker)))
    (or (org-entry-get org-marker "ARCHIVE_CATEGORY" t)
        (org-get-category))))


(defun get-done-tasks (days)
  (org-map-entries 'org-get-category-archive
                   (format "TODO=\"DONE\"+CLOSED>=\"<-%sd>\"" days)
                   'agenda-with-archives))

;;;###autoload
(defun org-done-count-per-category ()
  (interactive)
  (let* ((ndays (read-string "Number of days: "))
         (fd (count-items (get-done-tasks ndays)))
         (buffer (generate-new-buffer "*org-statistics*"))
         (total 0)
         (start-day (org-read-date nil nil (format "-%s" ndays)))
         (end-day (org-read-date nil nil "today" nil)))
    (switch-to-buffer buffer)
    (org-mode)
    (insert-before-markers (format "* Report of DONE tasks\n\n"))
    (insert-before-markers (format "  #+CAPTION: Task report from [%s] until [%s].\n" start-day end-day))
    (insert-before-markers "  | Task | completed |\n")
    (insert-before-markers "  |-\n")
    (dolist (entry fd)
      (insert-before-markers "  |" (car entry) "|" (format "%d" (cdr entry)) "|\n")
      (incf total (cdr entry)))
    (insert-before-markers   "|-\n|Total:|" (format "%d" total) "|")
    (org-table-align)
    (view-mode)
    (goto-char 0)
    buffer))

(provide 'org-done-statistics)
