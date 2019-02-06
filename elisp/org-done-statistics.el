(require 'org)
(require 'tabulated-list)

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

(defun format-category (entry)
  (format "[[elisp:(org-projectile-open-project%%20\"%s\")][%s]]" (car entry) (car entry)))

(defun get-done-tasks (days)
  (org-map-entries 'org-get-category-archive
                   (format "TODO=\"DONE\"+CLOSED>=\"<-%sd>\"" days)
                   'agenda-with-archives))

(defun statistics-table (fd start end)
  (insert-before-markers (format "* Report of DONE tasks\n\n"))
  (insert-before-markers (format "  #+CAPTION: Task report from [%s] until [%s].\n" start end))
  (insert-before-markers "  | Task | completed |\n")
  (insert-before-markers "  |-\n")
  (dolist (entry fd)
    (insert-before-markers "  |" (format-category entry)
                           "|" (format "%d" (cdr entry)) "|\n")
    (incf total (cdr entry)))
  (insert-before-markers   "|-\n|Total:|" (format "%d" total) "|"))

;;;###autoload
(defun org-done-count-per-category ()
  (interactive)
  (let* ((ndays (read-string "Number of days: "))
         (fd (count-items (get-done-tasks ndays)))
         (total 0)
         (start-day (org-read-date nil nil (format "-%s" ndays)))
         (end-day (org-read-date nil nil "today" nil)))
    (with-current-buffer (get-buffer-create "*org-statistics*")
      (erase-buffer)
      (org-mode)
      (statistics-table fd start-day end-day)
      (org-table-align)
      (view-mode)
      (goto-char 0)
      (pop-to-buffer (current-buffer)))))

(defun list-done-items ()
  (let ((category (org-get-category-archive)))
    (list
     category
     (vector
      category
      (org-entry-get nil "ITEM")
      (format-time-string "%Y-%m-%d %H:%M"
                          (org-read-date nil t (org-entry-get nil "CLOSED")))))))

(define-derived-mode org-done-statistics-mode tabulated-list-mode
  (setq tabulated-list-format
        ;; The sum of column width is 80 caracters:
        #[("Project" 20 t)
          ("Heading" 50 t)
          ("Closed" 10 t)])
  (tabulated-list-init-header))

;;;###autoload
(defun org-done-statistics-table ()
  (interactive)
  (let* ((n-days (read-string "Number of days: "))
         (done-tasks (org-map-entries #'list-done-items
                                      (format "TODO=\"DONE\"+CLOSED>=\"<-%sd>\"" n-days)
                                      'agenda-with-archives)))
    (with-current-buffer (get-buffer-create "*use-package done statistics*")
      (setq tabulated-list-entries done-tasks)
      (org-done-statistics-mode)
      (tabulated-list-print)
      (display-buffer (current-buffer)))))

(provide 'org-done-statistics)
