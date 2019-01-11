(require 'url)
(eval-when-compile
  (progn
    (require 'compile)
    (require 'simple)))

(defgroup ludwig-guru nil
  "Ludwig guru search utilities"
  :group 'convenience)

(defvar ludwig-base-url "https://ludwig.guru/s/")

(defvar luwdig-mode-submap)
(define-prefix-command 'ludwig-mode-submap)
(define-key ludwig-mode-submap [return] #'ludwig-search)
(define-key ludwig-mode-submap " " #'ludwig-region)
(define-key ludwig-mode-submap "t" #'ludwig)
(define-key ludwig-mode-submap "w" #'ludwig-word)
(define-key ludwig-mode-submap "l" #'ludwig-line)

(defun ludwig-parse-and-search-string (text)
  (browse-url (concat ludwig-base-url text)))

(defun ludwig-search (&optional text)
  (interactive)
  (if (stringp text)
      (ludwig-parse-and-search-string text)
    (let ((text (read-string "Search string: ")))
      (ludwig-search text))))

(defun ludwig-line ()
  (interactive)
    (ludwig-search (buffer-substring (line-beginning-position) (line-end-position))))

(defun ludwig-word ()
  (interactive)
  (ludwig-search (thing-at-point 'word)))

(defun ludwig-region ()
  (interactive)
  (ludwig-search (buffer-substring-no-properties (region-beginning) (region-end))))

(defun ludwig ()
  (interactive)
  (cond
   ((region-active-p) (ludwig-region))
   ((thing-at-point 'word) (ludwig-word))
   (t (ludwig-line))))

(defcustom ludwig-keybind (kbd "C-c /")
  "Keybindings for Ludwig search."
  :type 'string
  :group 'ludwig-guru)

(defcustom ludwig-modeline-indicator " Ludwig"
  "String to display in the modeline when command `ludwig-mode' is activated."
  :type 'string
  :group 'ludwig-guru)

(define-minor-mode ludwig-mode nil nil ludwig-modeline-indicator
  `((,ludwig-keybind . ,ludwig-mode-submap))
  :global t
  :group 'ludwig-guru)

(provide 'ludwig-guru)
