;;; -*- lexical-binding: t -*-
;;; init.el --- This is where all emacs start.

(setq gc-cons-threshold 100000000) 
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 10 1024 1024))))

(setq load-prefer-newer t)
(package-initialize)
(setq package-archives (append
			package-archives
			'(("melpa" . "https://melpa.org/packages/")
                          ("elpy" . "http://jorgenschaefer.github.io/packages/"))))

(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . 'nil)
                            (font . "-*-Fira Code-light-normal-normal-*-12-*-*-*-m-0-iso10646-1")
                            (height . 45) (width . 150)))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(customize-set-variable 'tramp-default-user "folgert")
(setenv "LANG" "en_US.UTF-8")

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(require 'diminish)
(require 'bind-key)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(fringe-mode 16) ;; I like a little more spacing
;; (setq ns-use-proxy-icon nil)
(setq frame-title-format "")
(show-paren-mode t)
(blink-cursor-mode -1)
(save-place-mode 1)
(global-hl-line-mode 1)
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
;; warn only for files larger than 100mb
(setq large-file-warning-threshold 100000000)

;; no tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq-default fill-column 90)
(prefer-coding-system 'utf-8)

;; Dired configurations
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
(setq dired-listing-switches "-Ahl  --group-directories-first")
(setq dired-recursive-deletes 'always)
(setq insert-directory-program "gls" dired-use-ls-dired t)

;; use async where possible
(use-package async
  :config (setq async-bytecomp-package-mode 1))

;; Stolen from somewhere. There should be a more robust way of doing this.
(defun comment-current-line-dwim ()
  "Comment or uncomment the current line."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (push-mark (beginning-of-line) t t)
      (end-of-line)
      (comment-dwim nil))))

(global-set-key (kbd "M-/") 'comment-current-line-dwim)
(global-set-key (kbd "M-+")  'mode-line-other-buffer)
(global-set-key (kbd "M-`") 'other-frame)

;; Remap meta to CMD on Mac
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Kill current buffer immediately
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer)

(global-auto-revert-mode t)
(add-hook 'text-mode-hook #'auto-fill-mode)

(setq ring-bell-function 'ignore)
(setq ns-use-native-fullscreen t)
(setq tramp-default-method "ssh")

;;*** Backups
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq version-control t)
(setq create-lockfiles nil)

;; scrolling
(setq scroll-preserve-screen-position t
      scroll-margin 0
      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101)

(defun new-scratch-pad ()
  "Create a new org-mode buffer for random stuff."
  (interactive)
  (let ((buffer (generate-new-buffer "*org-scratch*")))
    (switch-to-buffer buffer)
    (setq buffer-offer-save t)
    (org-mode)
    buffer))

(global-set-key (kbd "C-c s") 'new-scratch-pad)

(use-package move-text
  :config (move-text-default-bindings))

(use-package paradox
  :defer t
  :config
  (setq paradox-execute-asynchronously t)
  (paradox-enable))

(use-package tex
  :defer t
  :ensure auctex
  :init
  (progn
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-PDF-mode 1
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil)
    (add-hook 'TeX-mode-hook #'turn-on-reftex))
  :config
  (bind-key "C-c h l" 'hydra-langtool/body TeX-mode-map))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-medium t))

;; (use-package nord-theme
;;   ;; :straight
;;   ;; (nord-theme :type git :host github
;;   ;;             :repo "arcticicestudio/nord-emacs" :branch "develop")
;;   :defines
;;   (nord-comment-brightness
;;    nord-region-highlight)
;;   :init
;;   (setq nord-comment-brightness 15
;;         nord-region-highlight "frost")
;;   :config
;; (load-theme 'nord t))

(use-package minions
  :config (minions-mode 1))

;; fancy colors
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package company
  :config
  (global-company-mode)
  (setq company-global-modes '(not text-mode term-mode org-mode markdown-mode gfm-mode LaTeX-mode))
  (setq company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-idle-delay 0
        company-require-match nil
        company-minimum-prefix-length 2)
  ;; ;; Employ completion selection statistics for completion suggestions
  (use-package company-statistics
    :config (company-statistics-mode))
  ;; Bind next and previous selection to more intuitive keys
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (add-to-list 'company-frontends 'company-tng-frontend)
  :bind (("TAB" . 'company-indent-or-complete-common)))

(use-package elpy
  ;; :commands elpy-enable
  ;; :init (with-eval-after-load 'python (elpy-enable))
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :config
  (eldoc-add-command-completions "company-")
  (eldoc-add-command-completions "python-indent-dedent-line-backspace")
  ;; only use bare minimum of modules. No need for all fancy stuff
  (setq elpy-modules '(elpy-module-company elpy-module-eldoc))
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  :bind (("M-]" . 'elpy-nav-indent-shift-right)
         ("M-[" . 'elpy-nav-indent-shift-left)))

(use-package ess)

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (setq visual-line-column 90)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t))

(use-package recentf
  :config
  (setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                          "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
                          ".*png$" ".*cache$"))
  (setq recentf-max-saved-items 1000))

(use-package ivy
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-re-builders-alist
        '((ivy-bibtex . ivy--regex-ignore-order)
          (t . ivy--regex-plus)))
  :bind (("C-s" . 'swiper-isearch);;'counsel-grep-or-swiper)
         ("C-r" . 'swiper)
         ("C-c C-r" . 'ivy-resume)))

(use-package ivy-hydra)

(use-package smex)

(defun counsel-locate-cmd-mdfind (input)
  "Return a shell command based on INPUT."
  (format "mdfind -interpret kind:text %s" input))

(use-package counsel
  :init (counsel-mode t)
  :bind (("C-x C-r" . counsel-recentf)
         ("C-c r" . 'counsel-rg)
         ("C-c k" . 'counsel-ag)
         ("C-c g" . 'counsel-git)
         ("C-c i" . 'counsel-imenu)
         ("C-c l" . 'counsel-locate)
         ("C-c c" . 'counsel-org-capture))
  :config
  (setq counsel-git-cmd "rg --files")
  (setq counsel-grep-base-command "grep -niE %s %s")
  (setq counsel-grep-base-command
        ;; "ag --nocolor --nogroup %s %s")
        "rg -S -M 120 --no-heading --line-number --color never %s %s")
  (setq counsel-rg-base-command
      "rg -S -M 120 --no-heading --line-number --color never %s .")
  (setq counsel-find-file-occur-cmd
        "gls -a | grep -i -E '%s' | gxargs -d '\\n' gls -d --group-directories-first")
  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind))

(use-package wgrep :defer t)

(use-package deadgrep
  :bind*
  (("C-c r" . deadgrep)
   ("C-c f" . grep-org-files))
  :config
  (defun grep-org-files (words)
    (interactive "sSearch org files: ")
    (let ((default-directory org-directory)
          (deadgrep--file-type '(glob . "*.org"))
          (deadgrep--context '(1 . 1))
          (deadgrep--search-type 'regexp))
      (deadgrep words))))

(defun ivy-bibtex-format-pandoc-citation (keys)
  (concat "[" (mapconcat (lambda (key) (concat "@" key)) keys "; ") "]"))

(use-package ivy-bibtex
  :bind*
  ("C-c C-r" . ivy-bibtex)
  :config
  (setq bibtex-completion-bibliography "~/org/bibliography.bib")
  (setq bibtex-completion-notes-path "~/org/reading-notes.org")
  (setq ivy-bibtex-default-action #'ivy-bibtex-insert-citation)
  (setq bibtex-completion-display-formats '((t . "${author:36} ${title:*} ${year:4} ${=type=:7}")))
  (setf (alist-get 'org-mode bibtex-completion-format-citation-functions)
        'bibtex-completion-format-citation-pandoc-citeproc))

(defun counsel-bibtex-entry ()
  (interactive)
  (let ((entries '(("article" . bibtex-Article)
                   ("book" . bibtex-Book)
                   ("booklet" . bibtex-Booklet)
                   ("in proceedings" . bibtex-InProceedings)
                   ("in collection" . bibtex-InCollection)
                   ("misc" . bibtex-Misc)
                   ("manual" . bibtex-Manual)
                   ("online" . bibtex-Online)
                   ("phd thesis" . bibtex-PhdThesis)
                   ("master's thesis" . bibtex-MastersThesis)
                   ("collection" . bibtex-Collection)
                   ("tech report" . bibtex-TechReport)
                   ("unpubished" . bibtex-Unpublished))))
    (ivy-read "BibTeX entries: " entries
              :require-match t
              :action (lambda (f) (funcall (cdr f)))
              :caller 'counsel-bibtex-entry)))

(use-package bibtex
  :mode (("\\.bib\\'" . bibtex-mode))
  :bind (("C-c C-e <SPC>" . 'counsel-bibtex-entry)))

(use-package projectile
  :diminish
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode)
  ;; I want projectile to open dired upon selecting a project. 
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((move counsel-projectile-switch-project-action-dired 1)
     (setkey counsel-projectile-switch-project-action-dired "D"))))
                            
(use-package json-mode
  :mode "\\.json\\'")

(use-package avy
  :bind (("C-;" . 'avy-goto-char)
         ("C-\\" . 'avy-goto-line)
         ("C-'" . 'avy-goto-word-1)))

(use-package ace-window
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   :weight 'bold
   :height 2.0)  
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("M-o" . 'ace-window)))

(use-package which-key
  :diminish
  :init
  (progn
    (setq which-key-idle-delay 1.0)
    (which-key-mode)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)
         ("C-c m" . vr/mc-mark)))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c M-g" . magit-file-popup)))

(use-package forge
  :after magit)

(use-package git-gutter-fringe
  :config
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX....")
  (set-face-attribute
   'git-gutter:added nil :background nil)
  (set-face-attribute
   'git-gutter:deleted nil :background nil)
  (set-face-attribute
   'git-gutter:modified nil :background nil)
  ;; (add-hook 'text-mode-hook #'git-gutter-mode)
  (add-hook 'prog-mode-hook #'git-gutter-mode)
  (add-hook 'bibtex-mode-hook #'git-gutter-mode)
  (add-hook 'conf-mode #'git-gutter-mode))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("HIGH" "MID" "LOW")))

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
;;(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

;; remove ^ for refile searches
(mapc (lambda (item)
        (setf (alist-get item ivy-initial-inputs-alist) ""))
      '(org-refile org-agenda-refile org-capture-refile))

(setq org-todo-keywords '((sequence "TODO" "NEXT" "WAITING" "|" "DONE" "CANCELLED")))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
(setq org-agenda-show-future-repeats nil)
(setq org-src-fontify-natively t)
(setq org-hide-leading-stars t)
(setq org-agenda-search-view-always-boolean t)
(setq org-use-speed-commands t)
(setq org-latex-create-formula-image-program 'dvisvgm)

(use-package ob-ipython
  :after org)

(setq org-babel-load-languages '((R . t) (ipython . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-enforce-todo-dependencies t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(use-package org-cliplink
  :after org)

;; (require 'org-tempo)

(defun org-deadline-ahead (&optional pos)
  (let* ((days (time-to-number-of-days (org-deadline-ahead-time pos)))
         (future (if (< 0 days) "+" ""))
         (days (fceiling days)))
    (cond
     ((< days 30) (format "%4s" (format "%s%dd" future days)))
     ((< days 358) (format "%4s" (format "%s%dm" future (/ days 30))))
     (t            " "))))

(defun org-deadline-ahead-time (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "DEADLINE" t)))
    (when stamp
      (time-subtract (org-time-string-to-time
                      (org-entry-get (or pos (point)) "DEADLINE" t))
                     (current-time)))))

;; Custom sort function, after deadline-up broke with a recent update (9.2.1)
(defun org-compare-deadline-date (a b)
  (let ((time-a (org-deadline-ahead-time (get-text-property 0 'org-hd-marker a)))
        (time-b (org-deadline-ahead-time (get-text-property 0 'org-hd-marker b))))
    (if (time-less-p time-a time-b)
        -1
      (if (equal time-a time-b)
          0
        1))))

(defun org-agenda-add-overlays (&optional line)
  (let ((inhibit-read-only t) l c
        (buffer-invisibility-spec '(org-link)))
    (save-excursion
      (goto-char (if line (point-at-bol) (point-min)))
      (while (not (eobp))
        (let ((org-marker (get-text-property (point) 'org-marker)))
          (when (and org-marker
                     (null (overlays-at (point)))
                     (not (org-entry-get org-marker "CLOSED" t))
                     (org-entry-get org-marker "DEADLINE" t))
            (goto-char (line-end-position))
            (let* ((ol (make-overlay (line-beginning-position)
                                     (line-end-position)))
                   (days (time-to-number-of-days (org-deadline-ahead-time org-marker)))
                   (proplist (cond
                              ((< days 1) '(face org-warning))
                              ((< days 5) '(face org-upcoming-deadline))
                              ((< days 30) '(face org-scheduled)))))
              (when proplist
                (overlay-put ol (car proplist) (cadr proplist))))))
        (forward-line)))))

(add-hook 'org-agenda-finalize-hook 'org-agenda-add-overlays)

(defun clip-link-http-or-file ()
  (interactive)
  (if (yes-or-no-p "Web link? ")
      (org-cliplink-capture)
    (let ((link (read-file-name "Enter file path: "))
          (description (read-string "Description: ")))
      (org-make-link-string link description))))

(defun toggle-org-speed-keys ()
  (interactive)
  (if org-use-speed-commands
      (progn
        (setq org-use-speed-commands nil)
        (message "Org speed commands turned off."))
    (progn
      (setq org-use-speed-commands t)
      (message "Org speed commands turned on."))))

(use-package org-done-statistics
  :load-path "~/.emacs.d/elisp"
  :bind (("C-c d" . org-done-count-per-category)
         ("C-c t" . org-done-statistics-table)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %^{Todo} %^G \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n  %?"
         :empty-lines 1)
        ("i" "Idee" entry (file+olp+datetree "~/org/ideas.org")
         "* Idee: %^{Title} %^g\n\n  %?"
         :empty-lines 1)
        ("Q" "Quote" entry (file+headline "~/org/quotes.org" "Quotes")
         "* %^{Title} %^G \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n  %?"
         :empty-lines 1)
        ("r" "Read" entry (file+headline "~/org/reading-list.org" "Reading List")
         "* TODO %(clip-link-http-or-file)\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n  %?"
         :empty-lines 1)
        ("n" "Note" entry (file+headline "~/org/todo.org" "Notes")
         "* %^{Title} %^G \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n  %?"
         :empty-lines 1)
        ("l" "Link" entry (file+headline "~/org/bookmarks.org" "Bookmarks")
         "* %(org-cliplink-capture) %^g \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n  %?"
         :empty-lines 1)))

(defun format-closed-query ()
  (format "+TODO=\"DONE\"+CLOSED>=\"<-%sd>\"" (read-string "Number of days: ")))

(setq org-agenda-block-separator ?\u2015
      org-agenda-restore-windows-after-quit t
      org-agenda-window-setup 'only-window
      org-agenda-dim-blocked-tasks t
      ;; Was needed with a recent update of org. Reverted back to 9.2 for now.
      org-agenda-cmp-user-defined (quote org-compare-deadline-date)
      ;; TODO: make this a PR for gruvbox?
      org-todo-keyword-faces
      '(("WAITING" . (font-lock-function-name-face :weight bold))
        ("CANCELLED" . (font-lock-constant-face :weight bold))
        ("NEXT" . (font-lock-string-face :weight bold)))
      org-priority-faces
      '((?A . (font-lock-constant-face :weight bold))
        (?B . (font-lock-function-name-face :weight bold))
        (?C . (font-lock-variable-name-face :weight bold)))
      org-agenda-custom-commands
      '(("d" "Dagelijkse Takenlijst"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")
                 (org-agenda-sorting-strategy '(priority-down user-defined-up category-keep))))
          (todo "NEXT"
                ((org-agenda-overriding-header "Reading List")
                 (org-agenda-files '("~/org/reading-list.org"))))
          (todo "WAITING|CANCELLED"
                ((org-agenda-overriding-header "Pending Tasks")
                 (org-agenda-sorting-strategy '(priority-down user-defined-up category-keep))))
          (todo "TODO" ((org-agenda-overriding-header "Backlog")
                        (org-agenda-sorting-strategy '(priority-down user-defined-up category-keep))))
          (todo "DONE|CANCELLED" ((org-agenda-overriding-header "Tasks to Archive")))))

        ("w" "Weekly review"
         ((tags (format-closed-query)
                ((org-agenda-overriding-header "Overview DONE tasks")
                 (org-agenda-archives-mode t)))))))


(setq org-agenda-files
      (mapcar (lambda (f) (concat org-directory f))
              '("/todo.org" "/oc.org" "/projects.org")))

(defvar reading-list-file "~/org/reading-list.org")

(defun update-reading-list-todo ()
  (with-current-buffer (find-file-noselect reading-list-file)
    (goto-char (org-find-exact-headline-in-buffer "Reading List"))
    (let ((currently-reading (apply '+ (org-map-entries (lambda () 1) "/NEXT"))))
      (while (and (< currently-reading 2) (or (and (< (org-current-level) 2)
                                                   (org-goto-first-child))
                                              (org-get-next-sibling)))
        (when (string= (org-get-todo-state) "TODO")
          (org-todo "NEXT")
          (setq currently-reading (+ currently-reading 1)))))
    (save-buffer)))

(add-hook 'org-agenda-mode-hook 'update-reading-list-todo)

(defun counsel-find-org-file ()
  (interactive)
  (let ((file-list (seq-filter (lambda (f) (string-match-p "\\.org\\'" f))
                               (directory-files "~/org"))))
    (ivy-read "org files: " file-list
              :require-match nil
              :action (lambda (f) (find-file (concat org-directory "/" f)))
              :caller 'counsel-find-org-file)))

(define-key global-map "\C-co" 'counsel-find-org-file)

(use-package org-journal
  :defer t
  :init
  (setq org-journal-dir "~/journal/")
  (setq org-journal-date-format "#+TITLE: Journal Entry- %e %b %Y (%A)"))

(defun get-journal-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-name (format-time-string "%Y%m%d")))
    (expand-file-name (concat org-journal-dir daily-name))))

(defun journal-file-today ()
  "Create and load a journal file based on today's date."
  (interactive)
  (find-file (get-journal-file-today)))

(global-set-key (kbd "C-c j") 'journal-file-today)

;; org-projectile for project bases org projects.
(defvar project-todos "projects.org")
(defvar org-capture-before-config nil)

(defadvice org-capture (before save-config activate)
  "Save the window configuration before `org-capture'."
  (setq org-capture-before-config (current-window-configuration)))

(defun org-projectile-cleanup ()
  "Clean up the frame created while capturing via `org-projectile'."
  (when (get-buffer project-todos)
    (kill-buffer project-todos))
  (when org-capture-before-config
    (set-window-configuration org-capture-before-config)))

(add-hook 'org-capture-mode-hook 'delete-other-windows)
(add-hook 'org-capture-after-finalize-hook 'org-projectile-cleanup)

(use-package org-projectile
  :after org
  :demand t
  :config  
  (progn
    (setq org-projectile-projects-file (concat org-directory "/" project-todos))
    (setq org-confirm-elisp-link-function nil)
    (setq org-projectile-capture-template
          "* TODO %^{Todo} \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n  %?")
    (push (org-projectile-project-todo-entry :empty-lines 1) org-capture-templates))
  :bind (("C-c n" . org-projectile-project-todo-completing-read)))

;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                            :post (setq which-key-inhibit nil)
                            :hint none)
"
^Navigate^                 ^Headline^         ^Date^             ^Clock^      ^Filter^                 ^View^      ^Other^
^--------^---------------- ^--------^-------- ^----^------------ ^-----^----- ^------^---------------- ^----^----- ^-----^-----------
_n_:   next entry          _t_: toggle status _ds_: schedule     _I_:  in     _ft_: by tag             _vd_: day   _g_ rebuild agenda
_p_:   previous entry      _r_: refile        _dd_: set deadline _O_:  out    _fr_: refile by tag      _vw_: week  _x_ exit and kill
_SPC_: in other window     _a_: archive       _dt_: timestamp    _cq_: cancel _fc_: by category        _vm_: month _s_ save buffers
_TAB_: & go to location    _:_: set tags      _+_:  do later     _j_:  jump   _fh_: by headline        _vy_: year  ^^
_RET_: & del other windows _,_: set priority  _-_:  do earlier   ^^           _fx_: by regexp          _vr_: reset ^^
^^                         ^^                 ^^                 ^^           _fd_: delete all filters ^^          ^^
^^                         ^^                 ^^                 ^^           ^^                       ^^          ^^
_<f12>_ quit hydra
"
  ;; Navigate
  ("n" org-agenda-next-line)
  ("p" org-agenda-previous-line)
  ("SPC" org-agenda-show-and-scroll-up)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("RET" org-agenda-switch-to :exit t)
  ;; Headline
  ("t" org-agenda-todo)
  ("r" org-agenda-refile)
  ("a" org-agenda-archive-default)
  (":" org-agenda-set-tags)
  ("," org-agenda-priority)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("j" org-agenda-clock-goto :exit t)
  ("I" org-agenda-clock-in :exit t)
  ("O" org-agenda-clock-out)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vr" org-agenda-reset-view)
  ;; Other
  ("<f12>" nil :exit t)
  ("g" org-agenda-redo)
  ("s" org-save-all-org-buffers)
  ("x" org-agenda-exit :exit t))

(add-hook 'org-agenda-mode-hook (lambda () (local-set-key (kbd "<f12>") 'hydra-org-agenda/body)))

(defhydra hydra-dired (:hint nil)
  "
^Operate^       ^View^                       ^Navigate^         ^Mark^
^-------^------ ^----^---------------------- ^--------^-------- ^----^-----------
_D_: delete     _RET_ open file              _P_: Up dir        _m_: mark
_C_: copy       _o_ in other window          _n_: next line     _t_: toggle marks
_R_: rename     _co_ display in other window _p_: previous line _u_: unmark
_Z_: compress   _v_ view this file           _(_: show details  _U_: unmark all
_+_: create dir _=_ view diff                ^^                 ^^
^^              ^^                           ^^                 ^^
_<f12>_ quit hydra
"
  ;; Operate
  ("D" dired-do-delete)
  ("C" dired-do-copy)
  ("R" dired-do-rename)
  ("Z" dired-do-compress)
  ("+" dired-create-directory)
  ;; View
  ("RET" dired-find-file)
  ("o" dired-find-file-other-window :exit t)
  ("co" dired-display-file)
  ("v" dired-view-file)
  ("=" diredp-ediff)
  ;; Navigate
  ("P" dired-up-directory)
  ("n" dired-next-line)
  ("p" dired-previous-line)
  ("(" dired-hide-details-mode)
  ;; Mark
  ("m" dired-mark)
  ("t" dired-toggle-marks)
  ("u" dired-unmark)
  ("U" dired-unmark-all-marks)
  ;; quit hydra
  ("<f12>" nil :color blue))

(define-key dired-mode-map (kbd "<f12>") 'hydra-dired/body)

(use-package iflipb
  :bind*
  (("M-}" . iflipb-next-buffer)
   ("M-{" . iflipb-previous-buffer)))

(use-package smerge-mode
  :config
  (defhydra smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (smerge-hydra/body)))))

(use-package ox-pandoc
  :defer t)

(use-package visual-regexp
  :bind (("C-%" . vr/query-replace)))

(use-package visual-regexp-steroids
  :after visual-regexp)

(require 'ludwig-guru)
(ludwig-mode 1)

(use-package langtool
  :defer 1
  :config
  ;; install with `brew install languagetool`
  (setq langtool-language-tool-server-jar "/usr/local/Cellar/languagetool/4.4/libexec/languagetool-server.jar")
  (setq langtool-default-language "en-US")

  (defhydra hydra-langtool (:color pink
                            :hint nil)
"
_i_nit  /  _c_orrect  /  _n_ext error  /  _p_rev error  /  _d_one
"
      ("n"  langtool-goto-next-error)
      ("p"  langtool-goto-previous-error)
      ("i"  langtool-check)
      ("c"  langtool-correct-buffer)
      ("d"  langtool-check-done :color blue :exit t)))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d"))

(when (file-exists-p custom-file)
  (load custom-file))
