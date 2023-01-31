;;; -*- lexical-binding: t -*-
;;; init.el --- This is where all emacs start.

(setq gc-cons-threshold 100000000) 
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 10 1024 1024))))

(setq inhibit-startup-message t)
(defvar scratch-messages
  '("# There's a crack in everything. That's how the light gets in."
    "# Things don't become traditions because they're good, BoJack.\n# They become good because they're traditions. -- Todd Chavez"
    ))
(setq initial-scratch-message (nth (random (length scratch-messages)) scratch-messages))


(prefer-coding-system 'utf-8)

(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(setq load-prefer-newer t)
(package-initialize)
(setq package-archives (append
			package-archives
			'(("melpa" . "https://melpa.org/packages/")
                          ("elpa" . "https://elpa.nongnu.org/nongnu/"))))

(eval-when-compile
  (require 'use-package)
  (require 'quelpa-use-package))
(setq use-package-always-ensure t)
(require 'bind-key)

(use-package async
  :config (setq async-bytecomp-package-mode 1))

(setq default-frame-alist '((ns-transparent-titlebar . t)
                            ;; (undecorated-round . t)
                            (height . 50) (width . 190)
                            (top . 0.5) (left . 0.5)))
                            ;; (internal-border-width . 18)))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(fringe-mode 16)

(blink-cursor-mode nil)
(setq-default cursor-type 'hbar)

(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification))
;; (moody-replace-vc-mode))

(use-package minions
  :config (minions-mode 1))

(use-package diminish)

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

(use-package modus-themes
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-completions '((matches . (extrabold intense background))
                                   (selection . (semibold accented intense)))
        modus-themes-org-blocks 'gray-background
        modus-themes-variable-pitch-ui t)
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fringe unspecified)
          (underline-link border)
          (underline-link-visited border)
          (underline-link-symbolic border)
          (bg-region bg-ochre)
          (fg-region unspecified)
          (bg-tab-bar bg-main)
          (date-event fg-main)
          (bg-tab-current bg-inactive)
          (bg-tab-other bg-dim)))
  (setq modus-themes-headings
      '((t . (1.1))))
  :config
  (load-theme 'modus-operandi-tinted :no-confirm))

(setq tab-bar-mode t)
(setq tab-bar-show t)
(setq tab-bar-close-button nil)
(setq tab-bar-new-button nil)
(setq tab-bar-tab-name-format-function #'fk--tab-bar-tab-name-format)
(setq tab-bar-separator "   ")
;; dirty hack to get more space underneath the tab bar.
;; disadvantage is that tab-bar-line cannot be used anymore.
(defvar tab-bar-extra-space nil)

(if tab-bar-extra-space
    (progn
      (setq-default tab-line-format " ")
      (global-tab-line-mode)))

(defun fk--tab-bar-tab-name-format (tab i)
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (concat
      (propertize " " 'display '(space :width (8)))
      (alist-get 'name tab)
      (propertize " " 'display '(space :width (8))))
     'face (funcall tab-bar-tab-face-function tab))))

(defun my-modus-themes-tab-bar-faces ()
  (modus-themes-with-colors
    (set-face-attribute 'tab-bar nil :weight 'light :inherit `(variable-pitch default) :box `(:line-width (8 . 5) :color nil :style flat-button))
    (set-face-attribute 'tab-bar-tab nil :weight 'light) ; :underline `(:color ,fg-dim :position 0))
    (set-face-attribute 'header-line nil :background bg-main)))

(my-modus-themes-tab-bar-faces)
(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-tab-bar-faces)

(setq frame-title-format nil)
;;'((:eval (format "%s" (cdr (assoc 'name (tab-bar--current-tab)))))))
(setq-default ns-use-proxy-icon nil)

(set-face-attribute 'default nil :family "Roboto Mono" :height 120)
;; (set-face-attribute 'default nil
;;                     :family "Roboto Mono" :weight 'light :height 140)
;; (set-face-attribute 'bold nil
;;                     :family "Roboto Mono" :weight 'regular)
;; (set-face-attribute 'italic nil
;;                     :family "Victor Mono" :weight 'semilight :slant 'italic)

(set-face-attribute 'variable-pitch nil :family "SF Pro" :height 1.0 :weight 'light)
(setq-default fill-column 90)

(use-package fontawesome)
(use-package all-the-icons)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init (vertico-mode))

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq enable-recursive-minibuffers t)

(use-package savehist
  :init (savehist-mode))

(use-package marginalia
  :init (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ;; C-x bindings (ctl-x-map)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g i" . consult-imenu)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any)))

(use-package embark
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init (setq prefix-help-command #'embark-prefix-help-command)

  :config (add-to-list 'display-buffer-alist
                       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                         nil
                         (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :diminish
  :init
  (progn
    (setq which-key-idle-delay 1.0)
    (which-key-mode)))

(setq-default indent-tabs-mode nil
              tab-always-indent 'complete
              tab-width 4)

(setq-default initial-major-mode 'text-mode
              default-major-mode 'text-mode)

(setq sentence-end-double-space nil)

(show-paren-mode t)

(pixel-scroll-precision-mode)

(add-hook 'text-mode-hook #'auto-fill-mode)

(global-auto-revert-mode t)

(setq large-file-warning-threshold 100000000)

(use-package avy
  :bind (("M-j" . 'avy-goto-char-timer)
         ("M-\\" . 'avy-goto-line)))

(use-package ace-window
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   :weight 'bold
   :height 2.0)  
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("M-o" . 'ace-window)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)
         ("C-c m" . vr/mc-mark)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package electric
  :ensure nil
  :config (electric-pair-mode 1))

(use-package move-text
  :config (move-text-default-bindings))

(use-package hl-line
  :ensure nil
  :custom-face (hl-line ((t (:extend t))))
  :hook (after-init . global-hl-line-mode))

(use-package olivetti
  :config (setq olivetti-style 'fancy))

(use-package csv-mode
  :defer t)

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)))

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-recursive-deletes 'always)

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/.emacs.d/"                 "Emacs")
     ("p" "~/projects"                  "Projects")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  (dirvish-mode-line-format
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  ;; Don't worry, Dirvish is still performant even you enable all these attributes
  (dirvish-attributes '(all-the-icons collapse subtree-state vc-state git-msg))
  :config
  (setq dired-dwim-target t)
  (setq dirvish-mode-line-height 24)
  (setq dirvish-header-line-height 24)
  (setq dirvish-use-header-line nil) ;; works better with tab bar
  (setq delete-by-moving-to-trash t)
  ;; Enable mouse drag-and-drop files to other applications
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  :bind
  ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   ;; Dirvish has all the keybindings in `dired-mode-map' already
   :map dirvish-mode-map
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq version-control t)
(setq create-lockfiles nil)

(use-package recentf
  :config
  (setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                          "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
                          ".*png$" ".*cache$"))
  (setq recentf-max-saved-items 500))

(save-place-mode 1)

(use-package tramp
  :ensure nil
  :defer t
  :config
  (setq tramp-default-user "folgertk"
        tramp-default-method "ssh")
  (use-package counsel-tramp
    :bind ("C-c t" . counsel-tramp))
  (put 'temporary-file-directory 'standard-value '("/tmp")))

(use-package projectile
  :diminish
  :config
  (setq projectile-switch-project-action #'projectile-dired)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :init (projectile-mode +1)
  :bind ((:map projectile-mode-map
               ("C-c p k" . projectile-kill-buffers-and-enclosing-tab)
               ("C-c p p" . switch-to-project-in-dedicated-tab))))

(defun switch-to-project-in-dedicated-tab ()
  "Open or switch to project in a dedicated tab."
  (interactive)
  (let* ((project-root (projectile-completing-read "Switch to project: " projectile-known-projects))
         (project-name (projectile-project-name project-root))
         (tab-bar-index (tab-bar--tab-index-by-name project-name)))
    (if tab-bar-index
        (tab-bar-select-tab (+ tab-bar-index 1))
      (progn
        (tab-bar-new-tab)
        (let ((projectile-switch-project-action 'projectile-find-file))
          (projectile-switch-project-by-name project-root))
        (dirvish-side)))))

(defun projectile-kill-buffers-and-enclosing-tab ()
  (interactive)
  (let* ((project-name (projectile-project-name))
         (tab-bar-index (tab-bar--tab-index-by-name project-name)))
    (when tab-bar-index
      (projectile-kill-buffers)
      (tab-bar-switch-to-recent-tab)
      (tab-bar-close-tab (+ tab-bar-index 1)))))

(defun projectile-name-tab-by-project-name-or-default ()
  (let ((project-name (projectile-project-name)))
    (if (string= "-" project-name)
        (tab-bar-tab-name-current)
      project-name)))

(setq tab-bar-tab-name-function #'projectile-name-tab-by-project-name-or-default)

(use-package magit
  :config
  (setq magit-git-executable "/usr/bin/git")
  :bind (("C-x g" . magit-status)
         ("C-c M-g" . magit-file-popup)))

(use-package gitignore-templates
  :defer t)

(use-package org :ensure org-contrib)

(defvar my-agenda-files '("inbox.org" "projects.org" "habits.org" "agenda.org" "leeslijst.org"))
(setq org-directory "~/org"
      org-agenda-files (mapcar
                        (lambda (f) (concat (file-name-as-directory org-directory) f))
                        my-agenda-files)
      org-default-notes-file (concat (file-name-as-directory org-directory) "notes.org"))

(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files :maxlevel . 2))
      org-refile-targets '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))

(setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "CANCELLED"))
      org-enforce-todo-dependencies t)

(setq org-log-done 'time  ; when marking a todo as done, at the time
      org-log-into-drawer t)  ; log into drawers right underneath the heading

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %^{Todo} %^G \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
         :empty-lines 1)
        ("m" "Meeting" entry (file+headline "~/org/agenda.org" "Toekomstig")
         "* %^{Description} :meeting:\n%^t"
         :empty-lines 1)
        ("r" "Read" entry (file+headline "~/org/leeslijst.org" "Articles")
         "* TODO %c \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
         :empty-lines 1)
        ("n" "Note" entry (file+headline "~/org/inbox.org" "Notes")
         "* %^{Title} %^G \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
         :empty-lines 1)))

(add-to-list 'load-path (expand-file-name "org-mac-link" "~/.emacs.d/gitrepos"))
(require 'org-mac-link)
(add-hook 'org-mode-hook (lambda ()
  (define-key org-mode-map (kbd "C-c g") 'org-mac-link-get-link)))
(org-add-link-type "message" 'org-mac-message-open)

(defun org-mac-message-open (message-id)
  "Visit the message with MESSAGE-ID.
   This will use the command `open' with the message URL."
  (browse-url (concat "message://%3c" (substring message-id 2) "%3e")))

(setq org-capture-templates
      (append org-capture-templates
              '(("e" "Mail" entry (file+headline "~/org/inbox.org" "Mail")
                 "* TODO  %(org-mac-link-mail-get-links \"s\") :mail: \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
                 :empty-lines 1))))

(setq org-capture-templates
      (append org-capture-templates
              '(("l" "Link" entry (file+headline "~/org/bookmarks.org" "Bookmarks")
                 "* %(org-cliplink-capture) %^g \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
                 :empty-lines 1))))

(use-package org-cliplink
  :defer t
  :after org)

(use-package org-super-agenda
  :after org
  :config
  (use-package origami
    :bind (:map org-super-agenda-header-map ("<tab>" . origami-toggle-node))
    :hook (org-agenda-mode . origami-mode)))

(add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)

(setq org-agenda-search-view-always-boolean t
      org-agenda-block-separator " "
      org-habit-show-habits-only-for-today nil
      org-agenda-restore-windows-after-quit t
      org-agenda-show-future-repeats nil
      org-tags-column 1
      org-agenda-tags-column 0
      org-use-property-inheritance t
      org-deadline-warning-days 2
      org-agenda-window-setup 'current-window 
      org-agenda-start-on-weekday 1 ;; nil
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-sticky t
      org-agenda-format-date "\n%A, %-e %B %Y"
      org-agenda-category-icon-alist nil
      org-agenda-current-time-string " now"
      org-agenda-show-current-time-in-grid t
      org-agenda-dim-blocked-tasks t)

;; this ugly hack is necessary, because org-agenda-current-time's face
;; requires two spaces prior to the time indication. There is also no way
;; to just org-agenda-current-time-string.
(add-hook 'org-agenda-finalize-hook
      (lambda () (highlight-phrase " now" 'org-date)))

(setq org-agenda-time-grid
      '((daily today require-timed)
        ()
        "......" "----------------"))

(defvar my/org-agenda-update-delay 60)
(defvar my/org-agenda-update-timer nil)

(defun my/org-agenda-update ()
  "Refresh daily agenda view"
  
  (when my/org-agenda-update-timer
    (cancel-timer my/org-agenda-update-timer))
  
  (let ((window (get-buffer-window "*Org Agenda(a)*" t)))
    (when window
      (with-selected-window window
        (let ((inhibit-message t))
          (org-agenda-redo)))))

  (setq my/org-agenda-update-timer
    (run-with-idle-timer
     (time-add (current-idle-time) my/org-agenda-update-delay)
     nil
     'my/org-agenda-update)))

(run-with-idle-timer my/org-agenda-update-delay t 'my/org-agenda-update)

(setq org-agenda-remove-tags t)

(setq org-agenda-prefix-format
      '((agenda . "%?-12t");"%?-12t")
        (todo .   "%i")
        (tags .   "%i")
        (search . "%i")))

(setq org-agenda-sorting-strategy
      '((agenda habit-down deadline-down scheduled-down todo-state-up time-up
                priority-down category-keep)
        (todo   priority-down category-keep)
        (tags   timestamp-up priority-down category-keep)
        (search category-keep)))

(require 'svg-lib)
(require 'svg-tag-mode)

(defun my/org-agenda-custom-date ()
  (interactive)
  (let* ((timestamp (org-entry-get nil "TIMESTAMP"))
         (timestamp (or timestamp (org-entry-get nil "DEADLINE"))))
    (if timestamp
        (let* ((delta (- (org-time-string-to-absolute (org-read-date nil nil timestamp))
                         (org-time-string-to-absolute (org-read-date nil nil ""))))
               (delta (/ (+ 1 delta) 30.0))
               (face (cond ;; ((< delta 0.25) 'nano-popout)
                           ;; ((< delta 0.50) 'nano-salient)
                           ((< delta 1.00) 'default)
                           (t 'default))))
          (concat
           (propertize " " 'face nil
                       'display (svg-lib-progress-pie
                                 delta nil
                                 :background (face-background face nil 'default)
                                 :foreground (face-foreground face)
                                 :margin 0 :stroke 2 :padding 1))
           " "
           (propertize
            (format-time-string "%d/%m" (org-time-string-to-time timestamp))
            'face 'default)))
      "     ")))

(setq org-agenda-custom-commands
      '(("a" "Agenda"
         (
          (agenda ""
                  ((org-agenda-span 'week)
                  (org-deadline-warning-days 0)
                  (org-super-agenda-groups
                    '((:name "" :time-grid t :scheduled t :deadline t :category "verjaardag")
                      (:discard (:anything t))))))))))

(add-to-list 'org-agenda-custom-commands
             '("x" "Tasks"
         ((tags "+meeting+TIMESTAMP>=\"<now>\""
                 ((org-agenda-span 90)
                  (org-agenda-max-tags 10)
                  (org-agenda-skip-entry-if 'deadline)
                  (org-agenda-prefix-format '((tags . "%(my/org-agenda-custom-date) ")))
                  (org-agenda-overriding-header " Upcoming meetings\n")))
         (tags "DEADLINE>=\"<today>\""
               ((org-agenda-span 90)
                (org-agenda-max-entries 10)
                (org-agenda-prefix-format '((tags . "%(my/org-agenda-custom-date) ")))
                (org-agenda-overriding-header " Upcoming deadlines\n"))))))
      
(add-to-list 'load-path (expand-file-name "org-agenda-property" "~/.emacs.d/gitrepos"))
(require 'org-agenda-property)
(setq org-agenda-property-list '("GROUP"))

(org-super-agenda--def-auto-group category "their org-category property"
  :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
              (org-get-category))
  :header-form (concat " " key))

(setq org-agenda-custom-commands (append org-agenda-custom-commands
        '(("p" "Project backlog"
          ((todo "TODO|NEXT|WAITING|HOLD"
                ((org-agenda-overriding-header " Inbox\n")
                 (org-agenda-prefix-format "  ")
                 (org-agenda-files '("~/org/inbox.org"))))
          (todo "TODO|NEXT|WAITING|HOLD"
                 ((org-agenda-overriding-header " Project TODOs")
                 (org-agenda-prefix-format "  ")
                  (org-agenda-files '("~/org/projects.org"))
                  (org-super-agenda-groups
                   '((:discard (:scheduled t :date t))
                     (:auto-category t)
                     (:discard (:anything t))))))
          (todo "TODO|NEXT"
                ((org-agenda-overriding-header " Reading List")
                 (org-agenda-prefix-format "  ")
                 (org-agenda-files '("~/org/leeslijst.org"))
                 (org-super-agenda-groups
                  '((:discard (:scheduled t))
                    (:name " Priority A reading" :priority "A")
                    (:name " Priority B reading" :priority "B")
                    (:name " Priority C reading" :priority "C")
                    (:discard (:anything t)))))))))))

(defun format-closed-query ()
  (format "+TODO=\"DONE\"+CLOSED>=\"<-%sd>\"" (read-string "Number of days: ")))

(setq org-agenda-custom-commands (append org-agenda-custom-commands
        '(("w" "Weekly review"
         ((tags (format-closed-query)
                ((org-agenda-overriding-header "Overview of DONE tasks")
                 (org-agenda-archives-mode t))))))))

;; Functions to keep calendar in sight when working on the agenda
(defun fk-window-displaying-agenda-p (window)
  (equal (with-current-buffer (window-buffer window) major-mode)
         'org-agenda-mode)) 

(defun fk-position-calendar-buffer (buffer alist)
  (let ((agenda-window (car (remove-if-not #'fk-window-displaying-agenda-p (window-list)))))
    (when agenda-window
      (if (not (get-buffer-window "*Calendar*"))
          (let ((desired-window (split-window agenda-window nil 'below)))
            (set-window-buffer desired-window buffer)
            desired-window)))))

(add-to-list 'display-buffer-alist (cons "\\*Calendar\\*" (cons #'fk-position-calendar-buffer nil)))
(use-package calfw)
(use-package calfw-org)

;; Interesting package. Will follow development
;; (add-to-list 'load-path "~/.emacs.d/gitrepos/calfw-blocks")
;; (require 'calfw-blocks)
;; (setq calfw-blocks-earliest-visible-time '(8 0))
;; (setq calfw-blocks-lines-per-hour 2)

;; (defun my-open-calendar-agenda ()
;;   (interactive)
;;   (cfw:open-calendar-buffer
;;    :contents-sources
;;    (list
;;     (cfw:org-create-source "medium purple"))
;;    :view 'block-week))

(add-to-list 'load-path (expand-file-name "org-agenda-dashboard" "~/.emacs.d/gitrepos"))
(require 'org-agenda-dashboard)

(defun side-by-side-agenda-view ()
  (progn
    (org-agenda-dashboard)
    (other-window 1)
    (org-agenda nil "a") ; weekly view
    (org-agenda nil "x") ; task view in side bar
    ))
    ;; (setq org-agenda-sticky nil)))

(defun show-my-agenda ()
  (interactive)
  (let ((tab-bar-index (tab-bar--tab-index-by-name "Agenda")))
    (if tab-bar-index
        (tab-bar-select-tab (+ tab-bar-index 1))
      (progn
        (tab-bar-new-tab)
        (tab-bar-rename-tab "Agenda")
        (side-by-side-agenda-view)
        (message "Agenda loaded")))))

(use-package org-roam
  :init 
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/kaartenbak"))
  :bind (("C-c o l" . org-roam-buffer-toggle)
         ("C-c o f" . org-roam-node-find)
         ("C-c o g" . org-roam-graph)
         ("C-c o i" . org-roam-node-insert)
         ("C-c o c" . org-roam-capture)
         ;; Dailies
         ("C-c o j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  (setq org-roam-db-gc-threshold (* 10 1024 1024))
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (require 'org-roam-export) ;; check whether this helps exporting
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n")))))

(use-package org-roam-bibtex
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :after org-roam)

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-browser-function #'browse-url-chromium
        org-roam-ui-open-on-start nil))

(defun open-kaartenbak ()
  (interactive)
  (let ((tab-bar-index (tab-bar--tab-index-by-name "Kaartenbak")))
    (if tab-bar-index
        (tab-bar-switch-to-tab (+ tab-bar-index 1))
      (progn
        (tab-bar-new-tab)
        (tab-bar-rename-tab "Kaartenbak")
        (find-file "~/kaartenbak/20210727213932-kaartenbak.org")))))

(setq org-use-speed-commands t  ; set to true for navigation with shortcuts
      org-image-actual-width (list 550) ; resize the width of images
      org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
      org-src-fontify-natively t  ; use auctex for formatting latex in org
      org-hide-leading-stars nil  ; Show all stars of headers
      org-adapt-indentation nil   ; Don't indent subsections (helps org-babel code blocks)
      org-cite-global-bibliography '("~/org/bib.bib")  ; for citing references
      org-latex-create-formula-image-program 'dvisvgm
      org-latex-default-class "tufte-handout"
      org-highlight-latex-and-related '(native))

(use-package org-download)

(use-package ox-hugo
  :config
  (require 'oc-csl)
  (setq org-hugo-base-dir "~/local/folgertk/")
  (setq org-hugo--preprocess-buffer nil)
  (setq org-hugo-auto-set-lastmod t)
  (setq org-cite-csl-styles-dir "~/Zotero/styles")
  (setq org-cite-export-processors '((t csl)))
  :after ox)

(use-package tex
  :defer t
  :ensure auctex
  :init
  (progn
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-PDF-mode 1
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil
          TeX-view-program-list
          '(("Preview.app" "open -a Preview.app %o")
            ("Skim" "open -a Skim.app %o")
            ("displayline" "displayline -g -b %n %o %b")
            ("open" "open %o"))
          TeX-view-program-selection
          '((output-dvi "open")
            (output-pdf "Skim")
            (output-html "open")))
    (add-hook 'TeX-mode-hook #'turn-on-reftex))
  :config
  (bind-key "C-c h l" 'hydra-langtool/body TeX-mode-map)
  (company-auctex-init))

(use-package ox-latex
  :ensure nil
  :defer t
  :config
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (add-to-list 'org-latex-classes
             '("tufte-handout"
               "\\documentclass{tufte-handout}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package bibtex
  :mode (("\\.bib\\'" . bibtex-mode)))

(use-package citar
  :bind (("C-c b" . citar-insert-citation)
         ("C-c C-r" . citar-open)
         ;; :map org-mode-map :package org
         ;; ("C-c b" . #'org-cite-insert)  ;; is this really needed?
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography '("~/org/bib.bib")))

(setq citar-symbols
      `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
        (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
        (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
(setq citar-symbol-separator "  ")

(use-package citar-embark
  :after citar embark
  :config
  (citar-embark-mode)
  (define-key citar-embark-map (kbd "M") 'citar-read-on-remarkable)
  (define-key citar-embark-map (kbd "R") 'citar-add-to-reading-list)
  (define-key citar-embark-map (kbd "F") 'citar-show-pdf-in-finder)
  (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external))
  (add-to-list 'embark-multitarget-actions #'citar-show-pdf-in-finder)
  (add-to-list 'embark-multitarget-actions #'citar-read-on-remarkable))

(defun citar-read-on-remarkable (citekey-or-citekeys)
  "Add the PDF of the entry associated with CITEKEYS to Remarkable."
  (interactive (list (citar-select-refs)))
  (dolist (citekey citekey-or-citekeys)
    (let ((file (citar-get-value "file" citekey)))
      (call-process "rmapi" nil 0 nil "put" file))))

(defun citar-add-to-reading-list (citekey)
  "Capture entry and add to your reading list."
  (interactive (list (citar-select-ref)))
  (let ((link (format-pdf-path-as-org-link citekey)))
    (kill-new link)
    (org-capture nil "r")))

(defun format-pdf-path-as-org-link (citekey)
  (let* ((entry (citar-get-entry citekey))
         (title (citar-get-value "title" entry))
         (file (citar-get-value "file" entry)))
    (format "[[%s][%s]]" file title)))

(defun citar-show-pdf-in-finder (citekey-or-citekeys)
  "Open the directory associated with CITEKEY in finder."
  (interactive (list (citar-select-refs)))
  (dolist (citekey citekey-or-citekeys)
    (let ((file (citar-get-value "file" citekey)))
      (citar-open-dir file))))

(defun citar-open-dir (file)
  (let ((dir (file-name-directory file)))
    (cond
     ((> (length dir) 1)
      (shell-command (concat "open " dir)))
     (t
      (message "No PDF(s) found for this entry: %s" key)))))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq pdf-view-use-scaling t))

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

(use-package pandoc-mode
  :after org)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-enable-symbol-highlighting nil
        lsp-lens-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-diagnostics-provider :none
        lsp-modeline-diagnostics-enable nil
        lsp-completion-show-detail nil
        lsp-completion-show-kind nil
        lsp-pyright-python-executable-cmd "python3"
        )
  :commands (lsp lsp-deferred))

(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-global-modes '(not text-mode term-mode markdown-mode gfm-mode))
  (setq company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-idle-delay 0.5
        company-require-match nil
        company-minimum-prefix-length 2)
  ;; Bind next and previous selection to more intuitive keys
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;; (add-to-list 'company-frontends 'company-tng-frontend)
  ;; :bind (("TAB" . 'company-indent-or-complete-common)))
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map ("<tab>" . company-complete-selection))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common)))

(use-package company-prescient
  :config (company-prescient-mode))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package pyvenv
  :init (setenv "WORKON_HOME" "~/.virtualenvs/"))

(use-package jupyter
  :after org
  :defer t
  :config
  (setq org-babel-python-command "python3")
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages '((jupyter . t)))
  ;; default args for jupyter-python
  (setq org-babel-default-header-args:jupyter-python
   ;; NOTE: for converting Python Dataframes into org tables, I'm using code from
   ;; https://github.com/gregsexton/ob-ipython/blob/7147455230841744fb5b95dcbe03320313a77124/README.org#tips-and-tricks
   ;; which I put in .ipython/profile_default/startup/orgtable.py as a startup file for ipython. 
        '((:results . "replace")
          (:async . "yes")
          (:session . "py")
          (:kernel . "python3")))
  (setq org-babel-default-header-args:jupyter-R
        '((:results . "replace")
          (:async . "yes")
          (:session . "R")
          (:kernel . "R")))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(use-package ess
  :defer t
  :config
  (setq ess-eval-visibly 'nowait))

(use-package stan-mode :defer t)

(use-package company-stan
  :after stan-mode
  :hook (stan-mode . company-stan-setup))

(use-package eldoc-stan
  :after stan-mode
  :hook (stan-mode . eldoc-stan-setup))

(use-package deadgrep
  :bind*
  (("C-c r" . deadgrep)))

(use-package terminal-here
  :config
  (setq terminal-here-mac-terminal-command 'iterm2))

(defun comment-current-line-dwim ()
  "Comment or uncomment the current line."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (push-mark (beginning-of-line) t t)
      (end-of-line)
      (comment-dwim nil))))

(defun new-scratch-pad ()
  "Create a new org-mode buffer for random stuff."
  (interactive)
  (let ((tab-bar-index (tab-bar--tab-index-by-name "Kladblok")))
    (if tab-bar-index
        (progn
          (tab-bar-select-tab (+ tab-bar-index 1))
          (switch-to-buffer "kladblok")
          (olivetti-mode t))
      (progn
        (tab-bar-new-tab)
        (tab-bar-rename-tab "Kladblok")
        (let ((buffer (generate-new-buffer "kladblok")))
          (switch-to-buffer buffer)
          (setq buffer-offer-save t)
          (org-mode)
          (olivetti-mode t))))))

(defun xah-unfill-paragraph ()
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defhydra hydra-windows (:color red)
  ("s" shrink-window-horizontally "shrink horizontally" :column "Sizing")
  ("e" enlarge-window-horizontally "enlarge horizontally")
  ("b" balance-windows "balance window height")
  ("m" maximize-window "maximize current window")
  ("M" minimize-window "minimize current window")
  
  ("h" split-window-below "split horizontally" :column "Split management")
  ("v" split-window-right "split vertically")
  ("d" delete-window "delete current window")
  ("x" delete-other-windows "delete-other-windows")
  ("q" nil "quit menu" :color blue :column nil))

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(global-set-key (kbd "M-/") 'comment-current-line-dwim)
(global-set-key (kbd "M-+")  'mode-line-other-buffer)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer)
(global-set-key (kbd "C-c s") 'new-scratch-pad)
;; Turn off swiping to switch buffers (defined in mac-win.el)
(global-unset-key [swipe-left])
(global-unset-key [swipe-right])
(global-unset-key (kbd "C-<mouse-4>"))
(global-unset-key (kbd "C-<mouse-5>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))
(global-set-key (kbd "M-n") 'hydra-windows/body)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map (kbd "C-c M-a") 'show-my-agenda)
(global-set-key (kbd "C-x C-b") 'tab-bar-select-tab-by-name)
(global-set-key (kbd "C-c c") 'org-capture)

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-}") 'tab-bar-switch-to-next-tab)
    (define-key map (kbd "M-{") 'tab-bar-switch-to-prev-tab)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(setq custom-file (expand-file-name "custom.el" "~/.emacs.d"))

(when (file-exists-p custom-file)
  (load custom-file))
