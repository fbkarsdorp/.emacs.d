;;; -*- lexical-binding: t -*-
;;; init.el --- This is where all emacs start.

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

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
(require 'diminish)
(require 'bind-key)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(show-paren-mode t)
(blink-cursor-mode -1)
(save-place-mode 1)
(global-hl-line-mode 1)
(fringe-mode 16)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
;; warn only for files larger than 100mb
(setq large-file-warning-threshold 100000000)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq-default fill-column 90)
(prefer-coding-system 'utf-8)
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)))

(use-package async
  :config (setq async-bytecomp-package-mode 1))

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

(setq frame-title-format "")
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)
(global-set-key (kbd "C-x C-b") 'ibuffer)

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
      scroll-conservatively 101)

(defun new-scratch-pad ()
  (interactive)
  (let ((buffer (generate-new-buffer "scratch-pad")))
    (switch-to-buffer buffer)
    (setq buffer-offer-save t)
    (text-mode)
    buffer))

(global-set-key (kbd "C-c s") 'new-scratch-pad)

(use-package move-text
  :config (move-text-default-bindings))

(use-package paradox
  :config (paradox-enable))

(use-package tex
  :defer t
  :ensure auctex
  :init
  (progn
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-PDF-mode 1
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil)))

(use-package auctex-latexmk
  :init
  (progn
    (setq auctex-latexmk-inherit-TeX-PDF-mode t)
    :config (auctex-latexmk-setup)))

(use-package flyspell-correct-ivy
  :config
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  (setq flyspell-default-dictionary "nl_NL")
  (setenv "DICTIONARY" "nl_NL")
  (define-key flyspell-mode-map (kbd "C-c s") 'flyspell-correct-previous-word-generic))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      t
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  (require 'treemacs-magit)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-hard t))

;; (use-package doom-modeline
;;   :defer t
;;   :config (setq doom-modeline-height 20)
;;   :hook (after-init . doom-modeline-init))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package company
  :config
  (global-company-mode)
  (setq company-global-modes '(not text-mode term-mode org-mode markdown-mode gfm-mode))
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
  :config
  (eldoc-add-command-completions "company-")
  (eldoc-add-command-completions "python-indent-dedent-line-backspace")
  (setq elpy-modules '(elpy-module-company elpy-module-eldoc))
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  (elpy-enable)
  :bind (("M-]" . 'elpy-nav-indent-shift-right)
         ("M-[" . 'elpy-nav-indent-shift-left)))

(use-package ess)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

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
  :bind (("C-s" . 'counsel-grep-or-swiper)
         ("C-r" . 'swiper)
         ("C-c C-r" . 'ivy-resume)))

(use-package ivy-hydra)

(use-package smex)

(use-package counsel
  :init (counsel-mode t)
  :bind (("C-x C-r" . counsel-recentf)
         ("C-c r" . 'counsel-rg)
         ("C-c k" . 'counsel-ag)
         ("C-c f" . 'counsel-fzf)
         ("C-c g" . 'counsel-git)
         ("C-c i" . 'counsel-imenu))
  :config
  (setq counsel-git-cmd "rg --files")
  (setq counsel-grep-base-command "grep -niE %s %s")
  (setq counsel-grep-base-command
        ;; "ag --nocolor --nogroup %s %s")
        "rg -S -M 120 --no-heading --line-number --color never %s %s")
  (setq counsel-rg-base-command
      "rg -S -M 120 --no-heading --line-number --color never %s .")
  (setq counsel-find-file-occur-cmd
        "gls -a | grep -i -E '%s' | gxargs -d '\\n' gls -d --group-directories-first"))

(defun search-dir-with-ag ()
  (interactive)
  (let ((search-dir (read-directory-name "Directory:")))
    (counsel-ag "" search-dir)))

(add-to-list 'load-path "~/.emacs.d/wgrep")
(require 'wgrep)

(use-package deadgrep
  :bind*
  ("C-c r" . deadgrep))

(defun ivy-bibtex-format-pandoc-citation (keys)
  (concat "[" (mapconcat (lambda (key) (concat "@" key)) keys "; ") "]"))

(use-package ivy-bibtex
  :bind*
  ("C-c C-r" . ivy-bibtex)
  :config
  (setq bibtex-completion-bibliography "~/org/bibliography.bib")
  (setq ivy-bibtex-default-action #'ivy-bibtex-insert-citation)
  (setq bibtex-completion-display-formats '((t . "${author:36} ${title:*} ${year:4} ${=type=:7}")))
  (setf (alist-get 'org-mode bibtex-completion-format-citation-functions)
        'bibtex-completion-format-citation-pandoc-citeproc))

(use-package projectile
  :diminish
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package json-mode)

(use-package avy
  :bind (("C-'" . 'avy-goto-char)
         ("C-\\" . 'avy-goto-line)
         ("C-;" . 'avy-goto-word-1)))

(use-package ace-window
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
  :bind (("C-x g" . magit-status)))

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
  (add-hook 'text-mode-hook #'git-gutter-mode)
  (add-hook 'prog-mode-hook #'git-gutter-mode)
  (add-hook 'conf-mode #'git-gutter-mode))

(use-package all-the-icons)

;; org configuration
(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("HIGH" "MID" "LOW")))

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

;; remove ^ for refile searches
(mapc (lambda (item)
        (setf (alist-get item ivy-initial-inputs-alist) ""))
      '(org-refile org-agenda-refile org-capture-refile))

(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED")))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
(setq org-agenda-show-future-repeats nil)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %^{Todo} %^G \n  %i\n  %a\n\n  %?"
         :empty-lines 1)
        ("i" "Idee" entry (file+olp+datetree "~/org/ideas.org")
         "* Idee: %^{Title} %^g \n\n%?"
         :empty-lines 1)
        ("n" "Note" entry (file+headline "~/org/todo.org" "Notes")
         "* Note %^{Title} %^G \n\n  %?")
        ("l" "Link" entry (file+headline "~/org/bookmarks.org" "Bookmarks")
         "* Link [[%^{Link}][%^{Description}]] %^g \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
         :empty-lines 1)))

(defun skip-sprint ()
  (seq-filter (lambda (x) (not (string-equal x "~/org/sprint.org"))) (org-agenda-files)))

(setq org-agenda-block-separator ?\u2015
      org-todo-keyword-faces
      '(("WAITING"     :foreground "#fabd2f" :weight bold)
        ("CANCELLED"   :foreground "#d3869b" :weight bold)
        ("IN-PROGRESS" :foreground "#b8bb26" :weight bold))
      org-priority-faces
      '((?A . (:foreground "#d3869b" :weight bold))
        (?B . (:foreground "#fabd2f" :weight bold))
        (?C . (:foreground "#83a598" :weight bold)))
      org-agenda-custom-commands
      '(("d" "Dagelijkse Takenlijst"
         ((agenda "" ((org-agenda-overriding-header "Upcoming deadlines")
                      (org-agenda-span 7)
                      (org-agenda-time-grid nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-show-all-dates nil)
                      (org-agenda-entry-types '(:deadline :scheduled))))
          (todo "" ((org-agenda-overriding-header "Projects")
                    (org-agenda-sorting-strategy (quote ((agenda time-up priority-down))))
                    (org-agenda-files '("~/org/sprint.org"))))
          (todo "" ((org-agenda-overriding-header "OC")
                    (org-agenda-sorting-strategy (quote ((agenda time-up priority-down))))
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":OC:"))))
          (todo "" ((org-agenda-overriding-header "Backlog")
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":OC:"))
                    (org-agenda-sorting-strategy
                     (quote ((agenda time-up priority-down))))
                    (org-agenda-files (skip-sprint))))))

        ("w" "Wekelijkse review"
         ((agenda "" ((org-agenda-span 7)
                      (org-deadline-warning-days 0)
                      (org-agenda-show-all-dates nil)))
          (agenda "" ((org-agenda-overriding-header "Upcoming deadlines")
                      (org-agenda-span 'month)
                      (org-agenda-time-grid nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-show-all-dates nil)
                      (org-agenda-entry-types '(:deadline))))
          (todo "" ((org-agenda-overriding-header "Projects")
                    (org-agenda-sorting-strategy (quote ((agenda time-up priority-down))))
                    (org-agenda-files '("~/org/sprint.org"))))
          (todo "" ((org-agenda-overriding-header "OC")
                    (org-agenda-sorting-strategy (quote ((agenda time-up priority-down))))
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":OC:"))))
          (todo "" ((org-agenda-overriding-header "Backlog")
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":OC:"))
                    (org-agenda-sorting-strategy
                     (quote ((agenda time-up priority-down))))
                    (org-agenda-files (skip-sprint))))))))

(setq org-agenda-files
      (mapcar (lambda (f) (concat org-directory f)) '("/todo.org" "/projects.org" "/oc.org" "/sprint.org")))

(defun counsel-find-org-file ()
  (interactive)
  (let ((file-list (seq-filter (lambda (f) (s-suffix? ".org" f))
                               (directory-files "~/org"))))
    (ivy-read "org files: " file-list
              :require-match nil
              :action (lambda (f) (find-file (concat org-directory "/" f)))
              :caller 'counsel-find-org-file)))

(define-key global-map "\C-co" 'counsel-find-org-file)

(use-package org-journal
  :init
  (setq org-journal-dir "~/journal/")
  (setq org-journal-date-format "#+TITLE: Journal Entry- %e %b %Y (%A)")
  (setq org-journal-time-format ""))

(defun get-journal-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-name (format-time-string "%Y%m%d")))
    (expand-file-name (concat org-journal-dir daily-name))))

(defun journal-file-today ()
  "Create and load a journal file based on today's date."
  (interactive)
  (find-file (get-journal-file-today)))

(global-set-key (kbd "C-c j") 'journal-file-today)

(use-package iflipb
  :bind*
  (("M-}" . iflipb-next-buffer)
   ("M-{" . iflipb-previous-buffer)))

(use-package ox-pandoc)

(use-package visual-regexp
  :bind (("C-%" . vr/query-replace)))

(use-package visual-regexp-steroids
  :after visual-regexp)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-window-fixed-size t)
 '(org-agenda-files
   (quote
    ("~/org/leadership.org" "~/org/todo.org" "~/org/projects.org" "~/org/oc.org" "~/org/sprint.org")))
 '(package-selected-packages
   (quote
    (org-journal move-text iflipb org-fancy-priorities ivy-hydra treemacs-projectile treemacs kaolin-themes paradox visual-regexp-steroids visual-regexp neotree deadgrep seoul256-theme elpy json-mode ox-pandoc ivy-bibtex ess ess-site diff-hl flyspell-correct-ivy doom-themes auctex-latexmk auctex company company-statistics org-download smartparens org yaml-mode org-bullets diminish counsel-projectile gruvbox-theme magit avy smex multiple-cursors which-key counsel markdown-mode exec-path-from-shell use-package)))
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((org-image-actual-width))))
 '(tramp-default-user "folgert" nil (tramp))
 '(tramp-syntax (quote default) nil (tramp)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-checkbox ((t (:background "#fdf6e3" :foreground "#657b83" :box nil)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit ##))))
 '(org-agenda-date-today ((t (:foreground "#fdf4c1" :slant normal :weight bold :height 1.1))))
 '(org-agenda-structure ((t (:inherit default :height 1.25)))))
(put 'downcase-region 'disabled nil)
