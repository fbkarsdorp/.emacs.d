;;; -*- lexical-binding: t -*-
;;; init.el --- This is where all emacs start.
(setq gc-cons-threshold 100000000) 
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 10 1024 1024))))

(setq load-prefer-newer t)
(package-initialize)
(setq package-archives (append
			package-archives
			'(("melpa" . "https://melpa.org/packages/")
                          ("elpy" . "http://jorgenschaefer.github.io/packages/")
                          ("elpa" . "https://elpa.nongnu.org/nongnu/"))))
                          ;; ("org" . "https://orgmode.org/elpa/"))))

(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . 'nil)                          
                            (font . "-*-Iosevka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
                            (height . 58) (width . 219)))


(setq frame-inhibit-implied-resize t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "")

;; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(customize-set-variable 'tramp-default-user "folgertk")
(setenv "LANG" "en_US.UTF-8")

(eval-when-compile
  (require 'use-package)
  (require 'quelpa-use-package))
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(use-package org :ensure org-contrib)

(require 'diminish)
(require 'bind-key)

;; GUI simplifications
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(fringe-mode 16) ;; I like a little more spacing

;; Customizations
(setq inhibit-startup-message t)
(setq frame-title-format "")
(show-paren-mode t)
(blink-cursor-mode -1)
(setq-default cursor-type 'hbar)
(save-place-mode 1)
(setq sentence-end-double-space nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq large-file-warning-threshold 100000000)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq-default fill-column 90)
(prefer-coding-system 'utf-8)
(setq ring-bell-function 'ignore)
(setq ns-use-native-fullscreen t)
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq version-control t)
(setq create-lockfiles nil)

(use-package hl-line
  :ensure nil
  :custom-face (hl-line ((t (:extend t))))
  :hook (after-init . global-hl-line-mode))

;; (use-package hl-line+
;;   :load-path "elisp"
;;   :config
;;   (hl-line-when-idle-interval 0.3)
;;   (toggle-hl-line-when-idle 1))

(defun xah-unfill-paragraph ()
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map ("<tab>" . dired-subtree-toggle)))

;; Dired configurations
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
;; (setq dired-listing-switches "-Ahl  --group-directories-first")
(setq dired-recursive-deletes 'always)
;; (setq insert-directory-program "gls" dired-use-ls-dired t)

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

;; Remap meta to CMD on Mac
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(global-set-key (kbd "M-/") 'comment-current-line-dwim)
(global-set-key (kbd "M-+")  'mode-line-other-buffer)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer)

(global-auto-revert-mode t)
(add-hook 'text-mode-hook #'auto-fill-mode)

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

(use-package ox-pandoc
  :defer t)

(use-package csv-mode
  :defer t)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package company-auctex
  :defer t)

(use-package tramp
  :ensure nil
  :defer t
  :config
  (setq tramp-default-method "ssh")
  (use-package counsel-tramp
    :bind ("C-c t" . counsel-tramp))
  (put 'temporary-file-directory 'standard-value '("/tmp")))

(use-package electric
  :ensure nil
  :config (electric-pair-mode 1))

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package modus-themes
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-completions 'opionated
        modus-themes-diffs 'desaturated
        modus-themes-headings '((t . section))
        modus-themes-hl-line '(accented)
        modus-themes-links '(no-underline)
        modus-themes-mixed-fonts nil
        modus-themes-mode-line '(moody borderless)
        modus-themes-prompts '(background)
        modus-themes-syntax '(faint)
        )
  (setq modus-themes-org-agenda
      '((header-date . (grayscale workaholic bold-today))
        (scheduled . uniform)
        (habit . traffic-light)))
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))

(use-package minions
  :config (minions-mode 1))

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

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (julia-mode . lsp)
         (python-mode . lsp)
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

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)


;; (use-package elpy
;;   ;; :commands elpy-enable
;;   :defer t
;;   :init (advice-add 'python-mode :before 'elpy-enable)
;;   :config
;;   (setq elpy-rpc-python-command "python3")
;;   (eldoc-add-command-completions "company-")
;;   (eldoc-add-command-completions "python-indent-dedent-line-backspace")
;;   ;; only use bare minimum of modules. No need for all fancy stuff
;;   (setq elpy-modules '(elpy-module-company elpy-module-eldoc))
;;   (setq python-shell-interpreter "ipython"
;;         python-shell-interpreter-args "-i --simple-prompt")
;;   :bind (("M-]" . 'elpy-nav-indent-shift-right)
;;          ("M-[" . 'elpy-nav-indent-shift-left)))

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
  (setq recentf-max-saved-items 500))

(use-package ivy
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-display-style 'fancy
        ivy-re-builders-alist '((ivy-bibtex . ivy--regex-ignore-order)
                                (t . ivy--regex-plus)))
  :bind (("C-s" . 'swiper-isearch)
         ("C-r" . 'swiper-backward)
         ("C-c C-r" . 'ivy-resume)))

(use-package ivy-hydra)

(defun counsel-locate-cmd-mdfind (input)
  "Return a shell command based on INPUT."
  (format "mdfind -interpret kind:text %s" input))

(use-package counsel
  :init (counsel-mode t)
  :bind (("C-x C-r" . 'counsel-recentf)
         ("C-c r" . 'counsel-rg)
         ("C-c k" . 'counsel-ag)
         ("C-c g" . 'counsel-git)
         ("C-c i" . 'counsel-imenu)
         ;; ("C-c l" . 'counsel-locate)
         ("C-c c" . 'counsel-org-capture)
         ("C-x b" . 'ivy-switch-buffer))
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

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :config (ivy-prescient-mode))

(use-package company-prescient
  :config (company-prescient-mode))

(use-package rust-mode
  :bind ( :map rust-mode-map
               (("C-c C-t" . racer-describe)
                ([?\t] .  company-indent-or-complete-common)))
  :config
  (progn
    ;; add flycheck support for rust (reads in cargo stuff)
    ;; https://github.com/flycheck/flycheck-rust
    (use-package flycheck-rust)

    ;; cargo-mode for all the cargo related operations
    ;; https://github.com/kwrooijen/cargo.el
    (use-package cargo
      :hook (rust-mode . cargo-minor-mode)
      :bind
      ("C-c C-c C-n" . cargo-process-new)) ;; global binding

    ;;; racer-mode for getting IDE like features for rust-mode
    ;; https://github.com/racer-rust/emacs-racer
    (use-package racer
      :hook (rust-mode . racer-mode)
      :config
      (progn
        ;; package does this by default ;; set racer rust source path environment variable
        ;; (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
        (defun my-racer-mode-hook ()
          (set (make-local-variable 'company-backends)
               '((company-capf company-files)))
          (setq company-minimum-prefix-length 1)
          (setq indent-tabs-mode nil))

        (add-hook 'racer-mode-hook 'my-racer-mode-hook)

        ;; enable company and eldoc minor modes in rust-mode (racer-mode)
        (add-hook 'racer-mode-hook #'company-mode)
        (add-hook 'racer-mode-hook #'eldoc-mode)))

    (add-hook 'rust-mode-hook 'flycheck-mode)
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

    ;; format rust buffers on save using rustfmt
    (add-hook 'before-save-hook
              (lambda ()
                (when (eq major-mode 'rust-mode)
                  (rust-format-buffer))))))

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

(defun bibtex-pdf-open-function (fpath)
  (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath))

(defun add-to-reading-list (keys &optional fallback-action)
  (let ((link (bibtex-completion-format-citation-org-title-link-to-PDF keys)))
    (kill-new link)
    (org-capture nil "r")))
    
(use-package ivy-bibtex
  :bind*
  ("C-c C-r" . ivy-bibtex)
  :config
  (setq bibtex-completion-bibliography "~/org/bib.bib")
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-pdf-open-function 'bibtex-pdf-open-function)
  (setq ivy-bibtex-default-action #'ivy-bibtex-insert-citation)
  (setq bibtex-completion-display-formats '((t . "${author:36} ${title:*} ${year:4} ${=type=:7}")))
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-cite)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))
  (ivy-bibtex-ivify-action add-to-reading-list ivy-bibtex-add-to-reading-list)
  (ivy-add-actions 'ivy-bibtex '(("r" ivy-bibtex-add-to-reading-list "add to reading list"))))

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

(use-package avy
  :bind (("M-j" . 'avy-goto-char-timer)
         ("C-\\" . 'avy-goto-line)))

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
  :after magit
  :config
  (setq ghub-use-workaround-for-emacs-bug 'force))

(use-package vterm)

;; (use-package julia-mode)

;; (use-package julia-snail
;;   :requires vterm
;;   :hook (julia-mode . julia-snail-mode))

;; (use-package lsp-julia
;;   :config
;;   (setq lsp-julia-default-environment "~/.julia/environments/v1.6"))

(use-package terminal-here
  :config
  (setq terminal-here-mac-terminal-command 'iterm2))

(use-package gitignore-templates
  :defer t)

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("" "" "")))

(define-key global-map "\C-ca" 'org-agenda)
;; remove ^ for refile searches
(mapc (lambda (item)
        (setf (alist-get item ivy-initial-inputs-alist) ""))
      '(org-refile org-agenda-refile org-capture-refile))

(setq org-directory "~/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-todo-keywords '((sequence "TODO" "NEXT" "WAITING" "|" "DONE" "CANCELLED" "HOLD"))
      ;; Refiling customizations
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files :maxlevel . 2))
      org-refile-targets '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)"))
      ;; Stylistics
      org-src-fontify-natively t
      org-hide-leading-stars nil
      org-adapt-indentation nil
      org-agenda-search-view-always-boolean t
      org-use-speed-commands t ;; allows faster navigation in org buffers
      org-latex-create-formula-image-program 'dvisvgm
      org-confirm-babel-evaluate nil
      org-enforce-todo-dependencies t
      org-log-done 'time
      org-log-into-drawer t
      org-cite-global-bibliography '("~/org/bib.bib")
      org-agenda-files (mapcar (lambda (f) (concat org-directory f))
                               '("/inbox.org" "/projects.org" "/habits.org" "/agenda.org" "/leeslijst.org")))

(use-package org-cliplink
  :defer t
  :after org)

(defun clip-link-http-or-file ()
  (interactive)
  (if (yes-or-no-p "Web link? ")
      (org-cliplink-capture)
    (let ((link (read-file-name "Enter file path: "))
          (description (read-string "Description: ")))
      (org-make-link-string link description))))

(defun org-deadline-ahead (&optional pos)
  (let* ((days (time-to-number-of-days (org-deadline-ahead-time pos)))
         (future (if (< 0 days) "+" ""))
         (days (fceiling days)))
    (cond
     ((< days 30) (format "%4s" (format "%s%dd" future days)))
     ((< days 358) (format "%4s" (format "%s%dm" future (/ days 30))))
     (t            ""))))

(defun org-deadline-ahead-time (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "DEADLINE" t)))
    (when stamp
      (time-subtract (org-time-string-to-time
                      (org-entry-get (or pos (point)) "DEADLINE" t))
                     (current-time)))))

(defface org-deadline-face '((t (:inherit (font-lock-comment-face))))
  "org-deadline-face")

(add-hook 'org-agenda-finalize-hook
          (lambda ()
            (highlight-regexp "^[[:blank:]]+\\+[0-9]+[md]" 'org-deadline-face)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %^{Todo} %^G \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
         :empty-lines 1)
        ("m" "Meeting" entry (file+headline "~/org/agenda.org" "Toekomstig")
         "* %^{Description} :meeting:\n%^t"
         :empty-lines 1)
        ("r" "Read" entry (file+headline "~/org/leeslijst.org" "Articles")
         ;; "* TODO %(clip-link-http-or-file)\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
         "* TODO %c \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
         :empty-lines 1)
        ("n" "Note" entry (file+headline "~/org/inbox.org" "Notes")
         "* %^{Title} %^G \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
         :empty-lines 1)
        ("l" "Link" entry (file+headline "~/org/bookmarks.org" "Bookmarks")
         "* %(org-cliplink-capture) %^g \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
         :empty-lines 1)
        ("e" "Mail" entry (file+headline "~/org/inbox.org" "Mail")
         "* TODO  %(org-mac-message-get-links \"s\") %^g \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
         :empty-lines 1)))

(defun format-closed-query ()
  (format "+TODO=\"DONE\"+CLOSED>=\"<-%sd>\"" (read-string "Number of days: ")))

(use-package org-super-agenda
  :after org
  :config
  (use-package origami
    :bind (:map org-super-agenda-header-map ("<tab>" . origami-toggle-node))
    :hook (org-agenda-mode . origami-mode)))

(use-package org-download)
(use-package org-pomodoro :after 'org)

(setq org-agenda-block-separator (propertize
                                  (make-string (frame-width) ?\u2594)
                                  'face '(:foreground "grey38"))
      org-super-agenda-header-separator "\n"
      org-habit-show-habits-only-for-today nil
      org-agenda-sticky t
      org-agenda-restore-windows-after-quit t
      org-agenda-show-future-repeats nil
      org-agenda-span 'week
      org-agenda-start-on-weekday nil
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-format-date "\n%A, %-e %B %Y"
      org-agenda-dim-blocked-tasks t
      org-agenda-custom-commands
      '(("d" "Dagelijkse Takenlijst"
         ((agenda ""
                  ((org-agenda-overriding-header " Planner")
                   (org-agenda-prefix-format '((agenda . " %?-12t")))
                   (org-super-agenda-groups
                    '((:name "" :time-grid t :scheduled t :deadline t :category "verjaardag")
                      (:discard (:anything t))))))))
        ("p" "Project backlog"
          ((todo "TODO|NEXT|WAITING|HOLD"
                ((org-agenda-overriding-header " Inbox\n")
                 (org-agenda-prefix-format " %?(org-deadline-ahead) ")
                 (org-agenda-files '("~/org/inbox.org"))))
          (todo "TODO|NEXT|WAITING|HOLD"
                 ((org-agenda-overriding-header " Project TODOs")
                  (org-agenda-prefix-format " %?(org-deadline-ahead) ")
                  (org-agenda-files '("~/org/projects.org"))
                  (org-super-agenda-groups
                   '((:discard (:scheduled t :date t))
                     (:auto-map (lambda (item)
                        (concat " " (upcase-initials (org-find-text-property-in-string 'org-category item)) "\n")))
                     (:discard (:anything t))))))
          (todo "TODO|NEXT"
                ((org-agenda-overriding-header " Reading List")
                 (org-agenda-prefix-format " %?(org-deadline-ahead) ")
                 (org-agenda-files '("~/org/leeslijst.org"))
                 (org-super-agenda-groups
                   '((:discard (:scheduled t))
                     (:auto-map (lambda (item)
                        (concat " " (upcase-initials (org-find-text-property-in-string 'org-category item)) "\n")))
                     (:discard (:anything t))))))))

        ("w" "Weekly review"
         ((tags (format-closed-query)
                ((org-agenda-overriding-header "Overview of DONE tasks")
                 (org-agenda-archives-mode t)))))))

(defun show-my-agenda ()
  (interactive)
  (progn
    (org-agenda nil "d")
    (org-agenda nil "p"))
  (message "Agenda loaded"))

(define-key global-map "\C-ca" 'show-my-agenda)

;; Functions to keep alendar in sight when working on the agenda
(defun fk-window-displaying-agenda-p (window)
  (equal (with-current-buffer (window-buffer window) major-mode)
         'org-agenda-mode)) 

(defun fk-position-calendar-buffer (buffer alist)
  (let ((agenda-window (car (remove-if-not #'fk-window-displaying-agenda-p (window-list)))))
    (when agenda-window
      (let ((desired-window (split-window agenda-window nil 'below)))
        (set-window-buffer desired-window  buffer)
        desired-window))))

(add-to-list 'display-buffer-alist (cons "\\*Calendar\\*" (cons #'fk-position-calendar-buffer nil)))

(add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)
(add-hook 'org-mode-hook (lambda ()
  (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(org-add-link-type "message" 'org-mac-message-open)

(defun org-mac-message-open (message-id)
  "Visit the message with MESSAGE-ID.
   This will use the command `open' with the message URL."
  (message "%s" message-id)
  (browse-url (concat "message://%3c" (substring message-id 2) "%3e")))

(use-package fontawesome)

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

(use-package org-roam
  :init 
  (setq org-roam-v2-ack t)
  :hook
  (after-init . org-roam-mode)
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
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n")))))

(use-package org-roam-bibtex
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :after org-roam)
  ;; :config (require 'org-ref))
  ;; (setq org-ref-default-bibliography '("~/org/bib.bib")))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(use-package smerge-mode
  :defer t
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

(use-package visual-regexp
  :bind (("C-c %" . vr/query-replace)
         ("C-c $" . vr/replace)))

(use-package visual-regexp-steroids
  :after visual-regexp)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d"))

(when (file-exists-p custom-file)
  (load custom-file))
