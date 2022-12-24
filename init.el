;;; -*- lexical-binding: t -*-
;;; init.el --- This is where all emacs start.
(setq gc-cons-threshold 100000000) 
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 10 1024 1024))))

(setq load-prefer-newer t)
(package-initialize)
(setq package-archives (append
			package-archives
			'(("melpa" . "https://melpa.org/packages/")
                          ("elpa" . "https://elpa.nongnu.org/nongnu/"))))

(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . nil)                          
                            ;; (font . "-*-Input Mono Compressed-regular-normal-condensed-*-12-*-*-*-m-0-iso10646-1")
                            (height . 48) (width . 159)))

(set-face-attribute 'default nil :family "Input Mono Condensed" :height 120)
(setq frame-inhibit-implied-resize t)
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-startup-echo-area-message "There is a crack in everything, that's how the light gets in")

;; Turn off swiping to switch buffers (defined in mac-win.el)
(global-unset-key [swipe-left])
(global-unset-key [swipe-right])
(global-unset-key (kbd "C-<mouse-4>"))
(global-unset-key (kbd "C-<mouse-5>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))

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

;; ;; GUI simplifications
(tool-bar-mode -1)
;; (menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(fringe-mode 16) ;; I like a little more spacing

;; tab-bar
(setq tab-bar-mode t)
;; (setq tab-bar-show nil)
(global-set-key (kbd "C-x C-b") 'tab-bar-select-tab-by-name)

;; ;; Customizations
(setq inhibit-startup-message t)
(setq frame-title-format '((:eval (format "%s" (cdr (assoc 'name (tab-bar--current-tab)))))))
(show-paren-mode t)
(blink-cursor-mode nil)
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
(setq pixel-scroll-precision-mode t) ;; new in Emacs 29

(use-package hl-line
  :ensure nil
  :custom-face (hl-line ((t (:extend t))))
  :hook (after-init . global-hl-line-mode))

(defun xah-unfill-paragraph ()
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

;; Dired configurations
;; (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
;; (setq dired-listing-switches "-Ahl  --group-directories-first")
(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

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

;; Remap meta to CMD on Mac
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(global-set-key (kbd "M-/") 'comment-current-line-dwim)
(global-set-key (kbd "M-+")  'mode-line-other-buffer)
(global-set-key (kbd "M-`") 'other-frame)
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer)

(global-auto-revert-mode t)
(add-hook 'text-mode-hook #'auto-fill-mode)

(defun open-kaartenbak ()
  (interactive)
  (let ((tab-bar-index (tab-bar--tab-index-by-name "Kaartenbak")))
    (if tab-bar-index
        (tab-bar-switch-to-tab (+ tab-bar-index 1))
      (progn
        (tab-bar-new-tab)
        (tab-bar-rename-tab "Kaartenbak")
        (find-file "~/kaartenbak/20210727213932-kaartenbak.org")))))

(use-package olivetti
  :config (setq olivetti-style 'fancy))

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

(use-package pandoc-mode
  :after org)

(use-package ox-hugo
  :config
  (require 'oc-csl)
  (setq org-hugo-base-dir "~/local/folgertk/")
  (setq org-hugo--preprocess-buffer nil)
  (setq org-hugo-auto-set-lastmod t)
  ;; From the docs: "By default, ox-hugo will insert a Markdown heading with the
  ;; string defined in org-hugo-pandoc-cite-references-heading [...]". By setting it to
  ;; an empty string, we prevent that, which helps exporting to other formats.
  ;; (plist-put org-hugo-citations-plist :bibliography-section-heading "")
  (setq org-cite-csl-styles-dir "~/Zotero/styles")
  (setq org-cite-export-processors '((t csl)))
  :after ox)

(use-package pdf-tools
  :config (setq pdf-view-use-scaling t))

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
        modus-themes-completions '((matches . (extrabold intense background))
                                   (selection . (semibold accented intense))
                                   (popup . (accented)))
        modus-themes-diffs 'desaturated
        modus-themes-headings '((1 . (1.2))
                                (2 . (rainbow 1.1))
                                (3 . (1))
                                (t . (monochrome)))
        modus-themes-hl-line '(nil)
        modus-themes-links '(nil)
        modus-themes-mixed-fonts nil
        modus-themes-tabs-accented t
        modus-themes-mode-line '(moody borderless accented)
        modus-themes-prompts '(background)
        modus-themes-region '(accented bg-only)
        modus-themes-syntax '(faint)
        modus-themes-tabs-accented nil
        )
  (setq modus-themes-org-agenda
        '((header-date . (grayscale workaholic bold-today))
          (header-block . (1.5 semibold))
          (scheduled . uniform)
          (event . (italic))
          (habit . traffic-light)))
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))

;; (use-package ef-themes
;;   :config
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (load-theme 'ef-summer :no-confirm))

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

(use-package pyvenv
  :init (setenv "WORKON_HOME" "~/.virtualenvs/"))

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
  (ivy-bibtex-ivify-action show-pdf-in-finder ivy-bibtex-show-pdf-in-finder)
  (ivy-bibtex-ivify-action read-on-remarkable ivy-bibtex-read-on-remarkable)
  (ivy-add-actions 'ivy-bibtex '(("R" ivy-bibtex-add-to-reading-list "add to reading list")))
  (ivy-add-actions 'ivy-bibtex '(("F" ivy-bibtex-show-pdf-in-finder "show in finder")))
  (ivy-add-actions 'ivy-bibtex '(("M" ivy-bibtex-read-on-remarkable "read on remarkable"))))

(defun bibtex-pdf-open-function (fpath)
  (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath))

(defun add-to-reading-list (keys &optional fallback-action)
  (let ((link (bibtex-completion-format-citation-org-title-link-to-PDF keys)))
    (kill-new link)
    (org-capture nil "r")))

(defun read-on-remarkable (keys &optional fallback-action)
  (let ((fpath (car (bibtex-completion-find-pdf (car keys)))))
    (call-process "rmapi" nil 0 nil "put" fpath)))

(defun show-pdf-in-finder (keys &optional fallback-action)
  (let ((dir (file-name-directory (car (bibtex-completion-find-pdf (car keys))))))
    (cond
     ((> (length dir) 1)
      (shell-command (concat "open " dir)))
     (t
      (message "No PDF(s) found for this entry: %s" key)))))

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
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :init (projectile-mode +1))

(defun projectile-name-tab-by-project-name-or-default ()
  (let ((project-name (projectile-project-name)))
    (if (string= "-" project-name)
        (tab-bar-tab-name-current)
      project-name)))

(setq tab-bar-tab-name-function #'projectile-name-tab-by-project-name-or-default)

(defun counsel-projectile-switch-project-action-dired-new-tab (project)
  (let* ((project-name (file-name-nondirectory (directory-file-name project)))
         (tab-bar-index (tab-bar--tab-index-by-name project-name)))
    (if tab-bar-index
        (tab-bar-select-tab (+ tab-bar-index 1))
      (progn
        (tab-bar-new-tab)
        (let ((projectile-switch-project-action 'projectile-dired))
          (counsel-projectile-switch-project-by-name project))
        (dirvish-side)))))

(defun projectile-kill-buffers-and-enclosing-tab ()
  (interactive)
  (let* ((project-name (projectile-project-name))
         (tab-bar-index (tab-bar--tab-index-by-name project-name)))
    (when tab-bar-index
      (projectile-kill-buffers)
      (tab-bar-switch-to-recent-tab)
      (tab-bar-close-tab (+ tab-bar-index 1)))))

(use-package counsel-projectile
  :after projectile
  :init (counsel-projectile-mode)
  :config
  ;; I want projectile to open dired upon selecting a project. 
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((add ("T" counsel-projectile-switch-project-action-dired-new-tab "open in new tab") 1)))
  :bind (:map projectile-mode-map
              ("C-c p k" . projectile-kill-buffers-and-enclosing-tab)))

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
  :config
  (setq magit-git-executable "/usr/bin/git")
  :bind (("C-x g" . magit-status)
         ("C-c M-g" . magit-file-popup)))

(use-package forge
  :after magit
  :config
  (setq ghub-use-workaround-for-emacs-bug 'force))

(use-package terminal-here
  :config
  (setq terminal-here-mac-terminal-command 'iterm2))

(use-package gitignore-templates
  :defer t)

(define-key global-map "\C-ca" 'org-agenda)
;; remove ^ for refile searches
(mapc (lambda (item)
        (setf (alist-get item ivy-initial-inputs-alist) ""))
      '(org-refile org-agenda-refile org-capture-refile))

(setq org-directory "~/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-todo-keywords '((sequence "TODO" "WAITING" "RESCHEDULE" "|" "DONE" "CANCELLED"))
      org-deadline-warning-days 2
      ;; ;; Refiling customizations
      org-refile-use-outline-path 'file
      org-highlight-latex-and-related '(native)
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files :maxlevel . 2))
      org-refile-targets '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)"))
      ;; images
      org-image-actual-width (list 550)
      org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
      ;; Stylistics
      org-src-fontify-natively t
      org-hide-leading-stars nil
      org-adapt-indentation nil
      org-agenda-search-view-always-boolean t
      org-use-speed-commands t ;; allows faster navigation in org buffers
      org-latex-create-formula-image-program 'dvisvgm
      org-confirm-babel-evaluate nil
      org-enforce-todo-dependencies t
      org-latex-default-class "tufte-handout"
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

(defun my-agenda-indentation ()
  (format "%s" (my-agenda-indent-string (org-current-level))))

(defun my-agenda-indent-string (level)
  (if (= level 1)
      ""
    (let ((str ""))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "  ")))
      str)))

(org-super-agenda--def-auto-group category "their org-category property"
  :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
              (org-get-category))
  :header-form (concat " " key))

(setq org-agenda-block-separator (propertize
                                  (make-string (frame-width) ?\u2594)
                                  'face '(:foreground "grey38"))
      org-super-agenda-header-separator ""
      org-habit-show-habits-only-for-today nil
      org-agenda-restore-windows-after-quit t
      org-agenda-show-future-repeats nil
      org-agenda-window-setup 'current
      org-agenda-span 'day
      org-agenda-start-on-weekday 1 ;; nil
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
                   (org-agenda-span 'day)
                   (org-deadline-warning-days 0)
                   (org-super-agenda-groups
                    '((:name "" :time-grid t :scheduled t :deadline t :category "verjaardag")
                      (:discard (:anything t))))))))
        ("p" "Project backlog"
          ((todo "TODO|NEXT|WAITING|HOLD"
                ((org-agenda-overriding-header " Inbox\n")
                 (org-agenda-prefix-format "  ")
                 (org-agenda-files '("~/org/inbox.org"))))
          (todo "TODO|NEXT|WAITING|HOLD"
                 ((org-agenda-overriding-header " Project TODOs")
                 (org-agenda-prefix-format "%(my-agenda-indentation)")
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
                     (:discard (:anything t))))))))

        ("w" "Weekly review"
         ((tags (format-closed-query)
                ((org-agenda-overriding-header "Overview of DONE tasks")
                 (org-agenda-archives-mode t)))))))


(defun side-by-side-agenda-view ()
  (progn
    (org-agenda nil "a")
    (split-window-right)
    (org-agenda-redo)
    (split-window-below)
    (other-window 1)
    ;; (enlarge-window 4)
    (cfw:open-org-calendar)
    (setq org-agenda-sticky t)
    (other-window 1)
    (org-agenda nil "p")
    (setq org-agenda-sticky nil)))

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

(define-key global-map (kbd "C-c M-a") 'show-my-agenda)

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

(add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)

(add-to-list 'load-path (expand-file-name "org-mac-link" "~/.emacs.d/gitrepos"))
(require 'org-mac-link)
(add-hook 'org-mode-hook (lambda ()
  (define-key org-mode-map (kbd "C-c g") 'org-mac-link-get-link)))

(org-add-link-type "message" 'org-mac-message-open)

(defun org-mac-message-open (message-id)
  "Visit the message with MESSAGE-ID.
   This will use the command `open' with the message URL."
  (message "%s" message-id)
  (browse-url (concat "message://%3c" (substring message-id 2) "%3e")))

(use-package fontawesome)

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

(use-package jupyter
  :after org
  :config
  (setq org-babel-python-command "python3")
  (org-babel-do-load-languages 'org-babel-load-languages '((jupyter . t)))
  ;; default args for jupyter-python
  (setq org-babel-default-header-args:jupyter-python
        ;; NOTE: ofr converting Python Dataframes into org tables, I'm using code from
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

(global-set-key (kbd "M-n") 'hydra-windows/body)

(use-package visual-regexp
  :bind (("C-c %" . vr/query-replace)
         ("C-c $" . vr/replace)))

(use-package visual-regexp-steroids
  :after visual-regexp)

(use-package all-the-icons)

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

(use-package calfw)
(use-package calfw-org)
  
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d"))

(when (file-exists-p custom-file)
  (load custom-file))
