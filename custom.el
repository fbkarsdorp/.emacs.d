(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-ghostscript-program "gs")
 '(doc-view-pdfdraw-program "mudraw")
 '(doc-view-resolution 300)
 '(doc-view-scale-internally nil)
 '(elfeed-search-title-max-width 100)
 '(emojify-display-style 'unicode)
 '(jupyter-repl-prompt-margin-width 1)
 '(magit-diff-use-overlays nil)
 '(org-agenda-prefix-format
   '((agenda . "%-20c %-8(org-deadline-ahead)")
     (todo . "%-20c %-8(org-deadline-ahead)")
     (tags . "%-20c %-8(org-deadline-ahead)")
     (search . "%-20c %-8(org-deadline-ahead)")))
 '(package-selected-packages
   '(doom-modeline ivy-posframe flx night-owl-theme night-owl-emacs counsel-tramp vterm org counsel-notmuch visual-fill-column ox-latex elfeed all-the-icons jupyter solarized-theme gitignore-templates wgrep yaml-mode which-key visual-regexp-steroids smex smartparens seoul256-theme request rainbow-delimiters paradox ox-pandoc org-projectile org-journal org-fancy-priorities org-cliplink multiple-cursors move-text minions langtool ivy-hydra ivy-bibtex iflipb gruvbox-theme git-gutter-fringe flyspell-correct-ivy exec-path-from-shell esup ess elpy doom-themes diminish deadgrep counsel-projectile company-statistics blacken auctex-latexmk ace-window use-package))
 '(paradox-github-token t)
 '(tramp-default-user "folgert"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(deadgrep-match-face ((t (:inherit font-lock-function-name-face))))
 '(git-gutter:added ((t (:background nil))))
 '(git-gutter:deleted ((t (:background nil))))
 '(git-gutter:modified ((t :background nil)))
 '(hl-line ((t (:extend t))))
 '(jupyter-repl-input-prompt ((t (:foreground "#928374"))))
 '(jupyter-repl-output-prompt ((t (:foreground "#cc241d"))))
 '(line-number ((t (:background "#282828" :foreground "#7c6f64"))))
 '(line-number-current-line ((t (:background "#3c3836" :foreground "#fe8019"))))
 '(markdown-code-face ((t (:inherit ##))))
 '(org-agenda-structure ((t (:inherit default :height 1.25)))))
(put 'downcase-region 'disabled nil)
