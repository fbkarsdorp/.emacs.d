(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-ghostscript-program "gs")
 '(doc-view-pdfdraw-program "mudraw")
 '(doc-view-resolution 300)
 '(doc-view-scale-internally nil)
 '(org-agenda-prefix-format
   (quote
    ((agenda . "%-20c %-8(org-deadline-ahead)")
     (todo . "%-20c %-8(org-deadline-ahead)")
     (tags . "%-20c %-8(org-deadline-ahead)")
     (search . "%-20c %-8(org-deadline-ahead)"))))
 '(package-selected-packages
   (quote
    (counsel-tramp gitignore-templates dired-sidebar ob-async wgrep yaml-mode which-key visual-regexp-steroids smex smartparens seoul256-theme request rainbow-delimiters paradox ox-pandoc org-projectile org-journal org-fancy-priorities org-cliplink ob-ipython multiple-cursors move-text minions leuven-theme langtool json-mode ivy-hydra ivy-bibtex iflipb gruvbox-theme git-gutter-fringe forge flyspell-correct-ivy exec-path-from-shell esup ess elpy doom-themes diminish deadgrep counsel-projectile company-statistics blacken auctex-latexmk ace-window use-package)))
 '(paradox-github-token t)
 '(tramp-default-user "folgert" nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit ##))))
 '(org-agenda-structure ((t (:inherit default :height 1.25)))))
(put 'downcase-region 'disabled nil)
