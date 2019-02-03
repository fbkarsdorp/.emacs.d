(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-prefix-format
   (quote
    ((agenda . "%-20c %-8(org-deadline-ahead)")
     (todo . "%-20c %-8(org-deadline-ahead)")
     (tags . "%-20c %-8(org-deadline-ahead)")
     (search . "%-20c %-8(org-deadline-ahead)"))))
 '(package-selected-packages
   (quote
    (yaml-mode which-key visual-regexp-steroids use-package smex smartparens seoul256-theme restclient request rainbow-delimiters peep-dired paradox ox-pandoc org-projectile org-journal org-fancy-priorities org-cliplink org-bullets ob-ipython neotree multiple-cursors move-text minions leuven-theme langtool kaolin-themes json-mode ivy-hydra ivy-bibtex iflipb gruvbox-theme git-gutter-fringe forge flyspell-correct-ivy exec-path-from-shell esup ess elpy doom-themes diminish diff-hl deadgrep counsel-projectile company-statistics blacken auctex-latexmk ace-window)))
 '(tramp-default-user "folgert" nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit ##))))
 '(org-agenda-structure ((t (:inherit default :height 1.25))))
 )
(put 'downcase-region 'disabled nil)
