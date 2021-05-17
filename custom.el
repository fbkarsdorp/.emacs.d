(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#f7f3ee" "#955f5f" "#81895d" "#957f5f" "#7382a0" "#9c739c" "#5f8c7d" "#605a52"])
 '(column-number-mode t)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(cursor-type 'hbar)
 '(custom-safe-themes
   '("fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "c0a0c2f40c110b5b212eb4f2dad6ac9cac07eb70380631151fa75556b0100063" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" default))
 '(doc-view-ghostscript-program "gs")
 '(doc-view-pdfdraw-program "mudraw")
 '(doc-view-resolution 300)
 '(doc-view-scale-internally nil)
 '(doom-gruvbox-light-padded-modeline t)
 '(doom-gruvbox-light-variant soft)
 '(elfeed-search-title-max-width 100)
 '(emojify-display-style 'unicode)
 '(fci-rule-color "#605a52")
 '(jdee-db-active-breakpoint-face-colors (cons "#f1ece4" "#7382a0"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f1ece4" "#81895d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f1ece4" "#b9a992"))
 '(jupyter-repl-prompt-margin-width 1)
 '(mac-font-panel-mode nil)
 '(mac-mouse-wheel-mode t)
 '(mac-mouse-wheel-smooth-scroll nil)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(ns-alternate-modifier 'none)
 '(objed-cursor-color "#955f5f")
 '(org-agenda-files
   '("~/org/agenda.org" "~/org/inbox.org" "~/org/projects.org" "~/org/habits.org"))
 '(org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800)
     "......" "----------------"))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m org-mac-link))
 '(org-super-agenda-header-prefix "")
 '(org-super-agenda-header-separator "
")
 '(package-selected-packages
   '(org-plus-contrib grab-mac-link with-editor zotxt perspective gruvbox origami fontawesome org-roam python tron-legacy-theme zenburn-theme vterm ayu-theme racer cargo flycheck-rust paredit org-super-agenda expand-region rust-mode csv-mode dired-subtree doom-modeline counsel-tramp org counsel-notmuch visual-fill-column ox-latex elfeed all-the-icons jupyter solarized-theme gitignore-templates wgrep yaml-mode which-key visual-regexp-steroids smex smartparens seoul256-theme request rainbow-delimiters paradox ox-pandoc org-projectile org-fancy-priorities org-cliplink multiple-cursors move-text minions langtool ivy-hydra ivy-bibtex iflipb gruvbox-theme flyspell-correct-ivy exec-path-from-shell esup ess elpy doom-themes diminish deadgrep counsel-projectile company-statistics blacken auctex-latexmk ace-window use-package))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (cons "#605a52" "#f7f3ee"))
 '(rustic-ansi-faces
   ["#f7f3ee" "#955f5f" "#81895d" "#957f5f" "#7382a0" "#9c739c" "#5f8c7d" "#605a52"])
 '(tramp-default-user "folgertk")
 '(vc-annotate-background "#f7f3ee")
 '(vc-annotate-color-map
   (list
    (cons 20 "#81895d")
    (cons 40 "#87855d")
    (cons 60 "#8e825e")
    (cons 80 "#957f5f")
    (cons 100 "#957f5f")
    (cons 120 "#957f5f")
    (cons 140 "#957f5f")
    (cons 160 "#977b73")
    (cons 180 "#997787")
    (cons 200 "#9c739c")
    (cons 220 "#996c87")
    (cons 240 "#976573")
    (cons 260 "#955f5f")
    (cons 280 "#9e716b")
    (cons 300 "#a78378")
    (cons 320 "#b09685")
    (cons 340 "#605a52")
    (cons 360 "#605a52")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(deadgrep-match-face ((t (:inherit font-lock-function-name-face))))
 '(hl-line ((t (:extend t))))
 '(jupyter-repl-input-prompt ((t (:foreground "#928374"))))
 '(jupyter-repl-output-prompt ((t (:foreground "#cc241d"))))
 '(org-agenda-date-today ((t (:inherit default :height 1.25))))
 '(org-agenda-date ((t (:inherit default :height 1.25))))
 '(org-agenda-structure ((t (:inherit default :height 1.5))))
 '(org-super-agenda-header ((t (:inherit default :height 1.25)))))
(put 'downcase-region 'disabled nil)
