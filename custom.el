(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "Folgerts-MBP-2.lan")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)
     ((:application eshell)
      eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))
     (eshell-connection-default-profile
      (eshell-path-env-list))))
 '(custom-safe-themes
   '("47770bdfb2d9391ead441f90402728c25a55ea96a8774802600d7ac4555abba9" "3b228dab7cbc6d14ea583e0bb5c857284a01d9489c0e24f5ecc4845e77dc84b0" "05e7f4b2f9bb920acdaa119442f0048ec2285ee71c9f5d2891713c779978c03c" default))
 '(display-buffer-alist
   '(("*Org Agenda(x)*" display-buffer-in-side-window
      (dedicated . t)
      (side . right)
      (slot . 1)
      (window-parameters
       (no-delete-other-windows . t)
       (no-other-window)
       (mode-line-format . none)))))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(package-selected-packages
   '(spacious-padding f python-black stripes lambda-line consult-org-roam org-caldav caldav oauth2 vterm tablist jupyter git-commit org-mru-clock magit-section citar-org-roam zmq treesit citar-embark citar embark-consult embark marginalia consult vertico orderless magit svg-tag-mode pdf-tools visual-regexp-steroids ef-themes dirvish org-contrib pyenv citeproc ox-hugo pandoc-mode olivetti ctrlf ace-window all-the-icons auctex auctex-latexmk blacken cargo company-auctex company-statistics company-stan company-prescient csv-mode deadgrep diminish dired-subtree doom-modeline doom-themes eldoc-stan elpy ess esup exec-path-from-shell expand-region flyspell-correct-ivy fontawesome forge gitignore-templates grab-mac-link iflipb fivy-bibtex ivy-hydra ivy-prescient langtool lsp-mode lsp-ivy lsp-pyright matlab-mode minions moody move-text multiple-cursors nano-emacs org-cliplink org-download org-projectile org-roam org-roam-ui org-super-agenda origami ox-latex ox-pandoc paredit perspective prescient python quelpa quelpa-use-package rainbow-delimiters request rust-mode simple-httpd smartparens smex solarized-theme stan-mode terminal-here use-package visual-fill-column websocket wgrep which-key with-editor yaml-mode))
 '(tramp-default-user "folgert_karsdorp")
 '(warning-minimum-level :error)
 '(warning-suppress-log-types '((use-package) ((org-roam))))
 '(warning-suppress-types '(((org-roam)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
