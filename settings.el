(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-fit-frame-flag nil)
 '(abbrev-file-name "~/.emacs.d/data/abbrev" t)
 '(avy-case-fold-search t)
 '(avy-timeout-seconds 0.3)
 '(bm-repository-file "~/.emacs.d/data/bm-repository")
 '(column-number-mode t)
 '(company-global-modes (quote (emacs-lisp-mode c-mode c++-mode)))
 '(company-idle-delay nil)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-context-lines 10)
 '(compilation-scroll-output (quote first-error))
 '(compilation-skip-threshold 2)
 '(custom-buffer-done-function (quote kill-buffer))
 '(custom-file "~/.emacs.d/settings.el")
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(delete-by-moving-to-trash t)
 '(global-auto-complete-mode t)
 '(global-auto-revert-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(ido-save-directory-list-file "~/.emacs.d/data/ido.last")
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message "")
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 '(ispell-extra-args (quote ("--sug-mode=fast")))
 '(line-number-mode t)
 '(lsp-enable-eldoc nil)
 '(lsp-inhbit-message t)
 '(load-prefer-newer t)
 '(magithub-dir "~/.emacs.d/data/magithub" t)
 '(mc/list-file "~/.emacs.d/data/mc-lists.el")
 '(pcache-directory "~/data/emacs/var")
 '(persistent-scratch-autosave-interval 30)
 '(persistent-scratch-backup-directory "~/data/emacs/backups")
 '(persistent-scratch-file-name "~/.emacs.d/data/persistent-scratch" t)
 '(projectile-cache-file "~/.emacs.d/data/projectile.cache")
 '(projectile-enable-caching t)
 '(projectile-file-exists-local-cache-expire 300)
 '(projectile-globally-ignored-files (quote ("TAGS" "GPATH" "GRTAGS" "GTAGS" "ID")))
 '(projectile-known-projects-file "~/.emacs.d/data/projectile-bookmarks.eld")
 '(recentf-auto-cleanup 60)
 '(recentf-exclude
   (quote
    ("~\\'" "\\`out\\'" "\\.log\\'" "^/[^/]*:" "\\.el\\.gz\\'")))
 '(recentf-max-saved-items 2000)
 '(recentf-save-file "~/.emacs.d/data/recentf")
 '(save-abbrevs (quote silently))
 '(save-kill-file-name "~/.emacs.d/data/kill-ring-saved.el" t)
 '(save-place-file "~/.emacs.d/data/places")
 '(savehist-additional-variables
   (quote
    (tablist-named-filter file-name-history sr-history-registry kmacro-ring compile-histor regexp-search-ring search-ring)))
 '(savehist-autosave-interval 60)
 '(savehist-file "~/.emacs.d/data/history")
 '(savehist-ignored-variables (quote (load-history flyspell-auto-correct-ring kill-ring)))
 '(scroll-bar-mode nil)
 '(semanticdb-default-save-directory "~/.emacs.d/data/semanticdb" t)
 '(slime-repl-history-file "~/.emacs.d/data/slime-history.eld" t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(url-cache-directory "~/.emacs.d/data/url/cache")
 '(url-configuration-directory "~/.emacs.d/data/url/")
 '(use-package-enable-imenu-support t)
 '(user-full-name "Mark Colburn")
 '(user-initials "mhc")
 '(user-mail-address "colburn.mark@gmail.com")
 '(visible-bell nil)
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets")))
 '(yas-wrap-around-regon t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "grey50" :slant italic)))))