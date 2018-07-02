(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-fit-frame-flag nil)
 '(avy-case-fold-search t)
 '(avy-timeout-seconds 0.3)
 '(column-number-mode t)
 '(company-global-modes '(emacs-lisp-mode c-mode c++-mode))
 '(company-idle-delay nil)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-context-lines 10)
 '(compilation-scroll-output 'first-error)
 '(compilation-skip-threshold 2)
 '(custom-buffer-done-function 'kill-buffer)
 '(custom-file "~/.emacs.d/settings.el")
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(delete-by-moving-to-trash t)
 '(global-auto-complete-mode t)
 '(global-auto-revert-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message "")
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 '(ispell-extra-args '("--sug-mode=fast"))
 '(line-number-mode t)
 '(load-prefer-newer t)
 '(lsp-enable-eldoc nil)
 '(lsp-inhbit-message t)
 '(make-backup-files nil)
 '(nlinum-format "%5d ")
 '(persistent-scratch-autosave-interval 30)
 '(projectile-enable-caching t)
 '(projectile-file-exists-local-cache-expire 300)
 '(projectile-globally-ignored-files '("TAGS" "GPATH" "GRTAGS" "GTAGS" "ID"))
 '(recentf-auto-cleanup 60)
 '(recentf-exclude
   '("~\\'" "\\`out\\'" "\\.log\\'" "^/[^/]*:" "\\.el\\.gz\\'"))
 '(recentf-max-saved-items 2000)
 '(safe-local-variable-values '((bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)")))
 '(save-abbrevs 'silently)
 '(savehist-additional-variables
   '(tablist-named-filter file-name-history sr-history-registry kmacro-ring compile-histor regexp-search-ring search-ring))
 '(savehist-autosave-interval 60)
 '(savehist-ignored-variables '(load-history flyspell-auto-correct-ring kill-ring))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets nil (uniquify))
 '(use-package-enable-imenu-support t)
 '(user-full-name "Mark Colburn")
 '(user-initials "mhc")
 '(user-mail-address "colburn.mark@gmail.com")
 '(visible-bell nil)
 '(yas-snippet-dirs '("~/.emacs.d/snippets") t)
 '(yas-wrap-around-regon t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#eaeaea" :background "#181a26"))))
 '(linum ((t (:foreground "#8f8f8f" :height 0.8))))
 '(nlinum-current-line ((t (:inherit linum :foreground "#daa520" :weight bold)))))
