;;; init.el -- user customization for Emacs  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary

;; This is my emacs .init file. It uses straight.el and use-package
;; for all package maangement.

;;; Code:

(defconst emacs-start-time (current-time))
(setq package-enable-at-startup nil)

(defvar my/file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold (* 512 1024 1024)
      gc-cons-percentage 0.6)

(defun my/restore-default-values ()
  "Restore the default values of performance-critical variables."
  (setq file-name-handler-alist my/file-name-handler-alist-old
        message-log-max 1000
        gc-cons-threshold 800000
        gc-cons-percentage 0.1)
  (garbage-collect)
  (remove-hook 'after-init-hook #'my/restore-default-values))

(add-hook 'after-init-hook #'my/restore-default-values)

(defun my/after-init ()
  "Report loading time after packages are loaded."
  (let ((elapsed
         (float-time
          (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs) [after-init]"
             load-file-name elapsed)))

(add-hook 'after-init-hook #'my/after-init t)

;; Bootstrap straight.el
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; There is one final user-facing note about the transaction system,
;; which is important when you want to load your init-file after
;; Emacs init has already completed, but before straight.el has been
;; loaded (so you cannot just wrap the call in
;; straight-transaction). To cover this edge case (which arises, for
;; example, when you wish to profile your init-file using something
;; like esup), you should use the following pattern:
;;
;; (unwind-protect
;;     (let ((straight-treat-as-init t))
;; 	"load your init-file here")
;;   (straight-finalize-transaction))

;; Use straight.el to load missing packages for `use-package` by default.
(setq straight-use-package-by-default t)

;; Use imenu to find use-package declarations
(setq use-package-enable-imenu-support t)

;; Enable tracing of use-package declarations for easier debugging.
;; (setq use-package-verbose nil)

;; If `use-package-expand-minimally` is nil (the default), use-package
;; attempts to catch and report errors that occur during expansion of
;; use-package declarations in your init file. Setting
;; `use-package-expand-minimally` to t completely disables this checking.
;; (setq use-package-expand-minimally t)

;; Bootstrap `use-package` integration for straight.el.
(straight-use-package 'use-package)
(use-package bind-key :commands bind-key)

(use-package system-packages
  :preface
  (defun system-packages--run-command (action &optional pack args)
    "Run a command asynchronously using the system's package manager.
See `system-packages-get-command' for how to use ACTION, PACK,
and ARGS."
    (let ((command (if (and (eq system-packages-package-manager 'choco)
                            (memq action '(install uninstall update clean-cache)))
                       (format "runas /user:administrator@%s \"%s\""
                               system-name
                               (system-packages-get-command action pack args))
                     (system-packages-get-command action pack args)))
          (default-directory (if system-packages-use-sudo
                                 "/sudo::"
                               default-directory)))
      (if (eq system-type 'windows-nt)
          (progn
            (let ((shell-prompt-pattern "Enter the password for administrator@MHCOLBUR-LAP:")
                  (comint-use-prompt-regexp "Enter the password for administrator@MHCOLBUR-LAP:"))
              (shell-command command "*system-packages*")
              (send-invisible "Enter administrator password: ")))
        (async-shell-command command "*system-packages*"))))
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'system-packages-supported-package-managers
                 '(choco .
                         ((default-sudo . nil)
                          (install . "choco install --noprogress")
                          (search . "choco search")
                          (uninstall . "choco uninstall")
                          (update . "choco upgrade")
                          (clean-cache . "choco optimize")
                          (log . "type C:/ProgramData/chocolatey/logs/chocolatey.log")
                          (get-info . "choco info")
                          (get-info-remote . "choco info")
                          (list-files-provided-by . "choco info")
                          (verify-all-packages . nil)
                          (verify-all-dependencies . nil)
                          (remove-orphaned . nil)
                          (list-installed-packages . "choco list -lai")
                          (list-installed-packages-all . "choco list -ai")
                          (list-dependencies-of . nil)
                          (noconfirm . "-y"))))
    (setq system-packages-use-sudo nil)
    (setq system-packages-package-manager 'choco)))

(use-package use-package-ensure-system-package
  :after (use-package system-packages) )

(use-package use-package-chords
  :disabled
  :after (use-package)
  :config
  ;; Define your chord bindings in the same manner as :bind using a cons or a list of conses:
  ;;
  ;; (use-package ace-jump-mode
  ;;  :chords (("jj" . ace-jump-char-mode)
  ;;           ("jk" . ace-jump-word-mode)
  ;;           ("jl" . ace-jump-line-mode)))
  (key-chord-mode 1))

;;;
;;; Settings
;;;

(require 'cl-lib)

(defconst user-data-directory (expand-file-name "data" user-emacs-directory)
  "Directory for data files.")

(defconst user-document-directory (expand-file-name "~/Documents")
  "Directory for user documents.")

(defconst user-org-directory (expand-file-name "org" user-document-directory)
  "Directory for user ‘org-mode’ files.")

;; Create any missing directories
(dolist (dir (list user-data-directory user-document-directory user-org-directory))
  (make-directory dir t))

(load (expand-file-name "settings" user-emacs-directory) :noerror)

(show-paren-mode 1)
(size-indication-mode 1)
(delete-selection-mode 1)

(setq auto-revert-verbose nil          ; no messages about reverted files
      auto-save-default nil
      auto-save-list-file-name (expand-file-name "auto-save-list" user-data-directory)
      auto-window-vscroll nil
      browse-url-browser-function #'browse-url-chromium
      confirm-kill-emacs #'y-or-n-p
      css-indent-offset 2
      echo-keystrokes 0.3
      global-auto-revert-mode t
      mouse-drag-copy-region t
      scroll-conservatively 100000
      scroll-margin 0
      scroll-preserve-screen-position 1
      ;; Sentances can end with a '. ', rather than '.  '
      sentance-end-double-space nil
      tab-width 2)

(setq-default tab-wdith 2)

;; Use UTF-8 everywhere possible:
(set-language-environment 'UTF-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Do not use UTF-8 with `ansi-term`
(defun my/advise-ansi-term-coding-system ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(advice-add 'ansi-term :after #'my/advise-ansi-term-coding-system)

(defalias 'yes-or-no-p #'y-or-n-p)

;; Enable disabled commands
(put 'downcase-region 'disabled nil)

;;;
;;; Functions
;;;

(defun my/reload-init ()
  "Reload init.el using straight.el's transaction system."
  (interactive)
  (straight-transaction
    (straight-mark-transaction-as-init)
    (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

(defun my/eval-buffer ()
  "Evaluate current buffer with straight.el's transaction system."
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (straight-transaction
    (if (null buffer-file-name)
        (eval-buffer)
      (when (string= buffer-file-name user-init-file)
        (straight-mark-transaction-as-init))
      (load-file buffer-file-name)))
  (message "Evaluating %s... done." (buffer-name)))

(defmacro add-zeal-or-dash-docs (mode docs)
  "Add documentation for Zeal or Dash, depending on which is available."
  (let* ((app-name (if (eq system-type 'darwin) "dash" "zeal"))
         (app      (intern app-name))
         (app-list (intern (concat app-name "-at-point"))))
    (type-of mode)
    `(with-eval-after-load ',app
       (add-to-list ',app-list '(,mode . ,docs)))))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are
not at the end of the line, then comment current line.  Replaces
default behaviour of comment-dwim, when it inserts comment at the
end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun lookup-password (host user port)
  (require 'auth-source)
  (require 'auth-source-pass)
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
        (let ((secretf (plist-get (car auth) :secret)))
          (if secretf
              (funcall secretf)
            (error "Auth entry for %s@%s:%s has no secret!"
                   user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

(defvar saved-window-configuration nil)

(setq load-path
      (append (delete-dups load-path)
              '("~/.emacs.d/lisp")))

(defun filter (f args)
  (let (result)
    (dolist (arg args)
      (when (funcall f arg)
        (setq result (cons arg result))))
    (nreverse result)))

(defun my/indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my/sort-sexps-in-region (beg end)
  "Can be handy for sorting out duplicates.

Sorts the sexps from BEG to END. Leaves the point at where it
couldn't figure things out (ex: syntax errors)."
  (interactive "r")
  (let ((input (buffer-substring beg end))
        list last-point form result)
    (save-restriction
      (save-excursion
        (narrow-to-region beg end)
        (goto-char (point-min))
        (setq last-point (point-min))
        (setq form t)
        (while (and form (not (eobp)))
          (setq form (ignore-errors (read (current-buffer))))
          (when form
            (add-to-list
             'list
             (cons
              (prin1-to-string form)
              (buffer-substring last-point (point))))
            (setq last-point (point))))
        (setq list (sort list (lambda (a b) (string< (car a) (car b)))))
        (delete-region (point-min) (point))
        (insert (mapconcat 'cdr list "\n"))))))


;;;
;;; Key Bindings
;;;

(bind-key "M-;"     #'comment-dwim-line)
(bind-key "C-c n"   #'narrow-or-widen-dwim)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x C-r" #'revert-buffer)
(bind-key "M-:"     #'pp-eval-expression)

;;;
;;; Libraries
;;;

(use-package async           :defer t)
(use-package diminish        :demand t)
(use-package el-mock         :defer t)
(use-package elisp-refs      :defer t)
(use-package epl             :defer t)
(use-package f               :defer t)
(use-package fringe-helper   :defer t)
(use-package ghub            :defer t)
(use-package ghub+           :defer t)
(use-package ht              :defer t)
(use-package loop            :defer t)
(use-package marshal         :defer t)
(use-package narrow-indirect :defer t)
(use-package parsebib        :defer t)
(use-package pkg-info        :defer t)
(use-package popup           :defer t)
(use-package popup-pos-tip   :defer t)
(use-package pos-tip         :defer t)
(use-package request         :defer t)
(use-package s               :defer t)
(use-package tablist         :defer t)
(use-package uuidgen         :defer t)
(use-package web             :defer t)
(use-package web-server      :defer t)
(use-package websocket       :defer t)
(use-package with-editor     :defer t)
(use-package xml-rpc         :defer t)

;;;
;;; UI
;;;


(use-package afternoon-theme)

(use-package all-the-icons
  :if (display-graphic-p)
  :defer t)

(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(setq default-frame-alist '((height . 50)
			    (width  . 120)
			    (font   . "Fira Code-10")))

(defun my/unload-themes ()
  "Unload any loaded themes."
  (mapc #'disable-theme custom-enabled-themes))

;; When loading a different theme, first unload any loaded themes so
;; that they do not leave stray customizations behind.
(advice-add 'load-theme :before #'my/unload-themes)

(defun my/ui-settings (&rest frame)
  "Setup the UI settings for a newly created `frame'."
  (let ((f (or (car frame) (selected-frame))))
    (setq-default cursor-type 'box)
    (when (display-graphic-p)
      (if (fboundp 'toggle-frame-maximized)
          (add-hook 'emacs-startup-hook #'toggle-frame-maximized)
        (set-frame-size f 120 50))
      ;; Apply font based on what's available on system in priority
      ;; order. Keep the current frame size, but apply font to all
      ;; frames.
      (cond
       ((find-font (font-spec :name "Fira Code"))
        (set-frame-font "Fira Code-10" nil t))
       ((find-font (font-spec :name "Source Code Variable"))
        (set-frame-font "Source Code Variable-10" nil t))
       ((find-font (font-spec :name "Source Code Pro"))
        (set-frame-font "Source Code Pro-10" nil t))
       ((find-font (font-spec :name "Fantasque Sans Mono"))
        (set-frame-font "Fantasque Sans Mono-12" nil t))
       ((find-font (font-spec :name "Anonymous Pro"))
        (set-frame-font "Anonymous Pro-11" nil t))
       ((find-font (font-spec :name "Hack"))
        (set-frame-font "Hack-10" nil t))
       ((find-font (font-spec :name "3270-Medium"))
        (set-frame-font "3270-Medium-12" nil t))
       ((find-font (font-spec :name "courier"))
        (set-frame-font "courier-10" nil t)))

      (load-theme 'afternoon-theme t)
      (setq powerline-default-separator 'arrow-fade)
      (spaceline-emacs-theme))))

(add-hook 'after-make-frame-functions #'my/ui-settings t)

;;;
;;; Packages
;;;

;;;[START_USE_PACKAGE]

(use-package abbrev
  :straight f
  :defer 5
  :diminish
  :init
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t)
  :config
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
  :hook ((text-mode prog-mode erc-mode LaTeX-mode) . abbrev-mode)
  (expand-load . (lambda ()
                   (add-hook 'expand-expand-hook 'indent-according-to-mode)
                   (add-hook 'expand-jump-hook 'indent-according-to-mode))))

(use-package aggressive-indent
  :diminish aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package anzu
  :commands (global-anzu-mode)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :bind (:map isearch-mode-map
	      ([remap isearch-query-replace]  . anzu-isearch-query-replace)
	      ([remap isearch-query-replace-regexp]  . anzu-isearch-query-replace-regexp))
  :init
  (setq anzu-mode-lighter ""
	anzu-deactivate-region t
	anzu-search-threshold 1000
	anzu-replace-threshold 50
	anzu-replace-to-string-separator " => ")
  :config
  (global-anzu-mode +1))

(use-package auto-compile
  :demand t
  :init
  (setq auto-compile-display-buffer               nil
        auto-compile-mode-line-counter            t
        auto-compile-source-recreate-deletes-dest t
        auto-compile-toggle-deletes-nonlib-dest   t
        auto-compile-update-autoloads             t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  :hook (auto-compile-inhibit-comnpile . #'auto-compile-inhibit-compile-detached-git-head))

(use-package auto-fill-mode
  :straight f
  :init
  (setq comment-auto-fill-only-comments t)
  :hook ((text-mode prog-mode) . turn-on-auto-fill))

(use-package auto-yasnippet
  :disabled
  :after (yasnippet)
  :bind (("C-c y a" . aya-create)
         ("C-c y e" . aya-expand)
         ("C-c y o" . aya-open-line)))

(use-package avy
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package bm
  ;;; Visual bookmarks for emacs.
  ;;; https://github.com/joodland/bm
  :demand t
  :bind (("C-c b b" . bm-toggle)
         ("C-c b n" . bm-next)
         ("C-c b p" . bm-previous))
  :defines (bm-repository-file)
  :commands (bm-repository-load
             bm-buffer-save
             bm-buffer-save-all
             bm-buffer-restore)
  :preface
  (defun my/bm-save-all-buffers ()
    "Save all buffers and update the bm repository.
Used as hook function for `kill-emacs-hook', because
`kill-buffer-hook' is not called when Emacs is killed."
    (bm-buffer-save-all)
    (bm-repository-save))
  :init
  ;; Restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)
  :config
  ;; If you would like to cycle through bookmarks in all open buffers, uncomment the following line:
  ;; (setq bm-cycle-all-buffers t)

  ;; Save bookmarks.
  (setq-default bm-buffer-persistence t)

  :hook (after-init . bm-repository-load)
  :hook ((find-file-hooks after-revert) . bm-buffer-restore)
  :hook ((kill-buffer after-save vc-before-checkin) . bm-buffer-save)
  :hook (kill-emacs . my/bm-save-all-buffers))

(use-package company
  :defer t
  :config
  (global-company-mode))

(use-package counsel
  :after swiper
  :bind (("M-x"	  . counsel-M-x)
         ("C-x C-f"	  . counsel-find-file)
         ("C-h f"	  . counsel-describe-function)
         ("C-h v"	  . counsel-describe-variable)
         ("C-h i"	  . counsel-info-lookup-symbol)
         ("C-h u"	  . counsel-unicode-char)
         ("C-c k"	  . counsel-rg)
         ("C-x l"	  . counsel-locate)
         ("C-c g"	  . counsel-git-grep)
         ("C-c h i"	  . counsel-imenu)
         ("C-x p"	  . counsel-list-processes)
         ("M-y"         . counsel-yank-pop))
  :config
  (ivy-set-actions
   'counsel-find-file
   '(("j" find-file-other-window "other")))
  (ivy-set-actions 'counsel-git-grep
                   '(("j" find-file-other-window "other"))))

(use-package crux
  ;; https://github.com/bbatsov/crux
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-k"                          . crux-smart-kill-line)
         ("M-o"                          . crux-smart-open-line)
         ("C-c f"                        . crux-recentf-find-file)
         ("C-c D"                        . crux-delete-file-and-buffer)
         ("C-c R"                        . crux-rename-file-and-buffer)
         ("C-c I"                        . crux-find-user-init-file)
         ("C-c S"                        . crux-find-shell-int-file)
         ([remap kill-whole-line]        . crux-kill-whole-line)))

(use-package css-mode
  :mode "\\.css\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package dash
  :defer t
  :config
  (dash-enable-font-lock))

(use-package dash-at-point
  :if (eq system-type 'darwin)
  ;; :ensure-system-package "dash"
  :defer t)

(use-package dired
  :straight f
  :config
  (require 'dired-x)
  (use-package dired-details
    :after (dired)
    :init
    (setq-default dired-details-hidden-string "--- ")
    :config
    (dired-details-install))

  (when (memq system-type '(gnu/linux darwin))
    ;; Show human-readable file sizes and sort numbers properly
    (setq-default dired-listing-switches "-alhv")))

(use-package edit-server
  :if window-system
  :defer 5
  :config
  (when (not (process-status "edit-server"))
    (edit-server-start)))

(use-package edit-var
  :straight f
  :bind ("C-c e v" . edit-variable))

(use-package editorconfig
  :disabled
  :if (file-exists-p (expand-file-name ".editorconfig" (getenv "HOME")))
  ;; :require-system-package "EditorConfig"
  :defer t
  :config
  ;; Always the built-in core library instead of any EditorConfig executable to get properties.
  (set-variable 'editorconfig-get-properties-function #'editor-config-core-get-properties-hash)
  (editorconfig-mode 1))

(use-package eldoc
  :straight f
  :diminish
  :hook ((c-mode-common emacs-lisp-mode) . eldoc-mode))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)		; only needed on MacOS
  :defines (exec-patth-from-shell-variables)
  :config
  (mapc (lambda (variable)
          (add-to-list 'exec-path-from-shell-variables variable))
        '("ALTERNATE_EDITOR"
          "CDPATH"
          "EDITOR"
          "GOPATH"
          "GPG_AGENT_INFO"
          "HISTFILE"
          "INFOPATH"
          "LANG"
          "LC_ALL"
          "SSH_AUTH_SOCK"))
  (exec-path-from-shell-initialize))

(use-package expand-region
  :defer t
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package ffap
  :defer t
  :bind ("C-c v" . ffap))

(use-package fullframe
  :defer t
  :init
  (autoload #'fullframe "fullframe"))

(use-package gitignore-mode
  :defer t)

(use-package git-gutter-fringe
  :defer t
  :diminish git-gutter-mode
  :preface
  (defun my/git-gutter-refresh-all ()
    "Refresh all git-gutter windows."
    ;; Don't refresh until after fully initialized
    (when after-init-time
      (git-gutter:update-all-windows)))
  :config
  (global-git-gutter-mode +1)
  (with-eval-after-load 'magit
    ;; Refresh git-gutter buffers after Magit status changes
    (add-hook 'magit-post-refresh-hook #'my/git-gutter-refresh-all)))

(use-package git-timemachine
  :bind ("C-c x" . git-timemachine-toggle))

(use-package google-this
  :bind-keymap ("C-c /" . google-this-mode-submap)
  :bind* ("M-SPC" . google-this-search)
  :bind (:map google-this-mode-map
              ("/" . google-this-search)))

(use-package goto-chg
  :bind (("C-c b ," . goto-last-change)
         ("C-c b ." . goto-last-change-reverse)))

(use-package graphviz-dot-mode
  ;; :ensure-system-package graphviz
  :mode "\\.dot\\'")

(use-package grep
  :if (executable-find "grep")
  :bind (("M-s n" . find-name-dired)
         ("M-s F" . find-grep)
         ("M-s G" . grep)
         ("M-s d" . find-grep-dired)))

(use-package hl-todo
  :defer t
  :bind (:map hl-todo-mode-map
              ("C-x P" . hl-todo-previous)
              ("C-x N" . hl-todo-next)
              ("C-x O" . hl-todo-occur))
  :hook ((prog-mode . hl-todo-mode)))

(use-package imenu
  :defer t
  :config
  (use-package imenu-anywhere
    :after (imenu)
    :bind (("C-c i" . imenu-anywhere)
           ("s-i"   . imenu-anywhere)))
  :hook (emacs-lisp . imenu-add-menubar-index))

(use-package indent
  :straight f
  :commands indent-according-to-mode)

(use-package indent-shift
  :straight f
  :bind (("C-c <" . indent-shift-left)
         ("C-c >" . indent-shift-right)))

(use-package info
  :bind ("C-h C-i" . info-lookup-symbol)
  :preface
  (defun my/Info-mode-config ()
    "Info-mode configuration settings."
    (setq bufer-face-mode-face '(:family "Bookerly"))
    (buffer-face-mode)
    (text-scale-adjust 1))
  :hook (Info-mode . my/Info-mode-config)
  :config
  (use-package info-look
    :after (info)
    :defer t
    :config
    (autoload 'info-lookup-add-help "info-look")
    (use-package info-lookmore
      :after (info info-look)
      :config
      (info-lookmore-elisp-cl)
      (info-lookmore-apropos-elisp)
      (info-lookmore-elisp-userlast)
      (with-eval-after-load 'gnus
        (info-lookmore-elisp-gnus)))))

(use-package isearch
  :disabled
  :straight f
  :bind (("C-M-r" . isearch-backward-other-window)
         ("C-M-s" . isearch-forward-other-window))
  :bind (:map isearch-mode-map
              ("C-c" . isearch-toggle-case-fold)
              ("C-t" . isearch-toggle-regexp)
              ("C-^" . isearch-edit-string)
              ("C-i" . isearch-complete))
  :preface
  (defun isearch-backward-other-window ()
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (call-interactively 'isearch-backward))

  (defun isearch-forward-other-window ()
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (call-interactively 'isearch-forward)))

(use-package ispell
  :no-require t)

(use-package ivy
  :defer 5
  :diminish
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("M-H"   . ivy-resume))
  :bind (:map ivy-minibuffer-map
              ("<tab>" . ivy-alt-done)
              ("SPC"   . ivy-alt-done-or-space)
              ("C-d"   . ivy-done-or-delete-char)
              ("C-i"   . ivy-partial-or-done)
              ("C-r"   . ivy-previous-line-or-history)
              ("M-r"   . ivy-reverse-i-search)
              ("M-j"   . my/ivy-yank-whole-word)
              ("M-y"   . ivy-next-line))
  :bind (:map ivy-switch-buffer-map
              ("C-k" . ivy-switch-buffer-kill))
  :preface
  (defun ivy-done-or-delete-char ()
    (interactive)
    (call-interactively
     (if (eolp)
         #'ivy-immediate-done
       #'ivy-delete-char)))

  (defun ivy-alt-done-or-space ()
    (interactive)
    (call-interactively
     (if (= ivy--length 1)
         #'ivy-alt-done
       #'self-insert-command)))

  (defun ivy-switch-buffer-kill ()
    (interactive)
    (debug)
    (let ((bn (ivy-state-current ivy-last)))
      (when (get-buffer bn)
        (kill-buffer bn))
      (unless (buffer-live-p (ivy-state-buffer ivy-last))
        (setf (ivy-state-buffer ivy-last)
              (with-ivy-window (current-buffer))))
      (setq ivy--all-candidates (delete bn ivy--all-candidates))
      (ivy--exhibit)))

  ;; This is the value of `magit-completing-read-function', so that we see
  ;; Magit's own sorting choices.
  (defun my/ivy-completing-read (&rest args)
    (let ((ivy-sort-functions-alist '((t . nil))))
      (apply 'ivy-completing-read args)))

  ;; version of ivy-yank-word to yank from start of word
  (defun my/ivy-yank-whole-word ()
    "Pull next word from buffer into search string."
    (interactive)
    (let (amend)
      (with-ivy-window
        ;;move to last word boundary
        (re-search-backward "\\b")
        (let ((pt (point))
              (le (line-end-position)))
          (forward-word 1)
          (if (> (point) le)
              (goto-char pt)
            (setq amend (buffer-substring-no-properties pt (point))))))
      (when amend
        (insert (replace-regexp-in-string "  +" " " amend)))))

  :init
  (setq ivy-count-format             ""
        ivy-dynamic-exhibit-delay-ms 200
        ivy-height                   10
        ivy-initial-inputs-alist     nil
        ivy-magic-tilde              nil
        ivy-re-builders-alist        '((t . ivy--regex-ignore-order))
        ivy-use-selectable-promnpt   t
        ivy-use-virtual-buffers      t
        ivy-wrap                     t)
  :config
  (use-package ivy-pass
    :demand t
    :after (ivy)
    :commands ivy-pass)

  (use-package ivy-rich
    :demand t
    :after (ivy)
    :config
    (ivy-set-display-transformer 'ivy-switch-buffer
                                 'ivy-rich-switch-buffer-transformer)
    (setq ivy-virtual-abbreviate 'full
          ivy-rich-switch-buffer-align-virtual-buffer t
          ivy-rich-path-style 'abbrev))

  (use-package ivy-hydra
    :demand t
    :after (ivy hydra))

  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  (with-eval-after-load 'flx
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (use-package json-reformat
    :after (json-mode))

  (use-package json-snatcher
    :after (json-mode)))

(use-package lisp-mode
  :straight f
  :preface
  (defun my/lisp-mode-save-config ()
    "Check parenthesis in Lisp-like buffers."
    (when (memq major-mode '(lisp-mode emacs-lisp-mode))
      (check-parens)))
  :hook (after-save . my/lisp-mode-save-config)
  :preface
  (defun my/lisp-mode-config ()
    "My Lisp mode customizations"
    (abbrev-mode +1)
    (add-to-list 'imenu-generic-expression
                 '("Used Packages"
                   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  :hook ((emacs-lisp-mode lisp-mode) . my/lisp-mode-config)
  :config
  (dolist (mode '(ielm-mode
                  inferior-emacs-lisp-mode
                  inferior-lisp-mode
                  lisp-interaction-mode
                  lisp-mode
                  emacs-lisp-mode))
    (font-lock-add-keywords
     mode
     '(("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face
           nil t))))))

(use-package elint
  :after (emacs-lisp-mode)
  :commands (elint-initialize elint-current-buffer)
  :bind ("C-c e E" . my/elint-current-buffer)
  :preface
  (defun my/elint-current-buffer ()
    (interactive)
    (elint-initialize)
    (elint-current-buffer))
  :config
  (add-to-list 'elint-standard-variables 'current-prefix-arg)
  (add-to-list 'elint-standard-variables 'command-line-args-left)
  (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
  (add-to-list 'elint-standard-variables 'emacs-major-version)
  (add-to-list 'elint-standard-variables 'window-system))

(use-package elisp-depend
  :after (emacs-lisp-mode)
  :commands elisp-depend-print-dependencies)

(use-package elisp-docstring-mode
  :after (emacs-lisp-mode)
  :commands elisp-docstring-mode)

(use-package elisp-slime-nav
  :after (emacs-lisp-mode)
  :diminish
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point))

(use-package elmacro
  :after (emacs-lisp-mode)
  :bind (("C-c m e" . elmacro-mode)
         ("C-x C-)" . elmacro-show-last-macro)))

(use-package lsp-mode
  :config
  (use-package lsp-ui
    :after (lsp-mode)
    :config
    (with-eval-after-load 'flycheck
      (require 'lsp-flycheck))
    (with-eval-after-load 'imenu
      (add-hook 'lsp-after-open-hook #'lsp-enable-imenu))
    :hook (lsp-mode . lsp-ui-mode)))

(use-package macrostep
  :after (emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c e m" . macrostep-expand)))

(use-package magit
  :bind (("C-c g" . magit-status))
  :preface
  (defun my/magit-log-edit-config ()
    "Configuration for editing Git log messages."
    (setq fill-column 72)
    (when (featurep 'flyspell)
      (flyspell-mode t))
    (turn-on-auto-fill))
  :hook (magit-log-edit-mode . my/magit-log-edit-config)
  :config
  (setq magit-set-upstream-on-push      t
        magit-commit-summary-max-length 70)
  ;; no longer need vc-git
  (delete 'Git vc-handled-backends)

  ;; Check excessively long summary lines in commit messages
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line))

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :init
  (setq markdown-command (cond ((executable-find "multimarkdown") "multimarkdown")
                               ((executable-find "markdown")      "markdown")
                               (t                                 nil)))
  :config
  (use-package markdown-preview-mode
    :after markdown-mode
    :if (executable-find "multimarkdown")
    :init
    (setq markdown-preview-stylesheets
          (list (concat "https://github.com/dmarcotte/github-markdown-preview/"
                        "blob/master/data/css/github.css"))))

  (with-eval-after-load 'imenu
    (require 'imenu)
    (setq imenu-auto-rescan t)
    (add-hook 'markdown-mode-hook #'imenu-add-menubar-index))
  (with-eval-after-load 'flyspell-mode
    ;; Turn on `flyspell-mode` when available.
    (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1))))
  :hook (markdown-mode . visual-line-mode))

(use-package minibuffer
  :disabled
  :straight f
  :preface
  (defun my/minibuffer-setup-hook ()
    "Setup minibuffer for use."
    ;; Disable garbage collection while in minibuffer to avoid stalls.

    ;; TODO(markcol): setting `gc-cons-threshold' doesn't work in Emacs 27?!
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my/minibuffer-exit-hook ()
    "Restore minibuffer setup changes."
    ;; Restore garbage collection afer exiting the minibuffer.
    (setq gc-cons-threshold 800000))
  :hook (minibuffer-setup . my/minibuffer-setup-hook)
  :hook (minibuffer-exit  . my/minibuffer-exit-hook))

(use-package move-text
  :defer t
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)))

(use-package org
  :straight (org-plus-contrib
             :type git :repo "https://code.orgmode.org/bzg/org-mode.git"
             :local-repo "org" :files (:defaults "contrib/lisp/*.el"))
  ;; TODO(markcol): configure org-crypt
  :config
  ;; org-ql and org-agenda-ng are dependencies for org-sidebar, but
  ;; not on MELPA.
  (use-package org-ql
    :straight (org-ql :type git :host github :repo "alphapapa/org-agenda-ng"))

  (use-package org-agenda-ng
    :straight (org-agenda-ng :type git :host github :repo "alphapapa/org-agenda-ng"))

  (use-package org-sidebar
    :after (org org-ql org-agenda-ng)
    :straight (org-sidebar :type git :host github :repo "alphapapa/org-sidebar")
    :after org
    :commands (org-sidebar)
    :bind ("C-c o s" . org-sidebar))

  (use-package org-journal
    :after org
    :bind (("C-c o t" . org-journal-new-entry)
           ("C-c o y" . journal-file-yesterday))
    :init
    ;; TODO(markcol): could be set in settings.el?
    (setq org-journal-dir "~/Sync/shared/journal/2018/"
          org-journal-file-format "%Y%m%d"
          org-journal-date-format "%e %b %Y (%A)"
          org-journal-time-format "")
    :preface
    (defun get-journal-file-yesterday ()
      "Gets filename for yesterday's journal entry."
      (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
             (daily-name (format-time-string "%Y%m%d" yesterday)))
        (expand-file-name (concat org-journal-dir daily-name))))

    (defun journal-file-yesterday ()
      "Creates and load a file based on yesterday's date."
      (interactive)
      (find-file (get-journal-file-yesterday))))

  (setq org-todo-keywords
        '("TODO" "|" "CANCELLED" "DONE")))

(use-package paredit
  :diminish
  :commands (paredit-mode paredit-backward-delete paredit-close-round paredit-newline)
  :bind (:map lisp-mode-map
              ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map
              ("<return>" . paredit-newline))
  :preface
  (defun my/paredit-mode-hook ()
    "Paredit mode customizations."
    (require 'eldoc)
    (eldoc-add-command 'paredit-backward-delete
                       'paredit-close-round)
    (paredit-mode))
  :config
  (use-package paredit-ext
    :straight f
    :load-path "lisp"
    :after (paredit))

  :hook ((lisp-mode emacs-lisp-mode) . my/paredit-mode-hook))

(use-package smartparens
  :disabled
  :bind (:map smartparens-mode-map
              ;; Movement and navigation
              ("C-M-f"       . sp-forward-sexp)
              ("C-M-b"       . sp-backward-sexp)
              ("C-M-u"       . sp-backward-up-sexp)
              ("C-M-d"       . sp-down-sexp)
              ("C-M-p"       . sp-backward-down-sexp)
              ("C-M-n"       . sp-up-sexp)
              ;; Deleting and killing
              ("C-M-k"       . sp-kill-sexp)
              ("C-M-w"       . sp-copy-sexp)
              ;; Depth changing
              ("M-s"         . sp-splice-sexp)
              ("M-<up>"      . sp-splice-sexp-killing-backward)
              ("M-<down>"    . sp-splice-sexp-killing-forward)
              ("M-r"         . sp-splice-sexp-killing-around)
              ("M-?"         . sp-convolute-sexp)
              ;; Barfage & Slurpage
              ("C-)"         . sp-forward-slurp-sexp)
              ("C-<right>"   . sp-forward-barf-sexp)
              ("C-}"         . sp-backward-up-sexp)
              ("C-<left>"    . sp-forward-barf-sexp)
              ("C-("         . sp-backward-slurp-sexp)
              ("C-M-<left>"  . sp-backward-slurp-sexp)
              ("C-{"         . sp-backward-barf-sexp)
              ("C-M-<right>" . sp-backward-barf-sexp)
              ("M-S"         . sp-split-sexp)
              ("M-J"         . sp-join-sexp)
              ("C-M-t"       . sp-transpose-sexp))
  :bind (:map smartparens-strict-mode-map
              ("M-q"         . sp-indent-defun)
              ("C-j"         . sp-newline))
  :config
  (require 'smartparens-config)

  (setq smartparens-strict-mode t
        sp-autoskip-closing-pair 'always
        sp-base-key-bindings 'paredit
        sp-hybrid-kill-entire-symbol nil)

  (sp-local-pair 'emacs-lisp-mode "'" nil :when '(sp-in-string-p))
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  (sp-with-modes '(html-mode sgml-mode web-mode)
                 (sp-local-pair "<" ">"))
  (sp-with-modes sp--lisp-modes
                 (sp-local-pair "(" nil :bind "C-("))
  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
                 (sp-local-pair "*" "*" :bind "C-*")
                 (sp-local-tag "2" "**" "**")
                 (sp-local-tag "s" "```scheme" "```")
                 (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1)
  :hook ((lisp-mode emacs-lisp-mode) . smartparens-strict-mode))

(use-package projectile
  :after ivy
  :defer 5
  :diminish
  :bind* ("C-c TAB" . projectile-find-other-file)
  :bind-keymap ("C-c p" . projectile-command-map)
  :preface
  (defun my/projectile-invalidate-cache (&rest _args)
    ;; We ignore the args to `magit-checkout'.
    (projectile-invalidate-cache nil))

  ;; My answer to question to in http://www.reddit.com/r/emacs about creating an org directory
  ;; when switching to a projectile project to take notes for the project.
  ;; https://www.reddit.com/r/emacs/comments/8fze69/possible_to_configure_orgmode_capture_to_always/?st=jgmcmd5a&sh=bf6b15dd
  (defun my/set-projectile-org-notes ()
    "Create an org directory and open 'notes.org' file When switching to a Projectile project."
    (when (projectile-project-root)
      (let ((project-org-dir (expand-file-name "org" (projectile-project-root))))
        (make-directory project-org-dir t)
        (find-file-other-window (expand-file-name "notes.org" project-org-dir)))))

  (defun my/set-projectile-root ()
    "Automatically set the LSP workspace to the current Projectile root when changing projects."
    (when lsp--cur-workspace
      (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode +1)
  (with-eval-after-load 'ivy
    (setq projectile-completion-system 'ivy))
  (with-eval-after-load 'helm
    (setq projectile-completion-system 'helm))
  (with-eval-after-load 'magit-branch
    (advice-add 'magit-checkout :after #'my/projectile-invalidate-cache)
    (advice-add 'magit-branch-and-checkout :after #'my/projectile-invalidate-cache))
  (with-eval-after-load 'lsp
    (add-hook 'lsp-before-open-hook #'my/set-projectile-root)))

(use-package python-mode
  :mode "\\.py\\'"
  :interpreter "python"
  :bind (:map python-mode-map
              ("C-c c")
              ("C-c C-z" . python-shell))
  :preface
  (defvar python-mode-initialized nil)
  (defun my/python-mode-hook ()
    "My Python mode customizations."
    (unless python-mode-initialized
      (setq python-mode-initialized t)

      (info-lookup-add-help
       :mode 'python-mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec
       '(("(python)Python Module Index" )
         ("(python)Index"
          (lambda
            (item)
            (cond
             ((string-match
               "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
              (format "%s.%s" (match-string 2 item)
                      (match-string 1 item)))))))))
    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indent-tabs-mode nil))
  :config
  (add-zeal-or-dash-docs python-mode "python")
  :hook (python-mode . my/python-mode-hook))

(use-package rainbow-delimiters
  ;; rainbow-delimiters is a "rainbow parentheses"-like mode which
  ;; highlights delimiters such as parentheses, brackets or braces
  ;; according to their depth. Each successive level is highlighted in
  ;; a different color. This makes it easy to spot matching
  ;; delimiters, orient yourself in the code, and tell which
  ;; statements are at a given depth.
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :defer 10
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :preface
  (defun my/recentf-add-dired-directory ()
    "Add directory directory to the `recentf` list."
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :init
  (setq recentf-max-menu-items 15)
  :config
  (recentf-mode 1)
  :hook (dired-mode . my/recentf-add-dired-directory))

(use-package rect
  :straight f
  :bind ("C-c ]" . rectangle-mark-mode))

(use-package rg
  :defer t
  :if (executable-find "rg")
  ;; :ensure-system-package rg
  )

(use-package rust-mode
  :if (executable-find "rust")
  :mode "\\.rs\\'"
  :preface
  (defun my/rust-mode-hook ()
    "My Rust-mode configuration."
    (when (featurep 'flycheck)
      (flycheck-mode 1))
    (setq-local fill-column 100)
    (--each '((">=" . (?· (Br . Bl) ?≥))
              ("<=" . (?· (Br . Bl) ?≤))
              ("!=" . (?· (Br . Bl) ?≠))
              ("=>" . (?· (Br . Bl) ?➡))
              ("->" . (?· (Br . Bl) ?→)))
      (push it prettify-symbols-alist)))

  :config
  (use-package cargo
    :after (rust)
    :hook (rust-mode . cargo-minor-mode))

  (use-package lsp-rust
    :after (lsp-mode lsp-ui rust)
    :if (executable-find "rustup")
    :init
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
    :hook (rust-mode . lsp-rust-enable))

  (use-package racer
    :disabled
    :after (rust)
    :unless (featurep 'lsp-rust)
    ;; :ensure-system-package (racer . "cargo install racer")
    :init
    ;; Tell racer to use the rustup managed rust-src
    (setq racer-cmd              (executable-find "racer")
          rust-default-toolchain (car (s-split " " (-first
                                                    (lambda (line) (s-match "default" line))
                                                    (s-lines (shell-command-to-string "rustup toolchain list")))))
          rust-src-path          (concat (getenv "HOME") "/.multirust/toolchains/"
                                         rust-default-toolchain "/lib/rustlib/src/rust/src")
          rust-bin-path          (concat (getenv "HOME") "/.multirust/toolchains/"
                                         rust-default-toolchain "/bin")
          racer-rust-src-path    rust-src-path)
    (setenv "RUST_SRC_PATH" rust-src-path)
    (setenv "RUSTC" rust-bin-path)
    :config
    (with-eval-after-load 'company
      (add-to-list 'company-dabbrev-code-modes 'rust-mode)
      (add-hook 'racer-mode-hook #'company-mode))
    :hook (rust-mode . (racer-mode eldoc-mode)))

  (use-package flycheck-rust
    :after (flycheck rust)
    :config
    (flycheck-rust-setup))

  (add-zeal-or-dash-docs rust-mode "rust")
  :hook (rust-mode . my/rust-mode-hook))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package server
  :preface
  (require 'server)
  (defun my/server-enable ()
    "Start an Emacs server process if one is not already running."
    (unless server-process
      (server-start)))
  :hook (after-init . my/server-enable))

(use-package sql-indent
  :mode ("\\.sql\\'"))

(use-package string-edit
  :bind ("C-c C-'" . string-edit-at-point))

(use-package swiper
  :after ivy
  :bind (:map swiper-map
              ("M-y" . yank)
              ("M-%" . swiper-query-replace)
              ("C-." . swiper-avy)
              ("M-c" . my/swiper-mc-fixed))
  :bind (:map isearch-mode-map
              ("C-o" . swiper-from-isearch))
  :preface
  (defun my/swiper-mc-fixed ()
    (interactive)
    (setq swiper--current-window-start nil)
    (swiper-mc)))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package treemacs
  :commands (treemacs)
  :bind (([f8]          . treemacs)
         ("M-0"         . treemacs-select-window)
         ("C-c 1"       . treemacs-delete-other-windows)
         ("C-c t t"     . treemacs)
         ("C-c t T"     . treemacs)
         ("C-c t B"     . treemacs-bookmark)
         ("C-c t C-t"   . treemacs-find-file)
         ("C-c t M-t"   . treemacs-find-tag))
  :init
  (setq treemacs--persit-file               (expand-file-name "treemacs-cache" user-data-directory)
        treemacs-change-root-without-asking nil
        treemacs-collapse-dirs              (if (executable-find "python") 3 0)
        treemacs-file-event-delay           5000
        treemacs-follow-after-init          t
        treemacs-follow-recenter-distance   0.1
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-never-persist              nil
        treemacs-no-png-images              nil
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-width                      30)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (("C-c t P"    . treemacs-projectile)
         ("C-c t p"    . treemacs-projectile-toggle)))

(use-package web-mode
  :mode ("\\.html\\'"
         "\\.css\\'")
  :commands web-mode
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-enable-auto-quoting nil))

(use-package which-func
  :disabled
  :defer t
  :hook ((c-mode-common emacs-lisp-mode) . which-function-mode))

(use-package which-key
  :defer 5
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode))

(use-package whitespace
  :defer t
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing))
  :hook ((prog-mode text-mode) . whitespace-mode)
  :hook (before-save . whitespace-cleanup))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets" . snippet-mode)
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :preface
  (defun my/snippet-disable-newline ()
    "Disable newline at end of file to avoid extra newlines during expansion."
    (setq-local require-final-newline nil))
  :hook (snippet-mode . my/snippet-disable-newline)
  :config
  (yas-load-directory (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode 1))

(use-package zeal-at-point
  :if (memq system-type '(gnu/linux))
  ;; :ensure-system-package zeal
  :defer t)

;;;[END_USE_PACKAGE]

;;;
;;; Finalization
;;;

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(provide 'init)
;;; init.el ends here
