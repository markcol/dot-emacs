;; init.el --- Emacs init file -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; This is my Emacs .init file. It uses straight.el and use-package
;; for all package maangement.

;;; Code:

(defconst emacs-start-time (current-time))

(defconst user-document-directory (expand-file-name "~/Documents")
  "Directory for user documents.")

(defconst user-org-directory (expand-file-name "org" user-document-directory)
  "Directory for user ‘org-mode’ files.")

(defvar my/message-log-max-default message-log-max
  "Default value of `message-log-max'.")

(defvar my/file-name-handler-alist-default file-name-handler-alist
  "Default value of `file-name-handler-alist'.")

(defvar my/gc-cons-threshold-default gc-cons-threshold
  "Default value of `gc-cons-threshold'.")

(defvar my/gc-cons-percentage-default gc-cons-percentage
  "Default value of `gc-cons-percentage'."  )

(defvar my/saved-window-configuration nil
  "My saved window configuration.")

(setq file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold (* 512 1024 1024)
      gc-cons-percentage 0.6
      package-enable-at-startup nil)

(defun my/restore-default-values ()
  "Restore the default values of performance-critical variables."
  (setq file-name-handler-alist my/file-name-handler-alist-default
        message-log-max my/message-log-max-default
        gc-cons-threshold my/gc-cons-threshold-default
        gc-cons-percentage my/gc-cons-percentage-default)
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

;;; Straight.el configuration settings.
(setq
 ;; Read autoloads in bulk to speed up stratup.
 straight-cache-autoloads t

 ;; Detect package modifications as they are made instead of using find(1) at
 ;; init time.
 straight-check-for-modifications 'live
 ;; Use straight.el to load missing packages for `use-package` by default.
 straight-use-package-by-default t)

;;; Bootstrap straight.el
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

(eval-when-compile
  (require 'straight))

(defun my/wrap-in-straight-transaction (orig-fn &rest args)
  "Wrap ORIG-FN in a straight.el transaction.

Any arguments provided in ARGS are passed to ORIG-FN within the
transaction.

This is intended as an advising function for functions that load
the init file after Emacs initialization, such as `esup':

  (advice-add 'esup :around #'my/wrap-in-straight-transation).

For more information, refer to the straight.el documentation:
https://github.com/raxod502/straight.el#the-transaction-system."
  (unwind-protect
      (let ((straight-treat-as-init t))
        (apply orig-fn args))
    (straight-finalize-transaction)))

;;; Bootstrap `use-package`
(setq use-package-expand-minimally nil
      use-package-enable-imenu-support t
      use-package-verbose nil)
(straight-use-package 'use-package)

(use-package bind-key)
(use-package diminish)
(use-package use-package-ensure-system-package)
(use-package use-package-chords
  :disabled
  :config
  ;; Define your chord bindings in the same manner as :bind using a cons or a list of conses:
  ;; (use-package ace-jump-mode
  ;;  :chords (("jj" . ace-jump-char-mode)
  ;;           ("jk" . ace-jump-word-mode)
  ;;           ("jl" . ace-jump-line-mode)))
  (key-chord-mode 1))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish))


;;;
;;; Settings
;;;

(require 'cl-lib)

;; Create any missing directories
(dolist (dir (list user-document-directory user-org-directory))
  (make-directory dir t))

;; Needs to be used before importing settings.el
(use-package no-littering
  :defines (no-littering-var-directory no-littering-etc-directory)
  :config
  (cl-letf (((symbol-function 'etc)
             (symbol-function #'no-littering-expand-etc-file-name))
            ((symbol-function 'var)
             (symbol-function #'no-littering-expand-var-file-name)))
    (with-no-warnings ; some of these variables haven't been defined yet
      (setq server-auth-dir                 (etc "server-auth/"))
      (setq slime-repl-history-file         (var "slime-history.eld"))))
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(load (expand-file-name "settings" user-emacs-directory) :noerror)

(setq show-paren-delay 0)
(show-paren-mode 1)
(size-indication-mode 1)
(delete-selection-mode 1)
(mouse-wheel-mode 1)

(setq auto-revert-verbose             nil ; no messages about reverted files
      auto-save-default               nil
      auto-window-vscroll             nil
      browse-url-browser-function     #'browse-url-chromium
      c-basic-offset                  4
      confirm-kill-emacs              #'y-or-n-p
      css-indent-offset               2
      echo-keystrokes                 0.1
      global-auto-revert-mode         t
      mouse-drag-copy-region          t
      scroll-conservatively           100000
      scroll-margin                   0
      scroll-preserve-screen-position 1
      sentence-end-double-space       nil
      vc-follow-symlinks              t)

(setq-default tab-wdith 2)

;; Use UTF-8 everywhere possible:
(set-language-environment   'UTF-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system       'utf-8)

;; Do not use UTF-8 with `ansi-term`
(defun my/advise-ansi-term-coding-system ()
  "Set coding system for ANSI Term buffers."
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(advice-add 'ansi-term :after #'my/advise-ansi-term-coding-system)

;; History navigation in shell buffers
(with-eval-after-load "comint"
  (define-key comint-mode-map [(control ?p)] 'comint-previous-input)
  (define-key comint-mode-map [(control ?n)] 'comint-next-input))

(defalias 'yes-or-no-p #'y-or-n-p)

;; Enable some disabled commands
(dolist (cmd '(downcase-region upcase-region narrow-to-region narrow-to-page))
  (when (get cmd 'disabled)
    (put cmd 'disabled nil)
    (message "Disabled command '%s' enabled." cmd)))

(setq load-path
      (append (delete-dups load-path)
              '("~/.emacs.d/lisp")))

;;;
;;; Functions
;;;

(defun my/get-buffers-with-minor-mode (minor-mode)
  "Return the list of buffers in which MINOR-MODE is active."
  (interactive)
  (let ((minor-mode-buffers))
    (dolist (buffer (buffer-list) minor-mode-buffers)
      (when (memq minor-mode (my/get-active-minor-modes buffer))
        (push buffer minor-mode-buffers)))))

(defun my/get-active-minor-modes (&optional buffer)
  "Return list of minor modes active in the current buffer.

 If BUFFER is non-nil, it is searched instead the current buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
        (active-modes))
    (with-current-buffer buffer
      (dolist (mode minor-mode-list active-modes)
        (condition-case nil
            (if (and (symbolp mode) (symbol-value mode))
                (add-to-list 'active-modes mode))
          (error nil))))))

(defun ensure-space (direction)
  "Ensure that there is a space in the given DIRECTION."
  (let* ((char-fn (cond
                   ((eq direction :before)
                    #'char-before)
                   ((eq direction :after)
                    #'char-after)))
         (char-result (funcall char-fn)))
    (unless (and (not (eq char-result nil)) (string-match-p " " (char-to-string char-result)))
      (insert " "))
    (when (looking-at " ")
      (forward-char))))(defun ensure-space (direction)
  (let* ((char-fn (cond
                   ((eq direction :before)
                    #'char-before)
                   ((eq direction :after)
                    #'char-after)))
         (char-result (funcall char-fn)))
    (unless (and (not (eq char-result nil)) (string-match-p " " (char-to-string char-result)))
      (insert " "))
    (when (looking-at " ")
      (forward-char))))

(defun self-with-space ()
  "Insert character and add a space."
  (interactive)
  (call-interactively #'self-insert-command)
  (ensure-space :after))

(defun pad-equals ()
  "Insert character and pad with spaces."
  (interactive)
  (if (nth 3 (syntax-ppss))
      (insert "=")
    (cond ((looking-back "=[[:space:]]" nil)
           (delete-char -1))
          ((looking-back "[^#/|!<>+~]" nil)
           (ensure-space :before)))
    (call-interactively #'self-insert-command)
    (ensure-space :after)))

(defun my/disable-mode-temporarily (mode orig-fun &rest args)
  "Disable MODE before calling ORIG-FUN with ARGS; re-enable afterwards."
  (let ((was-initially-on (when (symbol-value mode)
                            (prog1
                                t
                              (funcall mode -1)))))
    (prog1
        (apply orig-fun args)
      (when was-initially-on
        (funcall mode 1)))))

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

(defmacro my/add-zeal-or-dash-docs (mode docs)
  "Add documentation for Zeal or Dash, depending on which is available.
MODE is name of a programming mode. DOCS is a string which
contains the name of the help documents which should be loaded for the
given MODE."
  (let* ((app-name (if (eq system-type 'darwin) "dash" "zeal"))
         (app      (intern app-name))
         (app-list (intern (concat app-name "-at-point"))))
    (type-of mode)
    `(with-eval-after-load ',app
       (add-to-list ',app-list '(,mode . ,docs)))))

(defun my/comment-dwim-line (&optional arg)
  "Replacement for the `comment-dwim' command.
If no region is selected and current line is not blank and we are
not at the end of the line, then comment current line. Replaces
default behaviour of `comment-dwim', when it inserts comment at the
end of the line.

ARG is passed to the `comment-dwim' function but is not used."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p))
           (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun my/narrow-or-widen-dwim (p)
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

(defun my/lookup-password (host user port)
  "Lookup password for the given HOST USER PORT."
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

(defun filter (f args)
  "Apply F to the list ARGS, removing items where F nil.
F is a function that takes a single argument and returns a
non-nil value if the argument should be included in the filtered
list. The list of filtered arguments is returned in the same
order it was given."
  (let (result)
    (dolist (arg args)
      (when (funcall f arg)
        (setq result (cons arg result))))
    (nreverse result)))

(defun my/indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun my/indent-region-or-buffer ()
  "Indent selected region or the entire buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (point-min) (point-max)))))

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

(defun my/sort-use-package-declarations (beg end)
  "Sort all `use-package' declarations in the region.
Sorts the `use-package' declarations from BEG to END. Leaves the
point at where it couldn't figure things out (ex: syntax
errors)."
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
        (insert (mapconcat (lambda (s) (string-trim (cdr s))) list "\n\n"))
        (insert "\n")))))

;;;
;;; Key Bindings
;;;

(bind-key [remap comment-dwim] #'my/comment-dwim-line)
(bind-key "C-c n"              #'my/narrow-or-widen-dwim)
(bind-key "C-x C-r"            #'revert-buffer)
(bind-key "C-M-\\"             #'my/indent-region-or-buffer)

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
(use-package fullframe       :defer t)
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

(defun my/unload-themes (&rest args)
  "Unload any loaded custom themes.
Any ARGS passed in are ignored."
  (when custom-enabled-themes
    (mapc #'disable-theme custom-enabled-themes)))

;; When loading a new theme, first unload any loaded themes so
;; that they do not leave stray customizations behind.
(advice-add 'load-theme :before #'my/unload-themes)

(when (display-graphic-p)
  ;; garbage collect when frame loses focus
  (add-hook 'focus-out-hook #'garbage-collect))

(defvar my/preferred-fonts '(("Fira Code" . -2)
                             ("Fantasque Sans Mono" . 0)
                             ("Source Code Variable" . -2)
                             ("Source Code Pro" . -2)
                             ("Anonymous Pro" . -1)
                             ("Hack" . -2)
                             ("3270-Medium" . 0))
  "List of preferred (font-name . point-size-adjustment) in priority order.")

(defvar my/installed-fonts (filter (lambda (x)
                                     (find-font (font-spec :name (car x))))
                                   my/preferred-fonts)
  "List of installed (font-name . point-size-adjustment) in priority order.")

(defun my/font-spec (size)
  "Return the best 'font-spec' based on what is installed.
SIZE is the default font size. The selected font spec may use a
different font size based on relative apperance."
  (if my/installed-fonts
      (let ((name  (car (car my/installed-fonts)))
            (adj  (cdr (car my/installed-fonts))))
        (format "%s-%d" name (+ size adj)))
    (format "Courier-%d" size)))

(defvar my/theme-name 'material
  "Name of preferred theme to use.")

(use-package material-theme)
(use-package spaceline
  :init
  (setq powerline-default-separator 'arrow)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off))
(use-package all-the-icons)
(use-package all-the-icons-ivy
  :after (ivy)
  :init
  (setq all-the-icons-ivy-file-commands
        '(counsel-file-jump
          counsel-find-file
          counsel-projectile-find-dir
          counsel-projectile-find-file
          counsel-recentf))
  :config
  (all-the-icons-ivy-setup))

(when (memq window-system '(mac ns))
  ;; Mac-specific settings
  (setq mac-option-modifier       nil
        mac-command-modifier      'meta
        x-select-enable-clipboard t
        ns-use-native-fullscreen  nil
        alert-default-style       'growl)
  (menu-bar-mode 1)
  (when (executable-find "python3")
    (setq-default org-babel-python-command "python3"))
  (setenv "LC_ALL"   "en_US.UTF-8")
  (setenv "LC_CTYPE" "en_US.UTF-8")
  (setenv "LANG"     "en_US.UTF-8"))

(defun my/apply-ui-settings (&rest frame)
  "Setup the UI settings for a newly created FRAME."
  (interactive)
  (when (display-graphic-p)
    ;; Size frame based on the screen resolution
    (let ((frame  (or (car frame) (selected-frame)))
          (ptsize (if (>= (display-pixel-height) 1800) 15 12))
          (height (if (>= (display-pixel-height) 1800) 150 120)))
      (set-face-attribute 'default nil :height (* ptsize 10))
      (set-frame-position frame 0 0)
      (set-frame-font (my/font-spec ptsize) nil t)
      (set-frame-size frame 120 (/ (- (display-pixel-height) height) (frame-char-height frame))))

    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
    (custom-theme-set-faces
     my/theme-name
     `(org-block-begin-line ((t (:underline "#a7a6aa" :height 0.8))))
     `(org-block-begin-line ((t (:underline "#a7a6aa" :height 0.8)))))
    (setq powerline-default-separator 'arrow-fade)
    (setq-default cursor-type 'box))

  (load-theme my/theme-name t)
  (spaceline-emacs-theme))

;; Apply my/ui-settings after making a new frame from a deamonized
;; client or after Emacs initialization.
(dolist (hook '(after-make-frame-functions window-setup-hook))
  (add-hook hook #'my/apply-ui-settings t))

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

(use-package ansi-color
  :straight f
  :hook (messages-buffer-mode . ansi-color-for-comint-mode-on))

(use-package anzu
  :diminish anzu-mode
  :commands (global-anzu-mode)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :bind (:map isearch-mode-map
         ([remap isearch-query-replace]  . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp]  . anzu-isearch-query-replace-regexp))
  :init
  (setq anzu-deactivate-region t
        anzu-search-threshold 1000
        anzu-replace-threshold 50
        anzu-replace-to-string-separator " => ")
  :config
  (global-anzu-mode +1))

(use-package auto-compile
  :demand t
  :defines auto-compile-display-buffer auto-comppile-mode-line-counter
  :init
  (setq auto-compile-display-buffer               nil
        auto-compile-mode-line-counter            t
        auto-compile-source-recreate-deletes-dest t
        auto-compile-toggle-deletes-nonlib-dest   t
        auto-compile-update-autoloads             t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq compilation-scroll-output t)
  :hook (auto-compile-inhibit-comnpile . #'auto-compile-inhibit-compile-detached-git-head))

(use-package auto-fill-mode
  :straight f
  :init
  (setq comment-auto-fill-only-comments t)
  :hook ((text-mode prog-mode) . turn-on-auto-fill))

(use-package auto-yasnippet
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

(use-package bug-hunter
  ;; See: https://github.com/Malabarba/elisp-bug-hunter
  )

(use-package cargo
  :after (rust)
  :bind (:map rust-mode-map
         ("C-c C-b" . cargo-process-build)
         ("C-c C-v" . cargo-process-bench)
         ("C-c C-t" . cargo-process-test))
  :hook (rust-mode . cargo-minor-mode))

(use-package cmake-mode
  :ensure-system-package cmake)

(use-package cmake-project
  :commands cmake-project-mode
  :preface
  (defun my/maybe-cmake-project-hook ()
    (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
  :hook ((c-mode c++-mode) . my/maybe-cmake-project-hook))

(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort)
         ("C-d" . company-show-doc-buffer)
         ("M-." . company-show-loation))
  :config
  (global-company-mode)
  (setq company-tooltip-limit 20
        company-idle-delay   .3
        company-echo-delay   0))

(use-package company-jedi
  :if (not (eq system-type 'windows-nt))
  :after (python-mode company jedi-core)
  :defer t
  :config
  (add-to-list 'company-backends '(company-jedi company-files)))

(use-package company-lsp
  :after (company lsp-mode)
  :preface
  (defun my/lsp-set-cfg ()
    (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
      ;; TODO: check lsp--cur-workspace here to decide per server / project
      (lsp--set-configuration lsp-cfg)))
  :hook (lsp-after-initialize . my/lsp-set-cfg)
  :config
  (push 'company-lsp company-backends))

(use-package compile
  :defer t
  :init
  (setq compilation-scroll-output 'first-error))

(use-package counsel
  :after ivy
  :bind (("M-x"	     . counsel-M-x)
         ("C-x C-f"  . counsel-find-file)
         ("C-h f"    . counsel-describe-function)
         ("C-h v"    . counsel-describe-variable)
         ("C-h i"    . counsel-info-lookup-symbol)
         ("C-h u"    . counsel-unicode-char)
         ("C-c k"    . counsel-rg)
         ("C-x l"    . counsel-locate)
         ("C-c h i"  . counsel-imenu)
         ("C-x p"    . counsel-list-processes)
         ("M-y"      . counsel-yank-pop))
  :config
  (ivy-set-actions 'counsel-find-file
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
  :mode "\\.css\\'"
  :init
  (setq-default css-indent-offset 2))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package dap-mode
  :disabled
  :straight (dap-mode
             :host github :repo "yyoncho/dap-mode")
  :preface
  (defun my/disable-dap-mode ()
    "Disable dap-mode (Debug Adapter Protocol)."
    (dap-mode nil)
    (dap-ui-mode nil))
  (defun my/enable-dap-mode ()
    "Enable dap-mode (Debug Adapter Protocol)."
    (dap-mode 1)
    (dap-ui-mode 1))
  :hook (prog-mode . my/enable-dap-mode))

(use-package dash
  :defer t
  :config
  (dash-enable-font-lock))

(use-package dash-at-point
  :if (eq system-type 'darwin)
  :ensure-system-package
  ("/Applications/Dash.app" . "brew cask install dash"))

(use-package dired
  :straight f
  :config
  (require 'dired-x)
  (when (memq system-type '(gnu/linux darwin))
    ;; Show human-readable file sizes and sort numbers properly
    (setq-default dired-listing-switches "-alhv")))

(use-package dired-details
  :after (dired)
  :init
  (setq-default dired-details-hidden-string "--- ")
  :config
  (dired-details-install))

(use-package ecb
  :config
  (require 'ecb-autoloads))

(use-package edbi
  ;; Emacs Database Interface
  ;; http://john.mercouris.online/emacs-database-interface.html
  ;; https://metacpan.org/pod/DBI#connect
  :disabled
  :ensure-system-package (perl)
  ;; :ensure-system-package ((DBI               . "cpan install DBI")
  ;;                         (RPC::EPC::Service . "cpan install RPC::EPC::Service")
  ;;                         (DBD::SQLite       . "cpan install DBD::SQLite")
  ;;                         (DBD::Pg           . "cpan install DBD::Pg")
  ;;                         (DBD::mysql        . "cpan install DBD::mysql"))
  :defer)

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

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
  :ensure-system-package "EditorConfig"
  :if (file-exists-p (expand-file-name ".editorconfig" (getenv "HOME")))
  :defer t
  :config
  ;; Always the built-in core library instead of any EditorConfig executable to get properties.
  (set-variable 'editorconfig-get-properties-function #'editor-config-core-get-properties-hash)
  (editorconfig-mode 1))

(use-package eglot
  :disabled
  :config
  (when (executable-find "bash-language-server")
    (add-to-list 'eglot-server-programs
                 `(sh-mode . ("bash-language-server" "start" "-v" "--tcp" "--host"
                              "localhost" "--port" :autoport))))
  (when (executable-find "rls")
    (add-to-list 'eglot-server-programs
                 `(rust-mode . ("rls" "-v" "--tcp" "--host"
                                "localhost" "--port" :autoport))))
  (when (executable-find "pyls")
    (add-to-list 'eglot-server-programs
                 `(python-mode . ("pyls" "-v" "--tcp" "--host"
                                  "localhost" "--port" :autoport)))))

(use-package eldoc
  :straight f
  :diminish
  :hook ((c-mode-common emacs-lisp-mode) . eldoc-mode))

(use-package elint
  :after (emacs-lisp-mode)
  :bind (("C-c e b" . my/elint-current-buffer)
         ("C-c e f" . my/elint-defn))
  :preface
  (defun my/elint-current-buffer ()
    "Lint the current Emacs Lisp buffer."
    (interactive)
    (if (eq major-mode 'emacs-lisp-mode)
        (save-excursion
          (elint-current-buffer))))
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

(use-package elpy
  :if (not (eq system-type 'windows-nt))
  :after (python-mode)
  :bind (:map elpy-mode-map
         ("C-c C-k" . python-shell-send-buffer)
         ("C-M-x"   . python-shell-send-defun))
  :ensure-system-package (pip
                          (autopep8 . "pip install autopep8")
                          (yapf     . "pip install yapf"))
  :config
  (elpy-enable)
  (with-eval-after-load 'flycheck
    (remove-hook 'elpy-modules #'elpy-module-flymake)
    (remove-hook 'elpy-modules #'elpy-module-yasnippet)
    (remove-hook 'elpy-mode-hook #'elpy-module-highlight-indentation))
  (with-eval-after-load 'rope
    (setq elpy-rpc-backend "rope"))
  (with-eval-after-load 'jedi-core
    (setq elpy-rpc-backend "jedi"))
  :hook (python-mode . elpy-mode))

(use-package emacs-lisp-mode
  :straight f
  :bind (:map emacs-lisp-mode-map
         ("M-:" . pp-eval-expression))
  :preface
  ;;; More sensible indentation of multiline lists containing symbols.
  ;;; See: https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
  (defun my/lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn
              (goto-char calculate-lisp-indent-last-sexp)
              (beginning-of-line)
              (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state))))))))

  (defun my/emacs-lisp-mode-config ()
    "Emacs Lisp Mode configuration."
    (setq-local lisp-indent-function #'my/lisp-indent-function))
  :hook (emacs-lisp-mode . my/emacs-lisp-mode-config))

(use-package emacs-powerthesaurus
  :defer
  :straight (emacs-powerthesaurus
             :host github :repo "SavchenkoValeriy/emacs-powerthesaurus")
  :bind ("C-c C-t" . powerthesuarus-lookup-word))

(use-package erefactor
  :disabled
  :after (emacs-lisp-mode elint)
  :bind (:map emacs-lisp-mode-map
         ("C-c C-v" . erefactor-map))
  ;; C-c C-v l : elint current buffer in clean environment.
  ;; C-c C-v L : elint current buffer by multiple emacs binaries.
  ;;             See `erefactor-lint-emacsen'
  ;; C-c C-v r : Rename symbol in current buffer.
  ;;             Resolve `let' binding as long as i can.
  ;; C-c C-v R : Rename symbol in requiring modules and current buffer.
  ;; C-c C-v h : Highlight current symbol in this buffer
  ;;             and suppress `erefacthr-highlight-mode'.
  ;; C-c C-v d : Dehighlight all by above command.
  ;; C-c C-v c : Switch prefix bunch of symbols.
  ;;             ex: '(hoge-var hoge-func) -> '(foo-var foo-func)
  )

(use-package ert
  :bind ("C-c e t" . ert-run-tests-interactively))

(use-package esup
  :init
  (advice-add 'esup :around #'my/wrap-in-straight-transaction))

(use-package etags
  :bind ("M-T" . tags-search))

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

(use-package fill-column-indicator
  :defer 1
  :preface
  (defvar my/htmlize-initial-fci-state nil
    "Variable to store state of `fci-mode' when `htmlize-buffer' is called.")

  (defvar my/htmlize-initial-flyspell-state nil
    "Variable to store state of `flyspell-mode' when `htmlize-buffer' is
called.")

  (defvar my/fci-status nil
    "Holds current FCI status.")

  (defun my/disable-fci-temporarily (orig-fun &rest args)
    "Disable fci-mode before calling ORIG-FUN with ARGS; re-enable afterwards."
    (apply #'my/disable-mode-temporarily 'fci-mode orig-fun args))

  ;; Fix for htmlize producing garbage newlines when using fci-mode.
  (defun my/htmlize-before-hook-fn ()
    (when (fboundp 'flyspell-mode)
      (setq my/htmlize-initial-fci-state fci-mode)
      (when fci-mode
        (turn-off-fci-mode))
      (setq my/htmlize-initial-flyspell-state flyspell-mode)
      (when flyspell-mode
        (flyspell-mode -1))))

  (defun my/htmlize-after-hook-fn ()
    (when (fboundp 'flyspell-mode)
      (when my/htmlize-initial-fci-state
        (turn-on-fci-mode))
      (when my/htmlize-initial-flyspell-state
        (flyspell-mode 1))))

  (defun my/disable-fci-during-company-complete (command)
    "Fixes the issue where the first item is shown far off to the right."
    (when (string= "show" command)
      (setq my/fci-status fci-mode)
      (turn-off-fci-mode))
    (when (string= "hide" command)
      (when my/fci-status
        (turn-on-fci-mode))))

  (defun my/change-fci-rule-color (color &optional only-current)
    "Changes the `fill-column-indicator' color to COLOR for all
existing and future buffers. Both `fci-rule-color' and
`fci-rule-character-color' are set.

If ONLY-CURRENT is non-nil, the color change will only be appled
to the current buffer. Existing and future buffers will retain
the current color setting."
    (let ((buffers  (if only-current
                        (list (current-buffer))
                      (my/get-buffers-with-minor-mode 'fci-mode))))
      (dolist (buffer buffers)
        (with-current-buffer buffer
          ;; change the local color for all active buffers
          (setq-local fci-rule-color color)
          (setq-local fci-rule-character-color color)
          ;; color changes do not take effect until `fci-mode' is run
          (fci-mode fci-mode)))
      (when (not only-current)
        ;; change the global default color for all buffers
        (setq-default fci-rule-color color
                      fci-rule-character-color color))))
  :hook (prog-mode . turn-on-fci-mode))

(use-package flycheck
  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled idle-change save)
        flycheck-idle-change-delay 2)
  (setq-default flycheck-disabled-checkers '(html-tidy))
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (with-eval-after-load 'hydra
    (defhydra hydra-flycheck (:color pink)
      "
^
^Flycheck^          ^Errors^            ^Checker^
^────────^──────────^──────^────────────^───────^───────────
_q_ quit            _<_ previous        _?_ describe
_m_ manual          _>_ next            _d_ disable
_v_ verify setup    _f_ check           _s_ select
^^                  _l_ list            ^^
^^                  ^^                  ^^
"
      ("q" nil)
      ("<" flycheck-previous-error)
      (">" flycheck-next-error)
      ("?" flycheck-describe-checker :color blue)
      ("d" flycheck-disable-checker :color blue)
      ("f" flycheck-buffer)
      ("l" flycheck-list-errors :color blue)
      ("m" flycheck-manual :color blue)
      ("s" flycheck-select-checker :color blue)
      ("v" flycheck-verify-setup :color blue)))
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-inline
  :requires flycheck
  :preface
  ;; You can change the way overlays are created by customizing
  ;; `flycheck-inline-display-function' and
  ;; `flychjeck-inline-clear-function'. Here is an example using
  ;; `quick-peek' to display the overlays which adds bars around them:
  (use-package quick-peek :defer t)
  (setq flycheck-inline-display-function
        (lambda (msg pos)
          (let* ((ov (quick-peek-overlay-ensure-at pos))
                 (contents (quick-peek-overlay-contents ov)))
            (setf (quick-peek-overlay-contents ov)
                  (concat contents (when contents "\n") msg))
            (quick-peek-update ov)))
        flycheck-inline-clear-function #'quick-peek-hide)
  :config
  (flycheck-inline-mode))

(use-package flymake
  :straight f
  :config
  (with-eval-after-load 'hydra
    (defhydra hydra-flymake (:color pink)
      "
^
^Flymake^          ^Errors^
^───────^──────────^──────^────
_q_ quit           _<_ previous
^^                 _>_ next
^^                 ^^
"
      ("q" nil)
      ("<" flymake-goto-previous-error)
      (">" flymake-goto-next-error)))
  :hook (find-file . flymake-find-file-hook))

(use-package gist
  :defer t
  :ensure-system-package gist
  :bind ("C-c G" . my/gist-region-or-buffer)
  :preface
  (defun my/gist-region-or-buffer (start end)
    "Create a Github Gist from the current region or buffer."
    (interactive "r")
    (copy-region-as-kill start end)
    (deactivate-mark)
    (let ((file-name buffer-file-name))
      (with-temp-buffer
        (if file-name
            (call-process "gist" nil t nil "-f" file-name "-P")
          (call-process "gist" nil t nil "-P"))
        (kill-ring-save (point-min) (1- (point-max)))
        (message (buffer-substring (point-min) (1- (point-max))))))))

(use-package git-commit
  :commands global-git-commit-mode
  :preface
  (defun my/show-trailing-ws()
    "Show trailing whitespaces."
    (setq-local show-trailing-whitespace t))
  :init
  (setq git-commit-summary-max-length 50
        fill-column 72)
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :hook (git-commit-setup . my/show-trailing-ws))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :preface
  (defun my/git-gutter-refresh-all ()
    "Refresh all git-gutter windows unless initializing.
If `git-gutter:update-all-windows' is called during Emacs
initialization, it can loop until OS handles are exhausted."
    ;; `after-init-time' is set to a non-nil value only after Emacs
    ;; initialization is completed.
    (when after-init-time
      (git-gutter:update-all-windows)))
  :init
  (add-hook 'prog-mode-hook #'git-gutter-mode)
  (with-eval-after-load 'magit
    ;; Refresh git-gutter buffers after Magit status changes
    (add-hook 'magit-post-refresh-hook #'my/git-gutter-refresh-all)))

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :bind (:map git-messenger-map
         ("m" . git-messenger:copy-message))
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

(use-package git-timemachine
  :bind ("C-c x" . git-timemachine-toggle))

(use-package git-timemachine
  :commands git-timemachine
  :config
  (setq git-timemachine-abbreviation-length 6))

(use-package gitattributes-mode)

(use-package gitconfig-mode
  :mode (("\\.gitconfig\\'"  . gitconfig-mode)
	 ("\\.git/config\\'" . gitconfig-mode)
	 ("\\.gitmodules\\'" . gitconfig-mode)))

(use-package github-browse-file
  :preface
  (defun github-issues (&optional new)
    (interactive)
    (let ((url (concat "https://github.com/"
                       (github-browse-file--relative-url)
                       "/issues/" new)))
      (browse-url url)))

  (defun github-new-issue ()
    (interactive)
    (github-issues "new")))

(use-package github-search
  :demand t)

(use-package gitignore-mode
  :mode ("\\.gitignore\\'" . gitignore-mode))

(use-package gitlab
  :demand t
  :init
  (setq gilab-host "https://gitlab.com"))

(use-package glab
  :demand t
  :after (gitlab))

(use-package google-this
  :bind-keymap ("C-c /" . google-this-mode-submap)
  :bind* ("M-SPC" . google-this-search)
  :bind (:map google-this-mode-map
         ("/" . google-this-search)))

(use-package goto-chg
  :bind (("C-c b ," . goto-last-change)
         ("C-c b ." . goto-last-change-reverse)))

(use-package graphviz-dot-mode
  :mode "\\.dot\\'"
  :ensure-system-package graphviz)

(use-package grep
  :if (executable-find "grep")
  :bind (("M-s n" . find-name-dired)
         ("M-s F" . find-grep)
         ("M-s G" . grep)
         ("M-s d" . find-grep-dired)))

(use-package hl-todo
  :bind (:map hl-todo-mode-map
         ("C-x P" . hl-todo-previous)
         ("C-x N" . hl-todo-next)
         ("C-x O" . hl-todo-occur))
  :hook ((prog-mode . hl-todo-mode)))

(use-package hydra
  :preface
  (defvar-local my/ongoing-hydra-body nil)
  (defun my/ongoing-hydra ()
    (interactive)
    (if my/ongoing-hydra-body
        (funcall my/ongoing-hydra-body)
      (user-error "Function my/ongoing-hydra: my/ongoing-hydra-body is not set")))
  :bind (;;("C-c f" . hydra-flycheck/body)
         ("C-c g" . hydra-magit/body)
         ("C-c p" . hydra-projectile/body))
  :config
  (setq-default hydra-default-hint nil))

(use-package imenu
  :defer t
  :hook (emacs-lisp . imenu-add-menubar-index))

(use-package imenu-anywhere
  :after (imenu)
  :bind (("C-c i" . imenu-anywhere)
         ("s-i"   . imenu-anywhere)))

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
    ;; Use Bookerly font, if available
    (when (find-font (font-spec :name "Bookerly"))
      (setq bufer-face-mode-face '(:family "Bookerly"))
      (buffer-face-mode)
      (text-scale-adjust 1)))
  :hook (Info-mode . my/Info-mode-config))

(use-package info-look
  :after (info)
  :defer t
  :config
  (autoload 'info-lookup-add-help "info-look"))

(use-package info-lookmore
  :after (info info-look)
  :config
  (info-lookmore-elisp-cl)
  (info-lookmore-apropos-elisp)
  (info-lookmore-elisp-userlast)
  (with-eval-after-load 'gnus
    (info-lookmore-elisp-gnus)))

(use-package isearch
  :straight f
  :bind (("C-M-r"    . my/isearch-backward-other-window)
         ("C-M-s"    . my/isearch-forward-other-window))
  :bind (:map isearch-mode-map
         ("C-c" . isearch-toggle-case-fold)
         ("C-t" . isearch-toggle-regexp)
         ("C-^" . isearch-edit-string)
         ("C-i" . isearch-complete))
  :preface
  (defun my/isearch-backward-other-window ()
    "Incrementally search backward using a new window."
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (call-interactively 'isearch-backward))

  (defun my/isearch-forward-other-window ()
    "Incrementally search forward using a new window."
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (call-interactively 'isearch-forward)))

(use-package ispell
  :ensure-system-package aspell
  :no-require t)

(use-package ivy
  :defer 5
  :diminish
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume))
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
  (setq ivy-count-format             "(%d/%d) "
        ivy-display-style            'fancy
        ivy-dynamic-exhibit-delay-ms 150
        ivy-extra-directories        nil     ; don't show ./ and ../
        ivy-height                   10
        ivy-initial-inputs-alist     nil
        ivy-magic-tilde              nil
        ivy-re-builders-alist        '((t . ivy--regex-ignore-order))
        ivy-use-ignore-default       'always
        ivy-use-selectable-promnpt   t
        ivy-use-virtual-buffers      t
        ivy-virtual-abbreviate       'full   ; show full path
        ivy-wrap                     t)
  :config
  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  (with-eval-after-load 'flx
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))))

(use-package ivy-bibtex
  ;; Ivy interface for BibTeX entries
  :after (ivy)
  :defer t
  :config
  (setq bibtex-completion-bibliography  '("~/Documents/bibtex/refs.bib")
        bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-pdf-field     "file"
        ivy-bibtex-default-action       'ivy-bibtex-insert-citation
        ivy-re-builders-alist           '((ivy-bibtex . ivy--regex-ignore-order)
                                          (t . ivy--regex-plus))))

(use-package ivy-gitlab
  :demand t
  :after (ivy gitlab))

(use-package ivy-hydra
  :demand t
  :after (ivy hydra))

(use-package ivy-pass
  :after (ivy)
  :ensure-system-package pass
  :demand t
  :commands ivy-pass)

(use-package ivy-rich
  :after (ivy)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev))

(use-package ivy-xref
  ;; Ivy interface for xref results
  :after (ivy)
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-yasnippet
  ;; Preview yasnippets with Ivy
  :after (ivy yasnippet)
  :bind ("C-c y" . ivy-yasnippet))

(use-package jedi-core
  :if (not (eq system-type 'windows-nt))
  :defer t
  :after (python)
  :ensure-system-package (pip
                          (jedi . "pip install jedi"))
  :config
  :hook (python-mode . jedi-mode))

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter ("node" . js2-mode)
  :init
  (setq js2-mode-show-strict-warnings nil
        js2-highlight-level           3)
  :config
  (defvaralias 'js-switch-indent-offset 'js2-basic-offset)
  (setenv "NODE_NO_READLINE" "1")
  (with-eval-after-load 'flycheck
    (flycheck-mode +1)
    (setq flycheck-javascript-eslint-executable "eslint_d"))
  :hook (js2-mode . js2-imenu-extras-mode))

(use-package json-mode
  :mode "\\.json\\'")

(use-package json-reformat
  :after (json-mode)
  :preface
  (defun my/reformat-buffer-or-region-as-json (&optional arg)
    "Reformat the current region or buffer as JSON.

 With prefix argument, set the buffer as unmodified, and read-only
only if the buffer was not modified prior to reformatting."
    (interactive "P")
    (let ((modified (buffer-modified-p)))
      (save-excursion
        (if region-active-p
            (json-reformat (region-beginning) (region-end))
          (json-reformat (point-min) (point-max)))
        (when (and (not modified) arg)
          (set-buffer-modified-p nil)
          (read-only-mode +1)))))
  :init
  (setq json-refotmat:indent-width 2
        json-reformat:pretty-string? t))

(use-package json-snatcher
  :after (js-mode js2-mode json-mode)
  :bind (:map js-mode-map
         ("C-c C-g" . json-print-path)
         :map js2-mode-map
         ("C-c C-g" . json-print-path)))

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

(use-package lsp-mode
  :preface
  (defun my/set-projectile-root ()
    "Set the LSP workspace to the current Projectile root."
    (when (and (featurep 'projectile) lsp--cur-workspace)
      (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))
  :config
  (with-eval-after-load 'projectile
    (add-hook 'lsp-before-open-hook #'my/set-projectile-root)))

(use-package lsp-rust
  :after (lsp-mode lsp-ui rust)
  :if (executable-find "rustup")
  :ensure-system-package (rls . "rustup component add rls-preview rust-analysis rust-src")
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  :hook (rust-mode . lsp-rust-enable))

(use-package lsp-ui
  :after (lsp-mode)
  :config
  (with-eval-after-load 'flycheck
    (require 'lsp-ui-flycheck)
    (add-hook 'lsp-after-open-hook #'lsp-ui-flycheck-enable))
  (with-eval-after-load 'imenu
    (require 'lsp-imenu)
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu))
  (setq lsp-ui-sideline-ignore-duplicate t)
  :hook (lsp-mode . lsp-ui-mode))

(use-package macrostep
  :after (emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
         ("C-c C-e" . macrostep-expand)))

(use-package magit
  :bind ("C-c g" . magit-status)
  :bind (:map magit-mode-map
         ("C-c C-a" . magit-just-amend))
  :preface
  (defun my/magit-just-amend ()
    (save-window-excursion
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
      (magit-refresh)))

  (defun my/magit-log-edit-config ()
    "Configuration for editing Git log messages."
    (setq fill-column 72)
    (when (featurep 'flyspell)
      (flyspell-mode t))
    (turn-on-auto-fill))
  :hook (magit-log-edit-mode . my/magit-log-edit-config)
  :config
  (setq magit-set-upstream-on-push      t
        magit-commit-summary-max-length 70
        magit-completing-read-function 'ivy-completing-read
        magit-popup-use-prefix-argument 'default)
  ;; no longer need vc-git
  (delete 'Git vc-handled-backends)

  ;; Check excessively long summary lines in commit messages
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line)
  (with-eval-after-load 'hydra
    (defhydra hydra-magit (:color blue)
      "
^
^Magit^             ^Do^
^─────^─────────────^──^────────────────
_q_ quit            _a_ amend
^^                  _b_ blame
^^                  _c_ clone
^^                  _i_ init
^^                  _s_ status
^^                  ^^
"
      ("q" nil)
      ("a" my/magit-just-amend)
      ("b" magit-blame)
      ("c" magit-clone)
      ("i" magit-init)
      ("s" magit-status))))

(use-package magithub
  :if (executable-find "hub")
  :after (:all magithub ghub ghub+)
  :config
  (magithub-feature-autoinject t))

(use-package markdown-mode
  :mode (("\\`INSTALL\\'"                 . gfm-mode)
         ("\\`CONTRIBUTORS\\'"            . gfm-mode)
         ("\\`LICENSE\\'"                 . gfm-mode)
         ("\\`README\\.md\\'"             . gfm-mode)
         ("\\.md\\'"                      . markdown-mode))
  :preface
  (defun my/markdown-mode-config ()
    "My markdown-mode configuration."
    (auto-fill-mode +1)
    (footnote-mode +1)
    (visual-line-mode +1)
    ;; do not treat "_" as a word separator
    (modify-syntax-entry ?- "w")
    (when (fboundp 'flyspell-mode)
      (flyspell-mode +1))
    (when (fboundp 'orgtbl-mode)
      (turn-on-orgtbl))
    (when (fboundp 'imenu)
      (setq imenu-auto-rescan t)
      (imenu-add-menubar-index)))
  :hook (markdown-mode . my/markdown-mode-config)
  :init
  (setq markdown-command
        (cond ((executable-find "multimarkdown") "multimarkdown")
              ((executable-find "markdown") "markdown")
              (t nil)))
  :config
  (with-eval-after-load 'hydra
    (defhydra hydra-markdown (:color pink)
      "
^
^Markdown^          ^Table Columns^     ^Table Rows^
^────────^──────────^─────────────^─────^──────────^────────
_q_ quit            _c_ insert          _r_ insert
^^                  _C_ delete          _R_ delete
^^                  _M-<left>_ left     _M-<down>_ down
^^                  _M-<right>_ right   _M-<up>_ up
^^                  ^^                  ^^
"
      ("q" nil)
      ("c" markdown-table-insert-column)
      ("C" markdown-table-delete-column)
      ("r" markdown-table-insert-row)
      ("R" markdown-table-delete-row)
      ("M-<left>" markdown-table-move-column-left)
      ("M-<right>" markdown-table-move-column-right)
      ("M-<down>" markdown-table-move-row-down)
      ("M-<up>" markdown-table-move-row-up))))

(use-package markdown-preview-mode
  :after (markdown-mode)
  :if (executable-find "multimarkdown")
  :init
  (setq markdown-preview-stylesheets
        (list (concat "https://github.com/dmarcotte/github-markdown-preview/"
                      "blob/master/data/css/github.css"))))

(use-package minibuffer
  ;; TODO(markcol): setting `gc-cons-threshold' doesn't work in Emacs 27?!
  :disabled
  :straight f
  :preface
  (defun my/minibuffer-setup ()
    "Setup minibuffer for use."
    ;; Disable garbage collection while in minibuffer to avoid stalls.
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my/minibuffer-exit ()
    "Restore minibuffer settings upon exit."
    ;; Restore garbage collection threshold to default.
    (setq gc-cons-threshold my/gc-cons-threshold-default))

  :hook (minibuffer-setup . my/minibuffer-setup)
  :hook (minibuffer-exit  . my/minibuffer-exit))

(use-package move-text
  :defer t
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)))

(use-package nlinum-hl
  :defer t
  :init
  (setq nlinum-format "%5d "
        nlinum-highlight-current-line t)
  :config
  ;; Changing fonts can leave nlinum line numbers in their original size; this
  ;; forces them to resize.
  (advice-add #'set-frame-font :after #'nlinum-hl-flush-all-windows)

  ;; Fix certain major modes that have known issues with line number
  ;; display. See: https://github.com/hlissner/emacs-nlinum-hl
  (with-eval-after-load 'markdown
    ;; With `markdown-fontify-code-blocks-natively' enabled in `markdown-mode',
    ;; line numbers tend to vanish next to code blocks.
    (advice-add #'markdown-fontify-code-block-natively
                :after #'nlinum-hl-do-markdown-fontify-region))
  (with-eval-after-load 'web-mode
    ;; When using `web-mode's code-folding an entire range of line numbers will
    ;; vanish in the affected area.
    (advice-add #'web-mode-fold-or-unfold :after #'nlinum-hl-do-generic-flush))
  :hook (prog-mode . nlinum-mode))

(use-package nodejs-repl
  :defer t
  :ensure-system-package node)

(use-package org
  :straight (org-plus-contrib
             :type git :repo "https://code.orgmode.org/bzg/org-mode.git"
             :local-repo "org" :files (:defaults "contrib/lisp/*.el"))
  :config
  ;; TODO(markcol): configure org-crypt
  (with-eval-after-load 'hydra
    (defhydra hydra-org (:color pink)
      "
^
^Org^               ^Links^             ^Outline^
^───^───────────────^─────^─────────────^───────^───────────
_q_ quit            _i_ insert          _<_ previous
^^                  _n_ next            _>_ next
^^                  _p_ previous        _a_ all
^^                  _s_ store           _o_ goto
^^                  ^^                  _v_ overview
^^                  ^^                  ^^
"
      ("q" nil)
      ("<" org-backward-element)
      (">" org-forward-element)
      ("a" outline-show-all)
      ("i" org-insert-link :color blue)
      ("n" org-next-link)
      ("o" helm-org-in-buffer-headings :color blue)
      ("p" org-previous-link)
      ("s" org-store-link)
      ("v" org-overview)))

  (setq org-hide-emphasis-markers t)
  (setq org-emphasis-alist '(("*" bold "<strong>" "</strong>")
                             ("/" italic "<i>" "</i>")
                             ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                             ("-" (:strike-through t) "<del>" "</del>")
                             ("=" (:foreground "yellow" :background "black"))
                             ("~" org-verbatim "<code>" "</code>" verbatim)))
  (setq org-todo-keywords
        '("TODO" "|" "CANCELLED" "DONE")))

(use-package org-agenda-ng
  ;; org-agenda-ng is a requirement for org-sidebar, but
  ;; not on MELPA.
  :straight (org-agenda-ng :host github :repo "alphapapa/org-agenda-ng")
  :after (org))

(use-package org-babel
  :disabled
  ;; See: http://cachestocaches.com/2018/6/org-literate-programming/
  :after (org)
  :init
  (setq org-src-fontify-natively t     ; Syntax highlight src blocks
        org-confirm-babel-evaluate nil ; Don't prompt before running code
        ;; Fix an incompatibility between the ob-async and ob-ipython packages
        ob-async-no-async-languages-alist '("ipython"))
  :config
  ;; TODO(markcol): avoid do-load-languages to improve performance.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (ipython . t)
     (sh . t)
     (shell . t)
     (rust . t))))

(use-package org-journal
  :after (org)
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

(use-package org-ql
  ;; org-ql is a requirement org-sidebar, but it is not on MELPA.
  :straight (org-ql :host github :repo "alphapapa/org-agenda-ng")
  :after (org))

(use-package org-sidebar
  :after (org)
  :straight (org-sidebar :host github :repo "alphapapa/org-sidebar")
  :commands (org-sidebar)
  :requires (org-ql org-agenda-ng)
  :bind ("C-c o s" . org-sidebar))

(use-package paredit
  :diminish
  :commands (paredit-mode paredit-backward-delete paredit-close-round paredit-newline)
  :bind (:map lisp-mode-map
         ("<return>" . paredit-newline)
         ([remap comment-dwim] . paredit-comment-dwim))
  :bind (:map emacs-lisp-mode-map
         ("<return>" . paredit-newline)
         ([remap comment-dwim] . paredit-comment-dwim))
  :preface
  (defun my/paredit-mode-config ()
    "My paredit mode customizations."
    (require 'eldoc)
    (eldoc-add-command 'paredit-backward-delete
                       'paredit-close-round)
    (paredit-mode))
  :hook ((lisp-mode emacs-lisp-mode) . my/paredit-mode-config)
  :config
  (use-package paredit-ext
    :straight f
    :load-path "lisp"
    :after (paredit)))

(use-package password-cache
  :disabled
  :config
  ;; package manager transactions such as installing and removing
  ;; packages are run via the `sudo` command in Eshell. If you’d like
  ;; to cache the password for, say, an hour, you can configure Eshell
  ;; to use its own version of sudo via TRAMP:
  (setq password-cache t
        password-cache-expiry 3600))

(use-package prescient
  :straight (:host github :repo "raxod502/prescient.el")
  :after ivy company
  :config
  (require 'ivy-prescient)
  (ivy-prescient-mode +1)
  (require 'company-prescient)
  (company-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package prettier-js
  :after js2-mode
  :bind (:map js2-mode-map
         ("s-b" . prettier))
  :ensure-system-package (npm
                          (prettier . "npm i -g prettier"))
  :init
  (setq prettier-args '("--no-semi" "--trailing-comma" "all")))

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
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode +1)
  (when (not (eq system-type 'windows-nt))
    (setq projectile-indexing-method 'native))
  (setq projectile-enable-caching t)
  (with-eval-after-load 'ivy
    (setq projectile-completion-system 'ivy))
  (with-eval-after-load 'helm
    (setq projectile-completion-system 'helm))
  (with-eval-after-load 'magit-branch
    (advice-add 'magit-checkout :after #'my/projectile-invalidate-cache)
    (advice-add 'magit-branch-and-checkout :after #'my/projectile-invalidate-cache))
  (with-eval-after-load 'hydra
    (defhydra hydra-projectile (:color blue)
      "
^
^Projectile^        ^Buffers^           ^Find^              ^Search^
^──────────^────────^───────^───────────^────^──────────────^──────^────────────
_q_ quit            _b_ list            _d_ directory       _r_ replace
_i_ reset cache     _K_ kill all        _D_ root            _R_ regexp replace
^^                  _S_ save all        _f_ file            _s_ ag
^^                  ^^                  _p_ project         ^^
^^                  ^^                  ^^                  ^^
"
      ("q" nil)
      ("b" helm-projectile-switch-to-buffer)
      ("d" helm-projectile-find-dir)
      ("D" projectile-dired)
      ("f" helm-projectile-find-file)
      ("i" projectile-invalidate-cache :color red)
      ("K" projectile-kill-buffers)
      ("p" helm-projectile-switch-project)
      ("r" projectile-replace)
      ("R" projectile-replace-regexp)
      ("s" helm-projectile-ag)
      ("S" projectile-save-project-buffers))))

(use-package python-mode
  :if (executable-find "python")
  :mode "\\.py\\'"
  :interpreter "python"
  :bind (:map python-mode-map
         ("C-c c")
         ("C-c C-z" . python-shell))
  :preface
  (defvar python-mode-initialized nil)
  (defun my/python-mode-config ()
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
    (setq python-shell-prompt-detect-failure-warning nil
          indent-tabs-mode nil)
    (set (make-local-variable 'parens-require-spaces) nil))
  :hook (python-mode . my/python-mode-config)
  :config
  (with-eval-after-load 'lsp-mode
    (add-hook 'python-mode-hook #'lsp-python-enable)
    (with-eval-after-load 'projectile
      (lsp-define-stdio-client lsp-python "python" #'projectile-project-root '("pyls"))))
  (my/add-zeal-or-dash-docs python-mode "python"))

(use-package racer
  :disabled
  :after (rust)
  :unless (featurep 'lsp-rust)
  :ensure-system-package (racer . "cargo install racer")
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

(use-package rainbow-delimiters
  ;; rainbow-delimiters is a "rainbow parentheses"-like mode which
  ;; highlights delimiters such as parentheses, brackets or braces
  ;; according to their depth. Each successive level is highlighted in
  ;; a different color. This makes it easy to spot matching
  ;; delimiters, orient yourself in the code, and tell which
  ;; statements are at a given depth.
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish)

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

(use-package ripgrep
  :ensure-system-package rg)

(use-package rjsx-mode
  :mode "\\.jsx\\'"
  :after js2-mode
  :preface
  (defun my/rjsx-config ()
    "Configuration for rjsx files."
    (with-eval-after-load 'flycheck
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))))
  :config
  (bind-key "=" #'pad-equals rjsx-mode-map
            (not (memq (js2-node-type (js2-node-at-point))
                       (list rjsx-JSX rjsx-JSX-ATTR rjsx-JSX-IDENT rjsx-JSX-MEMBER))))
  :hook (rjsx-mode . my/rjsx-config))

(use-package rust-mode
  :mode "\\.rs\\'"
  :if (executable-find "rust")
  :bind (:map rust-mode-map
         ("C-c C-p" . my/rust-toggle-visbility )
         ("C-c C-m" . my/rust-toggle-mutability)
         ("C-c C-v" . my/rust-vec-as-slice))
  :ensure-system-package (rustfmt . "rustup component add rustfmt-preview")
  :preface
  (defun my/rust-toggle-mutability ()
    "Toggle the mutability of the variable at point."
    (interactive)
    (save-excursion
      (racer-find-definition)
      (back-to-indentation)
      (forward-char 4)
      (if (looking-at "mut ")
          (delete-char 4)
        (insert "mut "))))

  (defun my/rust-toggle-visibility ()
    "Toggle the public visibility of the function at point."
    (interactive)
    (save-excursion
      ;; If we're already at the beginning of the function definition,
      ;; `beginning-of-defun' moves to the previous function, so move elsewhere.
      (end-of-line)

      (beginning-of-defun)
      (if (looking-at "pub ")
          (delete-char 4)
        (insert "pub "))))

  (defun my/rust-vec-as-slice ()
    "Convert the vector expression at point to a slice.
foo -> &foo[..]"
    (interactive)
    (insert "&")
    (forward-symbol 1)
    (insert "[..]"))

  (defun my/rust-mode-config ()
    "My Rust-mode configuration."
    (eldoc-mode)
    (lsp-ui-mode)
    (company-mode)
    (set (make-local-variable 'company-backends)
         '((company-lsp company-yasnippet company-files)))
    (setq-local fill-column 100)
    (--each '((">=" . (?· (Br . Bl) ?≥))
              ("<=" . (?· (Br . Bl) ?≤))
              ("!=" . (?· (Br . Bl) ?≠))
              ("=>" . (?· (Br . Bl) ?➡))
              ("->" . (?· (Br . Bl) ?→)))
      (push it prettify-symbols-alist)))
  :hook (rust-mode . my/rust-mode-config)
  :config
  (my/add-zeal-or-dash-docs rust-mode "rust"))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package scss-mode
  :mode ("\\.s[ac]ss\\'")
  :ensure-system-package (sass . "npm install -g scss")
  :preface
  (defun my/scss-mode-config ()
    (setq-local comment-end "")
    (setq-local comment-start "//"))
  :hook (scss-mode . my/scss-mode-config))

(use-package server
  :preface
  (require 'server)
  (defun my/server-start-maybe ()
    "Start an Emacs server process if one is not already running."
    (unless server-process
      (server-start)))
  :hook (after-init . my/server-start-maybe))

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

(use-package tide
  ;; See: https://github.com/ananthakumaran/tide
  :after (typescript-mode company)
  :bind (:map typescript-mode-map
         ("C-c C-r" . tide-rename-symbol))
  :preface
  (defun my/tide-config ()
    "Tide mode-specific configuration."
    (tide-setup)
    (tide-hl-identifier-mode +1)
    (setq tide-format-options
          '(:indentSize                                                  4
            :tabSize                                                     4
            :insertSpaceAfterFunctionKeywordForAnonymousFunctions        t
            :insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces nil
            :placeOpenBraceOnNewLineForControlBlocks                     nil
            :placeOpenBraceOnNewLineForFunctions                         nil))
    (eldoc-mode +1)
    (with-eval-after-load 'flycheck
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
      ;; Add these two lines to support tslint in web-mode
      ;; https://github.com/ananthakumaran/tide/issues/95
      (flycheck-add-next-checker 'tsx-tide '(warning . typescript-tslint) 'append)
      (flycheck-add-mode 'typescript-tslint 'web-mode))
    (with-eval-after-load 'company
      (company-mode +1)
      (setq company-tooltip-align-annotations t)))
  :hook (before-save . tide-format-before-save)
  :hook ((js2-mode rjsx-mode typescript-mode) . my/tide-config))

(use-package toc-org
  :after (org))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package tramp
  :defer t
  :straight f)

(use-package travis
  :demand t
  :preface
  (defun my/travis-show-projects ()
    "Display the status of my projects on Travis.ci."
    (interactive)
    (travis-show-projects "markcol")))

(use-package treemacs
  :commands (treemacs)
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-c 1"     . treemacs-delete-other-windows)
         ("C-c t t"   . treemacs)
         ("C-c t T"   . treemacs)
         ("C-c t B"   . treemacs-bookmark)
         ("C-c t C-t" . treemacs-find-file)
         ("C-c t M-t" . treemacs-find-tag))
  :init
  (setq treemacs-change-root-without-asking nil
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
  :bind (("C-c t P" . treemacs-projectile)
         ("C-c t p" . treemacs-projectile-toggle))
  :config
  (with-eval-after-load 'projectile
    (setq treemacs-header-function #'treemacs-projectile-create-header)))

(use-package ts-comint
  :after typescript-mode
  :ensure-system-package (node
                          (tsun . "npm install -g tsun"))
  :bind (:map typescript-mode-map
         ("C-x C-e" . ts-send-last-sexp)
         ("C-M-x"   . ts-send-last-sexp-and-go)
         ("C-c b"   . ts-send-buffer)
         ("C-c C-b" . ts-send-buffer-and-go)
         ("C-c l"   . ts-load-file-and-go)))

(use-package typescript-mode
  :mode (("\\.ts\\'"  . typescript-mode)
         ("\\.tsx\\'" . web-mode))
  :ensure-system-package (node
                          (tsc . "npm install -g typescript")
                          (tslint . "npm install -g --save-dev tslint tslint-config-standard tslint-eslint-rules"))
  :preface
  (defun my/web-tsx-config ()
    "tsx configuration."
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (my/tide-config)))
  :hook (web-mode . my/web-tsx-config))

(use-package unfill
  ;; Unfill paragraph/region, toggle between filled & unfilled.
  ;; https://github.com/purcell/unfill
  :bind ("M-q" . unfill-toggle))

(use-package web-beautify
  :after (web-mode)
  :bind ((:map sgml-mode-map
          ("s-b" . web-beautify-html))
         (:map css-mode-map
          ("s-b" . web-beautify-css)))
  :ensure-system-package (npm
                          (js-beautify . "npm install -g js-beautify")))

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.css\\'")
  :commands web-mode
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-enable-auto-quoting nil))

(use-package which-key
  :defer 5
  :diminish
  :init
  (setq which-key-separator " "
        which-key-prefix    "+")
  :config
  (which-key-mode)
  (dolist (prefix '("projectile-switch-project" "ember" "magit" "projectile"))
    (let ((pattern (concat "^" prefix "-\\(.+\\)")))
      (push `((nil . ,pattern) . (nil . "\\1"))
            which-key-replacement-alist))))

(use-package whitespace
  :defer t
  :defines whitespace-line-column whitespace-style
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing))
  :hook ((prog-mode text-mode) . whitespace-mode)
  :hook (before-save . whitespace-cleanup))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package yasnippet
  :defer 5
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
  :ensure-system-package zeal)

;;;[END_USE_PACKAGE]

;;;
;;; Finalization
;;;

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(provide 'init)
;;; init.el ends here
