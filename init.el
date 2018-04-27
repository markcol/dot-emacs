;;; init.el -- user customization for Emacs  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;;
;;; This is my emacs .init file. It uses straight.el and use-package forall
;;; package maangement. A significant portion of the configuration was cribbed
;;; from John Wiegley's configuration, with random bits thrown in from various
;;; places around the web.

;;; Code:

(defconst emacs-start-time (current-time))
(defvar my/file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defun my/restore-default-values ()
  "Restore the default values of performance-critical variables."
  (setq file-name-handler-alist my/file-name-handler-alist-old
        gc-cons-threshold 800000
        gc-cons-percentage 0.1)
  (garbage-collect))

(add-hook 'after-init-hook #'my/restore-default-values)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(line-number-mode t)

(eval-and-compile
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
  
  ;; Use straight.el to load missing packages for `use-package` by default.
  (setq straight-use-package-by-default t)

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
  (use-package use-package-ensure-system-package)
  (use-package use-package-chords
    :disabled
    :config
    ;; Define your chord bindings in the same manner as :bind using a cons or a list of conses:
    ;;
    ;; (use-package ace-jump-mode
    ;;  :chords (("jj" . ace-jump-char-mode)
    ;;           ("jk" . ace-jump-word-mode)
    ;;           ("jl" . ace-jump-line-mode)))
    (key-chord-mode 1)))

;;; Settings

(eval-and-compile
  (defconst user-data-directory (expand-file-name "data" user-emacs-directory)
    "Directory for data files.")

  (defconst user-document-directory (expand-file-name "~/Documents")
    "Directory for user documents.")

  (defconst user-org-directory (expand-file-name "org" user-document-directory)
    "Directory for user org-mode files.")

  (defconst user-bib-directory (expand-file-name "bib" user-document-directory)
    "Directory for user bibliography data.")

  ;; Create any missing directories
  (dolist (dir (list user-data-directory user-document-directory user-org-directory user-bib-directory))
    (unless (file-exists-p dir)
      (message "Creating directory %s..." dir)
      (make-directory dir)))
  
  (load (expand-file-name "settings" user-emacs-directory))
  
  (setq make-backup-files nil
        auto-save-default nil
        inhibit-splash-screen t
        visible-bell nil
        indent-tabs-mode nil
        tab-width 2
        css-indent-offset 2
        load-prefer-newer t
	      auto-window-vscroll nil
        mouse-drag-copy-region t
        echo-keystrokes 0.1)
  
  ;; nice scrolling
  (setq scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1)

  ;; Revert buffers automatically when underlying files are changed externally
  (global-auto-revert-mode t)
  
  ;; Do not display message about reverted files
  (setq auto-rever-verbose nil)

  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  
  (defalias 'yes-or-no-p #'y-or-n-p)
  (setq confirm-kill-emacs #'y-or-n-p)

  (require 'cl-lib)
  )


;;;
;;; Functions
;;;

(eval-and-compile
  ;; comment out line if no region is selected
  (defun comment-dwim-line (&optional arg)
    "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we are not at the end of the line,
   then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
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

  (define-inline emacs-path (path)
    (expand-file-name path user-emacs-directory))

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

  (defun push-window-configuration ()
    (interactive)
    (push (current-window-configuration) saved-window-configuration))

  (defun pop-window-configuration ()
    (interactive)
    (let ((config (pop saved-window-configuration)))
      (if config
          (set-window-configuration config)
        (if (> (length (window-list)) 1)
            (delete-window)
          (bury-buffer)))))

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

  (defun my/eval-last-sexp-or-region (prefix)
    "Eval region from BEG to END if active, otherwise the last sexp."
    (interactive "P")
    (if (and (mark) (use-region-p))
        (eval-region (min (point) (mark)) (max (point) (mark)))
      (pp-eval-last-sexp prefix)))

  (defun projectile-add-magit-repo-dirs-to-known-projects ()
    "Add `magit-repo-dirs' to `projectile-known-projects'."
    (interactive)
    (--each (mapcar 'cdr (magit-list-repos magit-repo-dirs))
      (projectile-add-known-project (file-name-as-directory
                                     (file-truename it)))))

  (defun switch-to-previous-buffer ()
    "Switch to most recent buffer. Repeated calls toggle back and forth between the most recent two buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

  ;; set key binding
  (bind-key "C-`"     #'switch-to-previous-buffer)
  (bind-key "C-x C-e" #'my/eval-last-sexp-or-region emacs-lisp-mode-map)
  (bind-key "M-;"     #'comment-dwim-line)
  (bind-key "C-c n"   #'narrow-or-widen-dwim)
  (bind-key "C-x C-b" #'ibuffer)
  (bind-key "C-x C-r" #'revert-buffer)
  (bind-key "M-:"     #'pp-eval-expression))

;;;
;;; Keymaps
;;;

(define-key input-decode-map [?\C-m] [C-m])

(eval-and-compile
  (mapc #'(lambda (entry)
            (define-prefix-command (cdr entry))
            (bind-key (car entry) (cdr entry)))
        '(("C-,"   . my/ctrl-comma-map)
          ("<C-m>" . my/ctrl-m-map)
          ("C-h e" . my/ctrl-h-e-map)
          ("C-h x" . my/ctrl-h-x-map)
          ("C-c b" . my/ctrl-c-b-map)
          ("C-c e" . my/ctrl-c-e-map)
          ("C-c m" . my/ctrl-c-m-map)
          ("C-c t" . my/treemacs-map)   ; treemacs
          ("C-c w" . my/ctrl-c-w-map)
          ("C-c y" . my/yasnippet-map)	; yasnippet
          ("C-c H" . my/ctrl-c-H-map)
          ("C-c N" . my/ctrl-c-N-map)
          ("C-c (" . my/paredit-map)	  ; paredit
          ("C-c -" . my/ctrl-c-minus-map)
          ("C-c =" . my/ctrl-c-equals-map)
          ("C-c ." . my/ctrl-c-r-map))))

;;;
;;; Libraries
;;;

(use-package async         :defer t)
(use-package dash          :defer t)
(use-package diminish      :demand t)
(use-package el-mock       :defer t)
(use-package elisp-refs    :defer t)
(use-package epl           :defer t)
(use-package f             :defer t)
(use-package fringe-helper :defer t)
(use-package ghub          :defer t)
(use-package ghub+         :defer t)
(use-package ht            :defer t)
(use-package loop          :defer t)
(use-package marshal       :defer t)
(use-package parsebib      :defer t)
(use-package pkg-info      :defer t)
(use-package popup         :defer t)
(use-package popup-pos-tip :defer t)
(use-package popwin        :defer t)
(use-package pos-tip       :defer t)
(use-package request       :defer t)
(use-package rich-minority :defer t)
(use-package s             :defer t)
(use-package tablist       :defer t)
(use-package uuidgen       :defer t)
(use-package web           :defer t)
(use-package web-server    :defer t)
(use-package websocket     :defer t)
(use-package with-editor   :defer t)
(use-package xml-rpc       :defer t)

;;;
;;; UI
;;;

(use-package afternoon-theme
  :unless noninteractive)

(use-package all-the-icons
  :unless noninteractive
  :defer t
  :if (display-graphic-p))

(use-package spaceline
  :unless noninteractive
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(setq default-frame-alist '((height . 58)
                            (width  . 136)
                            (font   . "Fantasque Sans Mono-12")))

;;;
;;; Packages
;;;

(use-package abbrev
  :straight f
  :defer 5
  :diminish
  :init
  (setq abbrev-file-name (expand-file-name "abbrev" user-data-directory)
        save-abbrevs 'silently)
  (setq-default abbrev-mode t)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  :hook
  ((text-mode prog-mode erc-mode LaTeX-mode) . abbrev-mode)
  (expand-load
   . (lambda ()
       (add-hook 'expand-expand-hook 'indent-according-to-mode)
       (add-hook 'expand-jump-hook 'indent-according-to-mode))))

(use-package ag
  :if (executable-find "ag")
  :config
  (setq ag-highlight-search t))

(use-package aggressive-indent
  :diminish aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package align
  :bind (("M-["       . align-code)
         ("C-c ["     . align-regexp)
         ("C-c ."     . my/align-dot)
         ("C-c <SPC>" . my/align-whitespace))
  :commands align
  :preface
  (defun my/align-whitespace (start end)
    "Align columns by whitespace."
    (interactive "r")
    (align-regexp start end
                  "\\(\\s-*\\)\\s-" 1 0 t))

  (defun my/align-dot (start end)
    "Align columns by period."
    (interactive "r")
    (align-regexp start end
                  "\\(\\s-*\\)\\." 1 1 t))

  (defun align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
        (align beg end)
      (let ((end-mark (copy-marker end)))
        (indent-region beg end-mark nil)
        (align beg end-mark)))))

(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :preface
  (defun latex-help-get-cmd-alist ()    ;corrected version:
    "Scoop up the commands in the index of the latex info manual.
   The values are saved in `latex-help-cmd-alist' for speed."
    ;; mm, does it contain any cached entries
    (if (not (assoc "\\begin" latex-help-cmd-alist))
        (save-window-excursion
          (setq latex-help-cmd-alist nil)
          (Info-goto-node (concat latex-help-file "Command Index"))
          (goto-char (point-max))
          (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
            (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
                  (value (buffer-substring (match-beginning 2)
                                           (match-end 2))))
              (add-to-list 'latex-help-cmd-alist (cons key value))))))
    latex-help-cmd-alist)

  :config
  (use-package biblio
    :requires auctex
    :commands biblio-lookup)

  (use-package latex
    :straight f
    :requires auctex
    :config
    (require 'preview)
    (load (exapand-file-name "site-lisp/auctex/style/minted" user-emacs-directory))
    (info-lookup-add-help :mode 'LaTeX-mode
                          :regexp ".*"
                          :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
                          :doc-spec '(("(latex2e)Concept Index")
                                      ("(latex2e)Command Index"))))
  
  (use-package texinfo
    :mode ("\\.texi\\'" . texinfo-mode)
    :requires auctex
    :preface
    (defun my/texinfo-mode-hook ()
      "My Texinfo mode customizations."
      (dolist (mapping '((?b . "emph")
                         (?c . "code")
                         (?s . "samp")
                         (?d . "dfn")
                         (?o . "option")
                         (?x . "pxref")))
        (local-set-key (vector (list 'alt (car mapping)))
                       `(lambda () (interactive)
                          (TeX-insert-macro ,(cdr mapping))))))

    (defun texinfo-outline-level ()
      "Calculate level of current texinfo outline heading."
      (require 'texinfo)
      (save-excursion
        (if (bobp)
            0
          (forward-char 1)
          (let* ((word (buffer-substring-no-properties
                        (point) (progn (forward-word) (point))))
                 (entry (assoc word texinfo-section-list)))
            (if entry
                (nth 1 entry)
              5)))))

    :hook (texinfo-mode . my/texinfo-mode-hook))

  :hook (Tex-after-compilation-finished-functions . TeX-revert-document-buffer))

(use-package auth-source-pass
  :defer t
  :config
  (auth-source-pass-enable)

  (defvar auth-source-pass--cache (make-hash-table :test #'equal))

  (defun auth-source-pass--reset-cache ()
    (setq auth-source-pass--cache (make-hash-table :test #'equal)))

  (defun auth-source-pass--read-entry (entry)
    "Return a string with the file content of ENTRY."
    (run-at-time 45 nil #'auth-source-pass--reset-cache)
    (let ((cached (gethash entry auth-source-pass--cache)))
      (or cached
          (puthash
           entry
           (with-temp-buffer
             (insert-file-contents (expand-file-name
                                    (format "%s.gpg" entry)
                                    (getenv "PASSWORD_STORE_DIR")))
             (buffer-substring-no-properties (point-min) (point-max)))
           auth-source-pass--cache))))

  (defun auth-source-pass-entries ()
    "Return a list of all password store entries."
    (let ((store-dir (getenv "PASSWORD_STORE_DIR")))
      (mapcar
       (lambda (file) (file-name-sans-extension (file-relative-name file store-dir)))
       (directory-files-recursively store-dir "\.gpg$")))))

(use-package anzu
  :commands (global-anzu-mode)
  :bind (("M-%"   . anzu-query-replace)
         ("M-S-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package auto-compile
  :demand t
  :init
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-count t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            #'auto-compile-inhibit-compile-detached-git-head))

(use-package avy
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package bm
  :bind (("C-c b b" . bm-toggle)
         ("C-c b n" . bm-next)
         ("C-c b p" . bm-previous))
  :commands (bm-repository-load
             bm-buffer-save
             bm-buffer-save-all
             bm-buffer-restore)
  :hook (after-init        . bm-repository-load)
  :hook (find-file-hooks   . bm-buffer-restore)
  :hook (after-revert      . bm-buffer-restore)
  :hook (kill-buffer       . bm-buffer-save)
  :hook (after-save        . bm-buffer-save)
  :hook (vc-before-checkin . bm-buffer-save)
  :hook (kill-emacs        . (lambda ()
                               (bm-buffer-save-all)
                               (bm-repository-save))))

(use-package bookmark+
  :after bookmark
  :bind ("M-B" . bookmark-bmenu-list)
  :commands bmkp-jump-dired)

(use-package bytecomp-simplify
  :defer 15)

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  :config
  (use-package cmake-font-lock
    :hook (cmake-mode . cmake-font-lock-activate)))

(use-package company
  :defer 5
  :diminish
  :commands (company-mode company-indent-or-complete-common)
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  haskell-mode-hook
                  c-mode-common-hook))
    (add-hook hook
              #'(lambda ()
                  (local-set-key (kbd "<tab>")
                                 #'company-indent-or-complete-common))))
  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))

  ;; See http://oremacs.com/2017/12/27/company-numbers/
  (defun ora-company-number ()
    "Forward to `company-complete-number'.
  Unless the number is potentially part of the candidate.
  In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
        (company-complete-number (string-to-number k)))))

  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1))))

  (defun check-expansion ()
    (save-excursion
      (if (outline-on-heading-p t)
          nil
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil))))))

  (define-key company-mode-map [tab]
    '(menu-item "maybe-company-expand" nil
                :filter (lambda (&optional _)
                          (when (check-expansion)
                            #'company-complete-common))))

  (with-eval-after-load 'yasnippet
    (defun company-mode/backend-with-yas (backend)
      (if (and (listp backend) (member 'company-yasnippet backend))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
    (setq company-backends
          (mapcar #'company-mode/backend-with-yas company-backends)))
  (global-company-mode 1))

(use-package company-auctex
  :after (company latex))

(use-package company-elisp
  :disabled
  :after company
  :config
  (push 'company-elisp company-backends))

(use-package company-math
  :after company math-symbol-lists
  :defer t)

(use-package company-quickhelp
  :after company
  :bind (:map company-active-map
              ("C-c ?" . company-quickhelp-manual-begin)))

(use-package company-restclient
  :after (company restclient))

(use-package compile
  :no-require
  :bind (("C-c c" . compile)
         ("M-O"   . my/show-compilation))
  :bind (:map compilation-mode-map
              ("z" . delete-window))
  :preface
  (defun my/show-compilation ()
    "Show the compilation buffer."
    (interactive)
    (let ((it
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
      (if it
          (display-buffer it)
        (call-interactively 'compile))))

  (defun my/compilation-ansi-color-process-output ()
    "Process buffer and apply ANSI color codes to output."
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :hook (compilation-filter . my/compilation-ansi-color-process-output))

(use-package counsel
  :after ivy
  :demand t
  :diminish
  :commands counsel-minibuffer-history
  :bind (("C-*"     . counsel-org-agenda-headlines)
         ("C-x C-f" . counsel-find-file)
         ("C-c e l" . counsel-find-library)
         ("C-c e q" . counsel-set-variable)
         ("C-h e l" . counsel-find-library)
         ("C-h e u" . counsel-unicode-char)
         ("C-h f"   . counsel-describe-function)
         ("C-x r b" . counsel-bookmark)
         ("M-x"     . counsel-M-x)
         ("M-s f"   . counsel-file-jump)
         ("M-s g"   . counsel-rg)
         ("M-s j"   . counsel-dired-jump))
  :bind (:map minibuffer-local-map
              ("M-r" . counsel-minibuffer-history))

  :preface
  (defun counsel-recoll-function (string)
    "Run recoll for STRING."
    (if (< (length string) 3)
        (counsel-more-chars 3)
      (counsel--async-command
       (format "recollq -t -b %s"
               (shell-quote-argument string)))
      nil))

  (defun counsel-recoll (&optional initial-input)
    "Search for a string in the recoll database.
  You'll be given a list of files that match.
  Selecting a file will launch `swiper' for that file.
  INITIAL-INPUT can be given as the initial minibuffer input."
    (interactive)
    (counsel-require-program "recollq")
    (ivy-read "recoll: " 'counsel-recoll-function
              :initial-input initial-input
              :dynamic-collection t
              :history 'counsel-git-grep-history
              :action (lambda (x)
                        (when (string-match "file://\\(.*\\)\\'" x)
                          (let ((file-name (match-string 1 x)))
                            (find-file file-name)
                            (unless (string-match "pdf$" x)
                              (swiper ivy-text)))))
              :unwind #'counsel-delete-process
              :caller 'counsel-recoll))

  :config
  (setq counsel-find-file-ignore-regexp (concat "\\(\\`\\.[^.]\\|"
						(regexp-opt completion-ignored-extensions)
						"\\'\\)"))
  
  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-find-file . ivy--sort-files-by-date))

  (use-package counsel-dash
    :after counsel
    :requires dash
    :bind ("C-c C-h" . counsel-dash))

  (use-package counsel-osx-app
    :if (eq system-type 'darwin)
    :after counsel
    :bind* ("S-M-SPC" . counsel-osx-app)
    :commands counsel-osx-app
    :config
    (setq counsel-osx-app-location
          (list "/Applications"
                "/Applications/Misc"
                "/Applications/Utilities"
                (expand-file-name "~/Applications")
                "/Applications/Xcode.app/Contents/Applications")))

  (use-package counsel-projectile
    :after counsel
    :requires projectile
    :bind (:map counsel-projectile-mode-map
                ([remap projectile-ag] . counsel-projectile-rg))
    :config
    (counsel-projectile-mode)))

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

(use-package diff-hl
  :disabled
  :commands (diff-hl-mode diff-hl-dired-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package diff-hl-flydiff
  :disabled
  :commands diff-hl-flydiff-mode)

(use-package diff-mode
  :commands diff-mode)

(use-package diffview
  :commands (diffview-current diffview-region diffview-message))

(use-package dired
  :straight f
  :bind ("C-c j" . dired-two-pane)
  :bind (:map dired-mode-map
              ("j"     . dired)
              ("z"     . pop-window-configuration)
              ("e"     . my/ediff-files)
              ("l"     . dired-up-directory)
              ("q"     . dired-up-directory)
              ("Y"     . ora-dired-rsync)
              ("M-!"   . async-shell-command)
              ("<tab>" . dired-next-window)
              ("M-G")
              ("M-s f"))
  :diminish dired-omit-mode
  :preface
  (defun dired-two-pane ()
    (interactive)
    (push-window-configuration)
    (let ((here default-directory))
      (delete-other-windows)
      (dired "~/dl")
      (split-window-horizontally)
      (dired here)))

  (defun dired-next-window ()
    (interactive)
    (let ((next (car (cl-remove-if-not #'(lambda (wind)
                                           (with-current-buffer (window-buffer wind)
                                             (eq major-mode 'dired-mode)))
                                       (cdr (window-list))))))
      (when next
        (select-window next))))

  (defvar mark-files-cache (make-hash-table :test #'equal))

  (defun mark-similar-versions (name)
    (let ((pat name))
      (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
          (setq pat (match-string 1 pat)))
      (or (gethash pat mark-files-cache)
          (ignore (puthash pat t mark-files-cache)))))

  (defun dired-mark-similar-version ()
    (interactive)
    (setq mark-files-cache (make-hash-table :test #'equal))
    (dired-mark-sexp '(mark-similar-versions name)))

  (defun ora-dired-rsync (dest)
    (interactive
     (list
      (expand-file-name
       (read-file-name "Rsync to: " (dired-dwim-target-directory)))))
    (let ((files (dired-get-marked-files
                  nil current-prefix-arg))
          (tmtxt/rsync-command "rsync -aP "))
      (dolist (file files)
        (setq tmtxt/rsync-command
              (concat tmtxt/rsync-command
                      (shell-quote-argument file)
                      " ")))
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument dest)))
      (async-shell-command tmtxt/rsync-command "*rsync*")
      (other-window 1)))

  (defun my/ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      `(lambda ()
                         (setq ediff-after-quit-hook-internal nil)
                         (set-window-configuration ,wnd))))
        (error "No more than 2 files should be marked"))))

  (defun dired-omit-regexp ()
    "Omit files that Git would ignore. This overwrites the default implementation."
    (let ((file (expand-file-name ".git"))
          parent-dir)
      (while (and (not (file-exists-p file))
                  (progn
                    (setq parent-dir
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory file))))
                    ;; Give up if we are already at the root dir.
                    (not (string= (file-name-directory file)
                                  parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file (expand-file-name ".git" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (if (file-exists-p file)
          (let ((regexp (funcall dired-omit-regexp-orig))
                (omitted-files
                 (shell-command-to-string "git clean -d -x -n")))
            (if (= 0 (length omitted-files))
                regexp
              (concat
               regexp
               (if (> (length regexp) 0)
                   "\\|" "")
               "\\("
               (mapconcat
                #'(lambda (str)
                    (concat
                     "^"
                     (regexp-quote
                      (substring str 13
                                 (if (= ?/ (aref str (1- (length str))))
                                     (1- (length str))
                                   nil)))
                     "$"))
                (split-string omitted-files "\n" t)
                "\\|")
               "\\)")))
        (funcall dired-omit-regexp-orig))))
  :init
  (setq dired-dwim-target         t
        dired-recursive-deletes   t
        dired-use-ls-dired        nil
        delete-by-moving-to-trash t)
  :config
  (use-package dired-toggle
    :after dired
    :bind ("C-c ~" . dired-toggle)
    :preface
    (defun my/dired-toggle-mode-hook ()
      (interactive)
      (visual-line-mode 1)
      (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
      (setq-local word-wrap nil))
    
    :hook (dired-toggle-mode . my/dired-toggle-mode-hook))

  (use-package dired-x
    :straight f)
  
  (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))
  :hook (dired-mode . dired-hide-details-mode)
  :hook (dired-mode . auto-revert-mode))

(use-package docker
  :defer 15
  :diminish
  :config
  (use-package docker-compose-mode
    :after docker
    :mode "docker-compose.*\.yml\\'")
  
  (use-package dockerfile-mode
    :mode "Dockerfile[a-zA-Z.-]*\\'")

  (use-package docker-tramp
    :after docker tramp
    :defer 5)
  
  (require 'docker-images)
  (require 'docker-containers)
  (require 'docker-volumes)
  (require 'docker-networks)
  (docker-global-mode))

(use-package ediff
  :bind (("C-c = b" . ediff-buffers)
         ("C-c = B" . ediff-buffers3)
         ("C-c = c" . compare-windows)
         ("C-c = =" . ediff-files)
         ("C-c = f" . ediff-files)
         ("C-c = F" . ediff-files3)
         ("C-c = m" . count-matches)
         ("C-c = r" . ediff-revision)
         ("C-c = p" . ediff-patch-file)
         ("C-c = P" . ediff-patch-buffer)
         ("C-c = l" . ediff-regions-linewise)
         ("C-c = w" . ediff-regions-wordwise)))

(use-package edit-rectangle
  :straight f
  :bind ("C-x r e" . edit-rectangle))

(use-package edit-server
  :if window-system
  :defer 5
  :config
  (edit-server-start))

(use-package edit-var
  :straight f
  :bind ("C-c e v" . edit-variable))

(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode 1))

(use-package eldoc
  :straight f
  :diminish
  :hook ((c-mode-common emacs-lisp-mode) . eldoc-mode))

(use-package elint
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
  :commands elisp-depend-print-dependencies)

(use-package elisp-docstring-mode
  :commands elisp-docstring-mode)

(use-package elisp-slime-nav
  :diminish
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point))

(use-package elmacro
  :bind (("C-c m e" . elmacro-mode)
         ("C-x C-)" . elmacro-show-last-macro)))

(use-package emmet-mode
  :defer t
  :config
  (setq emmet-move-cursor-between-quotes t)
  :hook (sgml-mode . emmet-mode)
  :hook (css-mode  . emmet-mode))

(use-package erefactor
  :after elisp-mode
  :hook ((emacs-lisp-mode lisp-interaction-mode) . (lambda ()
						     (bind-key "\C-c\C-v" erefactor-map emacs-lisp-mode-map))))

(use-package etags
  :bind ("M-T" . tags-search))

(use-package eval-expr
  :requires (paredit)
  :bind ("M-:" . eval-expr)
  :preface
  (defun eval-expr-minibuffer-setup ()
    (local-set-key (kbd "<tab>") #'completion-at-point)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (paredit-mode)))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)		; only needed on MacOS
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :commands (er/expand-region
             er/mark-inside-pairs
             er/mark-inside-quotes
             er/mark-outside-pairs
             er/mark-outside-quotes
             er/mark-defun
             er/mark-comment
             er/mark-text-sentence
             er/mark-text-paragraph
             er/mark-word
             er/mark-url
             er/mark-email
             er/mark-symbol)
  :config
  (with-eval-after-load 'hydra
    (bind-key "C-="
              (defhydra hydra-mark (:hint nil)
                "
^Structure^      ^Pairs^              ^Misc^
^^^^^^^^-------------------------------------------
_SPC_: region    _P_: inside pairs    _u_: url
_d_: defun       _p_: outside pairs   _m_: email
_c_: comment     _Q_: inside quotes   _s_: symbol
_._: sentence    _q_: outside quotes
_h_: paragraph
"
                ("SPC" er/expand-region)
                ("P" er/mark-inside-pairs)
                ("Q" er/mark-inside-quotes)
                ("p" er/mark-outside-pairs)
                ("q" er/mark-outside-quotes)
                ("d" er/mark-defun)
                ("c" er/mark-comment)
                ("." er/mark-text-sentence)
                ("h" er/mark-text-paragraph)
                ("w" er/mark-word)
                ("u" er/mark-url)
                ("m" er/mark-email)
                ("s" Er/mark-symbol)))))

(use-package eyebrowse
  :defer t
  :diminish eyebrowse-mode
  :bind-keymap ("C-\\" . eyebrowse-mode-map)
  :bind (:map eyebrowse-mode-map
              ("C-\\ C-\\" . eyebrowse-last-window-config))
  :config
  (eyebrowse-mode)
  (setq eyebrowse-new-workspace t))

(use-package fancy-narrow
  :bind (("C-c N N" . fancy-narrow-to-region)
         ("C-c N W" . fancy-widen))
  :commands (fancy-narrow-to-region fancy-widen))

(use-package ffap
  :defer t
  :bind ("C-c v" . ffap))

(use-package flx
  :defer t)

(use-package flycheck
  :defer t
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :init
  (dolist (where '((emacs-lisp-mode-hook . emacs-lisp-mode-map)
                   (haskell-mode-hook    . haskell-mode-map)
                   (rust-mode-hook       . rust-mode-map)
                   (js2-mode-hook        . js2-mode-map)
                   (c-mode-common-hook   . c-mode-base-map)))
    (add-hook (car where)
              `(lambda ()
                 (bind-key "M-n" #'flycheck-next-error ,(cdr where))
                 (bind-key "M-p" #'flycheck-previous-error ,(cdr where)))
              t))
  :config

  (use-package flycheck-haskell
    :requires (flycheck haskell-mode)
    :config
    (flycheck-haskell-setup))

  (use-package flycheck-package
    :requires (flycheck)
    :config
    (use-package package-lint
      ;; This library provides a linter for the metadata in Emacs Lisp
      ;; files which are intended to be packages. You can integrate it
      ;; into your build process.
      :commands package-lint-current-buffer)
    
    (flycheck-package-setup))
  
  (use-package flycheck-popup-tip
    :requires flycheck
    :hook (flycheck-mode . flycheck-popup-tip-mode))

  (use-package flycheck-rust
    :requires (flycheck rust-mode)
    :config
    (flycheck-rust-setup))
  
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc flycheck-rtags))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (defalias 'show-error-at-point-soon 'flycheck-show-error-at-point)
  
  ;; (setq-default flycheck-display-errors-delay 0.5)
  ;; https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(use-package flyspell
  :defer
  :preface
  (defun org-mode-flyspell-verify-ignore-blocks (return-value)
    "Disable spell checking of embedded snippets in `org-mode`.
  See: http://emacs.stackexchange.com/a/9347"
    (let ((rlt return-value)
          (begin-regexp "^[ \t]*#\\+BEGIN_\\(SRC\\|HTML\\|LATEX\\)")
          (end-regexp "^[ \t]*#\\+END_\\(SRC\\|HTML\\|LATEX\\)")
          old-flag
          b e)
      (when return-value
        (save-excursion
          (setq old-flag case-fold-search)
          (setq case-fold-search t)
          (setq b (re-search-backward begin-regexp nil t))
          (if b (setq e (re-search-forward end-regexp nil t)))
          (setq case-fold-search old-flag))
        (if (and b e (< (point) e)) (setq rlt nil)))
      return-value))

  :init
  (when (executable-find "aspell")
    ;; Use Aspell for spellcheck
    (setq ispell-program-name (executable-find "aspell"))
    (setq ispell-list-command "--list")
    (setq ispell-dictionary "en_US"))

  ;; Flyspell messages slow down the spellchecking process
  (setq flyspell-issue-message-flag nil)
  (with-eval-after-load 'org-mode
    (advice-add 'org-mode-flyspell-verify :filter-return 'org-mode-flyspell-verify-ignore-blocks))
  
  :config
  (use-package flyspell-correct-ivy
    :requires (flyspell ivy) 
    :defer t
    :bind (:map flyspell-mode-map
		("C-;" . flyspell-correct-word-generic))
    :init
    ;; Set ivy as correcting interface.
    (setq flyspell-correct-interface 'flyspell-correct-ivy)))

(use-package font-lock-studio
  :commands (font-lock-studio
             font-lock-studio-region))

(use-package fullframe
  :defer t
  :init
  (autoload #'fullframe "fullframe"))

(use-package google-this
  :bind-keymap ("C-c /" . google-this-mode-submap)
  :bind* ("M-SPC" . google-this-search)
  :bind (:map google-this-mode-map
              ("/" . google-this-search)))

(use-package goto-chg
  :bind (("C-c b ," . goto-last-change)
         ("C-c b ." . goto-last-change-reverse)))

(use-package graphviz-dot-mode
  :mode "\\.dot\\'")

(use-package grep
  :if (executable-find "grep")
  :bind (("M-s n" . find-name-dired)
         ("M-s F" . find-grep)
         ("M-s G" . grep)
         ("M-s d" . find-grep-dired)))

(use-package haskell-mode
  :defines (haskell-process-reload-with-fbytecode)
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :bind (:map haskell-mode-map
              ("C-c C-h" . my/haskell-hoogle)
              ("C-c C-," . haskell-navigate-imports)
              ("C-c C-." . haskell-mode-format-imports)
              ("C-c C-u" . my/haskell-insert-undefined)
              ("[f7]"    . haskell-navigate-imports)
              ("M-s")
              ("M-t"))
  :preface
  (defun my/haskell-insert-undefined ()
    (interactive) (insert "undefined"))

  (defun snippet (name)
    (interactive "sName: ")
    (find-file (expand-file-name (concat name ".hs") "~/src/notes"))
    (haskell-mode)
    (goto-char (point-min))
    (when (eobp)
      (insert "hdr")
      (yas-expand)))

  (defvar hoogle-server-process nil)
  (defun my/haskell-hoogle (query &optional arg)
    "Do a Hoogle search for QUERY."
    (interactive
     (let ((def (haskell-ident-at-point)))
       (if (and def (symbolp def)) (setq def (symbol-name def)))
       (list (read-string (if def
                              (format "Hoogle query (default %s): " def)
                            "Hoogle query: ")
                          nil nil def)
             current-prefix-arg)))
    (unless (and hoogle-server-process
                 (process-live-p hoogle-server-process))
      (message "Starting local Hoogle server on port 8687...")
      (with-current-buffer (get-buffer-create " *hoogle-web*")
        (cd temporary-file-directory)
        (setq hoogle-server-process
              (start-process "hoogle-web" (current-buffer) "hoogle"
                             "server" "--local" "--port=8687")))
      (message "Starting local Hoogle server on port 8687...done"))
    (browse-url
     (format "http://127.0.0.1:8687/?hoogle=%s"
             (replace-regexp-in-string
              " " "+" (replace-regexp-in-string "\\+" "%2B" query)))))

  (defvar haskell-prettify-symbols-alist
    '(("::"     . ?∷)
      ("forall" . ?∀)
      ("exists" . ?∃)
      ("->"     . ?→)
      ("<-"     . ?←)
      ("=>"     . ?⇒)
      ("~>"     . ?⇝)
      ("<~"     . ?⇜)
      ("<>"     . ?⨂)
      ("msum"   . ?⨁)
      ("\\"     . ?λ)
      ("not"    . ?¬)
      ("&&"     . ?∧)
      ("||"     . ?∨)
      ("/="     . ?≠)
      ("<="     . ?≤)
      (">="     . ?≥)
      ("<<<"    . ?⋘)
      (">>>"    . ?⋙)

      ("`elem`"             . ?∈)
      ("`notElem`"          . ?∉)
      ("`member`"           . ?∈)
      ("`notMember`"        . ?∉)
      ("`union`"            . ?∪)
      ("`intersection`"     . ?∩)
      ("`isSubsetOf`"       . ?⊆)
      ("`isProperSubsetOf`" . ?⊂)
      ("undefined"          . ?⊥)))

  :preface
  (require 'haskell)
  (require 'haskell-doc)

  (defun my/haskell-mode-hook ()
    (haskell-indentation-mode)
    (interactive-haskell-mode)
    (diminish 'interactive-haskell-mode)
    (flycheck-mode 1)
    (setq-local prettify-symbols-alist haskell-prettify-symbols-alist)
    (prettify-symbols-mode 1)
    (bug-reference-prog-mode 1))

  (defun haskell-process-load-complete (session process buffer reload module-buffer &optional cont)
    "Handle the complete loading response. BUFFER is the string of
  text being sent over the process pipe. MODULE-BUFFER is the
  actual Emacs buffer of the module being loaded."
    (when (get-buffer (format "*%s:splices*" (haskell-session-name session)))
      (with-current-buffer (haskell-interactive-mode-splices-buffer session)
        (erase-buffer)))
    (let* ((ok (cond
                ((haskell-process-consume
                  process
                  "Ok, \\(?:\\([0-9]+\\|one\\)\\) modules? loaded\\.$")
                 t)
                ((haskell-process-consume
                  process
                  "Failed, \\(?:[0-9]+\\) modules? loaded\\.$")
                 nil)
                ((haskell-process-consume
                  process
                  "Ok, modules loaded: \\(.+\\)\\.$")
                 t)
                ((haskell-process-consume
                  process
                  "Failed, modules loaded: \\(.+\\)\\.$")
                 nil)
                (t
                 (error (message "Unexpected response from haskell process.")))))
           (modules (haskell-process-extract-modules buffer))
           (cursor (haskell-process-response-cursor process))
           (warning-count 0))
      (haskell-process-set-response-cursor process 0)
      (haskell-check-remove-overlays module-buffer)
      (while
          (haskell-process-errors-warnings module-buffer session process buffer)
        (setq warning-count (1+ warning-count)))
      (haskell-process-set-response-cursor process cursor)
      (if (and (not reload)
               haskell-process-reload-with-fbytecode)
          (haskell-process-reload-with-fbytecode process module-buffer)
        (haskell-process-import-modules process (car modules)))
      (if ok
          (haskell-mode-message-line (if reload "Reloaded OK." "OK."))
        (haskell-interactive-mode-compile-error session "Compilation failed."))
      (when cont
        (condition-case-unless-debug e
            (funcall cont ok)
          (error (message "%S" e))
          (quit nil)))))

  :config
  (use-package company-cabal
    :defer t
    :after (company haskell-cabal))

  (use-package ghc
    :disabled
    :load-path
    (lambda ()
      (cl-mapcan
       #'(lambda (lib) (directory-files lib t "^ghc-"))
       (cl-mapcan
        #'(lambda (lib) (directory-files lib t "^elpa$"))
        (filter (apply-partially #'string-match "-emacs-ghc-") load-path))))
    :commands ghc-init
    :config
    (setenv "cabal_helper_libexecdir"
            (file-name-directory
             (substring
              (shell-command-to-string "which cabal-helper-wrapper") 0 -1)))
    :hook (haskell-mode . ghc-init))

  (use-package haskell-edit
    :straight f
    :load-path "lisp/haskell-config"
    :bind (:map haskell-mode-map
                ("C-c M-q" . haskell-edit-reformat)))

  (use-package hindent-mode
    :straight (:host github :repo "commercialhaskell/hindent")
    :hook (haskell-mode . hindent-mode))

  (eval-after-load 'align
    '(nconc
      align-rules-list
      (mapcar #'(lambda (x)
                  `(,(car x)
                    (regexp . ,(cdr x))
                    (modes quote (haskell-mode literate-haskell-mode))))
              '((haskell-types       . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                (haskell-assignment  . "\\(\\s-+\\)=\\s-+")
                (haskell-arrows      . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                (haskell-left-arrows . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")))))

  :hook (haskell-mode . my/haskell-mode-hook))

(use-package hydra
  :defer t
  :config
  (bind-key "M-s-o" (defhydra hydra-window (:color amaranth)
                      "window"
                      ("n" windmove-left)
                      ("r" windmove-down)
                      ("t" windmove-up)
                      ("d" windmove-right)
                      ("v" (lambda ()
                             (interactive)
                             (split-window-right)
                             (windmove-right))
                       "vert")
                      ("x" (lambda ()
                             (interactive)
                             (split-window-below)
                             (windmove-down))
                       "horz")
                      ;; ("t" transpose-frame "'")
                      ("o" delete-other-windows "one" :color blue)
                      ("a" ace-window "ace")
                      ("s" ace-swap-window "swap")
                      ("k" ace-delete-window "del")
                      ("i" ace-maximize-window "ace-one" :color blue)
                      ("b" ivy-switch-buffer "buf")
                      ("f" counsel-find-file "file")
                      ;; ("m" headlong-bookmark-jump "bmk")
                      ("q" nil "cancel")))

  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")
    ("0" text-scale-adjust "reset"))

  (defhydra hydra-error (global-map "M-g")
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit")))

(use-package imenu-list
  :commands imenu-list-minor-mode)

(use-package indent
  :straight f
  :commands indent-according-to-mode)

(use-package indent-shift
  :straight f
  :bind (("C-c <" . indent-shift-left)
         ("C-c >" . indent-shift-right)))

(use-package info
  :bind ("C-h C-i" . info-lookup-symbol)
  :config
  :hook (Info-mode . (lambda ()
		                   (setq buffer-face-mode-face '(:family "Bookerly"))
		                   (buffer-face-mode)
		                   (text-scale-adjust 1))))

(use-package info-look
  :after (info)
  :defer t
  :init
  (autoload 'info-lookup-add-help "info-look")
  :config
  (use-package info-lookmore
    :config
    (info-lookmore-elisp-cl)
    (info-lookmore-elisp-userlast)
    (info-lookmore-elisp-gnus)
    (info-lookmore-apropos-elisp)))

(use-package isearch
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
  :no-require t
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region)))

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
              ("M-j"   . my/ivy-yank-whole-word))
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

  :config
  (use-package counsel
    :after (swiper)
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
    :bind (:map ivy-minibuffer-map
                ("M-y"    . ivy-next-line))
    :config
    (ivy-set-actions
     'counsel-find-file
     '(("j" find-file-other-window "other")))
    (ivy-set-actions 'counsel-git-grep
                     '(("j" find-file-other-window "other"))))
  (use-package swiper
    :diminish ivy-mode
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
      (swiper-mc))
    :config
    (use-package ivy-bibtex
      :after (ivy)
      :defer t
      :commands ivy-bibtex)

    (use-package ivy-hydra
      :after (hydra)
      :defer t)

    (use-package ivy-pass
      :defer t
      :commands ivy-pass)

    (use-package ivy-rich
      :demand t
      :config
      (ivy-set-display-transformer 'ivy-switch-buffer
                                   'ivy-rich-switch-buffer-transformer)
      (setq ivy-virtual-abbreviate 'full
            ivy-rich-switch-buffer-align-virtual-buffer t
            ivy-rich-path-style 'abbrev))

    (setq ivy-count-format             ""
	  ivy-dynamic-exhibit-delay-ms 200
	  ivy-height                   10
	  ivy-initial-inputs-alist     nil
	  ivy-magic-tilde              nil
	  ivy-re-builders-alist        '((t . ivy--regex-ignore-order))
	  ivy-use-selectable-promnpt   t
	  ivy-use-virtual-buffers      t
	  ivy-wrap                     t)
    
    (ivy-mode 1)
    (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
    (with-eval-after-load 'flx
      (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (use-package json-reformat)
  (use-package json-snatcher))

(use-package key-chord
  :commands key-chord-mode)

(use-package lisp-mode
  :straight f
  :defer t
  :hook ((emacs-lisp-mode lisp-mode)
         . (lambda () (add-hook 'after-save-hook 'check-parens nil t)))
  :init
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

(use-package lsp-haskell
  ;; https://github.com/emacs-lsp/lsp-haskell
  ;; Reuqires installation of haskell-lsp: https://github.com/alanz/haskell-lsp
  :if (executable-find "hie")
  :requires (lsp-mode lsp-ui haskell-mode)
  :hook (haskell-mode . lsp-haskell-enable))

(use-package lsp-mode
  :after prog-mode
  :preface
  (defun my/set-projectile-root ()
    "Automatically set the LSP workspace to the current Projectile root when changing projects."
    (when lsp--cur-workspace
      (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))

  :config
  (use-package lsp-ui
    :requires (lsp-mode imenu)
    :hook (lsp-after-open . lsp-enable-imenu)
    :hook (lsp-mode       . lsp-ui-mode))

  (use-package lsp-flycheck
    :straight f
    :requires (lsp-mode lsp-ui flycheck))

  (with-eval-after-load 'projectile
    (add-hook 'lsp-before-open-hook #'my/set-projectile-root)))

(use-package lsp-rust
  :requires (lsp-mode lsp-ui rust-mode)
  :if (executable-find "rustup")
  :config
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  :hook (rust-mode . lsp-rust-enable))

(use-package macrostep
  :unless noninteractive
  :bind ("C-c e m" . macrostep-expand))

(use-package magit
  :if (executable-find "git")
  :bind (("C-x g" . magit-status)
         ("C-x G" . my/magit-status-with-prefix))
  :bind (:map magit-mode-map
              ("U" . magit-unstage-all)
              ("M-h") ("M-s") ("M-m") ("M-w"))
  :bind (:map magit-file-section-map ("<C-return>"))
  :bind (:map magit-hunk-section-map ("<C-return>"))
  :preface
  (defun my/magit-monitor (&optional no-display)
    "Start git-monitor in the current directory."
    (interactive)
    (let* ((path (file-truename
                  (directory-file-name
                   (expand-file-name default-directory))))
           (name (format "*git-monitor: %s*"
                         (file-name-nondirectory path))))
      (unless (and (get-buffer name)
                   (with-current-buffer (get-buffer name)
                     (string= path (directory-file-name default-directory))))
        (with-current-buffer (get-buffer-create name)
          (cd path)
          (ignore-errors
            (start-process "*git-monitor*" (current-buffer)
                           "git-monitor" "-d" path))))))

  (defun my/magit-status-with-prefix ()
    "Start magit status with a particular prefix"
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))

  (require 'dash)

  (defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
    "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
    `(prog1
	 (add-to-list 'pretty-magit-alist
		      (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
			    ,ICON ',PROPS))
       (unless ,NO-PROMPT?
	 (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

  (defun add-magit-faces ()
    "Add face properties and compose symbols for buffer from pretty-magit."
    (interactive)
    (with-silent-modifications
      (--each pretty-magit-alist
	(-let (((rgx icon props) it))
	  (save-excursion
	    (goto-char (point-min))
	    (while (search-forward-regexp rgx nil t)
	      (compose-region
	       (match-beginning 1) (match-end 1) icon)
	      (when props
		(add-face-text-property
		 (match-beginning 1) (match-end 1) props))))))))
  
  (defun use-magit-commit-prompt (&rest args)
    (setq use-magit-commit-prompt-p t))

  (defun magit-commit-prompt ()
    "Magit prompt and insert commit header with faces."
    (interactive)
    (when use-magit-commit-prompt-p
      (setq use-magit-commit-prompt-p nil)
      (if (featurep 'ivy)
	  (insert (ivy-read "Commit Type " pretty-magit-prompt
			    :require-match t :sort t :preselect "Add: ")))
      (if (featurep 'helm)
	  (insert (helm :sources (helm-build-sync-source "Commit Type "
							 :candidates pretty-magit-prompt)
			:buffer "*magit cmt prompt*")))
      (add-magit-faces)
      (if (featurep 'evil)
	  (evil-insert 1))))
  
  :config
  (use-package gist
    :no-require t
    :bind ("C-c G" . my/gist-region-or-buffer)
    :preface
    (defun my/gist-region-or-buffer (start end)
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

  (use-package git-gutter-fringe+
    :diminish git-gutter-fringe+-mode
    :config
    (global-git-gutter+-mode))

  (use-package git-link
    :bind ("C-c Y" . git-link)
    :commands (git-link git-link-commit git-link-homepage))

  (use-package git-timemachine
    :commands git-timemachine)

  (use-package git-undo
    :load-path "lisp/git-undo"
    :commands git-undo)

  (use-package gitattributes-mode
    :defer 5)

  (use-package gitconfig-mode
    :defer 5)

  (use-package github-pullrequest
    :commands (github-pullrequest-new
	       github-pullrequest-checkout))

  (use-package gitignore-mode
    :defer 5)

  (use-package gitpatch
    :commands gitpatch-mail)
  
  (use-package magit-commit
    :straight f
    :config
    (use-package git-commit))

  (use-package magit-files
    :straight f
    :config
    (global-magit-file-mode))

  (use-package magit-imerge
    :if (executable-find "git-imerge")
    :after magit)
  
  (use-package magit-popup
    :defer t)
  
  (use-package magithub
    :after magit
    :config
    (use-package magithub-completion
      :straight f
      :commands magithub-completion-enable)

    (magithub-feature-autoinject t)

    (require 'auth-source-pass)
    (defvar my/ghub-token-cache nil)

    (advice-add
     'ghub--token :around
     #'(lambda (orig-func host username package &optional nocreate forge)
         (or my/ghub-token-cache
             (setq my/ghub-token-cache
                   (funcall orig-func host username package nocreate forge))))))

  (with-eval-after-load 'magit-remote
    (magit-define-popup-action 'magit-fetch-popup
      ?f 'magit-get-remote #'magit-fetch-from-upstream ?u t)
    (magit-define-popup-action 'magit-pull-popup
      ?F 'magit-get-upstream-branch #'magit-pull-from-upstream ?u t)
    (magit-define-popup-action 'magit-push-popup
      ?P 'magit--push-current-to-upstream-desc
      #'magit-push-current-to-upstream ?u t))
  (put 'magit-clean 'dsabled nil)

  (setq pretty-magit-alist nil)
  (setq pretty-magit-prompt nil)
  
  (pretty-magit "Feature" ? (:foreground "slate gray" :height 1.2))
  (pretty-magit "Add"     ? (:foreground "#375E97" :height 1.2))
  (pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.2))
  (pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.2))
  (pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "master"  ? (:box t :height 1.2) t)
  (pretty-magit "origin"  ? (:box t :height 1.2) t)

  (advice-add 'magit-status :after 'add-magit-faces)
  (advice-add 'magit-refresh-buffer :after 'add-magit-faces)

  (setq use-magit-commit-prompt-p nil)
  (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
  (advice-add 'magit-commit :after 'use-magit-commit-prompt)

  :hook (git-comit-setup       . magit-commit-prompt)
  :hook (magit-mode            . hl-line-mode)
  :hook (magit-status-mode     . (lambda () (my/magit-monitor t))))

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
    :if (executable-find "multimarkdown")
    :config
    (setq markdown-preview-stylesheets
          (list (concat "https://github.com/dmarcotte/github-markdown-preview/"
			"blob/master/data/css/github.css"))))
  
  (with-eval-after-load 'flyspell-mode
    ;; Turn on `flyspell-mode` when available.
    (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1))))
  
  :hook (markdown-mode . visual-line-mode))

(use-package math-symbol-lists
  :defer t)

(use-package minibuffer
  :straight f
  :preface
  (defun my/minibuffer-setup-hook ()
    "Setup minibuffer for use."
    ;; Disable garbage collection while in minibuffer to avoid stalls.
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my/minibuffer-exit-hook ()
    "Restore minibuffer setup changes."
    ;; Restore garbage collection afer exiting the minibuffer.
    (setq gc-cons-threshold 800000))

  :hook (minibuffer-setup . my/minibuffer-setup-hook)
  :hook (minibuffer-exit  . my/minibuffer-exit-hook))

(use-package multiple-cursors
  :defer 5
  ;; - Sometimes you end up with cursors outside of your view. You can scroll
  ;;   the screen to center on each cursor with `C-v` and `M-v`.
  ;;
  ;; - If you get out of multiple-cursors-mode and yank - it will yank only
  ;;   from the kill-ring of main cursor. To yank from the kill-rings of every
  ;;   cursor use yank-rectangle, normally found at C-x r y.

  :bind (("<C-m> ^"     . mc/edit-beginnings-of-lines)
         ("<C-m> `"     . mc/edit-beginnings-of-lines)
         ("<C-m> $"     . mc/edit-ends-of-lines)
         ("<C-m> '"     . mc/edit-ends-of-lines)
         ("<C-m> R"     . mc/reverse-regions)
         ("<C-m> S"     . mc/sort-regions)
         ("<C-m> W"     . mc/mark-all-words-like-this)
         ("<C-m> Y"     . mc/mark-all-symbols-like-this)
         ("<C-m> a"     . mc/mark-all-like-this-dwim)
         ("<C-m> c"     . mc/mark-all-dwim)
         ("<C-m> l"     . mc/insert-letters)
         ("<C-m> n"     . mc/insert-numbers)
         ("<C-m> r"     . mc/mark-all-in-region)
         ("<C-m> s"     . set-rectangular-region-anchor)
         ("<C-m> %"     . mc/mark-all-in-region-regexp)
         ("<C-m> t"     . mc/mark-sgml-tag-pair)
         ("<C-m> w"     . mc/mark-next-like-this-word)
         ("<C-m> x"     . mc/mark-more-like-this-extended)
         ("<C-m> y"     . mc/mark-next-like-this-symbol)
         ("<C-m> C-x"   . reactivate-mark)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> ("     . mc/mark-all-symbols-like-this-in-defun)
         ("<C-m> C-("   . mc/mark-all-words-like-this-in-defun)
         ("<C-m> M-("   . mc/mark-all-like-this-in-defun)
         ("<C-m> ["     . mc/vertical-align-with-space)
         ("<C-m> {"     . mc/vertical-align)

         ("S-<down-mouse-1>")
         ("S-<mouse-1>" . mc/add-cursor-on-click))

  :bind (:map selected-keymap
              ("c"   . mc/edit-lines)
              ("."   . mc/mark-next-like-this)
              ("<"   . mc/unmark-next-like-this)
              ("C->" . mc/skip-to-next-like-this)
              (","   . mc/mark-previous-like-this)
              (">"   . mc/unmark-previous-like-this)
              ("C-<" . mc/skip-to-previous-like-this)
              ("y"   . mc/mark-next-symbol-like-this)
              ("Y"   . mc/mark-previous-symbol-like-this)
              ("w"   . mc/mark-next-word-like-this)
              ("W"   . mc/mark-previous-word-like-this))
  :config
  (use-package mc-extras
    :requires (multiple-cursors)
    :bind (("<C-m> M-C-f" . mc/mark-next-sexps)
           ("<C-m> M-C-b" . mc/mark-previous-sexps)
           ("<C-m> <"     . mc/mark-all-above)
           ("<C-m> >"     . mc/mark-all-below)
           ("<C-m> C-d"   . mc/remove-current-cursor)
           ("<C-m> C-k"   . mc/remove-cursors-at-eol)
           ("<C-m> M-d"   . mc/remove-duplicated-cursors)
           ("<C-m> |"     . mc/move-to-column)
           ("<C-m> ~"     . mc/compare-chars)))

  (use-package mc-freeze
    :straight f
    :requires (multiple-cursors)
    :bind ("<C-m> f" . mc/freeze-fake-cursors-dwim))

  (use-package mc-rect
    :straight f
    :requires (multiple-cursors)
    :bind ("<C-m> ]" . mc/rect-rectangle-to-multiple-cursors))

  (use-package phi-search
    :defer 5
    :requires (multiple-cursors)
    :config
    (use-package phi-search-mc
      :requires (multiple-cursors)
      :config 
      (phi-search-mc/setup-keys)
      
      :hook (isearch-mode-mode . phi-search-from-isearch-mc/setup-keys))
    ))

(use-package nxml-mode
  :straight f
  :commands nxml-mode
  :bind (:map nxml-mode-map
              ("<return>" . newline-and-indent)
              ("C-c M-h"  . tidy-xml-buffer))
  :preface
  (defun tidy-xml-buffer ()
    (interactive)
    (save-excursion
      (call-process-region (point-min) (point-max)
			                     "tidy" t t nil
                           "-xml" "-i" "-wrap" "0" "-omit" "-q" "-utf8")))
  :init
  (defalias 'xml-mode 'nxml-mode)
  :config
  (autoload 'sgml-skip-tag-forward "sgml-mode")
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"
                 "<!--"
                 sgml-skip-tag-forward
                 nil)))

(use-package org
  :straight org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :bind* (("M-m"     . org-smart-capture)
          ("M-M"     . org-inline-note)
          ("C-c S"   . org-store-link)
          ("C-c l"   . org-insert-link))
  :defines (org-directory org-default-notes-files)
  :preface
  ;; Remove comments from org document for use with export hook.
  ;; https://emacs.stackexchange.com/questions/22574/orgmode-export-how-to-prevent-a-new-line-for-comment-lines
  (defun my/delete-org-comments (backend)
    "Remove comments from org document.
  For use with export hook."
    (loop for comment in (reverse (org-element-map (org-element-parse-buffer)
						   'comment 'identity))
          do
          (setf (buffer-substring (org-element-property :begin comment)
                                  (org-element-property :end comment))
                "")))

  (defun my/is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun my/list-sublevels-for-projects-indented ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels 'indented)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defun my/skip-non-stuck-projects ()
    "Skip trees that are not stuck projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (my/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next (save-excursion
                               (forward-line 1)
                               (and (< (point) subtree-end)
                                    (re-search-forward "^\\*+ \\(TODO\\) " subtree-end t)))))
              (if has-next
                  next-headline
                nil)) ; a stuck project, has subtasks but no next task
          next-headline))))

  (defun my/skip-non-projects ()
    "Skip trees that are not projects"
    (my/list-sublevels-for-projects-indented)
    (if (save-excursion (my/skip-non-stuck-projects))
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (if (my/is-project-p)
                nil
              subtree-end)))
      (org-end-of-subtree t)))

  (defun my/skip-projects ()
    "Skip trees that are projects."
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((my/is-project-p)
          next-headline)
         (t
          nil)))))

  (defun my/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  
  :init
  (load "org-settings" :noerror)
  (setq org-directory user-org-directory
	org-enforce-todo-dependencies t
	org-fast-tag-selection-single-key 'expert
	org-footnote-auto-adjust nil
	org-footnote-define-inline t
	org-hide-leading-stars t
	org-highlight-latex-and-related '(latex)
	org-log-into-drawer "LOGBOOK"
	org-outline-path-complete-in-steps t
	org-refile-targets '((org-agenda-files :maxlevel . 3) (nil :maxlevel . 3))
	org-refile-use-outline-path 'file
	org-tag-alist '((:startgroup . nil)
			("@call" . ?c)
			("@office" . ?o)
			("@home" . ?h)
			("@read" . ?r)
			("@computer" . ?m)
			("@dev" . ?d)
			("@write" . ?w)
			(:endgroup . nil)
			("REFILE" . ?f)
			("SOMEDAY" . ?s)
			("PROJECT" . ?p))
	org-tags-exclude-from-inheritance '("@call"
					    "@office"
					    "@home"
					    "@read"
					    "@computer"
					    "@dev"
					    "@write"
					    "PROJECT")
	org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")
			    (sequence "WAITING(w@/!)" "|" "CANCELLED" "DELEGATED(e@)"))
	org-use-fast-todo-selection t
	org-use-speed-commands t
	org-use-sub-superscripts "{}"
	org-use-tag-inheritance t)
  
  :config
  (use-package org-agenda
    :straight f
    :bind (("<f6>"  . my/org-agenda)
           ("C-c a" . org-agenda))
    :preface
    (defun my/org-agenda ()
      (interactive)
      (org-agenda nil "w"))
    
    (defun my/org-agenda-width ()
      "Set the width of the Org Agenda window based on the contents."
      (setq org-agenda-tags-column (- (window-width))))
    
    :config
    (use-package org-super-agenda
      ;; https://github.com/alphapapa/org-super-agenda      
      :after org-agenda
      :config
      (let ((org-super-agenda-groups
             '(;; Each group has an implicit boolean OR operator between its selectors.
               (:name "Today"  ; Optionally specify section name
                      :time-grid t  ; Items that appear on the time grid
                      :todo "TODAY")  ; Items that have this TODO keyword
               (:name "Important"
                      ;; Single arguments given alone
                      :tag "bills"
                      :priority "A")
               ;; Set order of multiple groups at once
               (:order-multi (2 (:name "Shopping in town"
                                       ;; Boolean AND group matches items that match all subgroups
                                       :and (:tag "shopping" :tag "@town"))
                                (:name "Food-related"
                                       ;; Multiple args given in list with implicit OR
                                       :tag ("food" "dinner"))
                                (:name "Personal"
                                       :habit t
                                       :tag "personal")
                                (:name "Space-related (non-moon-or-planet-related)"
                                       ;; Regexps match case-insensitively on the entire entry
                                       :and (:regexp ("space" "NASA")
                                                     ;; Boolean NOT also has implicit OR between selectors
                                                     :not (:regexp "moon" :tag "planet")))))
               ;; Groups supply their own section names when none are given
               (:todo "WAITING" :order 8)  ; Set order of this section
               (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                      ;; Show this group at the end of the agenda (since it has the
                      ;; highest number). If you specified this group last, items
                      ;; with these todo keywords that e.g. have priority A would be
                      ;; displayed in that group instead, because items are grouped
                      ;; out in the order the groups are listed.
                      :order 9)
               (:priority<= "B"
                            ;; Show this section after "Today" and "Important", because
                            ;; their order is unspecified, defaulting to 0. Sections
                            ;; are displayed lowest-number-first.
                            :order 1)
               ;; After the last group, the agenda will display items that didn't
               ;; match any of these groups, with the default order position of 99
               )))
        (org-agenda nil "a")))

    (setq org-agenda-custom-commands '(("w" "Day's Agenda and Tasks"
                                        ((agenda "" (( org-agenda-span 1)))
                                         (tags-todo "-SOMEDAY/!"
                                                    ((org-agenda-overriding-header "Stuck Projects")
                                                     (org-agenda-skip-function 'my/skip-non-stuck-projects)))
                                         (tags-todo "-SOMEDAY/!"
                                                    ((org-agenda-overriding-header "Projects")
                                                     (org-agenda-skip-function 'my/skip-non-projects)
                                                     (org-agenda-ignore-scheduled 'future)
                                                     (org-agenda-ignore-deadlines 'future)
                                                     (org-agenda-sorting-strategy
                                                      '(category-keep))))
                                         (tags-todo "-CANCELLED/!WAITING"
                                                    ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                                                     (org-agenda-skip-function 'my/skip-projects)
                                                     (org-agenda-todo-ignore-scheduled t)
                                                     (org-agenda-todo-ignore-deadlines t)))
                                         (tags-todo "-SOMEDAY/!-WAITING"
                                                    ((org-agenda-overriding-header "Tasks")
                                                     (org-agenda-skip-function 'my/skip-projects)
                                                     (org-agenda-todo-ignore-scheduled t)
                                                     (org-agenda-todo-ignore-deadlines t)
                                                     (org-agenda-sorting-strategy
                                                      '(category-keep))))
                                         nil))
                                       ("#" "Stuck Projects" tags-todo "-SOMEDAY/!"
                                        ((org-agenda-overriding-header "Stuck Projects")
                                         (org-agenda-skip-function 'my/skip-non-stuck-projects)))
                                       ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-WAITING"
                                        ((org-agenda-overriding-header "Tasks")
                                         (org-agenda-skip-function 'my/skip-projects)
                                         (org-agenda-sorting-strategy
                                          '(category-keep))))
                                       ("p" "Project Lists" tags-todo "-SOMEDAY/!"
                                        ((org-agenda-overriding-header "Projects")
                                         (org-agenda-skip-function 'my/skip-non-projects)
                                         (org-agenda-ignore-scheduled 'future)
                                         (org-agenda-ignore-deadlines 'future)
                                         (org-agenda-sorting-strategy
                                          '(category-keep))))
                                       ("b" "Waiting Tasks" tags-todo "-CANCELLED/!WAITING"
                                        ((org-agenda-overriding-header "Waiting and Postponed tasks")
                                         (org-agenda-skip-function 'my/skip-projects)
                                         (org-agenda-todo-ignore-scheduled 'future)
                                         (org-agenda-todo-ignore-deadlines 'future)))
                                       ("e" "Errand List" tags-todo "@shops"
                                        ((org-agenda-prefix-format "[ ]")
                                         (org-agenda-todo-keyword-format "")))
                                       ("c" todo "TODO"
                                        ((org-agenda-sorting-strategy '(tag-up priority-down)))))
          org-agenda-compact-blocks t
          org-agenda-diary-file (expand-file-name "diary.org" org-directory)
          org-agenda-files (list (expand-file-name org-directory) (expand-file-name "projects/" org-directory))
          org-agenda-prefix-format '((agenda    . " %i %-12:c%?-12t% s %b")
                                     (timeline  . "  % s %b")
                                     (todo      . " %i %-12:c %b")
                                     (tags      . " %i %-12:c %b")
                                     (search    . " %i %-12:c %b"))
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-done t
          org-agenda-tags-todo-honor-ignore-options t
          org-agenda-todo-ignore-with-date t
          org-agenda-window-setup 'current-window)
    
    :hook (org-agenda-mode . my/org-agenda-width))

  (use-package org-capture
    :straight f	
    :bind (("C-c r"  . org-capture)
           ("C-9"    . my/org-capture-todo))
    :preface
    (defun my/org-capture-todo ()
      "Capture a TODO action in `org-mode`."
      (interactive)
      (org-capture nil "t"))

    :config
    (setq org-capture-templates '(("i" "Interruption" entry
                                   (expand-file-name "inbox.org" user-org-directory)
                                   "* %?\n"
                                   :clock-in t)
                                  ("n" "Notes" entry
                                   (expand-file-name "inbox.org" user-org-directory)
                                   "* %?\n%U\n%i\n%a")
                                  ("t" "Todo" entry
                                   (expand-file-name "inbox.org" user-org-directory)
                                   "* TODO %?\n%U\n%i\n%a")
                                  ("b" "Book" entry
                                   (file+headline (expand-file-name "reading.org" user-org-directory) "Read")
                                   "** %^{Title}\n:PROPERTIES:\n:Author: %^{Author}p \n:Started: %u\n:Finished: \n:END:\n\n"
                                   :immediate-finish t))))

  (use-package org-babel
    :straight f
    :no-require
    :config
    (use-package ob-diagrams)
    (use-package ob-restclient)
    (use-package ob-rust)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (calc       . t)
       (ditaa      . t)
       (dot        . t)
       (emacs-lisp . t)
       (latex      . t)
       (plantuml   . t)
       (python     . t)
       (restclient . t)
       (rust       . t)
       (sh         . t)
       (sql        . t)
       (sqlite     . t)
       )))

  (use-package org-noter
    :commands org-noter)

  (use-package org-pdfview
    :requires pdf-tools
    :config
    (delete '("\\.pdf\\'" . default) org-file-apps)
    (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
    (add-to-list 'org-file-apps
                 '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open)))

  (use-package org-protocol :straight f)

  (use-package org-ref
    ;; See https://github.com/jkitchin/org-ref/blob/master/org-ref.org
    :requires bibtex
    :preface
    (defun my/org-ref-open-pdf-at-point ()
      "Open the pdf for bibtex key under point if it exists."
      (interactive)
      (let* ((results (org-ref-get-bibtex-key-and-file))
             (key (car results))
             (pdf-file (funcall org-ref-get-pdf-filename-function key)))
        (if (file-exists-p pdf-file)
            (find-file pdf-file)
          (message "No PDF found for %s" key))))
    :init
    (setq org-ref-completion-library 'org-ref-ivy-cite
          org-ref-open-pdf-function #'my/org-ref-open-pdf-at-point)
    
    :config
    (require 'org-ref-ivy)
    (require 'org-ref-isbn)

    (setq bibtex-completion-bibliography   (expand-file-name "references.bib" user-bib-directory)
          bibtex-completion-library-path   (expand-file-name "bibtex-pdfs" user-bib-directory)
          bibtex-completion-notes-path	   (expand-file-name "bibtex-notes" user-bib-directory)
          org-ref-bibliography-notes       (expand-file-name "notes.org" user-bib-directory)
          org-ref-default-bibliography     (expand-file-name "references.bib" user-bib-directory)
          org-ref-pdf-directory            (expand-file-name "bibtex-pdfs/" user-bib-directory)
          reftex-default-bibliography      (expand-file-name "references.bib" user-bib-directory)
          
          org-ref-show-broken-links t)
    
    ;; Settings that control the format of the autogenerated key.
    (setq bibtex-autokey-year-length           4
          bibtex-autokey-name-year-separator   "-"
          bibtex-autokey-year-title-separator  "-"
          bibtex-autokey-titleword-separator   "-"
          bibtex-autokey-titlewords            2
          bibtex-autokey-titlewords-stretch    1
          bibtex-autokey-titleword-length      5)
    
    (when (executable-find "pdflatex")
      (setq  org-latex-pdf-process
             '("pdflatex -interaction nonstopmode -output-directory %o %f"
               "bibtex %b"
               "pdflatex -interaction nonstopmode -output-directory %o %f"
               "pdflatex -interaction nonstopmode -output-directory %o %f")))

    (when (and (eq system-type 'darwin)
               (fboundp 'pdf-tools))
      ;; Open PDF files with system PDF viewer (works on Mac).
      (setq bibtex-completion-pdf-open-function 'org-open-file)))

  (use-package org-reveal
    ;; Make slides with org-mode and reveal.js.
    ;; https://github.com/yjwen/Org-Reveal
    :straight (:host github :repo "yjwen/org-reveal")
    :after org-mode
    :defer t
    :config
    (use-package ox-reveal
      :init
      (setq org-reveal-hlevel 2
            ;; NOTE: org-reveal-root *must* be in URL form (e.g., "file:///...")
            org-reveal-root (concat "file:///" (getenv "HOME") "/Documents/slides/reveal.js")
            org-reveal-title-slide 'auto)))
  
  (use-package org-rich-yank
    :defer 5
    :bind (:map org-mode-map
                ("C-M-y" . org-rich-yank)))

  (use-package org-velocity
    :straight f
    :bind ("C-, C-." . org-velocity))

  (use-package org-web-tools
    :bind (("C-, C-y"   . my/org-insert-url)
           ("C-, C-M-y" . org-web-tools-insert-web-page-as-entry))
    :functions (org-web-tools--org-link-for-url
                org-web-tools--get-first-url)
    :preface
    (declare-function org-web-tools--org-link-for-url "org-web-tools")
    (declare-function org-web-tools--get-first-url "org-web-tools")
    
    (defun my/org-insert-url (&optional arg)
      "Insert a URL into an org-mode document."
      (interactive "P")
      (require' org-web-tools)
      (let ((link (org-web-tools--org-link-for-url
                   (org-web-tools--get-first-url))))
        (if arg
            (progn
              (org-set-property "URL" link)
              (message "Added pasteboard link to URL property"))
          (insert link)))))

  (use-package orgnav)

  (use-package orgtbl-aggregate
    :config
    (load "org-insert-dblock"))

  (use-package ox-confluence
    ;; Export Org files to confluence:
    ;; M-x org-confluence-export-as-confluence RET
    ;; https://github.com/emacsmirror/org/blob/master/contrib/lisp/ox-confluence.el
    :straight f
    :defer t)

  (use-package ox-jira
    ;; Transforms Org files to JIRA markup for pasting into JIRA tickets & comments.
    ;; https://github.com/stig/ox-jira.el
    :defer t
    :init
    (setq org-export-copy-to-kill-ring 'if-interactive))

  (use-package ox-md
    :straight f
    :requires markdown
    :defer t)

  (use-package ox-pandoc
    :requires pandoc
    :defer t)

  (use-package ox-publish
    :disabled
    :defer t
    :commands (my/publish-blog)
    :preface
    (defun my/publish-blog ()
      "Publish my blog"
      (interactive)
      (org-publish-project "blog" t))
    
    :config
    (require 'ox-html)
    (require 'ox-rss)

    (setq org-confirm-babel-evaluate nil
          org-publish-project-alist '(("blog-content"
                                       :base-directory "~/personal/markcol/"
                                       :base-extension "org"
                                       :recursive t
                                       :publishing-directory "~/Sites/markcol/"
                                       :publishing-function (pd-html-publish-to-html)
                                       :with-toc nil
                                       :html-html5-fancy t
                                       :section-numbers nil
                                       :exclude "rss.org")
                                      ("blog-static"
                                       :base-directory "~/personal/markcol/"
                                       :base-extension "jpg\\|png\\|css\\|js\\|ico\\|gif"
                                       :recursive t
                                       :publishing-directory "~/Sites/markcol/"
                                       :publishing-function org-publish-attachment)
                                      ("blog-rss"
                                       :base-directory "~/personal/markcol/"
                                       :base-extension "org"
                                       :publishing-directory "~/Sites/markcol/"
                                       :publishing-function (org-rss-publish-to-rss)
                                       :html-link-home "~/Sites/markcol/"
                                       :html-link-use-abs-url t
                                       :exclude ".*"
                                       :include ("rss.org")
                                       :with-toc nil
                                       :section-numbers nil
                                       :title "Mark Colburn")
                                      ("blog"
                                       :components
                                       ("blog-content" "blog-static" "blog-rss")))))

  (use-package ox-texinfo-plus
    :straight f
    :requires texinfo
    :defer t)

  ;; Set the initial buffer to be the org agenda view.
  (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (setq initial-buffer-choice org-default-notes-files)
  
  :hook (org-export-before-processing . my/delete-org-comments))

(use-package paredit
  :diminish
  :defer t
  :commands (paredit-mode paredit-backward-delete paredit-close-round paredit-newline)
  :bind (:map lisp-mode-map
              ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map
              ("<return>" . paredit-newline))
  ;; :bind (("C-c ( n"    . paredit-add-to-next-list)
  ;;        ("C-c ( p"    . paredit-add-to-previous-list)
  ;;        ("C-c ( j"    . paredit-join-with-next-list)
  ;;        ("C-c ( J"    . paredit-join-with-previous-list))
  ;; :bind (:map paredit-mode-map
  ;;             ("[")
  ;;             ("M-k"   . paredit-raise-sexp)
  ;;             ("M-I"   . paredit-splice-sexp)
  ;;             ("C-M-l" . paredit-recentre-on-sexp))
  :preface
  (defun my/paredit-mode-hook ()
    "Paredit mode customizations."
    (require 'eldoc)
    ;; (unbind-key "M-r" paredit-mode-map)
    ;; (unbind-key "M-s" paredit-mode-map)
    (eldoc-add-command 'paredit-backward-delete
		       'paredit-close-round)
    (paredit-mode))
  
  :config
  (use-package paredit-ext
    :straight f
    :after paredit
    :load-path "lisp")

  :hook ((lisp-mode emacs-lisp-mode) . my/paredit-mode-hook))

(use-package pdf-tools
  :if (executable-find "pdf-tools")
  :magic ("%PDF" . pdf-view-mode)
  :config
  (dolist
      (pkg
       '(pdf-annot pdf-cache pdf-dev pdf-history pdf-info pdf-isearch
                   pdf-links pdf-misc pdf-occur pdf-outline pdf-sync
                   pdf-util pdf-view pdf-virtual))
    (require pkg))
  (pdf-tools-install))

(use-package pkgbuild-mode
  :mode "/PKGBULD$")

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
  
  :config
  (setq projectile-enable-caching      nil
	projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-data-directory)
	projectile-cache-file          (expand-file-name "projectile.cache" user-data-directory))
  
  (projectile-global-mode)
  
  (with-eval-after-load 'ivy
    (setq projectile-completion-system 'ivy))
  
  (with-eval-after-load 'magit-branch
    (advice-add 'magit-checkout :after #'my/projectile-invalidate-cache)
    (advice-add 'magit-branch-and-checkout :after #'my/projectile-invalidate-cache)))

(use-package python-mode
  :mode "\\.py\\'"
  :interpreter "python"
  :bind (:map python-mode-map
              ("C-c c")
              ("C-c C-z" . python-shell))
  :config
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

  :hook (python-mode . my/python-mode-hook))

(use-package rainbow-delimiters
  ;; rainbow-delimiters rainbow-delimiters is a "rainbow
  ;; parentheses"-like mode which highlights delimiters such as
  ;; parentheses, brackets or braces according to their depth. Each
  ;; successive level is highlighted in a different color. This makes
  ;; it easy to spot matching delimiters, orient yourself in the code,
  ;; and tell which statements are at a given depth.
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  ;; Colorize color-names in Emacs buffers.
  :commands rainbow-mode)

(use-package recentf
  :defer 10
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :preface
  (defun recentf-add-dired-directory ()
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
  (setq recentf-save-file (expand-file-name "recentf" user-data-directory)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1)
  :hook (dired-mode . recentf-add-dired-directory))

(use-package rect
  :straight f
  :bind ("C-c ]" . rectangle-mark-mode))

(use-package redshank
  :diminish
  :hook ((lisp-mode slime-repl-mode) . redshank-mode))

(use-package reftex
  :after auctex
  :hook (LaTeX-mode . reftex-mode))

(use-package regex-tool
  :disabled
  :load-path "lisp/regex-tool"
  :commands regex-tool)

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin))

(use-package rjsx-mode
  :interpreter (("node" . rjsx-mode))
  :mode (("\\.js?\\'"   . rjsx-mode)
         ("\\.jsx?\\'"  . rjsx-mode))
  :preface
  (defun my/rjxs-mode-hook ()
    "My rjxs-mode customizations."
    (electric-indent-mode 1)
    (aggresive-indent-mode 1))
  
  :init
  (setq js2-basic-offset 2
        js2-highlight-level 3
        js2-bounce-indent-p t
        js2-mode-show-strict-warnings nil)
  
  :hook (rjxs-mode . my/rjxs-mode-hook))

(use-package rust-mode
  :mode "\\.rs\\'"
  :preface
  (defun my/compile-single-rust-file ()
    "Compile a single rust file."
    (interactive)
    (when (and (f-exists? (buffer-name))
               (f-file? (buffer-name)))
      (compile (concat "rustc " (buffer-name) " -o " (f-no-ext (buffer-name))))))
  
  (defun my/rust-mode-hook ()
    "My Rust-mode configuration."
    (flycheck-mode 1)
    (setq-local fill-column 100)
    (--each '((">=" . (?· (Br . Bl) ?≥))
              ("<=" . (?· (Br . Bl) ?≤))
              ("!=" . (?· (Br . Bl) ?≠))
              ("=>" . (?· (Br . Bl) ?➡))
              ("->" . (?· (Br . Bl) ?→)))
      (push it prettify-symbols-alist)))
  
  :config
  (use-package cargo
    :after rust-mode
    :config
    (with-eval-after-load 'hydra
      ;; Hydra for rust's cargo
      (defhydra hydra-cargo (:color blue :columns 4)
        "cargo"
        ("c"  cargo-process-build         "build")
        ("tt" cargo-process-test          "test all")
        ("tf" cargo-process-current-test  "test current function")
        ("b"  cargo-process-bench         "benchmark all")
        ("C"  cargo-process-clean         "clean")
        ("dd" cargo-process-doc           "build documentation")
        ("do" cargo-process-doc-open      "build and open documentation")
        ("r"  cargo-process-run           "run")
        ("y"  cargo-process-clippy        "clippy"))
      (general-define-key :keymaps 'rust-mode-map :states 'normal "c" #'hydra-cargo/body))
    
    :hook (rust-mode . cargo-minor-mode))

  (use-package racer
    :disabled
    :unless (featurep 'lsp-mode)
    :if (executable-find "racer")
    :defines (racer-cmd racer-rust-src-path)
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
    
    :config
    (setenv "RUST_SRC_PATH" rust-src-path)
    (setenv "RUSTC" rust-bin-path)
    (with-eval-after-load 'company
      (add-to-list 'company-dabbrev-code-modes 'rust-mode)
      (add-hook 'racer-mode-hook #'company-mode))
    
    :hook (rust-mode . (racer-mode eldoc-mode)))

  :hook (rust-mode . my/rust-mode-hook))

(use-package savehist
  :unless noninteractive
  :init
  (setq savehist-additional-variaables '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" user-data-directory))
  :config
  (savehist-mode 1))

(use-package saveplace
  :unless noninteractive
  :init
  (setq save-place-file (expand-file-name "saveplace" user-data-directory))
  :config
  (save-place-mode 1))

(use-package selected
  :unless noninteractive
  :defer 5
  :diminish selected-minor-mode
  :bind (:map selected-keymap
              ("[" . align-code)
              ("f" . fill-region)
              ("U" . unfill-region)
              ("d" . downcase-region)
              ("u" . upcase-region)
              ("r" . reverse-region)
              ("s" . sort-lines))
  :config
  (selected-global-mode))

(use-package server
  :preface
  (require 'server)
  (defun my/server-enable ()
    "Start an Emacs server process if one is not already running."
    (unless server-process
      (server-start)))
  
  :hook (after-init . my/server-enable))

(use-package shell-pop
  :if (or (eq system-type 'gnu/linux) (eq system-type 'darwin))
  :bind (("C-t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
        shell-pop-term-shell "/bin/zsh"
        shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package slime
  :defer t
  :commands slime slime-insert-balanced-comments slime-reindent-defun slime-selector
  :bind (:map slime-repl-mode-map
              ("C-c ;" . slime-insert-balanced-cmments)
              ("M-q"   . slime-reindent-defun)
              ("M-l"   . slime-selector))
  :init
  (if (executable-find "clisp")
      (setq inferior-lisp-program "clisp"
            slime-contribs '(slime-fancy)))
  :config
  (use-package cldoc
    :hook ((lisp-mode ilisp-mode slime-repl-mode) . turn-on-cldoc-mode)))

(use-package smart-newline
  :diminish
  :commands smart-newline-mode
  :config
  (smart-newline-mode))

(use-package sql-indent
  :mode ("\\.sql\\'"))

(use-package string-edit
  :bind ("C-c C-'" . string-edit-at-point))

(use-package super-save
  :diminish super-save-mode
  :config
  (super-save-mode 1)
  (setq super-save-auto-save-when-idle t))

(use-package systemd
  :if (not (eq system-type 'windows-nt)) ; only for Unix-based systems
  :defer t
  :after company
  :defines (systemd-use-company-p)
  :init
  (setq systemd-use-company-p t))

(use-package term
  :bind (:map term-mode-map
              ("C-c C-y" . term-paste)))

(use-package tidy
  :commands (tidy-buffer
             tidy-parse-config-file
             tidy-save-settings
             tidy-describe-options))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package treemacs
  :unless noninteractive
  :defer t
  :bind (([f8]          . treemacs-toggle)
         ("M-0"         . treemacs-select-window)
         ("C-c 1"       . treemacs-delete-other-windows)
         ("C-c t t"     . treemacs-toggle)
         ("C-c t T"     . treemacs)
         ("C-c t B"     . treemacs-bookmark)
         ("C-c t C-t"   . treemacs-find-file)
         ("C-c t M-t"   . treemacs-find-tag))
  :config
  (use-package treemacs-projectile
    :requires projectile
    :defer t
    :config
    (setq treemacs-header-function #'treemacs-projectile-create-header)
    :bind (("C-c t P"    . treemacs-projectile)
           ("C-c t p"    . treemacs-projectile-toggle)))

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
        treemacs-width                      35)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package visual-fill-column
  :defer t
  :unless noninteractive
  :commands visual-fill-column-mode)

(use-package visual-regexp
  :defer t
  :unless noninteractive
  :bind (("C-c r"   . vr/replace)
         ("C-c %"   . vr/query-replace)
         ("<C-m> /" . vr/mc-mark)))

(use-package w3m
  :defer t
  :commands w3m-browse-url)

(use-package web-mode
  :mode ("\\.html\\'"
         "\\.css\\'")
  :commands web-mode
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-enable-auto-quoting nil))

(use-package which-func
  :defer t
  :unless noninteractive
  :hook (c-mode-common . which-function-mode))

(use-package which-key
  :unless noninteractive
  :defer 5
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode))

(use-package whitespace
  :disabled
  :defer t
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode
             whitespace-turn-off)
  :defines  (whitespace-auto-cleanup
	     whitespace-rescan-timer-time
	     whitespace-silent)
  :preface
  (defun my/normalize-file ()
    "Cleanup whitespace in a file."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (whitespace-cleanup)
      (delete-trailing-whitespace)
      (goto-char (point-max))
      (delete-blank-lines)
      (set-buffer-file-coding-system 'unix)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match ""))
      (set-buffer-file-coding-system 'utf-8)
      (let ((require-final-newline t))
        (save-buffer))))

  (defun my/maybe-turn-on-whitespace ()
    "Cleanup whitespace in a file based on the file type."
    (when (and (not (or (memq major-mode '(markdown-mode))
                        (and buffer-file-name
                             (string-match "\\(\\.texi\\|COMMIT_EDITMSG\\)\\'"
                                           buffer-file-name))))
               (locate-dominating-file default-directory ".clean")
               (not (locate-dominating-file default-directory ".noclean")))
      (whitespace-mode 1)
      ;; For some reason, having these in settings.el gets ignored if
      ;; whitespace loads lazily.
      (setq whitespace-auto-cleanup t
            whitespace-line-column 80
            whitespace-rescan-timer-time nil
            whitespace-silent t
            whitespace-style '(face trailing lines space-before-tab empty))
      (add-hook 'write-contents-hooks
                #'(lambda () (ignore (whitespace-cleanup))) nil t)
      (whitespace-cleanup)))

  :config
  (remove-hook 'find-file-hooks 'whitespace-buffer)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer)
  
  :hook (find-file-hooks . my/maybe-turn-on-whitespace))

(use-package whitespace-cleanup-mode
  ;; Intelligently call whitespace-cleanup before buffers are saved.
  ;; To enable it for an entire project, set whitespace-cleanup-mode
  ;; to t in your .dir-locals.el file.
  :defer 5
  :diminish
  :commands whitespace-cleanup-mode
  :config
  ;; (global-whitespace-cleanup-mode 1)
  )

(use-package windmove
  :after (hydra)
  :preface
  (defun my/hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun my/hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun my/hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun my/hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))
  
  :config
  (with-eval-after-load 'hydra
    (bind-key "M-C-u"
              (defhydra hydra-splitter ()
                "splitter"
                ("n" my/hydra-move-splitter-left)
                ("r" my/hydra-move-splitter-down)
                ("t" my/hydra-move-splitter-up)
                ("d" my/hydra-move-splitter-right)
                ("q" nil "quit")))))

(use-package winner
  :unless noninteractive
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package yasnippet
  :after prog-mode
  :defer 10
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets" . snippet-mode)
  :bind (("C-c y a" . yas-reload-all)
         ("C-c y d" . yas-load-directory)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y g" . yas-global-mode)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y m" . yas-minor-mode)
         ("C-c y n" . yas-new-snippet)
	 ("C-c y t" . yas-tryout-snippet)
         ("C-c y x" . yas-expand))
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :config
  (use-package auto-yasnippet
    :requires (yasnippet)
    :bind (("C-c y a" . aya-create)
           ("C-c y e" . aya-expand)
           ("C-c y o" . aya-open-line)))
  
  (yas-load-directory (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode 1))

(use-package zeal-at-point
  :if (executable-find "zeal")
  :defer t
  :config
  (with-eval-after-load 'python
    (add-to-list 'zeal-at-point-mode-alist '(python-mode . "python")))
  (with-eval-after-load 'rust
    (add-to-list 'zeal-at-point-mode-alist '(rust-mode   . "rust"))))

(use-package dash-at-point
  :if (and (eq system-type 'darwin)
           (executable-find "dash"))
  :defer t
  :config
  (with-eval-after-load 'python
    (add-to-list 'dash-at-point-mode-alist '(python-mode . "python")))
  (with-eval-after-load 'rust
    (add-to-list 'dash-at-point-mode-alist '(rust-mode   . "rust"))))

;;;
;;; Finalization
;;;

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(org-agenda-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;;   mode: emacs-lisp
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:
