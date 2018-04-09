;;; dot-org.el -- Org-mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-verbose nil)
  (setq use-package-expand-minimally t)
  (load "org-settings" :noerror))

(eval-when-compile
  (require 'cl)
  (setplist 'string-to-multibyte
	    (use-package-plist-delete
	     (symbol-plist 'string-to-multibyte) 'byte-obsolete-info)))

(defun org-release () "8.2.11")
(defun org-git-version () "8.2.11")

(defconst my-org-soft-red    "#fcebeb")
(defconst my-org-soft-orange "#fcf5eb")
(defconst my-org-soft-yellow "#fcfceb")
(defconst my-org-soft-green  "#e9f9e8")
(defconst my-org-soft-blue   "#e8eff9")
(defconst my-org-soft-purple "#f3e8f9")

(use-package org
  :straight org-plus-contrib
  :preface
  (defun org-fit-agenda-window ()
    "Fit the window to the buffer size."
    (and (memq org-agenda-window-setup '(reorganize-frame))
	 (fboundp 'fit-window-to-buffer)
	 (fit-window-to-buffer)))

  (defun my/org-startup ()
    (org-agenda-list)
    (org-fit-agenda-window)
    (org-agenda-to-appt)
    (call-interactively #'org-resolve-clocks))

  (defadvice org-refile-get-location (before clear-refile-history activate)
    "Fit the Org Agenda to its buffer."
    (setq org-refile-history nil))

  (defun jump-to-org-agenda ()
    (interactive)
    (push-window-configuration)
    (let ((recordings-dir "~/Dropbox/Apps/Dropvox"))
      (ignore-errors
	(if (directory-files recordings-dir nil "\\`[^.]")
	    (find-file recordings-dir))))
    (let ((buf (get-buffer "*Org Agenda*"))
	  wind)
      (if buf
	  (if (setq wind (get-buffer-window buf))
	      (when (called-interactively-p 'any)
		(select-window wind)
		(org-fit-window-to-buffer))
	    (if (called-interactively-p 'any)
		(progn
		  (select-window (display-buffer buf t t))
		  (org-fit-window-to-buffer))
	      (with-selected-window (display-buffer buf)
		(org-fit-window-to-buffer))))
	(call-interactively 'org-agenda-list))))

  (defun org-get-global-property (name)
    (save-excursion
      (goto-char (point-min))
      (and (re-search-forward (concat "#\\+PROPERTY: " name " \\(.*\\)") nil t)
	   (match-string 1))))

  (defun org-agenda-add-overlays (&optional line)
    "Add overlays found in OVERLAY properties to agenda items.
   Note that habitual items are excluded, as they already
   extensively use text properties to draw the habits graph.
   For example, for work tasks I like to use a subtle, yellow
   background color; for tasks involving other people, green; and
   for tasks concerning only myself, blue.  This way I know at a
   glance how different responsibilities are divided for any given
   day.
 To achieve this, I have the following in my todo file:
  * Work
    :PROPERTIES:
    :CATEGORY: Work
    :OVERLAY:  (face (:background \"#fdfdeb\"))
    :END:
  ** TODO Task
  * Family
    :PROPERTIES:
    :CATEGORY: Personal
    :OVERLAY:  (face (:background \"#e8f9e8\"))
    :END:
  ** TODO Task
  * Personal
    :PROPERTIES:
    :CATEGORY: Personal
    :OVERLAY:  (face (:background \"#e8eff9\"))
    :END:
  ** TODO Task
The colors (which only work well for white backgrounds) are:
  Yellow: #fdfdeb
  Green:  #e8f9e8
  Blue:   #e8eff9
To use this function, add it to `org-agenda-finalize-hook':
  (add-hook 'org-finalize-agenda-hook 'org-agenda-add-overlays)"
    (let ((inhibit-read-only t) l c
	  (buffer-invisibility-spec '(org-link)))
      (save-excursion
	(goto-char (if line (point-at-bol) (point-min)))
	(while (not (eobp))
	  (let ((org-marker (get-text-property (point) 'org-marker)))
	    (when (and org-marker
		       (null (overlays-at (point)))
		       (not (get-text-property (point) 'org-habit-p))
		       (string-match "\\(sched\\|dead\\|todo\\)"
				     (get-text-property (point) 'type)))
	      (let ((overlays
		     (or (org-entry-get org-marker "OVERLAY" t)
			 (with-current-buffer (marker-buffer org-marker)
			   (org-get-global-property "OVERLAY")))))
		(when overlays
		  (goto-char (line-end-position))
		  (let ((rest (- (window-width) (current-column))))
		    (if (> rest 0)
			(insert (make-string rest ? ))))
		  (let ((ol (make-overlay (line-beginning-position)
					  (line-end-position)))
			(proplist (read overlays)))
		    (while proplist
		      (overlay-put ol (car proplist) (cadr proplist))
		      (setq proplist (cddr proplist))))))))
	  (forward-line)))))
  
  (defun org-todo-age-time (&optional pos)
    (let ((stamp (org-entry-get (or pos (point)) "CREATED" t)))
      (when stamp
	(time-subtract (current-time)
		       (org-time-string-to-time
			(org-entry-get (or pos (point)) "CREATED" t))))))

  (defun org-todo-age (&optional pos)
    (let ((days (time-to-number-of-days (org-todo-age-time pos))))
      (cond
       ((< days 1)   "today")
       ((< days 7)   (format "%dd" days))
       ((< days 30)  (format "%.1fw" (/ days 7.0)))
       ((< days 358) (format "%.1fM" (/ days 30.0)))
       (t            (format "%.1fY" (/ days 365.0))))))

  (defun org-compare-todo-age (a b)
    (let ((time-a (org-todo-age-time (get-text-property 0 'org-hd-marker a)))
	  (time-b (org-todo-age-time (get-text-property 0 'org-hd-marker b))))
      (if (time-less-p time-a time-b)
	  -1
	(if (equal time-a time-b)
	    0
	  1))))

  (defun org-my/message-open (message-id)
    (if (get-buffer "*Group*")
	(gnus-goto-article
	 (gnus-string-remove-all-properties (substring message-id 2)))
      (error "Gnus is not running")))

  (defun save-org-mode-files ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when (eq major-mode 'org-mode)
	  (if (and (buffer-modified-p) (buffer-file-name))
	      (save-buffer))))))

  :config
  (unbind-key "C-," org-mode-map)
  (unbind-key "C-'" org-mode-map)
  ;;(add-to-list 'org-link-protocols (list "message" 'org-my/message-open nil))
  (run-with-idle-timer 25 t 'save-org-mode-files)
  (setq org-agenda-files
	'("~/Documents/org/todo.txt"
	  "~/Documents/org/agenda.txt"))
  :hook (org-finalize-agenda . org-agenda-add-overlays)
  )

(use-package org-agenda
  :after org
  :straight f)



(defun my/org-push-mobile ()
  (interactive)
  (with-current-buffer (find-file-noselect "~/Documents/tasks/todo.txt")
    (org-mobile-push)))

(eval-when-compile
  (defvar org-clock-current-task)
  (defvar org-mobile-directory)
  (defvar org-mobile-capture-file))

(defun quickping (host)
  (= 0 (call-process "ping" nil nil nil "-c1" "-W50" "-q" host)))

(defun org-my/auto-exclude-function (tag)
  (and (cond
	((string= tag "call")
	 (let ((hour (nth 2 (decode-time))))
	   (or (< hour 8) (> hour 21))))
	((string= tag "errand")
	 (let ((hour (nth 2 (decode-time))))
	   (or (< hour 12) (> hour 17))))
	((or (string= tag "home") (string= tag "nasim"))
	 (with-temp-buffer
	   (call-process "ifconfig" nil t nil "en0" "inet")
	   (call-process "ifconfig" nil t nil "en1" "inet")
	   (call-process "ifconfig" nil t nil "bond0" "inet")
	   (goto-char (point-min))
	   (not (re-search-forward "inet 192\\.168\\.1\\." nil t))))
	((string= tag "net")
	 (not (quickping "imap.fastmail.com")))
	((string= tag "fun")
	 org-clock-current-task))
       (concat "-" tag)))

(defun my/mobileorg-convert ()
  (interactive)
  (while (re-search-forward "^\\* " nil t)
    (goto-char (match-beginning 0))
    (insert ?*)
    (forward-char 2)
    (insert "TODO ")
    (goto-char (line-beginning-position))
    (forward-line)
    (re-search-forward "^\\[")
    (goto-char (match-beginning 0))
    (let ((uuid
	   (save-excursion
	     (re-search-forward "^\\*\\* Note ID: \\(.+\\)")
	     (prog1
		 (match-string 1)
	       (delete-region (match-beginning 0)
			      (match-end 0))))))
      (insert (format "SCHEDULED: %s\n:PROPERTIES:\n"
		      (format-time-string (org-time-stamp-format))))
      (insert (format ":ID:       %s\n:CREATED:  " uuid)))
    (forward-line)
    (insert ":END:")))

(defun my/org-convert-incoming-items ()
  (interactive)
  (with-current-buffer
      (find-file-noselect (expand-file-name org-mobile-capture-file
					    org-mobile-directory))
    (goto-char (point-min))
    (unless (eobp)
      (my/mobileorg-convert)
      (goto-char (point-max))
      (if (bolp)
	  (delete-char -1))
      (let ((tasks (buffer-string)))
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer))
	(with-current-buffer (find-file-noselect "~/Documents/tasks/todo.txt")
	  (save-excursion
	    (goto-char (point-min))
	    (re-search-forward "^\\* Inbox$")
	    (re-search-forward "^:END:")
	    (forward-line)
	    (goto-char (line-beginning-position))
	    (if (and tasks (> (length tasks) 0))
		(insert tasks ?\n))))))))

(defun my/org-mobile-pre-pull-function ()
  (my/org-convert-incoming-items))

(add-hook 'org-mobile-pre-pull-hook 'my/org-mobile-pre-pull-function)

(defun org-my/state-after-clock-out (state)
  (if (string= state "STARTED") "TODO" state))

(defvar org-my/archive-expiry-days 9
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

(defconst org-my/ts-regexp
  "[[<]\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]>\r\n]*?\\)[]>]"
  "Regular expression for fast inactive time stamp matching.")

(defun org-my/closing-time ()
  (let* ((state-regexp
	  (concat "- State \"\\(?:" (regexp-opt org-done-keywords)
		  "\\)\"\\s-*\\[\\([^]\n]+\\)\\]"))
	 (regexp (concat "\\(" state-regexp "\\|" org-my/ts-regexp "\\)"))
	 (end (save-excursion
		(outline-next-heading)
		(point)))
	 begin
	 end-time)
    (goto-char (line-beginning-position))
    (while (re-search-forward regexp end t)
      (let ((moment (org-parse-time-string (match-string 1))))
	(if (or (not end-time)
		(time-less-p (apply #'encode-time end-time)
			     (apply #'encode-time moment)))
	    (setq end-time moment))))
    (goto-char end)
    end-time))

(defun org-archive-expired-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((done-regexp
	   (concat "^\\*\\* \\(" (regexp-opt org-done-keywords) "\\) ")))
      (while (re-search-forward done-regexp nil t)
	(if (>= (time-to-number-of-days
		 (time-subtract (current-time)
				(apply #'encode-time (org-my/closing-time))))
		org-my/archive-expiry-days)
	    (org-archive-subtree))))
    (save-buffer)))

(defalias 'archive-expired-tasks 'org-archive-expired-tasks)

(defun org-archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\* \\(DONE\\|CANCELED\\) " nil t)
      (if (save-restriction
	    (save-excursion
	      (org-narrow-to-subtree)
	      (search-forward ":LOGBOOK:" nil t)))
	  (forward-line)
	(org-archive-subtree)
	(goto-char (line-beginning-position))))))

(defalias 'archive-done-tasks 'org-archive-done-tasks)

(defun org-get-inactive-time ()
  (float-time (org-time-string-to-time
	       (or (org-entry-get (point) "TIMESTAMP")
		   (org-entry-get (point) "TIMESTAMP_IA")
		   (debug)))))

(defun org-get-completed-time ()
  (let ((begin (point)))
    (save-excursion
      (outline-next-heading)
      (and (re-search-backward
	    (concat "\\(- State \"\\(DONE\\|DEFERRED\\|CANCELED\\)\""
		    "\\s-+\\[\\(.+?\\)\\]\\|CLOSED: \\[\\(.+?\\)\\]\\)")
	    begin t)
	   (float-time (org-time-string-to-time (or (match-string 3)
						    (match-string 4))))))))

(defun org-sort-done-tasks ()
  (interactive)
  (goto-char (point-min))
  (org-sort-entries t ?F #'org-get-inactive-time #'<)
  (goto-char (point-min))
  (while (re-search-forward "
+" nil t)
    (delete-region (match-beginning 0) (match-end 0))
    (insert "
"))
  (let (after-save-hook)
    (save-buffer))
  (org-overview))

(defalias 'sort-done-tasks 'org-sort-done-tasks)

(defun org-sort-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\* " nil t)
      (goto-char (match-beginning 0))
      (condition-case err
	  (progn
	    ;; (org-sort-entries t ?a)
	    (org-sort-entries t ?p)
	    (org-sort-entries t ?o))
	(error nil))
      (forward-line))
    (goto-char (point-min))
    (while (re-search-forward "\* PROJECT " nil t)
      (goto-char (line-beginning-position))
      (ignore-errors
	;; (org-sort-entries t ?a)
	(org-sort-entries t ?p)
	(org-sort-entries t ?o))
      (forward-line))))

(defun org-cleanup ()
  (interactive)
  (org-archive-expired-tasks)
  (org-sort-all))

(defvar my/org-wrap-region-history nil)

(defun my/org-wrap-region (&optional arg)
  (interactive "P")
  (save-excursion
    (goto-char (region-end))
    (if arg
	(insert "#+end_src\n")
      (insert ":END:\n"))
    (goto-char (region-beginning))
    (if arg
	(insert "#+begin_src "
		(read-string "Language: " nil 'my/org-wrap-region-history)
		?\n)
      (insert ":OUTPUT:\n"))))

(defun org-get-message-link (&optional title)
  (let (message-id subject)
    (with-current-buffer gnus-original-article-buffer
      (setq message-id (substring (message-field-value "message-id") 1 -1)
	    subject (or title (message-field-value "subject"))))
    (org-make-link-string (concat "message://" message-id)
			  (rfc2047-decode-string subject))))

(defun org-insert-message-link (&optional arg)
  (interactive "P")
  (insert (org-get-message-link (if arg "writes"))))

(defun org-set-message-link ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "Message" (org-get-message-link)))

(defun org-get-message-sender ()
  (let (message-id subject)
    (with-current-buffer gnus-original-article-buffer
      (message-field-value "from"))))

(defun org-set-message-sender ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "Submitter" (org-get-message-sender)))

(defun org-get-safari-link ()
  (let ((subject (substring (do-applescript
			     (string-to-multibyte "tell application \"Safari\"
	name of document of front window
end tell")) 1 -1))
	(url (substring (do-applescript
			 (string-to-multibyte "tell application \"Safari\"
	URL of document of front window
end tell")) 1 -1)))
    (org-make-link-string url subject)))

(defun org-get-chrome-link ()
  (let ((subject (do-applescript
		  (string-to-multibyte "tell application \"Google Chrome\"
	title of active tab of front window
end tell")))
	(url (do-applescript
	      (string-to-multibyte "tell application \"Google Chrome\"
	URL of active tab of front window
end tell"))))
    (org-make-link-string (substring url 1 -1) (substring subject 1 -1))))

(defun org-insert-url-link ()
  (interactive)
  (insert (org-get-safari-link)))

(defun org-set-url-link ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "URL" (org-get-safari-link)))

(defun org-get-file-link ()
  (let* ((subject (do-applescript "tell application \"Path Finder\"
     set theItems to the selection
     name of beginning of theItems
end tell"))
	 (path (do-applescript "tell application \"Path Finder\"
     set theItems to the selection
     (POSIX path of beginning of theItems) as text
end tell"))
	 (short-path
	  (replace-regexp-in-string abbreviated-home-dir "~/"
				    (substring path 1 -1))))
    (org-make-link-string (concat "file:" short-path)
			  (substring subject 1 -1))))

(defun org-insert-file-link ()
  (interactive)
  (insert (org-get-file-link)))

(defun org-set-file-link ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "File" (org-get-file-link)))

(defun org-set-dtp-link ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "Document" (org-get-dtp-link)))

(defun org-dtp-message-open ()
  "Visit the message with the given MESSAGE-ID.
This will use the command `open' with the message URL."
  (interactive)
  (re-search-backward "\\[\\[message://\\(.+?\\)\\]\\[")
  (do-applescript
   (format "tell application \"DEVONthink Pro\"
	set searchResults to search \"%%3C%s%%3E\" within URLs
	open window for record (get beginning of searchResults)
end tell" (match-string 1))))

(add-hook 'org-log-buffer-setup-hook
	  (lambda ()
	    (setq fill-column (- fill-column 5))))

(defun org-message-reply ()
  (interactive)
  (let* ((org-marker (get-text-property (point) 'org-marker))
	 (author (org-entry-get (or org-marker (point)) "Author"))
	 (subject (if org-marker
		      (with-current-buffer (marker-buffer org-marker)
			(goto-char org-marker)
			(nth 4 (org-heading-components)))
		    (nth 4 (org-heading-components)))))
    (setq subject (replace-regexp-in-string "\\`(.*?) " "" subject))
    (compose-mail-other-window author (concat "Re: " subject))))

;;;_  . keybindings

(defvar org-mode-completion-keys
  '((?d . "DONE")
    (?g . "DELEGATED")
    (?n . "NOTE")
    (?r . "DEFERRED")
    (?s . "STARTED")
    (?t . "TODO")
    (?w . "WAITING")
    (?x . "CANCELED")
    (?y . "SOMEDAY")
    ))

(eval-and-compile
  (defvar org-todo-state-map nil)
  (define-prefix-command 'org-todo-state-map))

(dolist (ckey org-mode-completion-keys)
  (let* ((key (car ckey))
	 (label (cdr ckey))
	 (org-sym (intern (concat "my/org-todo-" (downcase label))))
	 (org-sym-no-logging
	  (intern (concat "my/org-todo-" (downcase label) "-no-logging")))
	 (org-agenda-sym
	  (intern (concat "my/org-agenda-todo-" (downcase label))))
	 (org-agenda-sym-no-logging
	  (intern (concat "my/org-agenda-todo-"
			  (downcase label) "-no-logging"))))
    (eval
     `(progn
	(defun ,org-sym ()
	  (interactive)
	  (org-todo ,label))
	(bind-key (concat "C-c x " (char-to-string ,key)) ',org-sym
		  org-mode-map)

	(defun ,org-sym-no-logging ()
	  (interactive)
	  (let ((org-inhibit-logging t))
	    (org-todo ,label)))
	(bind-key (concat "C-c x " (char-to-string  ,(upcase key)))
		  ',org-sym-no-logging org-mode-map)

	(defun ,org-agenda-sym ()
	  (interactive)
	  (let ((org-inhibit-logging
		 (let ((style (org-entry-get
			       (get-text-property (point) 'org-marker)
			       "STYLE")))
		   (and style (stringp style)
			(string= style "habit")))))
	    (org-agenda-todo ,label)))
	(define-key org-todo-state-map [,key] ',org-agenda-sym)

	(defun ,org-agenda-sym-no-logging ()
	  (interactive)
	  (let ((org-inhibit-logging t))
	    (org-agenda-todo ,label)))
	(define-key org-todo-state-map [,(upcase key)]
	  ',org-agenda-sym-no-logging)))))

(bind-keys :map org-mode-map
	   ("C-c x l" . org-insert-dtp-link)
	   ("C-c x L" . org-set-dtp-link)
	   ("C-c x m" . org-insert-message-link)
	   ("C-c x M" . org-set-message-link)
	   ("C-c x u" . org-insert-url-link)
	   ("C-c x U" . org-set-url-link)
	   ("C-c x f" . org-insert-file-link)
	   ("C-c x F" . org-set-file-link)

	   ("C-c C-x @" . visible-mode)
	   ("C-c M-m"   . my/org-wrap-region)

	   ([return]                . org-return-indent)
	   ([(control return)]      . other-window)
	   ([(control meta return)] . org-insert-heading-after-current))

(remove-hook 'kill-emacs-hook 'org-babel-remove-temporary-directory)

;;;_  . org-agenda-mode

(defun my/org-publish-ical ()
  (interactive)
  (async-shell-command "make -C ~/Documents/tasks"))

(bind-keys :map org-agenda-mode-map
	   ("C-c C-x C-p" . my/org-publish-ical)
	   ("C-n" . next-line)
	   ("C-p" . previous-line)
	   ("M-n" . org-agenda-later)
	   ("M-p" . org-agenda-earlier)
	   (" "   . org-agenda-tree-to-indirect-buffer)
	   (">"   . org-agenda-filter-by-top-headline)
	   ("g"   . org-agenda-redo)
	   ("f"   . org-agenda-date-later)
	   ("b"   . org-agenda-date-earlier)
	   ("r"   . org-agenda-refile)
	   ("F"   . org-agenda-follow-mode)
	   ("q"   . delete-window)
	   ("x"   . org-todo-state-map)
	   ("z"   . pop-window-configuration))

(unbind-key "M-m" org-agenda-keymap)

(defadvice org-agenda-redo (after fit-windows-for-agenda-redo activate)
  "Fit the Org Agenda to its buffer."
  (org-fit-agenda-window))

(defadvice org-agenda (around fit-windows-for-agenda activate)
  "Fit the Org Agenda to its buffer."
  (let ((notes
	 (ignore-errors
	   (directory-files
	    "~/Library/Mobile Documents/iCloud~com~agiletortoise~Drafts4/Documents"
	    t "[0-9].*\\.txt\\'" nil))))
    (when notes
      (with-current-buffer (find-file-noselect "~/Documents/tasks/todo.txt")
	(save-excursion
	  (goto-char (point-min))
	  (re-search-forward "^\\* Inbox$")
	  (re-search-forward "^:END:")
	  (forward-line 1)
	  (dolist (note notes)
	    (insert
	     "** TODO "
	     (with-temp-buffer
	       (insert-file-contents note)
	       (goto-char (point-min))
	       (forward-line)
	       (unless (bolp))
	       (insert ?\n)
	       (insert (format "SCHEDULED: %s\n"
			       (format-time-string (org-time-stamp-format))))
	       (goto-char (point-max))
	       (unless (bolp)
		 (insert ?\n))
	       (let ((uuid (substring (shell-command-to-string "uuidgen") 0 -1))
		     (file (file-name-nondirectory note)))
		 (insert (format (concat ":PROPERTIES:\n:ID:       %s\n"
					 ":CREATED:  ") uuid))
		 (string-match
		  (concat "\\`\\([0-9]\\{4\\}\\)"
			  "-\\([0-9]\\{2\\}\\)"
			  "-\\([0-9]\\{2\\}\\)"
			  "-\\([0-9]\\{2\\}\\)"
			  "-\\([0-9]\\{2\\}\\)"
			  "-\\([0-9]\\{2\\}\\)"
			  "\\.txt\\'") file)
		 (let ((year (string-to-number (match-string 1 file)))
		       (mon (string-to-number (match-string 2 file)))
		       (day (string-to-number (match-string 3 file)))
		       (hour (string-to-number (match-string 4 file)))
		       (min (string-to-number (match-string 5 file)))
		       (sec (string-to-number (match-string 6 file))))
		   (insert (format "[%04d-%02d-%02d %s %02d:%02d]\n:END:\n"
				   year mon day
				   (calendar-day-name (list mon day year) t)
				   hour min))))
	       (buffer-string)))
	    (delete-file note t)))
	(when (buffer-modified-p)
	  (save-buffer)))))
  ad-do-it
  (org-fit-agenda-window))

(defun org-inline-note ()
  (interactive)
  (switch-to-buffer-other-window "notes.txt")
  (goto-char (point-min))
  (forward-line)
  (goto-char (line-beginning-position))
  (insert "* NOTE ")
  (save-excursion
    (insert (format "\n:PROPERTIES:\n:ID:       %s\n:CREATED:  %s\n:END:\n"
		    (substring (shell-command-to-string "uuidgen") 0 -1)
		    (format-time-string (org-time-stamp-format t t)))))
  (save-excursion
    (forward-line)
    (org-cycle)))

(defadvice org-archive-subtree (before set-billcode-before-archiving activate)
  "Before archiving a task, set its BILLCODE and TASKCODE."
  (let ((billcode (org-entry-get (point) "BILLCODE" t))
	(taskcode (org-entry-get (point) "TASKCODE" t))
	(project  (org-entry-get (point) "PROJECT" t)))
    (if billcode (org-entry-put (point) "BILLCODE" billcode))
    (if taskcode (org-entry-put (point) "TASKCODE" taskcode))
    (if project (org-entry-put (point) "PROJECT" project))))

(font-lock-add-keywords
 'org-mode
 '(("^ *\\(-\\) "
    (0 (ignore (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org
  )

(use-package anki-editor
  :commands anki-editor-submit)

(use-package calfw
  :bind (("C-c A" . my/calendar)
	 :map cfw:calendar-mode-map
	 ("M-n" . cfw:navi-next-month-command)
	 ("M-p" . cfw:navi-previous-month-command)
	 ("j"   . cfw:navi-goto-date-command)
	 ("g"   . cfw:refresh-calendar-buffer))
  :commands cfw:open-calendar-buffer
  :functions (cfw:open-calendar-buffer
	      cfw:refresh-calendar-buffer
	      cfw:org-create-source
	      cfw:cal-create-source)
  :preface
  (defun my/calendar ()
    (interactive)
    (let ((buf (get-buffer "*cfw-calendar*"))
	  (org-agenda-files
	   (cons "~/Documents/tasks/Nasim.org"
		 (cons "~/Documents/tasks/Sacramento.org"
		       org-agenda-files))))
      (if buf
	  (pop-to-buffer buf nil)
	(cfw:open-calendar-buffer
	 :contents-sources
	 (list (cfw:org-create-source "Dark Blue")
	       (cfw:cal-create-source "Dark Orange"))
	 :view 'two-weeks)
	(setq-local org-agenda-files org-agenda-files))))

  :config
  (require 'calfw-cal)
  (use-package calfw-org
    :config
    (setq cfw:org-agenda-schedule-args '(:deadline :timestamp :sexp)))

  (setq cfw:fchar-junction ?╋
	cfw:fchar-vertical-line ?┃
	cfw:fchar-horizontal-line ?━
	cfw:fchar-left-junction ?┣
	cfw:fchar-right-junction ?┫
	cfw:fchar-top-junction ?┯
	cfw:fchar-top-left-corner ?┏
	cfw:fchar-top-right-corner ?┓))

(use-package helm-org-rifle
  :disabled
  :bind ("A-M-r" . helm-org-rifle))

(use-package jobhours
  :disabled
  :defer 5
  :bind ("M-o j" . jobhours-update-string)
  :preface
  (defun my/org-insert-jobhours-string ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (goto-char (line-end-position))
      (let* ((width (- (window-width) (current-column)))
	     (jobhours (jobhours-get-string t))
	     (spacer (- width (length jobhours)))
	     (inhibit-read-only t))
	(when (> spacer 0)
	  (insert (make-string spacer ? ) jobhours)))))

  (defun my/org-delayed-update ()
    (run-with-idle-timer
     1 nil
     `(lambda ()
	(with-current-buffer ,(current-buffer)
	  (org-save-all-org-buffers)
	  (my/org-insert-jobhours-string)))))
  
  :hook (orgagenda-finalize . my/org-delayed-update)
  :hook (org-clock-in . my/org-delayed-update)
  :hook (org-clock-out . my/org-delayed-update))

(use-package ob-diagrams)

(use-package ob-restclient)

(use-package ob-rust)

(use-package org-babel
  :straight f
  :no-require
  :after ob-restclient
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python     . t)
     (emacs-lisp . t)
     (calc       . t)
     (ditaa      . t)
     (plantuml   . t)
     (rust       . t)
     (sh         . t)
     (sql        . t)
     (dot        . t)
     (restclient . t))))

(use-package org-bookmark-heading)

(use-package org-noter
  :commands org-noter)

(use-package org-opml
  :disabled t)

(use-package org-pdfview
  :disabled
  :config
  (delete '("\\.pdf\\'" . default) org-file-apps)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
  (add-to-list 'org-file-apps
	       '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open)))

(use-package org-protocol
  :straight f)

(use-package org-ref
  ;; jww (2017-12-10): Need to configure.
  :disabled t)

(use-package org-rich-yank
  :defer 5
  :bind (:map org-mode-map
	      ("C-M-y" . org-rich-yank)))

(use-package org-smart-capture
  :disabled)

(use-package org-super-agenda
  :disabled t
  :preface
  (defun super-jump-to-org-agenda ()
    (interactive)
    (let ((org-super-agenda-groups
	   '((:name "Today"
		    :time-grid t
		    :todo "TODAY")
	     (:name "Important"
		    :tag "bills"
		    :priority "A")
	     (:order-multi
	      (2 (:name "Shopping in town"
			:and (:tag "shopping" :tag "@town"))
		 (:name "Food-related"
			:tag ("food" "dinner"))
		 (:name "Personal"
			:habit t
			:tag "personal")
		 (:name "Space-related (non-moon-or-planet-related)"
			:and (:regexp ("space" "NASA")
				      :not (:regexp "moon" :tag "planet")))))
	     (:todo "WAITING" :order 8)
	     (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
		    :order 9)
	     (:priority<= "B" :order 1))))
      (org-agenda nil "a")))
  :config
  (org-super-agenda-mode))

(use-package org-velocity
  :straight f
  :bind ("C-, C-." . org-velocity))

(use-package org-web-tools
  :bind (("C-, C-y" . my/org-insert-url)
	 ("C-, C-M-y" . org-web-tools-insert-web-page-as-entry))
  :functions (org-web-tools--org-link-for-url
	      org-web-tools--get-first-url)
  :preface
  (declare-function org-web-tools--org-link-for-url "org-web-tools")
  (declare-function org-web-tools--get-first-url "org-web-tools")
  (defun my/org-insert-url (&optional arg)
    (interactive "P")
    (require' org-web-tools)
    (let ((link (org-web-tools--org-link-for-url
		 (org-web-tools--get-first-url))))
      (if arg
	  (progn
	    (org-set-property "URL" link)
	    (message "Added pasteboard link to URL property"))
	(insert link)))))

(use-package orgit
  :disabled t)

(use-package orgnav)

(use-package orgtbl-aggregate
  :config
  (load "org-insert-dblock"))

(use-package ox-md
  :straight f)

(use-package ox-pandoc
  :disabled
  :after pandoc)

(use-package ox-texinfo-plus
  :straight f
  :defer t)

;; Local Variables:
;;   mode: emacs-lisp
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; dot-org.el ends here
