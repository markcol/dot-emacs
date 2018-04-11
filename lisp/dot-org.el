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

(use-package org
  :straight org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :preface
  (defun my/org-agenda-width ()
    (setq org-agenda-tags-column (- (window-width))))

  (defun bh/is-project-p ()
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

  (defun bh/list-sublevels-for-projects-indented ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels 'indented)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defun bh/skip-non-stuck-projects ()
    "Skip trees that are not stuck projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (bh/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next (save-excursion
                               (forward-line 1)
                               (and (< (point) subtree-end)
                                    (re-search-forward "^\\*+ \\(TODO\\) " subtree-end t)))))
              (if has-next
                  next-headline
                nil)) ; a stuck project, has subtasks but no next task
          next-headline))))

  (defun bh/skip-non-projects ()
    "Skip trees that are not projects"
    (bh/list-sublevels-for-projects-indented)
    (if (save-excursion (bh/skip-non-stuck-projects))
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (if (bh/is-project-p)
                nil
              subtree-end)))
      (org-end-of-subtree t)))

  (defun pd/skip-projects ()
    "Skip trees that are projects."
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((bh/is-project-p)
          next-headline)
         (t
          nil)))))

  (defun pd/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  :config
  (setq org-directory user-org-directory)
  (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (setq org-agenda-diary-file (expand-file-name "diary.org" org-directory))
  (setq org-agenda-files (list (expand-file-name org-directory) (expand-file-name "projects/" org-directory)))
  (setq org-hide-leading-stars t)
  (setq org-use-sub-superscripts "{}")
  (setq org-footnote-define-inline t)
  (setq org-footnote-auto-adjust nil)

  (setq org-fast-tag-selection-single-key 'expert)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-tag-alist
        '((:startgroup . nil)
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
          ("PROJECT" . ?p)))
  (setq org-use-tag-inheritance t)
  (setq org-tags-exclude-from-inheritance '("@call"
                                            "@office"
                                            "@home"
                                            "@read"
                                            "@computer"
                                            "@dev"
                                            "@write"
                                            "PROJECT"))

  (setq org-use-speed-commands t)
  (setq org-use-fast-todo-selection t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d!)")
          (sequence "WAITING(w@/!)" "|" "CANCELLED" "DELEGATED(e@)")))
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-custom-commands
        '(("w" "Day's Agenda and Tasks"
           ((agenda "" (( org-agenda-span 1)))
            (tags-todo "-SOMEDAY/!"
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
            (tags-todo "-SOMEDAY/!"
                       ((org-agenda-overriding-header "Projects")
                        (org-agenda-skip-function 'bh/skip-non-projects)
                        (org-agenda-ignore-scheduled 'future)
                        (org-agenda-ignore-deadlines 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-CANCELLED/!WAITING"
                       ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                        (org-agenda-skip-function 'pd/skip-projects)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-todo-ignore-deadlines t)))
            (tags-todo "-SOMEDAY/!-WAITING"
                       ((org-agenda-overriding-header "Tasks")
                        (org-agenda-skip-function 'pd/skip-projects)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-todo-ignore-deadlines t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            nil))
          ("#" "Stuck Projects" tags-todo "-SOMEDAY/!"
           ((org-agenda-overriding-header "Stuck Projects")
            (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
          ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-WAITING"
           ((org-agenda-overriding-header "Tasks")
            (org-agenda-skip-function 'pd/skip-projects)
            (org-agenda-sorting-strategy
             '(category-keep))))
          ("p" "Project Lists" tags-todo "-SOMEDAY/!"
           ((org-agenda-overriding-header "Projects")
            (org-agenda-skip-function 'bh/skip-non-projects)
            (org-agenda-ignore-scheduled 'future)
            (org-agenda-ignore-deadlines 'future)
            (org-agenda-sorting-strategy
             '(category-keep))))
          ("b" "Waiting Tasks" tags-todo "-CANCELLED/!WAITING"
           ((org-agenda-overriding-header "Waiting and Postponed tasks")
            (org-agenda-skip-function 'pd/skip-projects)
            (org-agenda-todo-ignore-scheduled 'future)
            (org-agenda-todo-ignore-deadlines 'future)))
          ("e" "Errand List" tags-todo "@shops"
           ((org-agenda-prefix-format "[ ]")
            (org-agenda-todo-keyword-format "")))
          ("c" todo "TODO"
           ((org-agenda-sorting-strategy '(tag-up priority-down))))))
  
  ;; Refile setup
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3) (nil :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps t)
  :hook (org-agenda-mode-hook my/org-agenda-width))

(use-package org-agenda
  :straight f 				; part of org
  :bind (("<f6>"  . my/org-agenda)
	 ("C-c a" . org-agenda))
  :init
  (defun my/org-agenda ()
    (interactive)
    (org-agenda nil "w"))
  :config
  (setq org-agenda-prefix-format
	'((agenda    . " %i %-12:c%?-12t% s %b")
	  (timeline  . "  % s %b")
	  (todo      . " %i %-12:c %b")
	  (tags      . " %i %-12:c %b")
	  (search    . " %i %-12:c %b"))))

(use-package org-capture
  :straight f				; psrt of org
  :bind (("C-c r" . org-capture))
  :config
  (setq org-capture-templates
        '(("i" "Interruption" entry
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
  :straight f				; part of org
  :no-require
  :after ob-restclient
  :config
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

(use-package ob-diagrams
  :after org-babel)

(use-package ob-restclient
  :after org-babel)

(use-package ob-rust
  :after org-babel)

(use-package org-noter
  :commands org-noter)

(use-package org-pdfview
  :disabled
  :config
  (delete '("\\.pdf\\'" . default) org-file-apps)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
  (add-to-list 'org-file-apps
	       '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open)))

(use-package org-protocol
  :straight f				; part of org
  )

(use-package org-ref
  ;; See documentation at https://github.com/jkitchin/org-ref/blob/master/org-ref.org
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
	bibtex-completion-notes-path	 (expand-file-name "bibtex-notes" user-bib-directory)
	org-ref-bibliography-notes       (expand-file-name "notes.org" user-bib-directory)
	org-ref-default-bibliography     (expand-file-name "references.bib" user-bib-directory)
	org-ref-pdf-directory            (expand-file-name "bibtex-pdfs/" user-bib-directory)
	reftex-default-bibliography      (expand-file-name "references.bib" user-bib-directory)
	
	org-ref-show-broken-links t
	org-latex-pdf-process
	'("pdflatex -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -interaction nonstopmode -output-directory %o %f")

	;; Settings that control the format of the autogenerated key.
	bibtex-autokey-year-length           4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords            2
	bibtex-autokey-titlewords-stretch    1
	bibtex-autokey-titleword-length      5)

  ;; open pdf with system pdf viewer (works on mac)
  (setq bibtex-completion-pdf-open-function 'org-open-file))

(use-package org-rich-yank
  :defer 5
  :bind (:map org-mode-map
	      ("C-M-y" . org-rich-yank)))

(use-package org-super-agenda
  ;; https://github.com/alphapapa/org-super-agenda
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

(use-package org-velocity
  :straight f				; part of org
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

(use-package orgnav)

(use-package orgtbl-aggregate
  :config
  (load "org-insert-dblock"))

(use-package ox-confluence
  ;; Export Org files to confluence:
  ;; M-x org-confluence-export-as-confluence RET
  ;; https://github.com/emacsmirror/org/blob/master/contrib/lisp/ox-confluence.el
  :straight f)

(use-package ox-jira
  ;; Transforms Org files to JIRA markup for pasting into JIRA tickets & comments.
  ;; https://github.com/stig/ox-jira.el
  :defer t
  :init
  (setq org-export-copy-to-kill-ring 'if-interactive))

(use-package ox-publish
  :disabled
  :defer t
  :commands my/publish-blog
  :preface
  (defun my/publish-blog ()
    "Publish my blog"
    (interactive)
    (org-publish-project "blog" t))
  :config
  (require 'ox-html)
  (require 'ox-rss)

  (setq org-confirm-babel-evaluate nil)

  (setq org-publish-project-alist
        '(("blog-content"
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

(use-package ox-md
  :straight f
  :defer t)

(use-package ox-pandoc
  :after pandoc
  :defer t)

(use-package ox-texinfo-plus
  :straight f
  :defer t)

;; Local Variables:
;;   mode: emacs-lisp
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; dot-org.el ends here
