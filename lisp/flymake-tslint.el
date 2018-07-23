;; flymake-tslint.el --- A flymake handler for Typescript using tslint

;; Copyright (c) 2018 Mark H. Colburn

;; Author: Mark H. Colburn <colburn.mark@gmail.com>
;; Homepage: https://github.com/markcol/flymake-tslint
;; Package-Version: 0
;; Package-Requires: ((flymake-easy "0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; References:
;;   https://github.com/palantir/tslint
;;
;; Usage:
;;   (require 'flymake-tslint)
;;   (add-hook 'ts-mode-hook 'flymake-tslint-load)
;;
;; Uses flymake-easy, from https://github.com/purcell/flymake-easy

;;; Code:

(require 'flymake-easy)

(defgroup flymake-tslint nil
  "Flymake checking of Typescript using tslint"
  :group 'programming
  :prefix "flymake-tslint-")

;;;###autoload
(defcustom flymake-tslint-detect-trailing-comma t
  "Whether or not to report warnings about trailing commas."
  :type 'boolean :group 'flymake-tslint)

;;;###autoload
(defcustom flymake-tslint-command
  "tslint"
  "Name (and optionally full path) of tslint executable."
  :type 'string :group 'flymake-tslint)

;;;###autoload
(defcustom flymake-tslint-args
  (mapcar
   'symbol-name
   '(--white --undef --nomen --regexp --plusplus --bitwise --newcap --sloppy --vars --eqeq))
  "Command-line args for tslint executable."
  :type '(repeat string) :group 'flymake-tslint)

(defconst flymake-tslint-err-line-patterns
  '(("^ *#[0-9]+ \\(.*?\\)\n.*?// Line \\([0-9]+\\), Pos \\([0-9]+\\)$" nil 2 3 1)
    ;; jsl
    ("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" nil 2 nil 3)
    ("^\\(.+\\)(\\([0-9]+\\)): \\(SyntaxError:.+\\)$" nil 2 nil 3)
    ("^\\(.+\\)(\\([0-9]+\\)): \\(lint \\)?\\(warning:.+\\)$" nil 2 nil 4)))
(defconst flymake-tslint-trailing-comma-err-line-pattern
  '("^\\(.+\\)\:\\([0-9]+\\)\: strict \\(warning: trailing comma.+\\)\:$" nil 2 nil 3))

(defun flymake-tslint-command (filename)
  "Construct a command that flymake can use to check Typescript source in FILENAME."
  (append
   (list flymake-tslint-command)
   flymake-tslint-args
   (unless (string-match "tslint" flymake-tslint-command)
     ;; jsl required option
     (list "-process"))
   (list filename)))

;;;###autoload
(defun flymake-tslint-load ()
  "Configure flymake mode to check the current buffer's Typescript syntax."
  (interactive)
  (flymake-easy-load 'flymake-tslint-command
                     (append flymake-tslint-err-line-patterns
                             (when flymake-tslint-detect-trailing-comma
                               (list flymake-tslint-trailing-comma-err-line-pattern)))
                     'tempdir
                     "ts"))


(provide 'flymake-tslint)
;;; flymake-tslint.el ends here
