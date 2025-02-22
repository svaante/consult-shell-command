;;; consult-shell-command.el --  -*- lexical-binding: t -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Daniel Pettersson
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.0.1
;; Homepage: https://github.com/svaante/consult-shell-command
;; Package-Requires: ((emacs "29.1") (consult))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Manage and launch shell commands from consult.

;;   (consult-shell-command-mode 1)

;;; Code:

(require 'cl-lib)
(require 'consult)


;;; Custom

(defgroup consult-shell-command nil
  "Manage and launch shell commands from consult."
  :prefix "consult-shell-command-"
  :group 'consult)

(defcustom consult-shell-command-modes
  '((shell-command-mode async-shell-command shell-command-mode-hook)
    (compilation-mode   compilation-mode    compilation-mode-hook))
  "Alist of (MODE COMMAND HOOK)."
  :type '(alist :key-type symbol
                :value-type (group (symbol :tag "Command")
                                   (symbol :tag "Hook"))))

(defcustom consult-shell-command-sources
  '( consult-shell-command-source--live
     consult-shell-command-source--exited
     consult-shell-command-source--directory
     consult-shell-command-source--project)
  "Sources used by `consult-shell-command' commands.
See `consult--multi'."
  :type '(repeat symbols))


;;; Internals

(defvar consult-shell-command-metadata nil)

(defun consult-shell-command--source-items (filter)
  (lambda ()
    (cl-loop for command in consult-shell-command-metadata for n upfrom 0
             when (funcall filter command)
             collect (cons (consult--tofu-append command n) command))))

(defvar consult-shell-command-source--live
  `( :name "Live" :narrow ?a
     :annotate consult-shell-command--annotate
     :category shell-history
     :items ,(consult-shell-command--source-items
              (lambda (command)
                (not (get-char-property 0 'end-time command))))))

(defvar consult-shell-command-source--exited
  `( :name "Exited" :narrow ?e
     :annotate consult-shell-command--annotate
     :category shell-history
     :items ,(consult-shell-command--source-items
              (lambda (command)
                (get-char-property 0 'end-time command)))))

(defvar consult-shell-command-source--directory
  `( :narrow (?. . "Directory")
     :annotate consult-shell-command--annotate
     :category shell-history
     :hidden t
     :items ,(consult-shell-command--source-items
              (lambda (command)
                (equal (get-char-property 0 'directory command)
                       default-directory)))))

(defvar consult-shell-command-source--project
  `( :narrow (?p . "Project")
     :annotate consult-shell-command--annotate
     :category shell-history
     :hidden t
     :items ,(consult-shell-command--source-items
              (lambda (command)
                (equal (get-char-property 0 'directory command)
                       (consult--project-root))))))

(defun consult-shell-command--annotate (command)
  (cl-flet ((format-time-diff (diff)
              (cond ((> diff (* 24 60 60)) (format-seconds "%dd %hh" diff))
                    ((> diff (* 60 60)) (format-seconds "%hh %mm" diff))
                    (t (format-seconds "%mm %ss%z" diff)))))
    (let ((directory
           (string-truncate-left (get-char-property 0 'directory command) 30))
          (age (format-time-diff
                (- (or (get-char-property 0 'end-time command) (time-to-seconds))
                   (get-char-property 0 'start-time command))))
          (exit-code (if-let* ((status-code (get-char-property 0 'exit-status command)))
                         (format "%d" status-code)
                       "--"))
          (since-birth
           (let ((seconds (- (time-to-seconds)
                             (get-char-property 0 'start-time command))))
             (if (> seconds (* 7 24 60 60))
                 (format-time-string "%b %d %R" (get-char-property 0 'start-time command))
               (format "%s ago" (format-time-diff seconds))))))
      (format "%+33s %+6.6s %+3s %+12s"
              (propertize directory 'face 'dired-directory)
              (propertize age 'face 'completions-annotations)
              (propertize exit-code 'face (pcase exit-code
                                            ("0" 'success)
                                            ("--" nil)
                                            (_ 'error)))
              (propertize since-birth 'face 'completions-annotations)))))

(defun consult-shell-command--poll (timer process)
  (when-let* (((memq (process-status process) '(exit signal)))
              (metadata (process-get process 'metadata)))
    (put-text-property 0 (length metadata)
                       'exit-status (process-exit-status process)
                       metadata)
    (put-text-property 0 (length metadata)
                       'end-time (time-to-seconds (current-time))
                       metadata)
    (cancel-timer timer)))

(defvar consult-shell-command--repeat 2)

(defun consult-shell-command--hook ()
  (let* ((process (get-buffer-process (current-buffer)))
         (command (process-command process))
         (name (string-join
                (if (equal (nth 1 command) shell-command-switch)
                    (nthcdr 2 command)
                  command)
                " "))
         (metadata
          (propertize name
                      'command (cadr (assoc major-mode consult-shell-command-modes))
                      'directory default-directory
                      'start-time (time-to-seconds (current-time)))))
    (push metadata consult-shell-command-metadata)
    (process-put process 'metadata metadata)
    (let ((timer (timer-create)))
      (timer-set-time timer nil consult-shell-command--repeat)
      (timer-set-function timer #'consult-shell-command--poll
                          (list timer process))
      (timer-activate timer))))

(defun consult-shell-command--completing-read (prompt)
  (car (consult--multi consult-shell-command-sources
                       :require-match t :prompt prompt :sort nil)))


;;; Commands

(defun consult-shell-command-re (command)
  "Re run shell COMMAND."
  (interactive (list (consult-shell-command--completing-read
                      "Re shell command: ")))
  (let ((default-directory (or (get-char-property 0 'directory command)
                               default-directory))
        (function (get-char-property 0 'command command)))
    (unless function
      (user-error "No associated `command' for `%s'" command))
    (funcall function (substring-no-properties command))))

(defun consult-shell-command-edit (command)
  "Edit shell COMMAND in minibuffer."
  (interactive (list (consult-shell-command--completing-read
                      "Edit shell command: ")))
  (let ((default-directory (or (get-char-property 0 'directory command)
                               default-directory))
        (function (get-char-property 0 'command command)))
    (unless function
      (user-error "No associated `command' for `%s'" command))
    (minibuffer-with-setup-hook (lambda () (insert command))
      (call-interactively function))))

(defun consult-shell-command-kill-process (command)
  "Kill process of COMMAND."
  (interactive (list (consult-shell-command--completing-read
                      "Kill shell command: ")))
  (cl-loop for process in (process-list)
           for metadata = (process-get process 'metadata)
           when (equal metadata command)
           return (kill-process process)
           finally do (user-error "No associated process found for `%s'" command)))

(defun consult-shell-command-switch-to-buffer (command)
  "Switch to buffer of COMMAND."
  (interactive (list (consult-shell-command--completing-read
                      "Switch to buffer for shell command: ")))
  (cl-loop for process in (process-list)
           for metadata = (process-get process 'metadata)
           for buffer = (process-buffer process)
           when (and buffer (equal metadata command))
           return (switch-to-buffer buffer)
           finally (user-error "No associated buffer found for `%s'" command)))


;;; Integration's

(with-eval-after-load 'save-hist
  (add-to-list 'savehist-minibuffer-history-variables
               'consult-shell-command-metadata))

(with-eval-after-load 'embark
  (defvar-keymap consult-shell-command-embark-actions-map
    :parent embark-general-map
    "k" #'consult-shell-command-kill-process
    "b" #'consult-shell-command-switch-to-buffer
    "r" #'consult-shell-command-re
    "e" #'consult-shell-command-edit)
  (add-to-list 'embark-keymap-alist
               '(shell-history . consult-shell-command-embark-actions-map)))


;;; Mode

(define-minor-mode consult-shell-command-mode
  "Hook up triggers for metadata annotation."
  :global t
  (cl-loop for (_ _ hook) in consult-shell-command-modes do
           (funcall (if consult-shell-command-mode #'add-hook #'remove-hook)
                    hook 'consult-shell-command--hook)))

(provide 'consult-shell-command)
;;; consult-shell-command.el ends here

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
