;;; consult-shell-command.el -- Manage and launch shell commands from consult -*- lexical-binding: t -*-

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
(require 'tramp)
(require 'shell)


;;; Custom

(defgroup consult-shell-command nil
  "Manage and launch shell commands from consult."
  :prefix "consult-shell-command-"
  :group 'consult)

(defcustom consult-shell-command-modes
  '((shell-command-mode async-shell-command shell-command-mode-hook)
    (compilation-mode   compile             compilation-start-hook))
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
                       (abbreviate-file-name default-directory))))))

(defvar consult-shell-command-source--project
  `( :narrow (?p . "Project")
     :annotate consult-shell-command--annotate
     :category shell-history
     :hidden t
     :items ,(lambda ()
               (let ((root (consult--project-root)))
                 (funcall (consult-shell-command--source-items
                           (lambda (command)
                             (string-prefix-p
                              (abbreviate-file-name (or root default-directory))
                              (let ((directory (get-char-property 0 'directory command)))
                                (if (file-remote-p directory)
                                    directory
                                  (abbreviate-file-name directory)))))))))))

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
      (format "%+33s %+7.7s %+3s %+12s"
              (propertize directory 'face 'dired-directory)
              (propertize age 'face 'completions-annotations)
              (propertize exit-code 'face (pcase exit-code
                                            ("0" 'success)
                                            ("--" nil)
                                            (_ 'error)))
              (propertize since-birth 'face 'completions-annotations)))))

(defun consult-shell-command--set (metadata &rest plist)
  (cl-loop for (key value) on plist by 'cddr
           do (put-text-property 0 (length metadata) key value metadata)))

(defun consult-shell-command--log (metadata)
  (file-name-concat
   temporary-file-directory
   (concat (car (last (file-name-split (car (split-string metadata)))))
           "-"
           (thread-last metadata
                        (get-char-property 0 'start-time)
                        (seconds-to-time)
                        (format-time-string "%Y%m%d-%H%M%S.log")))))

(defvar-local consult-shell-command--append-point 1)

(defun consult-shell-command--append (file)
  (when (> (point-max) consult-shell-command--append-point)
    (let ((coding-system-for-write 'raw-text))
      (write-region consult-shell-command--append-point (point-max)
                    file 'append 'no-echo))
    (setq-local consult-shell-command--append-point (point-max))))

(defun consult-shell-command--poll (timer process)
  (when-let* ((metadata (process-get process 'metadata)))
    (when-let* ((buffer (process-buffer process))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (consult-shell-command--append (consult-shell-command--log metadata))))
    (when (memq (process-status process) '(exit signal))
      (cancel-timer timer)
      (consult-shell-command--set metadata
                                  'exit-status (process-exit-status process)
                                  'end-time (time-to-seconds)))))

(defun consult-shell-command--hook (&rest _)
  (when-let* ((process (get-buffer-process (current-buffer)))
              (command (or (process-get process 'remote-command)
                           (process-command process)))
              (name (string-join
                     (if (equal (nth 1 command) shell-command-switch)
                         (nthcdr 2 command)
                       command)
                     " "))
              (metadata (substring-no-properties name)))
    (when-let* ((tramp-connection-entry (process-get process 'metadata)))
      ;; HACK If under tramp file handler this will called twice,
      ;; first with the connection handle then with the "fake"
      ;; process.  We are not interested in the connection process.
      (setq consult-shell-command-metadata
            (delq tramp-connection-entry consult-shell-command-metadata)))
    ;; Setup command metadata
    (consult-shell-command--set
     metadata
     'command (cadr (assoc major-mode consult-shell-command-modes))
     'directory (abbreviate-file-name default-directory)
     'start-time (time-to-seconds (current-time)))
    (push metadata consult-shell-command-metadata)
    ;; If buffer is suddenly killed, rescue contents into log file
    (add-hook 'kill-buffer-hook
              (apply-partially #'consult-shell-command--append
                               (consult-shell-command--log metadata))
              nil t)
    ;; Start polling process for process output and exit status
    (process-put process 'metadata metadata)
    (let ((timer (timer-create)))
      (timer-set-time timer nil 1)
      (timer-set-function timer #'consult-shell-command--poll
                          (list timer process))
      (timer-activate timer))))

(defun consult-shell-command--completing-read (prompt)
  (car (consult--multi consult-shell-command-sources
                       :require-match t :prompt prompt :sort nil)))


;;; Commands

(defun consult-shell-command (command)
  "Run run shell COMMAND."
  (interactive (list (consult-shell-command--completing-read
                      "Run shell command: ")))
  (let ((default-directory (or (get-char-property 0 'directory command)
                               default-directory))
        (function (get-char-property 0 'command command)))
    (unless function
      (user-error "No associated `command' for `%s'" command))
    (funcall function (substring-no-properties command))))


;;; Supporting cast

(defun consult-shell-command-edit (command)
  "Edit shell COMMAND in minibuffer."
  (let ((default-directory (or (get-char-property 0 'directory command)
                               default-directory))
        (function (get-char-property 0 'command command)))
    (unless function
      (user-error "No associated `command' for `%s'" command))
    (minibuffer-with-setup-hook
        (lambda ()
          (delete-region (minibuffer-prompt-end) (point-max))
          (insert command))
      (call-interactively function))))

(defun consult-shell-command-kill-process (command)
  "Kill process of COMMAND."
  (cl-loop for process in (process-list)
           for metadata = (process-get process 'metadata)
           when (eq command metadata)
           return (kill-process process)
           finally do
           (consult-shell-command--set metadata 'end-time (time-to-seconds))
           (user-error "No associated process found for `%s'" command)))

(defun consult-shell-command-switch-to-buffer (command)
  "Switch to buffer of COMMAND."
  (cl-loop for process in (process-list)
           for metadata = (process-get process 'metadata)
           for buffer = (process-buffer process)
           when (and buffer (eq command metadata))
           return (switch-to-buffer buffer)
           finally (user-error "No associated buffer found for `%s'" command)))

(defun consult-shell-command-find-log (command)
  "Find log file of COMMAND."
  (find-file-read-only (consult-shell-command--log command)))


;;; Integration's

(with-eval-after-load 'savehist
  (add-to-list 'savehist-minibuffer-history-variables
               'consult-shell-command-metadata)

  (cl-loop for metadata in consult-shell-command-metadata do
           (consult-shell-command--set
            metadata 'end-time (or (get-char-property 0 'end-time metadata)
                                   (time-to-seconds)))))


(with-eval-after-load 'embark
  (defvar-keymap consult-shell-command-embark-actions-map
    :parent embark-general-map
    "k" #'consult-shell-command-kill-process
    "b" #'consult-shell-command-switch-to-buffer
    "e" #'consult-shell-command-edit
    "f" #'consult-shell-command-find-log)

  (add-to-list 'embark-keymap-alist
               '(shell-history . consult-shell-command-embark-actions-map)))

(defun async-shell-command--fix-tramp (process)
  ;; TODO Should upstream, this is clearly an oversight
  (when (processp process)
    (with-current-buffer (process-buffer process)
      (shell-command-mode))))

(advice-add #'tramp-handle-shell-command
            :filter-return #'async-shell-command--fix-tramp)

;;; Mode

;;;###autoload
(define-minor-mode consult-shell-command-mode
  "Hook up triggers for metadata annotation."
  :global t
  (cl-loop for (_ _ hook) in consult-shell-command-modes do
           (funcall (if consult-shell-command-mode #'add-hook #'remove-hook)
                    hook #'consult-shell-command--hook)))

(provide 'consult-shell-command)
;;; consult-shell-command.el ends here

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
