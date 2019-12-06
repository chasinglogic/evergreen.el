;;; evergreen-cli.el --- functions for interacting with the evergreen cli

;; Copyright (C) 2019 Mathew Robinson

;; Author: Mathew Robinson <mathew.robinson@mongodb.com>
;; Created: 05 Aug 2019

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This abstracts interacting with the Evergreen CLI. There's just
;; enough complexity involved that it's worth having it's own
;; package. It also gives a convenient way to phase some of these
;; functions out in favor of the API where it makes sense.

;;; Code:

(require 'cl)


(defclass evergreen-cli-flagset () ()
  "A base class for defining flagset classes.")

(cl-defmethod get-flags ((flagset evergreen-cli-flagset))
  (mapcar
   #'(lambda (s)
       (message "slot %s" s)
       (let* ((flagname (format "%s" s))
              (flagvalue (oref flagset s)))
         (message "flagname %s flagvalue %s" flagname flagvalue)
         (cond
          ((and (booleanp flagvalue) flagvalue)
           (format "--%s" flagname))
          (flagvalue
           (if (listp flagvalue)
               (format "--%s=%s" flagname
                       (mapconcat (lambda (x) x)) flagvalue ",")
           (format "--%s=%s"))))))
       (eieio-class-slots flagset)))

(defclass evergreen-patch-flagset (evergreen-cli-flagset)
  ((project :initarg :project :type string)
   (variants :initarg :variants :type list)
   (alias :initarg :alias :type string)
   (no-confirm :initarg :no-confirm :type boolean))
  "Evergreen patch command flags")

(defmacro evergreen-define-cli-patchset (command &rest flags)
  "Define an evergreen patch set"
  `(defun ,(intern (format "evergreen-%s-flagset" commmand)) ()
     ))

(defun evergreen-patch-flagset (&rest kwargs)
  "Build an evergreen patch flagset using the property list KWARGS.

Accepted keys are:
:project   Project to run patch against
:alias	   Patch alias (set by project admin)
:variants  Variants to run on
:tasks     List of tasks to run
:description	description for the patch
:no-confirm				skip confirmation text
:large	   Enable submitting larger patches (>16MB)
:browse				open patch url in browser
:verbose				show patch summary
:finalize			schedule tasks immediately
:committed-only			diff with HEAD, ignoring working tree changes"
  (when (and (plist-get kwargs :variants)
             (not (listp (plist-get kwargs :variants))))
    (error ":variants must be a list"))
  (when (and (plist-get kwargs :tasks)
             (not (listp (plist-get kwargs :tasks))))
    (error ":tasks must be a list"))
  (remove
   nil
   (apply
    #'append
    (list
     (list (format "--project=%s" (plist-get kwargs :project)))
     (if (plist-get kwargs :alias)
         (list (format "--alias=%s" (plist-get kwargs :alias)))
       (list
        (format "--variants=%s"
                (string-join (plist-get kwargs :variants) ","))
        (format "--tasks=%s"
                (string-join (plist-get kwargs :tasks) ","))))
     (list
      (when (plist-get kwargs :description)
        (format "--description=%s" (plist-get kwargs :description)))
      (when (plist-get kwargs :no-confirm) "--yes")
      (when (plist-get kwargs :large) "--large")
      (when (plist-get kwargs :browse) "--browse")
      (when (plist-get kwargs :verbose) "--verbose")
      (when (plist-get kwargs :finalize) "--finalize")
      (when (plist-get kwargs :committed-only) "--committed-only"))))))


(defun evergreen--command-to-string (command)
  "Run COMMAND removing evergreen's self update message if necessary."
  (let ((output (shell-command-to-string command)))
    (if (string-match-p (regexp-quote "^A new version is available.*") output)
        (progn
          (message "Evergreen CLI is ready for update. Run evergreen-update-cli to update it.")
          (mapconcat
           'identity
           (cdr (split-string output "\n"))
           "\n"))
      output)))

(defun evergreen-command (command &rest args)
  "Run the evergreen command COMMAND with ARGS."
  (let ((real-args (remove nil (append (list command) args))))
    (pop-to-buffer
     (apply 'make-comint-in-buffer
            (append
             (list
              (format "evergreen %s" command)
              evergreen-command-output-buffer
              evergreen-binary-path
              nil)
              real-args)))
    (message "Running evergreen with: %s"
             (append (list evergreen-binary-path) real-args))
     (with-current-buffer evergreen-command-output-buffer
       (goto-char (point-max)))))

(provide 'evergreen-cli)

;;; evergreen-cli.el ends here
