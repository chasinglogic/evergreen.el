;;; evergreen.el --- Interact with Evergreen via Emacs

;; Copyright (C) 2019 Mathew Robinson

;; Author: Mathew Robinson <chasinglogic@gmail.com>
;; Created: 19 Apr 2019

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

;; 

;;; Code:

(defgroup evergreen nil
  "Evergreen interaction from inside Emacs."
  :group 'external)

(defcustom evergreen-projects
  (split-string
   (shell-command-to-string "evergreen list --projects")
   "\n")
  "List of evergreen projects used for completion in evergreen.el commands.

Defaults to the result of `evergreen list --projects`."
  :type 'list
  :group 'evergreen)


(defun evergreen-command (command &rest args)
  "Run the evergreen command COMMAND with ARGS."
  (let ((real-args (remove nil (append '(command) args))))
    (pop-to-buffer
     (apply 'make-comint-in-buffer
            "evergreen" evergreen-command-output-buffer
            evergreen-binary-path
            nil
            real-args)
     (message "Running upload.py with: %s" (append '(evergreen-binary-path) real-args))
     (with-current-buffer evergreen-command-output-buffer
       (goto-char (point-max))))))

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
  (remove
   nil
   (list
    (format "--project=%s" (plist-get kwargs :project))
    (if (plist-get kwargs :alias)
        (format "--alias=%s" (plist-get kwargs :alias))
      (format "--variants=%s"
              (mapconcat 'identity
                         (plist-get kwargs :variants) ","))
      (format "--tasks=%s"
              (mapconcat 'identity
                         (plist-get kwargs :tasks) ",")))
    (when (plist-get kwargs :description)
      (format "--description=%s" (plist-get kwargs :description)))
    (when (plist-get kwargs :no-confirm) "--yes")
    (when (plist-get kwargs :large) "--large")
    (when (plist-get kwargs :browse) "--browse")
    (when (plist-get kwargs :verbose) "--verbose")
    (when (plist-get kwargs :finalize) "--finalize")
    (when (plist-get kwargs :committed-only) "--committed-only"))))

(defun evergreen-list-for-project (project what)
  "Return a list of WHAT for PROJECT."
  (split-string
   (shell-command-to-string
    (format "evergreen list --project %s --%s" project what))
   "\n"))

(provide 'evergreen)

;;; evergreen.el ends here
