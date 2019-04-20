;;; evergreen.el --- Interact with Evergreen via Emacs
;;; Version: 0.1.0

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

;; This is mostly a nice wrapper / porcelain for the Evergreen CLI
;; tool.  As such, you need to install the Evergreen CLI tool to use
;; this package.

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

(defcustom evergreen-default-project nil
  "When prompting for project completion select the item matching this first."
  :type 'string
  :group 'evergreen)

(defcustom evergreen-command-output-buffer "*evergreen command output*"
  "Buffer to output evergreen stdout to."
  :type 'string
  :group 'evergreen)

(defcustom evergreen-binary-path "evergreen"
  "Location of evergreen binary, assumes evergreen is in your $PATH."
  :type 'string
  :group 'evergreen)

(defcustom evergreen-browse-when-patching nil
  "Whether or not to open a patch in your browser after creation."
  :type 'boolean
  :group 'evergreen)

(defcustom evergreen-finalize-when-patching nil
  "If not nil schedule every patch right away."
  :type 'boolean
  :group 'evergreen)

(defcustom evergreen-never-finalize-when-patching nil
  "If not nil never finalize and do not prompt for finalize when patching.

This option is ignored if evergreen-finalize-when-patching is non-nil."
  :type 'boolean
  :group 'evergreen)

(defcustom evergreen-browse-when-patching nil
  "If not nil always open new patches in your web browser after submitting."
  :type 'boolean
  :group 'evergreen)

(defcustom evergreen-never-browse-when-patching nil
  "If not nil never browse and do not prompt for browse when patching.

This option is ignored if evergreen-browse-when-patching is non-nil."
  :type 'boolean
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
              (mapconcat 'identity (plist-get kwargs :variants) ","))
      (format "--tasks=%s"
              (mapconcat 'identity (plist-get kwargs :tasks) ",")))
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

(defun evergreen--trim-extra-output (s)
  "Return the first word from the string S.

Evergreen sometimes returns a lot of extra information that makes it
not suitable for use in later commands."
  (car (split-string s)))

(defun evergreen--list-trimmed (project what)
  "Generate a trimmed list of WHAT for PROJECT."
  (mapcar
   'evergreen--trim-extra-output
   (evergreen-list-for-project project what)))

(defun evergreen-patch--get-user-args ()
  "Builds args for evergreen-patch using user input."
  (let* (
         (project (evergreen--trim-extra-output
                   (completing-read
                    "Evergreen Project: "
                    evergreen-projects
                    nil t nil nil
                    evergreen-default-project)))
         (finalize (if evergreen-finalize-when-patching
                       t
                     (if evergreen-never-finalize-when-patching
                         nil
                       (y-or-n-p "Finalize this patch (schedule right away)? "))))
         (browse (if evergreen-browse-when-patching
                       t
                     (if evergreen-never-browse-when-patching
                         nil
                       (y-or-n-p "Open patch in your web browser? "))))
         (use-alias (y-or-n-p "Use an alias? "))
         (alias (when use-alias
                  (completing-read
                   "Alias: "
                   (evergreen--list-trimmed project "aliases"))))
         (variants (when (not use-alias)
                     (completing-read-multiple
                      "Variants (comma-separated press tab to see completions): "
                      (evergreen--list-trimmed project "variants"))))
         (tasks (when (not use-alias)
                     (completing-read-multiple
                      "Tasks (comma-separated press tab to see completions): "
                      (evergreen--list-trimmed project "tasks"))))
         )
    (list project finalize browse alias variants tasks)))

(defun evergreen-patch (project &optional finalize browse alias variants tasks)
  "Run an evergreen patch for PROJECT.

If FINALIZE will schedule patch without human interaction
If BROWSE open the patch in a web browser after submitting
If ALIAS is provided that will be used to select VARIANTS and
TASKS.
If ALIAS is nil VARIANTS and TASKS must be provided instead."
  (interactive (evergreen-patch--get-user-args))
  (when (and (not alias)
             (or (not tasks)
                 (not variants)))
    (error "Either ALIAS or both TASKS and VARIANTS must be provided"))
  (evergreen-command
   "patch"
   (evergreen-patch-flagset
    :project project
    :alias alias
    :variants variants
    :tasks tasks
    :finalize finalize
    :browse browse)))

(provide 'evergreen)

;;; evergreen.el ends here
