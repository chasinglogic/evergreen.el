;;; evergreen.el --- Interact with Evergreen via Emacs

;; Copyright (C) 2019 Mathew Robinson

;; Author: Mathew Robinson <chasinglogic@gmail.com>
;; Version: 0.1.0
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

(require 'subr-x)
(require 'vc-git)
(require 'eieio)

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

(defun evergreen-list-projects ()
  "Return a list of available evergreen projects."
  (cdr ;; removes the project count
   (split-string
    (evergreen--command-to-string "evergreen list --projects")
    "\n")))

(defgroup evergreen nil
  "Evergreen interaction from inside Emacs."
  :group 'external)

(defcustom evergreen-projects
  (evergreen-list-projects)
  "List of evergreen projects used for completion in evergreen.el commands.

Defaults to the result of `evergreen list --projects`."
  :type 'list
  :group 'evergreen)

(defcustom evergreen-default-project nil
  "When prompting for project completion select the item matching this first."
  :type 'string
  :group 'evergreen)

(defcustom evergreen-default-alias nil
  "When prompting for an alias default the completion selection to this value."
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

(defcustom evergreen-assume-yes nil
  "If not nil the Evergreen CLI will assume yes to all prompts."
  :type 'boolean
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

(defcustom evergreen-generate-description nil
  "If not nil generates patch descriptions of form $git_branch_name: $git_head_commit_msg."
  :type 'boolean
  :group 'evergreen)

(defcustom evergreen-spawn-key-name nil
  "The key pair name (as shown in Evergreen) to use when spawning a host."
  :type 'string
  :group 'evergreen)

(defcustom evergreen-spawn-default-dir nil
  "If non-nil open this directory on spawn hosts by default.

If nil the SSH user's home directory will be opened."
  :type 'string
  :group 'evergreen)

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

(defun evergreen-list-for-project (project what)
  "Return a list of WHAT for PROJECT."
  (split-string
   (evergreen--command-to-string
    (format "evergreen list --project %s --%s" project what))
   "\n"))

(defun evergreen-list-thing (thing)
  "Return a list of THING that does not require a project.

If THING requires a project use `evergreen-list-for-project` instead."
  (split-string
   (evergreen--command-to-string
    (format "evergreen list --%s" thing))
   "\n"))

;;;###autoload
(defun evergreen-update-cli ()
  "Update the Evergreen CLI via it's self update mechanism."
  (if (or (eq system-type 'windows-nt)
          (eq system-type 'ms-dos))
      (evergreen-command "get-update")
  (evergreen-command "get-update" "--install")))

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

(defun evergreen--generate-description ()
  "Generate patch description using git information."
  (let* (
         (branch-name (car (vc-git-branches)))
         (commit-msg (string-trim-right
                      (shell-command-to-string "git log -n 1 --format='%s'"))))
    (concat branch-name ": " commit-msg)))

;;;###autoload
(defun evergreen-patch--get-user-args ()
  "Builds args for evergreen-patch using user input."
  (require 'evergreen)
  (let* (
         (project (evergreen--trim-extra-output
                   (completing-read
                    "Evergreen Project: "
                    evergreen-projects
                    nil t nil nil
                    (cond
                     ;; If evergreen-default-project is set use that
                     (evergreen-default-project evergreen-default-project)
                     ;; If projectile is available try that
                     ((and (boundp 'projectile-project-name)
                           (projectile-project-name))
                      (projectile-project-name))
                     ;; Default to nil
                     (t nil)))))
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
                   (evergreen--list-trimmed project "aliases")
                   nil t nil t evergreen-default-alias)))
         (variants (when (not use-alias)
                     (completing-read-multiple
                      "Variants (comma-separated press tab to see completions): "
                      (evergreen--list-trimmed project "variants"))))
         (tasks (when (not use-alias)
                     (completing-read-multiple
                      "Tasks (comma-separated press tab to see completions): "
                      (evergreen--list-trimmed project "tasks"))))
         (description (if evergreen-generate-description
                          (evergreen--generate-description)
                        (read-string "Description: ")))
         )
    (list project finalize browse alias variants tasks description)))

;;;###autoload
(defun evergreen-patch (project &optional finalize browse alias variants tasks description large)
  "Run an evergreen patch for PROJECT.

If DESCRIPTION is a string will set patch description to that value
If FINALIZE is non-nil will schedule patch without human interaction
If BROWSE is non-nil open the patch in a web browser after submitting
If ALIAS is provided that will be used to select VARIANTS and
TASKS.
If ALIAS is nil VARIANTS and TASKS must be provided instead."
  (interactive (evergreen-patch--get-user-args))
  (when (and (not alias)
             (or (not tasks)
                 (not variants)))
    (error "Either ALIAS or both TASKS and VARIANTS must be provided"))
  (apply 'evergreen-command
         (append
          (list "patch")
          (evergreen-patch-flagset
           :project project
           :alias alias
           :variants variants
           :description description
           :tasks tasks
           :no-confirm evergreen-assume-yes
           :finalize finalize
           :large large
           :browse browse))))

;;;###autoload
(defun evergreen-large-patch ()
  "Run evergreen patch with the large flag."
  (interactive)
  (apply 'evergreen-patch
         (append
          (evergreen-patch--get-user-args)
          (list t))))

;;;###autoload
(defun evergreen-spawn-host (distro key-name &optional script)
  "Create an Evergreen Spawn Host of DISTRO.

If KEY-NAME is the name in Evergreen or value of the public key to use
when creating the host..
If SCRIPT is non-nil it's content is written to a temporary file then
used as a userdata script for the host."
  (interactive
   (list
    (completing-read "Distro: " (cdr (evergreen-list-thing "spawnable")))
    (if evergreen-spawn-key-name
        evergreen-spawn-key-name
      (read-string "Key Name: "))))
  (when script
    (with-temp-file "userdata.sh"
      (insert script)))
  (apply 'evergreen-command
         (remove
          nil
          (list
           "host"
           "create"
           (format "--key=%s" key-name)
           (format "--distro=%s" distro)
           (when script "--script=userdata.sh"))))
  (when script
    (delete-file "userdata.sh")))

;;;###autoload
(defun evergreen-spawn-host-with-userdata (distro key-name &optional script)
  "Create a Spawn Host of DISTRO using the current region as a userdata script.

If KEY-NAME is the name in Evergreen or value of the public key to use
when creating the host..
If SCRIPT is non-nil it's content is written to a temporary file then
used as a userdata script for the host."
  (interactive
   (list
    (completing-read "Distro: " (cdr (evergreen-list-thing "spawnable")))
    (if evergreen-spawn-key-name
        evergreen-spawn-key-name
      (read-string "Key Name: "))
    (when (region-active-p)
      (buffer-substring (region-beginning) (region-end)))))
  (evergreen-spawn-host distro key-name script))

;;;###autoload
(defun evergreen-terminate-host (host-id)
  "Terminate the spawn host with HOST-ID."
  (interactive
   (list
    (car
     (split-string
      (completing-read "Host: "
                       (mapcar
                        #'(lambda (host)
                            (format "%s %s %s"
                                    (slot-value host 'id)
                                    (slot-value host 'distro)
                                    (slot-value host 'hostname)))
                        (evergreen-get-spawn-hosts)))))))
  (shell-command
   (format "evergreen host terminate --host=%s &" host-id)
   evergreen-command-output-buffer))

(defclass evergreen-spawn-host-obj ()
  ((id
    :initarg :id
    :custom string
    :type string
    :documentation "EC2 Instance ID")
   (distro
    :initarg :distro
    :type string
    :documentation "Spawn Host's Evergreen Distro")
   (status
    :initarg :status
    :type string
    :documentation "Spawn host ready status")
   (hostname
    :initarg :hostname
    :type string
    :documentation "Spawn host hostname")
   (ssh-user
    :initarg :ssh-user
    :type string
    :documentation "User for ssh'ing into the spawn host"))
  "An Evergreen Spawn Host")

(provide 'evergreen)

;;; evergreen.el ends here
