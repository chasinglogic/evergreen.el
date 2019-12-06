;;; evergreen-spawn.el --- Functions and classes for interacting with Spawn Hosts

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

;; 

;;; Code:

(require 'evergreen-cli)

(defclass evergreen-spawn-host ()
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

(defun evergreen--parse-spawn-host-line (line)
  "Parse LINE of output from host list into an evergreen-spawn-host.

Example output line:
ID: i-0d03609daa1c39784; Distro: amazon1-2018-build; Status: running; Host name: ec2-3-84-84-12.compute-1.amazonaws.com; User: ec2-user 

This is awful and hacky but I'm too lazy to do real parsing of any
kind here because when EVG-6119 is done we can just use JSON."
  (unless (string-equal line "")
    (let* ((fields-and-values (split-string line ";"))
           (values
            (mapcar
             #'(lambda (fv)
                 ;; Remove the preceding whitespace
                 (string-trim-left 
                  ;; Get the actual string out of it instead of a list
                  (car 
                   ;; Drop the field name
                   (cdr
                    ;; Split on : because some fields are empty and so are
                    ;; space only meaning the cdr would return the field
                    ;; name as the value on incorrectly.
                    (split-string fv ":")))))
             fields-and-values)))
      ;; If you're reading this I hope you're not happy about it because
      ;; I'm not either.
      (evergreen-spawn-host
       :id (nth 0 values)
       :distro (nth 1 values)
       :status (nth 2 values)
       :hostname (nth 3 values)
       :ssh-user (nth 4 values)))))

(defun evergreen-get-spawn-hosts ()
  "Return a list of Evergreen spawn hosts."
  (remove
   nil
   (mapcar
    #'evergreen--parse-spawn-host-line
    ;; Drop the first line of output since we don't need it. (it's not a host)
    (cdr
     (split-string
      (evergreen--command-to-string "evergreen host list --mine") "\n")))))

(defun evergreen--refresh-spawn-host-list ()
  "Refresh the `tabulated-list-entries` for the Evergreen Spawn Host menu."
  ;; I don't know why this uses setq but it's buffer local
  ;; automagically. See the wacky tabulated list mode docs if you're
  ;; really curious why this is so crazy.
  ;;
  ;; docs: https://www.gnu.org/software/emacs/manual/html_node/elisp/Tabulated-List-Mode.html
  (setq
   tabulated-list-entries
   (mapcar 
    #'(lambda (host)
        (list
         ;; This is used to remember cursor
         ;; position and uniquely identify
         ;; entries
         (slot-value host 'id) 
         ;; This vector actually gets
         ;; printed and must match column
         ;; order
         (vector
          ;; You might be thinking to yourself "Mat, why all these
          ;; commas?" and I think that's a great question.
          ;;
          ;; This is some super crazy lisp macro syntax, the backtick
          ;; in from of this s-expression means that it's going to
          ;; make a list of "forms" which is basically unevaluated
          ;; code. The comma tells this backtick to actually evluate
          ;; that s-expression at list build time and store that value
          ;; as the actuall element. Otherwise we'd end up trying to
          ;; call slot-value on host when host is undefined (since the
          ;; lambda in this case is not a closure of this function
          ;; confusingly.)
          ;;
          ;; As for the action bit / what this is actually doing, this
          ;; is what makes the id in the spawn host list a button. See
          ;; the `tabulated-list-entries` help page for more
          ;; information but I ultimately found line number 2953 of
          ;; package.el to be the most helpful reference for this.
          `(
            ,(slot-value host 'id)
            action (lambda (x)
                     (message "Connecting to spawn host: %s" ,(slot-value host 'hostname))
                     (dired (format "/ssh:%s@%s:%s"
                                    (if evergreen-spawn-default-dir
                                        evergreen-spawn-default-dir
                                      (format "~%s" ,(slot-value host 'ssh-user)))
                                    ,(slot-value host 'hostname)
                                    ,(slot-value host 'ssh-user)))))
          (slot-value host 'status)
          (slot-value host 'distro)
          (slot-value host 'hostname)
          (slot-value host 'ssh-user)
          `(
            "Terminate"
            action (lambda (x)
                     (evergreen-terminate-host ,(slot-value host 'id))))
          )))
    (evergreen-get-spawn-hosts)))
  ;; This adds a fake entry which should almost always be sorted to
  ;; the bottom (unless the user sorts by ID and even then it should
  ;; still be).
  ;;
  ;; It's purpose to add a "Spawn Host" button to the menu.
  (add-to-list
   'tabulated-list-entries
   (list
    ;; Fake ID, basically gets thrown away
    1000000
    (vector
     ;; Create our button as the first column (ID)
     `("Spawn Host"
       action (lambda (x) (call-interactively 'evergreen-spawn-host)))
     ;; Status, Distro, Hostname, SSH User, Terminate columns should be empty
     "" "" "" "" ""))))

(define-derived-mode evergreen-spawn-host-menu-mode tabulated-list-mode "Spawn Host Menu"
  "Major mode for interacting with Evergreen Spawn Hosts."
  (setq-local tabulated-list-format [
                                     ;; ColumnName Width CanBeSorted
                                     ("ID"        20 t)
                                     ("Status"    10 t)
                                     ("Distro"    20 t)
                                     ("Hostname"  40 t)
                                     ("SSH User"  10 t)
                                     ("Terminate" 9  t)])
  (setq-local tabulated-list-sort-key (cons "ID" nil))
  ;; buffer revert updates list of spawn hosts
  (add-hook 'tabulated-list-revert-hook 'evergreen--refresh-spawn-host-list nil t))



(provide 'evergreen-spawn)

;;; evergreen-spawn.el ends here
