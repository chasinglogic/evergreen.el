;;; evergreen.el-test.el --- Tests for evergreen.el

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

(require 'evergreen)
(require 'dash)

(defmacro with-no-evergreen-binary (&rest forms)
  "Use true for the evergreen binary so it always succeeds."
  `(let ((evergreen-binary-path "true"))
     ,@forms))

(defmacro with-mock-evergreen-command (&rest forms)
  "Patches evergreen-command to just return it's args for comparison."
  `(let ((evergreen-command '(lambda (&rest args) args)))
     ,@forms))

(ert-deftest evergreen-patch-fails-with-bad-arguments ()
  "Should fail when alias tasks and variants are nil."
  (with-no-evergreen-binary
   (should (string-equal
            (car
             (cdr (should-error
                   (evergreen-patch
                    ""
                    nil ;; finalize
                    nil ;; browse
                    nil ;; alias
                    nil ;; variants
                    nil ;; tasks
                    ))))
            "Either ALIAS or both TASKS and VARIANTS must be provided"))

   (should (string-equal
            (car
             (cdr (should-error
                   (evergreen-patch
                    ""
                    nil ;; finalize
                    nil ;; browse
                    nil ;; alias
                    t   ;; variants
                    nil ;; tasks
                    ))))
            "Either ALIAS or both TASKS and VARIANTS must be provided"))

   (should (string-equal
            (car
             (cdr (should-error
                   (evergreen-patch
                    ""
                    nil ;; finalize
                    nil ;; browse
                    nil ;; alias
                    nil ;; variants
                    t   ;; tasks
                    ))))
            "Either ALIAS or both TASKS and VARIANTS must be provided"))))

(ert-deftest evergreen-flagset-all-alias-overrides-tasks-and-variants ()
  "Should return all flags except tasks and variants as alias overrides them."
  (should
   (eq
    (length (-difference
             (evergreen-patch-flagset
              :project "test"
              :alias "alias"
              :tasks '("tasks")
              :variants '("variants")
              :description "desc"
              :no-confirm t
              :large t
              :browse t
              :verbose t
              :finalize t
              :committed-only t)
             '("--project=test"
               "--alias=alias"
               "--description=desc"
               "--yes"
               "--large"
               "--browse"
               "--verbose"
               "--finalize"
               "--committed-only")))
    0)))

(ert-deftest evergreen-patch-flagset-tasks-and-variants ()
  (should
   (eq
    (length (-difference
             (evergreen-patch-flagset
              :project "test"
              :tasks '("tasks")
              :variants '("variants"))
             '("--project=test"
               "--tasks=tasks"
               "--variants=variants")))
    0)))

(provide 'evergreen.el-test)
;;; evergreen.el-test.el ends here
