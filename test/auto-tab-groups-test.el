;;; auto-tab-groups-test.el --- Tests for auto-tab-groups -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; URL: https://github.com/MArpogaus/auto-tab-groups

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run with: make test

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'auto-tab-groups)
(require 'auto-tab-groups-project)

(defvar auto-tab-groups-test--switched nil
  "Group name the stubbed switch function received.")

(defun auto-tab-groups-test--command (&rest _)
  "Stand-in for a command that opens something."
  'result)

(defmacro auto-tab-groups-test--with-stub (&rest body)
  "Evaluate BODY with tab group switching recorded, not performed."
  (declare (indent 0))
  `(let ((auto-tab-groups-test--switched nil))
     (cl-letf (((symbol-function 'auto-tab-groups--switch-or-create-tab-group)
                (lambda (name) (setq auto-tab-groups-test--switched name))))
       ,@body)))

(ert-deftest auto-tab-groups-test-group-spec ()
  "A group specification is normalized to a plist."
  (should (equal (auto-tab-groups--get-group-spec '(cmd . "group"))
                 '(:tab-group-name "group")))
  (should (equal (auto-tab-groups--get-group-spec '(cmd . ignore))
                 '(:tab-group-name ignore)))
  (should (equal (plist-get (auto-tab-groups--get-group-spec
                             '(cmd "group" :ignore-result t))
                            :tab-group-name)
                 "group"))
  (should (plist-get (auto-tab-groups--get-group-spec
                      '(cmd "group" :ignore-result t))
                     :ignore-result))
  ;; the plist form from the docstring examples
  (should (equal (auto-tab-groups--get-group-spec
                  '(cmd :tab-group-name "group" :ignore-result t))
                 '(:tab-group-name "group" :ignore-result t)))
  ;; a lambda is a spec, and in interpreted code also a list
  (let ((spec (auto-tab-groups--get-group-spec
               (cons 'cmd (lambda () "dyn")))))
    (should (functionp (plist-get spec :tab-group-name)))))

(ert-deftest auto-tab-groups-test-group-spec-copies ()
  "Normalizing never modifies the user's customization data."
  (let ((user (list "group" :ignore-result t)))
    (auto-tab-groups--get-group-spec (cons 'cmd user))
    (should (equal user '("group" :ignore-result t))))
  (let ((user (list :tab-group-name "group")))
    (plist-put (auto-tab-groups--get-group-spec (cons 'cmd user))
               :ignore-result t)
    (should (equal user '(:tab-group-name "group")))))

(ert-deftest auto-tab-groups-test-commands ()
  "Both a single command and a list of commands are accepted."
  (should (equal (auto-tab-groups--commands '(cmd . "group")) '(cmd)))
  (should (equal (auto-tab-groups--commands '((cmd1 cmd2) . "group")) '(cmd1 cmd2))))

(ert-deftest auto-tab-groups-test-advice-round-trip ()
  "Advice is removed again.
Each call to the advice constructor returns a fresh closure, so the
advice carries a name; without it the advice would stay behind."
  (let ((data '(auto-tab-groups-test--command . "group"))
        (name (auto-tab-groups--advice-name 'create)))
    (unwind-protect
        (progn
          (auto-tab-groups--advice-add 'create data)
          (should (advice-member-p name 'auto-tab-groups-test--command))
          (auto-tab-groups--advice-remove 'create data)
          (should-not (advice-member-p name 'auto-tab-groups-test--command)))
      (advice-remove 'auto-tab-groups-test--command name))))

(ert-deftest auto-tab-groups-test-create-advice-static-name ()
  "A static group name is created before the command runs."
  (auto-tab-groups-test--with-stub
    (let ((advice (auto-tab-groups--get-create-advice '(:tab-group-name "group"))))
      (should (eq (funcall advice #'auto-tab-groups-test--command) 'result))
      (should (equal auto-tab-groups-test--switched "group")))))

(ert-deftest auto-tab-groups-test-create-advice-from-result ()
  "A group name function receives the command result by default."
  (auto-tab-groups-test--with-stub
    (let ((advice (auto-tab-groups--get-create-advice
                   (list :tab-group-name (lambda (result) (format "%s" result))))))
      (should (eq (funcall advice #'auto-tab-groups-test--command) 'result))
      (should (equal auto-tab-groups-test--switched "result")))))

(ert-deftest auto-tab-groups-test-create-advice-ignore-result ()
  "With :ignore-result the group is created before the command runs."
  (auto-tab-groups-test--with-stub
    (let ((advice (auto-tab-groups--get-create-advice
                   (list :tab-group-name (lambda (&rest _) "early")
                         :ignore-result t))))
      (should (eq (funcall advice #'auto-tab-groups-test--command) 'result))
      (should (equal auto-tab-groups-test--switched "early")))))

(ert-deftest auto-tab-groups-test-create-advice-carries-the-buffer ()
  "What the command showed goes to the group, and the old tab stays put.
The group name is only known once the command has run, and by then it
has shown its buffer in the tab that was current.  So the tab is put
back the way it was, and the buffer follows into the new group."
  (let ((home (get-buffer-create "*auto-tab-groups-test home*"))
        (opened (get-buffer-create "*auto-tab-groups-test opened*"))
        at-switch)
    (unwind-protect
        (cl-letf (((symbol-function 'auto-tab-groups--switch-or-create-tab-group)
                   (lambda (name) (setq at-switch (cons name (current-buffer)))))
                  ((symbol-function 'auto-tab-groups--current-group)
                   (lambda () "the old one")))
          (switch-to-buffer home)
          (let ((advice (auto-tab-groups--get-create-advice
                         (list :tab-group-name (lambda (&rest _) "the new one")))))
            (funcall advice (lambda () (switch-to-buffer opened) opened)))
          ;; the group is switched with the old tab as the user left it
          (should (equal (car at-switch) "the new one"))
          (should (eq (cdr at-switch) home))
          ;; and the buffer arrives in the new group afterwards
          (should (eq (current-buffer) opened)))
      (kill-buffer home)
      (kill-buffer opened))))

(ert-deftest auto-tab-groups-test-create-advice-carries-a-current-buffer ()
  "A command that shows the buffer already current carries it too.
Switching to the current buffer changes nothing to compare before and
after, so the buffer it hands back is the only sign that it showed
one at all."
  (let ((home (get-buffer-create "*auto-tab-groups-test home*"))
        at-switch)
    (unwind-protect
        (cl-letf (((symbol-function 'auto-tab-groups--switch-or-create-tab-group)
                   (lambda (name) (setq at-switch (cons name (current-buffer)))))
                  ((symbol-function 'auto-tab-groups--current-group)
                   (lambda () "the old one")))
          (switch-to-buffer home)
          (let ((advice (auto-tab-groups--get-create-advice
                         (list :tab-group-name (lambda (&rest _) "the new one")))))
            ;; the command switches to what is current already
            (funcall advice (lambda () (switch-to-buffer home) home)))
          (should (equal (car at-switch) "the new one"))
          (should (eq (current-buffer) home)))
      (kill-buffer home))))

(ert-deftest auto-tab-groups-test-create-advice-carries-nothing-extra ()
  "A command that showed nothing leaves the new group as it was."
  (let ((home (get-buffer-create "*auto-tab-groups-test home*"))
        at-switch)
    (unwind-protect
        (cl-letf (((symbol-function 'auto-tab-groups--switch-or-create-tab-group)
                   (lambda (name) (setq at-switch (cons name (current-buffer)))))
                  ((symbol-function 'auto-tab-groups--current-group)
                   (lambda () "the old one")))
          (switch-to-buffer home)
          (let ((advice (auto-tab-groups--get-create-advice
                         (list :tab-group-name (lambda (&rest _) "the new one")))))
            (funcall advice (lambda () 'a-directory)))
          (should (equal (car at-switch) "the new one"))
          (should (eq (current-buffer) home)))
      (kill-buffer home))))

(ert-deftest auto-tab-groups-test-find-tab-by-group-name ()
  "Tabs are found by their group name."
  (cl-letf (((symbol-function 'tab-bar-tabs-function)
             (lambda (&rest _) nil))
            (tab-bar-tabs-function
             (lambda (&rest _) '(((name . "a") (group . "one"))
                                 ((name . "b") (group . "two"))))))
    (should (equal (alist-get 'name (auto-tab-groups--find-tab-by-group-name "two"))
                   "b"))
    (should-not (auto-tab-groups--find-tab-by-group-name "three"))))

(ert-deftest auto-tab-groups-test-project-group-name ()
  "Without a project there is no group name."
  (should-not (auto-tab-groups-project-group-name "/does/not/exist/")))

(ert-deftest auto-tab-groups-test-project-group-name-from-any-return-value ()
  "The name comes out for all three things the commands return.
`project-prompt-project-dir' returns a directory,
`project-switch-to-buffer' a buffer and `project-prompt-project-name'
the name of a known project.  Only the directory used to work, so
switching to a buffer of another project left the tab in the group of
the project one came from."
  (let* ((dir (file-name-as-directory (make-temp-file "auto-tab-groups-" t)))
         (project-find-functions (list (lambda (d) (cons 'transient d))))
         (name (project-name (cons 'transient dir)))
         (expected (format "[P] %s" name)))
    (unwind-protect
        (cl-letf (((symbol-function 'project-known-project-roots)
                   (lambda () (list dir))))
          (should (equal (auto-tab-groups-project-group-name dir) expected))
          (with-temp-buffer
            (setq default-directory dir)
            (should (equal (auto-tab-groups-project-group-name (current-buffer))
                           expected)))
          (should (equal (auto-tab-groups-project-group-name name) expected))
          ;; a name nobody knows still answers with nothing
          (should-not (auto-tab-groups-project-group-name "no such project")))
      (delete-directory dir t))))

(provide 'auto-tab-groups-test)
;;; auto-tab-groups-test.el ends here
