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
one at all.  The stub switches buffers the way selecting another tab
does, which is what leaves the scratch buffer on screen when the
carry is skipped."
  (let ((home (get-buffer-create "*auto-tab-groups-test home*"))
        (elsewhere (get-buffer-create "*auto-tab-groups-test elsewhere*")))
    (unwind-protect
        (cl-letf (((symbol-function 'auto-tab-groups--switch-or-create-tab-group)
                   (lambda (_name) (switch-to-buffer elsewhere)))
                  ((symbol-function 'auto-tab-groups--current-group)
                   (lambda () "the old one")))
          (switch-to-buffer home)
          (let ((advice (auto-tab-groups--get-create-advice
                         (list :tab-group-name (lambda (&rest _) "the new one")))))
            ;; the command switches to what is current already
            (funcall advice (lambda () (switch-to-buffer home) home)))
          ;; the buffer followed into the new group
          (should (eq (current-buffer) home)))
      (kill-buffer home)
      (kill-buffer elsewhere))))

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

(ert-deftest auto-tab-groups-test-close-advice-answers-as-the-command-did ()
  "The close advice returns what the command returned.
It stands in for the command everywhere, so a caller that uses the
answer — `project-kill-buffers' among the commands people close on —
must get the command's and not the bookkeeping's."
  (let (closed)
    (cl-letf (((symbol-function 'auto-tab-groups--close-tab-group)
               (lambda (name) (setq closed name) nil)))
      (let ((advice (auto-tab-groups--get-close-advice
                     '(:tab-group-name "group"))))
        (should (eq (funcall advice #'auto-tab-groups-test--command) 'result))
        (should (equal closed "group"))))))

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

(ert-deftest auto-tab-groups-test-project-close-asks-nothing-and-counts-the-dead ()
  "The close advice prompts for no project and reads the buffer list, not the answer.
`project-kill-buffers' asks with a prompt of its own, and a prompt in
the advice came first — through the advised `project-prompt-project-dir',
which creates a group for the answer to a question the command then asks
again.  And where the command finds no buffer to kill it returns the
string of its own message, which is not nil, so the group went although
nothing had.

The buffer list is what answers, not the project.  An earlier attempt
asked whether the project had buffers left, mocked that away here, and
shipped: `project-kill-buffer-conditions' keeps every buffer it does not
name, and `project-buffers' counts `*scratch*' and the minibuffer for a
project holding the directory Emacs started in, so no group ever closed."
  (let (prompted)
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional maybe-prompt &rest _)
                 (when maybe-prompt (setq prompted t))
                 '(vc Git "/tmp/project/")))
              ((symbol-function 'project-root) (lambda (_project) "/tmp/project/")))
      ;; a command that kills nothing and answers with its own message
      (should-not (auto-tab-groups-project--project-kill-buffers-advice
                   (lambda (&rest _) "No buffers to kill")))
      ;; one that kills a buffer and answers nil: the answer says nothing
      (let ((doomed (generate-new-buffer "*doomed*")))
        (should (equal (auto-tab-groups-project--project-kill-buffers-advice
                        (lambda (&rest _) (kill-buffer doomed) nil))
                       "/tmp/project/")))
      (should-not prompted))))

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

(ert-deftest auto-tab-groups-test-the-sole-tab-goes-where-the-reader-said ()
  "With `tab-bar-close-last-tab-choice' set, the last tab is theirs to close.
The refusal is there for the default, where `tab-bar-close-tab' would
signal; a reader who named a choice gets what they asked for."
  (let ((tab-bar-tabs-function (lambda (&optional _frame)
                                 '(((group . "only")))))
        closed)
    (cl-letf (((symbol-function 'tab-bar-close-group-tabs)
               (lambda (name) (push name closed))))
      (let ((tab-bar-close-last-tab-choice nil))
        (auto-tab-groups--close-tab-group "only")
        (should-not closed))
      (let ((tab-bar-close-last-tab-choice 'delete-frame))
        (auto-tab-groups--close-tab-group "only")
        (should (equal closed '("only")))))))

(ert-deftest auto-tab-groups-test-a-command-that-quits-leaves-no-group ()
  "A group made for a command that never ran goes with it.
Its empty tab would stay, and every later run of the command would
switch to that tab instead of doing the work."
  (let (closed made)
    (cl-letf (((symbol-function 'auto-tab-groups--find-tab-by-group-name)
               (lambda (_name &optional _tabs) nil))
              ((symbol-function 'auto-tab-groups--switch-or-create-tab-group)
               (lambda (name) (push name made)))
              ((symbol-function 'auto-tab-groups--close-tab-group)
               (lambda (name) (push name closed))))
      (should-error (auto-tab-groups--create-before
                     "group" (lambda (&rest _) (error "No")) nil))
      (should (equal made '("group")))
      (should (equal closed '("group")))
      ;; and a group that was already there is left alone
      (setq closed nil)
      (cl-letf (((symbol-function 'auto-tab-groups--find-tab-by-group-name)
                 (lambda (_name &optional _tabs) 'tab)))
        (should-error (auto-tab-groups--create-before
                       "group" (lambda (&rest _) (error "No")) nil)))
      (should-not closed))))

(ert-deftest auto-tab-groups-test-teardown-follows-the-setup ()
  "The advice comes off the commands it went on.
The options may change while the mode is on: the advice on the commands
they named before must still be removable."
  (let ((auto-tab-groups-create-commands
         '((auto-tab-groups-test--command . "group")))
        (auto-tab-groups-close-commands nil)
        (auto-tab-groups-initial-group-name nil)
        (auto-tab-groups--advised nil)
        (name (auto-tab-groups--advice-name 'create)))
    (unwind-protect
        (progn
          (auto-tab-groups--setup)
          (should (advice-member-p name 'auto-tab-groups-test--command))
          ;; the reader changes the option while the mode is on
          (setq auto-tab-groups-create-commands nil)
          (auto-tab-groups--teardown)
          (should-not (advice-member-p name 'auto-tab-groups-test--command)))
      (advice-remove 'auto-tab-groups-test--command name))))

(ert-deftest auto-tab-groups-test-a-command-that-only-asks-gets-no-group ()
  "`project-forget-project' names a project to forget, not one to enter.
It prompts with `project-prompter', which the create advice sits on."
  (let ((this-command 'project-forget-project))
    (should-not (auto-tab-groups-project-group-name default-directory)))
  (let ((this-command 'project-switch-project))
    (should (equal (auto-tab-groups-project-group-name
                    (expand-file-name "../auto-tab-groups/"))
                   (auto-tab-groups-project-group-name
                    default-directory)))))

(defun auto-tab-groups-test--groups ()
  "Return the group of each tab of this frame, in order."
  (mapcar (lambda (tab) (alist-get 'group tab)) (tab-bar-tabs)))

(ert-deftest auto-tab-groups-test-close-leaves-ungrouped-tabs-alone ()
  "A nil group name closes nothing.
An ungrouped tab carries nil as its group, so `tab-bar-close-group-tabs'
took a nil name as a match for every one of them: measured with one
grouped and two ungrouped tabs, both ungrouped ones went.  A close
command whose name function answers nil reaches this."
  (let ((tab-bar-tab-post-open-functions nil))
    (tab-bar-change-tab-group "kept")
    (tab-bar-new-tab)
    (tab-bar-change-tab-group "")
    (tab-bar-new-tab)
    (tab-bar-change-tab-group "")
    (should (equal (auto-tab-groups-test--groups) '("kept" nil nil)))
    (auto-tab-groups--close-tab-group nil)
    (should (equal (auto-tab-groups-test--groups) '("kept" nil nil)))
    ;; and the named group still closes
    (auto-tab-groups--close-tab-group "kept")
    (should (equal (auto-tab-groups-test--groups) '(nil nil)))))

(ert-deftest auto-tab-groups-test-close-keeps-the-sole-tab ()
  "A group that holds every tab of the frame is left alone.
`tab-bar-close-group-tabs' ends on the last tab, and `tab-bar-close-tab'
answers that one with \"Attempt to delete the sole tab in a frame\" —
which came out of the command the reader had just run."
  (tab-bar-change-tab-group "only")
  (should (equal (auto-tab-groups-test--groups) '("only")))
  ;; no error, and the tab stays
  (should-not (auto-tab-groups--close-tab-group "only"))
  (should (equal (auto-tab-groups-test--groups) '("only"))))
