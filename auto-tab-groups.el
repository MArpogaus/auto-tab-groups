;;; auto-tab-groups.el --- Simple auto tab group creator for specified commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tabs
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

;; `auto-tab-groups-mode' allows you to automatically create and manage
;; tab groups based on specific Emacs commands.  It switches to existing
;; tab groups or creates new ones based on user-defined configurations.
;; The mode can also delete tab groups when specific commands are
;; invoked.
;;
;; The work is heavily inspired by project-tab-groups.el:
;; https://github.com/fritzgrabo/project-tab-groups

;;; Code:
(require 'tab-bar)

(defgroup auto-tab-groups nil
  "Automatically create and delete tab groups based on command execution."
  :group 'project)

(defun auto-tab-groups--set-option (symbol value)
  "Set SYMBOL to VALUE, and follow the new value while the mode is on.
The advice sits on the commands the options named when the mode went
on, so a change made afterwards would otherwise wait for the next
toggle of the mode."
  (set-default symbol value)
  (when (bound-and-true-p auto-tab-groups-mode)
    (auto-tab-groups--teardown)
    (auto-tab-groups--setup)))

(defcustom auto-tab-groups-create-commands nil
  "Alist mapping commands to tab group specifications for creation.

Each element should be a cons cell:
- CAR: Command (symbol) or list of commands.
- CDR: Group specification, which can be:
  - A string: Name of the tab group.
  - A function: Called to determine the group name.  Its result should be a
                string.  The command runs first and its result is passed to
                the function, unless `:ignore-result' is non-nil: then the
                group is created before the command runs and the function is
                called with no argument.
  - A plist: Provides additional options.  Currently supported properties:
    - `:tab-group-name': Group name (string) or a function returning a string.
    - `:ignore-result':  If non-nil, the tab group is created before the
                         command runs, and a name function is called with no
                         argument.  If nil, the command runs first and its
                         result is passed to a name function.

Example:

 ((my-open-command1 \"my-group\" :ignore-result t)
  ((my-open-command2 my-open-command3) :tab-group-name \"my-group2\"
                                       :ignore-result t)
  ((my-open-command4 my-open-command5) :tab-group-name \"my-group3\"))

See `auto-tab-groups-project-group-name' for a group name that follows
the current project."
  :set #'auto-tab-groups--set-option
  :type '(alist :key-type (choice symbol (repeat symbol))
                :value-type (choice string function (plist :key-type symbol
                                                           :value-type (choice string function boolean)))))

(defcustom auto-tab-groups-close-commands nil
  "Alist mapping commands to tab group specifications for closure.

Each element should be a cons cell:
- CAR: Command (symbol) or list of commands.
- CDR: Group specification, which can be:
  - A string: Name of the tab group.
  - A function: Called to determine the tab group name.  Its result should be a
                string.
  - A plist: Provides additional options.  Currently supported properties:
    - `:tab-group-name': Group name (string) or a function returning a string.
    - `:ignore-result':  If non-nil, the tab group will be closed regardless of
                         the command's result.  If nil, the group is only closed
                         if the command returns non-nil.

Example:

 ((my-close-command1 :tab-group-name \"my-group2\" :ignore-result t)
  (my-close-command2 \"my-group3\" :ignore-result t))

See `auto-tab-groups-project-group-name' for a group name that follows
the current project."
  :set #'auto-tab-groups--set-option
  :type '(alist :key-type (choice symbol (repeat symbol))
                :value-type (choice string function (plist :key-type symbol
                                                           :value-type (choice string function boolean)))))

(defcustom auto-tab-groups-initial-group-name "HOME"
  "Define the name of the tab group created in new frames.
A new frame that comes from a frame with a group of its own keeps that
group; this is the name for the frames that have none.  Nil leaves
every new frame alone."
  :set #'auto-tab-groups--set-option
  :type '(choice (const :tag "Leave them alone" nil) string))

(defcustom auto-tab-groups-new-choice "*scratch*"
  "Adjust the behavior when a new tab is created.
Refer to `tab-bar-new-tab-choice' for details."
  :type '(choice (const :tag "Current buffer" t)
                 (const :tag "Current window" window)
                 (string :tag "Buffer" "*scratch*")
                 (directory :tag "Directory" :value "~/")
                 (file :tag "File")
                 (function  :tag "Function")
                 (const :tag "Duplicate tab" clone)))

(defcustom auto-tab-groups-before-create-hook nil
  "Hook run before a tab group is created."
  :type 'hook)

(defcustom auto-tab-groups-after-create-hook nil
  "Hook run after a tab group is created."
  :type 'hook)

(defcustom auto-tab-groups-before-delete-hook nil
  "Hook run before a tab group is deleted."
  :type 'hook)

(defcustom auto-tab-groups-after-delete-hook nil
  "Hook run after a tab group is deleted."
  :type 'hook)

;;;###autoload
(define-minor-mode auto-tab-groups-echo-mode
  "Print messages in the echo area when creating or switching tab groups."
  :global t
  :group 'auto-tab-groups)

(defun auto-tab-groups--find-tab-by-group-name (tab-group-name &optional tabs)
  "Return the first of TABS with the group name TAB-GROUP-NAME.
TABS is what `tab-bar-tabs-function' answers, where a caller that has
the list already does not pass its own."
  (seq-find (lambda (tab) (equal tab-group-name (alist-get 'group tab)))
            (or tabs (funcall tab-bar-tabs-function))))

(defun auto-tab-groups--get-group-spec (command-data)
  "Return the group specification of COMMAND-DATA as a plist.

The returned plist contains:
`:tab-group-name' - The group name (string or function).
`:ignore-result' - Whether to ignore the command's result (boolean).

The result is a fresh list: the input belongs to the user's
customization and may not be modified."
  (let ((spec (cdr command-data)))
    (cond
     ;; a bare name or a function, `(command . "name")'
     ((or (nlistp spec) (functionp spec))
      (list :tab-group-name spec))
     ;; already a plist, `(command :tab-group-name "name" ...)'
     ((keywordp (car spec))
      (copy-sequence spec))
     ;; name first, `(command "name" :ignore-result t)'
     (t
      (append (list :tab-group-name (car spec)) (cdr spec))))))

(defun auto-tab-groups--switch-tab-group (tab tabs)
  "Switch to TAB, the first tab of the wanted tab group, among TABS.
The index comes from the list the caller searched: `tab-bar--tab-index'
walks the list of the moment and answers nil for a tab that is not in
it, and `1+' of nil is an error rather than a missing tab."
  (tab-bar-select-tab (1+ (seq-position tabs tab)))
  (when auto-tab-groups-echo-mode
    (message "Switched to tab group: %s" (alist-get 'group tab))))

(defun auto-tab-groups--current-group ()
  "Return the group name of the current tab, or nil."
  (alist-get 'group (tab-bar--current-tab-find)))

(defun auto-tab-groups--switch-or-create-tab-group (tab-group-name)
  "Switch to or create a tab group with the name TAB-GROUP-NAME."
  (when tab-group-name
    (let ((tabs (funcall tab-bar-tabs-function)))
      (if-let* ((existing-tab (auto-tab-groups--find-tab-by-group-name
                               tab-group-name tabs)))
          (auto-tab-groups--switch-tab-group existing-tab tabs)
        (auto-tab-groups-new-group tab-group-name)))))

(defun auto-tab-groups--close-tab-group (tab-group-name)
  "Close the tab group with the name TAB-GROUP-NAME.
Nothing happens where no such group exists, nor in two cases that used
to end badly.

A nil name: an ungrouped tab carries nil as its group, so nil matches
every one of them, and `tab-bar-close-group-tabs' then deleted the lot.
Measured with one grouped and two ungrouped tabs, both ungrouped ones
went.  A close command whose name function answers nil reaches this.

A group that holds every tab of the frame, where the reader has no
`tab-bar-close-last-tab-choice': `tab-bar-close-group-tabs' ends on the
last tab, and `tab-bar-close-tab' refuses that one with \"Attempt to
delete the sole tab in a frame\" — which came out of the command the
reader had just run.  With a choice set, that last tab is theirs to
close and `tab-bar-close-tab' does as they asked."
  (when-let* ((tabs (and tab-group-name (funcall tab-bar-tabs-function)))
              ((auto-tab-groups--find-tab-by-group-name tab-group-name tabs))
              ((or tab-bar-close-last-tab-choice
                   (seq-some (lambda (tab)
                               (not (equal tab-group-name
                                           (alist-get 'group tab))))
                             tabs))))
    (run-hooks 'auto-tab-groups-before-delete-hook)
    (tab-bar-close-group-tabs tab-group-name)
    (when auto-tab-groups-echo-mode
      (message "Closed tab group: %s" tab-group-name))
    (run-hooks 'auto-tab-groups-after-delete-hook)))

(defun auto-tab-groups--create-before (name orig-fun args)
  "Open the group NAME, then run ORIG-FUN on ARGS in it.
The name is known before the command runs, so the group is there to run
it in.  A command that quits or signals instead of running leaves no
group behind: the empty tab of a group made for it would stay, and
every later run of the command would switch to that tab."
  (let ((made (and name (not (auto-tab-groups--find-tab-by-group-name name)))))
    (auto-tab-groups--switch-or-create-tab-group name)
    (condition-case err
        (apply orig-fun args)
      ((quit error)
       (when made (auto-tab-groups--close-tab-group name))
       (signal (car err) (cdr err))))))

(defun auto-tab-groups--create-after (name-function orig-fun args)
  "Run ORIG-FUN on ARGS, then open the group NAME-FUNCTION names for it.
The group name is only known once the command has run, and by then the
command has shown whatever it produced in the tab that was current.
That tab is left as it was and the buffer goes along to the group it
belongs to."
  (let* ((buffer (current-buffer))
         (windows (current-window-configuration))
         (results (apply orig-fun args))
         (shown (current-buffer))
         (name (funcall name-function results))
         ;; A command that only prompts shows no buffer and the new tab
         ;; stays as it is.  Switching to the buffer that was current
         ;; already changes nothing to compare, so a returned buffer,
         ;; another current buffer, or a window layout that is not the
         ;; one from before are the three signs of a buffer shown: a
         ;; command that ends in `display-buffer' gives only the last.
         (carry (and name
                     (or (buffer-live-p results)
                         (not (eq shown buffer))
                         (not (window-configuration-equal-p
                               windows (current-window-configuration))))
                     (not (equal name (auto-tab-groups--current-group))))))
    (when carry (set-window-configuration windows))
    (auto-tab-groups--switch-or-create-tab-group name)
    (when (and carry (buffer-live-p shown)) (switch-to-buffer shown))
    results))

(defun auto-tab-groups--get-create-advice (tab-group-spec)
  "Get advice function to handle tab group creation based on TAB-GROUP-SPEC."
  (lambda (orig-fun &rest args)
    (let* ((name-or-function (plist-get tab-group-spec :tab-group-name))
           (functionp (functionp name-or-function)))
      (if (or (not functionp) (plist-get tab-group-spec :ignore-result))
          (auto-tab-groups--create-before
           (if functionp (funcall name-or-function) name-or-function)
           orig-fun args)
        (auto-tab-groups--create-after name-or-function orig-fun args)))))

(defun auto-tab-groups--get-close-advice (tab-group-spec)
  "Get advice function to handle tab group closing based on TAB-GROUP-SPEC."
  (lambda (orig-fun &rest args)
    (let ((result (apply orig-fun args))
          (name (plist-get tab-group-spec :tab-group-name)))
      ;; The name is asked for inside the `when': a command that
      ;; answered nil closes nothing, and a name function has no
      ;; business running for an answer nobody reads.
      (when (or (plist-get tab-group-spec :ignore-result) result)
        (auto-tab-groups--close-tab-group
         (if (functionp name) (funcall name result) name)))
      result)))

(defun auto-tab-groups--after-make-frame-function (&optional frame)
  "Initialize new group or clone existing one when new FRAME is created."
  (let ((tab-group-name (funcall tab-bar-tab-group-function (tab-bar--current-tab))))
    ;; `select-frame' without a restore leaves the wrong frame selected
    ;; for whoever made one it did not mean to show.
    (with-selected-frame (or frame (selected-frame))
      (tab-bar-change-tab-group
       (or tab-group-name auto-tab-groups-initial-group-name)))))

(defun auto-tab-groups--commands (command-data)
  "Return the list of commands named by COMMAND-DATA."
  (ensure-list (car command-data)))

(defun auto-tab-groups--advice-name (kind command-data)
  "Return the name under which the advice of COMMAND-DATA is registered.
KIND is either the symbol `create' or the symbol `close'.  The advice
carries a name because each call to the advice constructor returns a
fresh closure, which `advice-remove' could not find again.

The rule is part of the name, and not the kind alone.  `advice-add'
takes advice of a name that is there already off the command first, so
two rules that name one command shared one piece of advice: the second
replaced the first, and only the group of the second was ever made."
  (intern (format "auto-tab-groups--%s-%s" kind (sxhash-equal command-data))))

(defun auto-tab-groups--advice-add (kind command-data)
  "Advise the commands in COMMAND-DATA to manage tab groups.
KIND is either the symbol `create' or the symbol `close'."
  (let ((tab-group-spec (auto-tab-groups--get-group-spec command-data))
        (get-advice-fun (if (eq kind 'create)
                            #'auto-tab-groups--get-create-advice
                          #'auto-tab-groups--get-close-advice))
        (name (auto-tab-groups--advice-name kind command-data)))
    (dolist (command (auto-tab-groups--commands command-data))
      (advice-add command :around (funcall get-advice-fun tab-group-spec)
                  `((name . ,name))))))

(defun auto-tab-groups--advice-remove (kind command-data)
  "Remove the advice of KIND from the commands in COMMAND-DATA.
KIND is either the symbol `create' or the symbol `close'."
  (let ((name (auto-tab-groups--advice-name kind command-data)))
    (dolist (command (auto-tab-groups--commands command-data))
      (advice-remove command name))))

(defvar auto-tab-groups--advised nil
  "What the advice went on, as a list of (KIND . COMMAND-DATA).
The two options may change while the mode is on, and the advice has to
come off the commands it went on rather than off the ones the options
name by the time the mode goes off.")

(defun auto-tab-groups--setup ()
  "Advise the commands the two options name, and follow new frames.
A rule is written down before its advice goes on the commands.  A rule
that signals half way through then leaves advice the teardown knows
about, where a record written afterwards would have lost it."
  (dolist (command-data auto-tab-groups-create-commands)
    (push (cons 'create command-data) auto-tab-groups--advised)
    (auto-tab-groups--advice-add 'create command-data))
  (dolist (command-data auto-tab-groups-close-commands)
    (push (cons 'close command-data) auto-tab-groups--advised)
    (auto-tab-groups--advice-add 'close command-data))
  (when auto-tab-groups-initial-group-name
    (add-hook 'after-make-frame-functions #'auto-tab-groups--after-make-frame-function)))

(defun auto-tab-groups--teardown ()
  "Remove the advice from the commands it was added to.
The hook comes off first, and a record that cannot be honoured is
reported and skipped rather than left to stop the rest.  A rule that
names something other than a command signals in the setup, and its
record is there because a record is written before the advice goes
on.

The error is caught rather than demoted: `with-demoted-errors' lets it
through where the debugger is on, and the rest of the records would
then keep their advice for the rest of the session."
  (remove-hook 'after-make-frame-functions #'auto-tab-groups--after-make-frame-function)
  (pcase-dolist (`(,kind . ,command-data) auto-tab-groups--advised)
    (condition-case err
        (auto-tab-groups--advice-remove kind command-data)
      (error (message "auto-tab-groups: %S" err))))
  (setq auto-tab-groups--advised nil))

;;;###autoload
(define-minor-mode auto-tab-groups-mode
  "Toggle automatic tab group management based on command execution."
  :global t
  :group 'auto-tab-groups
  (if auto-tab-groups-mode
      (progn
        (auto-tab-groups--setup)
        ;; The frame that turns the mode on wants its group too, and only
        ;; here: the setup runs again for every option a reader changes
        ;; while the mode is on, and that must not put the current tab
        ;; into a group nobody asked for.
        (when auto-tab-groups-initial-group-name
          (auto-tab-groups--after-make-frame-function)))
    (auto-tab-groups--teardown)))

;;;###autoload
(defun auto-tab-groups-new-group (tab-group-name)
  "Create a new tab group with the name TAB-GROUP-NAME."
  (interactive (list (read-string "Group name: ")))
  (run-hooks 'auto-tab-groups-before-create-hook)
  (let ((tab-bar-new-tab-choice auto-tab-groups-new-choice))
    (tab-bar-new-tab)
    ;; A new tab keeps the window of the tab it was made from, and a
    ;; window keeps the buffers it showed before.  Without this the
    ;; new group would walk back into the buffers of the old one with
    ;; `previous-buffer'.  Only where the choice is a buffer of its
    ;; own: any other choice leaves the window where it was, and its
    ;; history is the history of that window.
    (when (stringp tab-bar-new-tab-choice)
      (set-window-prev-buffers (get-buffer-window) nil)))
  (tab-bar-change-tab-group tab-group-name)
  (when auto-tab-groups-echo-mode
    (message "Created new tab group: %s" tab-group-name))
  (run-hooks 'auto-tab-groups-after-create-hook))

(provide 'auto-tab-groups)
;;; auto-tab-groups.el ends here
