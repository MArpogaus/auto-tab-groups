;;; auto-tab-groups.el --- Simple auto tab group creator for specified commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Version: 0.4
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
;; The mode can also delete tab groups when specific commands are invoked.
;; This work has been heavily inspired by [[https://github.com/fritzgrabo/project-tab-groups][project-tab-groups.el]]

;;; Code:
(require 'tab-bar)

(defgroup auto-tab-groups nil
  "Automatically create and delete tab groups based on command execution."
  :group 'project)

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
  :type '(alist :key-type (choice symbol (repeat symbol))
                :value-type (choice string function (plist :key-type symbol
                                                           :value-type (choice string function boolean)))))

(defcustom auto-tab-groups-initial-group-name "HOME"
  "Define the name of the tab group created in new frames."
  :type 'string)

(defcustom auto-tab-groups-new-choice "*scratch*"
  "Adjust the behavior when a new tab is created.
Refer to `tab-bar-new-tab-choice' for details."
  :type '(choice (const :tag "Current buffer" t)
                 (const :tag "Current window" window)
                 (string :tag "Buffer" "*scratch*")
                 (directory :tag "Directory" :value "~/")
                 (file :tag "File" :value "~/.emacs")
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

(defun auto-tab-groups--find-tab-by-group-name (tab-group-name)
  "Return the first tab with the group name TAB-GROUP-NAME."
  (seq-find (lambda (tab) (equal tab-group-name (alist-get 'group tab)))
            (funcall tab-bar-tabs-function)))

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

(defun auto-tab-groups--switch-tab-group (tab)
  "Switch to TAB, the first tab of the wanted tab group."
  (tab-bar-select-tab (1+ (tab-bar--tab-index tab)))
  (when auto-tab-groups-echo-mode
    (message "Switched to tab group: %s" (alist-get 'group tab))))

(defun auto-tab-groups--current-group ()
  "Return the group name of the current tab, or nil."
  (alist-get 'group (tab-bar--current-tab-find)))

(defun auto-tab-groups--switch-or-create-tab-group (tab-group-name)
  "Switch to or create a tab group with the name TAB-GROUP-NAME."
  (when tab-group-name
    (if-let* ((existing-tab (auto-tab-groups--find-tab-by-group-name tab-group-name)))
        (auto-tab-groups--switch-tab-group existing-tab)
      (auto-tab-groups-new-group tab-group-name))))

(defun auto-tab-groups--close-tab-group (tab-group-name)
  "Close the tab group with the name TAB-GROUP-NAME.
Nothing happens when no such group exists."
  (when (auto-tab-groups--find-tab-by-group-name tab-group-name)
    (run-hooks 'auto-tab-groups-before-delete-hook)
    (tab-bar-close-group-tabs tab-group-name)
    (when auto-tab-groups-echo-mode
      (message "Closed tab group: %s" tab-group-name))
    (run-hooks 'auto-tab-groups-after-delete-hook)))

(defun auto-tab-groups--get-create-advice (tab-group-spec)
  "Get advice function to handle tab group creation based on TAB-GROUP-SPEC."
  (lambda (orig-fun &rest args)
    (let* ((tab-group-name-or-func (plist-get tab-group-spec :tab-group-name))
           (tab-group-name-functionp (functionp tab-group-name-or-func))
           (ignore-result (plist-get tab-group-spec :ignore-result)))
      (if (or (not tab-group-name-functionp) ignore-result)
          (let ((tab-group-name (if tab-group-name-functionp (funcall tab-group-name-or-func)
                                  tab-group-name-or-func)))
            (auto-tab-groups--switch-or-create-tab-group tab-group-name)
            (apply orig-fun args))
        ;; The group name is only known once the command has run, and
        ;; by then the command has shown whatever it produced in the
        ;; tab that was current.  Leave that tab as it was and take
        ;; the buffer along to the group it belongs to.
        (let* ((buffer (current-buffer))
               (windows (current-window-configuration))
               (results (apply orig-fun args))
               (shown (current-buffer))
               (tab-group-name (if tab-group-name-functionp (funcall tab-group-name-or-func results)
                                 tab-group-name-or-func))
               ;; Only when the command showed a buffer and the group
               ;; it belongs to is another one.  A command that merely
               ;; prompts leaves the new tab as it was.  It shows one
               ;; when it hands one over, or when the current buffer
               ;; changed: switching to the buffer that was current
               ;; already changes nothing to compare, so the returned
               ;; buffer is the only sign that anything was shown.
               (carry (and tab-group-name
                           (or (buffer-live-p results)
                               (not (eq shown buffer)))
                           (not (equal tab-group-name
                                       (auto-tab-groups--current-group))))))
          (when carry (set-window-configuration windows))
          (auto-tab-groups--switch-or-create-tab-group tab-group-name)
          (when (and carry (buffer-live-p shown)) (switch-to-buffer shown))
          results)))))

(defun auto-tab-groups--get-close-advice (tab-group-spec)
  "Get advice function to handle tab group closing based on TAB-GROUP-SPEC."
  (lambda (orig-fun &rest args)
    (let* ((result (apply orig-fun args))
           (tab-group-name-or-func (plist-get tab-group-spec :tab-group-name))
           (ignore-result (plist-get tab-group-spec :ignore-result))
           (tab-group-name (if (functionp tab-group-name-or-func)
                               (funcall tab-group-name-or-func result)
                             tab-group-name-or-func)))
      (when (or ignore-result result)
        (auto-tab-groups--close-tab-group tab-group-name))
      ;; The advice stands in for the command, so it answers as the
      ;; command did.  Closing a group is bookkeeping and its value is
      ;; nobody's business.
      result)))

(defun auto-tab-groups--after-make-frame-function (&optional frame)
  "Initialize new group or clone existing one when new FRAME is created."
  (let ((tab-group-name (funcall tab-bar-tab-group-function (tab-bar--current-tab))))
    (when frame (select-frame frame))
    (tab-group (if tab-group-name tab-group-name auto-tab-groups-initial-group-name))))

(defun auto-tab-groups--commands (command-data)
  "Return the list of commands named by COMMAND-DATA."
  (if (listp (car command-data))
      (car command-data)
    (list (car command-data))))

(defun auto-tab-groups--advice-name (kind)
  "Return the name under which advice of KIND is registered.
KIND is either the symbol `create' or the symbol `close'.  The advice
carries a name because each call to the advice constructor returns a
fresh closure, which `advice-remove' could not find again."
  (intern (format "auto-tab-groups--%s" kind)))

(defun auto-tab-groups--advice-add (kind command-data)
  "Advise the commands in COMMAND-DATA to manage tab groups.
KIND is either the symbol `create' or the symbol `close'."
  (let ((tab-group-spec (auto-tab-groups--get-group-spec command-data))
        ;; Named rather than made from KIND: there are two kinds, and
        ;; a name that the compiler reads is a name that grep finds.
        (get-advice-fun (if (eq kind 'create)
                            #'auto-tab-groups--get-create-advice
                          #'auto-tab-groups--get-close-advice)))
    (dolist (command (auto-tab-groups--commands command-data))
      (advice-add command :around (funcall get-advice-fun tab-group-spec)
                  `((name . ,(auto-tab-groups--advice-name kind)))))))

(defun auto-tab-groups--advice-remove (kind command-data)
  "Remove the advice of KIND from the commands in COMMAND-DATA.
KIND is either the symbol `create' or the symbol `close'."
  (dolist (command (auto-tab-groups--commands command-data))
    (advice-remove command (auto-tab-groups--advice-name kind))))

(defun auto-tab-groups--setup ()
  "Setup advice for commands specified in the configuration."
  (dolist (command-data auto-tab-groups-create-commands)
    (auto-tab-groups--advice-add 'create command-data))
  (dolist (command-data auto-tab-groups-close-commands)
    (auto-tab-groups--advice-add 'close command-data))
  (when auto-tab-groups-initial-group-name
    (auto-tab-groups--after-make-frame-function)
    (add-hook 'after-make-frame-functions #'auto-tab-groups--after-make-frame-function)))

(defun auto-tab-groups--teardown ()
  "Remove advice from commands specified in the configuration."
  (dolist (command-data auto-tab-groups-create-commands)
    (auto-tab-groups--advice-remove 'create command-data))
  (dolist (command-data auto-tab-groups-close-commands)
    (auto-tab-groups--advice-remove 'close command-data))
  (remove-hook 'after-make-frame-functions #'auto-tab-groups--after-make-frame-function))

;;;###autoload
(define-minor-mode auto-tab-groups-mode
  "Toggle automatic tab group management based on command execution."
  :global t
  :group 'auto-tab-groups
  (if auto-tab-groups-mode
      (auto-tab-groups--setup)
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
