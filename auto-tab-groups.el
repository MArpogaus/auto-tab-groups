;;; auto-tab-groups.el --- Simple auto tab group creator for specified commands -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Version: 0.2
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, tabs

;;; Commentary:

;; `auto-tab-groups-mode' allows you to automatically create and manage
;; tab groups based on specific Emacs commands. It switches to existing
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
  - A function: Called to dynamically determine the group name. Its result
                should be a string. If `:ignore-result' is nil, the command's
                result will be passed as argument to this function. Otherwise
                the command is called first, the result passed to the function
                and then the tab group is created.
  - A plist: Provides additional options. Currently supported properties:
    - `:tab-group-name': Group name (string) or a function returning a string.
    - `:ignore-result':  If non-nil and value is a function, command's result is
                         passed to the function. If nil, the tab group is
                         created before the command is run.

Example:

 (((project-prompt-project-dir project-switch-to-buffer) .
     auto-tab-groups-group-name-project)
   (my-open-command1 \"my-group\" :ignore-result t)
   ((my-open-command2 my-open-command3) :tab-group-name \"my-group2\"
                                        :ignore-result t)
   ((dirvish dirvish-fd) :tab-group-name \"dirvish\"))"

  :type '(alist :key-type (choice symbol (repeat symbol))
                :value-type (choice string function (plist :key-type symbol
                                                           :value-type (choice string function boolean)))))

(defcustom auto-tab-groups-close-commands nil
  "Alist mapping commands to tab group specifications for closure.

Each element should be a cons cell:
- CAR: Command (symbol) or list of commands.
- CDR: Group specification, which can be:
  - A string: Name of the tab group.
  - A function: Called to determine the tab group name. Its result should be a
                string.
  - A plist: Provides additional options. Currently supported properties:
    - `:tab-group-name': Group name (string) or a function returning a string.
    - `:ignore-result':  If non-nil, the tab group will be closed regardless of
                         the command's result. If nil, the group is only closed
                         if the command returns non-nil.

Example:

  ((project-kill-buffers . auto-tab-groups-group-name-project)
   (my-close-command :tab-group-name \"my-group2\" :ignore-result t)
   (dirvish-quit \"dirvish\" :ignore-result t))"
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

(defun auto-tab-groups--find-tab-by-group-name (tab-group-name)
  "Return the first tab with the group name TAB-GROUP-NAME."
  (seq-find (lambda (tab) (equal tab-group-name (alist-get 'group tab)))
            (funcall tab-bar-tabs-function)))

(defun auto-tab-groups--get-group-spec (command-data)
  "Return valid group specification for the given COMMAND-DATA.

  The returned plist contains:
  `:tab-group-name' - The group name (string or function).
  `:ignore-result' - Whether to ignore the command's result (boolean)."
  (let ((groups-spec (cdr command-data)))
    (if (nlistp groups-spec) (list :tab-group-name groups-spec)
      (plist-put (cdr groups-spec) :tab-group-name (car groups-spec)))))

(defun auto-tab-groups--switch-tab-group (tab-group-name)
  "Switch to the tab group with the name TAB-GROUP-NAME."
  (tab-bar-select-tab (1+ (tab-bar--tab-index tab-group-name)))
  (when auto-tab-groups-echo-mode
    (message "Switched to tab group: %s" tab-group-name)))

(defun auto-tab-groups--switch-or-create-tab-group (tab-group-name)
  "Switch to or create a tab group with the name TAB-GROUP-NAME."
  (when tab-group-name
    (if-let ((existing-tab (auto-tab-groups--find-tab-by-group-name tab-group-name)))
        (auto-tab-groups--switch-tab-group existing-tab)
      (auto-tab-groups-new-group tab-group-name))))

(defun auto-tab-groups--close-tab-group (tab-group-name)
  "Close the tab group with the name TAB-GROUP-NAME."
  (run-hooks 'auto-tab-groups-before-delete-hook)
  (when-let ((tab (auto-tab-groups--find-tab-by-group-name tab-group-name)))
    (tab-bar-close-group-tabs tab-group-name))
  (when auto-tab-groups-echo-mode
    (message "Closed tab group: %s" tab-group-name))
  (run-hooks 'auto-tab-groups-after-delete-hook))

(defun auto-tab-groups--get-command-name (orig-fun)
  "Return the symbol name of ORIG-FUN."
  (if (subrp orig-fun) (intern (subr-name orig-fun)) orig-fun))

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
        (let* ((results (apply orig-fun args))
               (tab-group-name (if tab-group-name-functionp (funcall tab-group-name-or-func results)
                                 tab-group-name-or-func)))
          (auto-tab-groups--switch-or-create-tab-group tab-group-name)
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
        (auto-tab-groups--close-tab-group tab-group-name)))))

(defun auto-tab-groups-new-group--tab-bar-format-new ()
  "Button to add a new tab and assign it to a new group."
  `((add-tab menu-item ,tab-bar-new-button auto-tab-groups-new-group
             :help "New")))

(defun auto-tab-groups--after-make-frame-function (&optional frame)
  "Initialize new group or clone existing one when new FRAME is created."
  (let ((tab-group-name (funcall tab-bar-tab-group-function (tab-bar--current-tab))))
    (when frame (select-frame frame))
    (tab-group (if tab-group-name tab-group-name auto-tab-groups-initial-group-name))))

(defun auto-tab-groups--advice-add (kind command-data)
  "Add advice to commands in COMMAND-DATA."
  (let ((tab-group-spec (auto-tab-groups--get-group-spec command-data))
        (get-advice-fun (intern (format "auto-tab-groups--get-%s-advice" (symbol-name kind)))))
    (dolist (command (if (listp (car command-data))
                         (car command-data)
                       (list (car command-data))))
      (advice-add command :around (funcall get-advice-fun tab-group-spec)))))

(defun auto-tab-groups--advice-remove (kind command-data)
  "Remove advice from commands in COMMAND-DATA."
  (let ((tab-group-spec (auto-tab-groups--get-group-spec command-data))
        (get-advice-fun (intern (format "auto-tab-groups--get-%s-advice" (symbol-name kind)))))
    (dolist (command (if (listp (car command-data))
                         (car command-data)
                       (list (car command-data))))
      (advice-remove command (funcall get-advice-fun tab-group-spec)))))

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
(define-minor-mode auto-tab-groups-echo-mode
  "Print messages in the echo area when creating or switching tab groups."
  :global t
  :group 'auto-tab-groups)

(defun auto-tab-groups-new-group (tab-group-name)
  "Create a new tab group with the name TAB-GROUP-NAME."
  (interactive (list (read-shell-command "Group Name: ")))
  (run-hooks 'auto-tab-groups-before-create-hook)
  (let* ((tab-bar-new-tab-choice auto-tab-groups-new-choice)
         (choice-buffer-name-p (stringp tab-bar-new-tab-choice)))
    (tab-bar-new-tab)
    ;; HACK: When a new tab is created the previous buffers list seems to stay untouched,
    ;;       so we set it to nil here.
    (when choice-buffer-name-p
      (set-window-prev-buffers (get-buffer-window) nil)))
  (tab-bar-change-tab-group tab-group-name)
  (when auto-tab-groups-echo-mode
    (message "Created new tab group: %s" tab-group-name))
  (run-hooks 'auto-tab-groups-after-create-hook))

(provide 'auto-tab-groups)
;;; auto-tab-groups.el ends here
