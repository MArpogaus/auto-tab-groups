;;; auto-tab-groups.el --- Simple auto tab group creator for specified commands -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Version: 0.1
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
(require 'project)

(defgroup auto-tab-groups nil
  "Automatically create and delete tab groups based on command execution."
  :group 'project)

(defcustom auto-tab-groups-create-commands nil
  "Alist mapping commands to tab group names/functions for creation.
Each element should be a cons cell:
  - CAR: A command (symbol) or list of commands.
  - CDR: A string (group name) or a function that returns a string."
  :type '(alist :key-type (choice symbol (repeat symbol))
                :value-type (choice string function)))

(defcustom auto-tab-groups-close-commands nil
  "Alist mapping commands to tab group names/functions for closure.
Each element should be a cons cell:
  - CAR: A command (symbol) or list of commands.
  - CDR: A string (group name) or a function that returns a string."
  :type '(alist :key-type (choice symbol (repeat symbol))
                :value-type (choice string function)))

(defcustom auto-tab-groups-new-choice 'group-scratch
  "Adjust the behavior when new tab is created.
Refer to `tab-bar-new-tab-choice' for details."
  :type '(choice (const     :tag "Current buffer" t)
                 (const     :tag "Current window" window)
                 (const     :tag "Group scratch buffer" group-scratch)
                 (string    :tag "Buffer" "*scratch*")
                 (directory :tag "Directory" :value "~/")
                 (file      :tag "File" :value "~/.emacs")
                 (function  :tag "Function")
                 (const     :tag "Duplicate tab" clone)))


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

(defun auto-tab-groups--get-group-name (command command-alist &optional results)
  "Get the tab group name for COMMAND from COMMAND-ALIST."
  (let* ((group-data (seq-find (lambda (data)
                                 (if (listp (car data))
                                     (memq command (car data))
                                   (equal command (car data))))
                               command-alist))
         (tab-group-name-or-func (cdr group-data))
         (tab-group-name (if (functionp tab-group-name-or-func)
                             (funcall tab-group-name-or-func results)
                           tab-group-name-or-func)))
    tab-group-name))

(defun auto-tab-groups--dynamic-group-name-p (command command-alist)
  "Return non-nil if the group name for COMMAND in COMMAND-ALIST is dynamic."
  (let* ((group-data (seq-find (lambda (data)
                                 (if (listp (car data))
                                     (memq command (car data))
                                   (equal command (car data))))
                               command-alist))
         (tab-group-name-or-func (cdr group-data)))
    (functionp tab-group-name-or-func)))


(defun auto-tab-groups--switch-tab-group (tab-group-name)
  "Switch to the tab group with the name TAB-GROUP-NAME."
  (tab-bar-select-tab (1+ (tab-bar--tab-index tab-group-name)))
  (when auto-tab-groups-echo-mode
    (message "Switched to tab group: %s" tab-group-name)))

(defun auto-tab-groups--tab-group-exists (command command-alist)
  "Check if a tab group for COMMAND in COMMAND-ALIST exists, and return its name."
  (when-let* ((tab-group-name (auto-tab-groups--get-group-name command command-alist))
              (tab (auto-tab-groups--find-tab-by-group-name tab-group-name)))
    tab-group-name))

(defun auto-tab-groups--switch-or-create-tab-group (tab-group-name)
  "Switch to or create a tab group with the name TAB-GROUP-NAME."
  (if-let ((existing-tab (auto-tab-groups--find-tab-by-group-name tab-group-name)))
      (auto-tab-groups--switch-tab-group existing-tab)
    (auto-tab-groups-new-group tab-group-name)))

(defun auto-tab-groups--close-tab-group (tab-group-name)
  "Close the tab group with the name TAB-GROUP-NAME."
  (run-hooks 'auto-tab-groups-before-delete-hook)
  (when-let ((tab (auto-tab-groups--find-tab-by-group-name tab-group-name)))
    (tab-bar-close-group-tabs tab-group-name))
  (when auto-tab-groups-echo-mode
    (message "Closing tab group: %s" tab-group-name))
  (run-hooks 'auto-tab-groups-after-delete-hook))

(defun auto-tab-groups--get-command-name (orig-fun)
  "Return the symbol name of ORIG-FUN."
  (if (subrp orig-fun) (intern (subr-name orig-fun)) orig-fun))

(defun auto-tab-groups--create-advice (orig-fun &rest args)
  "Advice function to handle tab group creation based on command.
Call ORIG-FUN with ARGS and manage tab groups."
  (let ((command-name (auto-tab-groups--get-command-name orig-fun)))
    (if (auto-tab-groups--dynamic-group-name-p command-name auto-tab-groups-create-commands)
        (let* ((results (apply orig-fun args))
               (tab-group-name (auto-tab-groups--get-group-name command-name auto-tab-groups-create-commands results)))
          (auto-tab-groups--switch-or-create-tab-group tab-group-name)
          results)
      (when-let ((tab-group-name (auto-tab-groups--get-group-name command-name auto-tab-groups-create-commands)))
        (auto-tab-groups--switch-or-create-tab-group tab-group-name))
      (apply orig-fun args))))

(defun auto-tab-groups--close-advice (orig-fun &rest args)
  "Advice function to handle tab group deletion based on command.
Call ORIG-FUN with ARGS and then manage tab groups."
  (let* ((command-name (auto-tab-groups--get-command-name orig-fun))
         (tab-group-name (auto-tab-groups--get-group-name command-name auto-tab-groups-close-commands)))
    (apply orig-fun args)
    (auto-tab-groups--close-tab-group tab-group-name)))

(defun auto-tab-groups--setup ()
  "Setup advice for commands specified in the configuration."
  (dolist (command-data auto-tab-groups-create-commands)
    (dolist (command (if (listp (car command-data))
                         (car command-data)
                       (list (car command-data))))
      (advice-add command :around #'auto-tab-groups--create-advice)))
  (dolist (command-data auto-tab-groups-close-commands)
    (dolist (command (if (listp (car command-data))
                         (car command-data)
                       (list (car command-data))))
      (advice-add command :around #'auto-tab-groups--close-advice))))

(defun auto-tab-groups--teardown ()
  "Remove advice from commands specified in the configuration."
  (dolist (command-data auto-tab-groups-create-commands)
    (dolist (command (if (listp (car command-data))
                         (car command-data)
                       (list (car command-data))))
      (advice-remove command #'auto-tab-groups--create-advice)))
  (dolist (command-data auto-tab-groups-close-commands)
    (dolist (command (if (listp (car command-data))
                         (car command-data)
                       (list (car command-data))))
      (advice-remove command #'auto-tab-groups--close-advice))))

(defun auto-tab-groups-new-group--tab-bar-format-new ()
  "Button to add a new tab and assign it to a new group."
  `((add-tab menu-item ,tab-bar-new-button auto-tab-groups-new-group
             :help "New")))

;;;###autoload
(define-minor-mode auto-tab-groups-mode
  "Toggle automatic tab group management based on command execution."
  :global t
  :group 'auto-tab-groups
  (if auto-tab-groups-mode
      (auto-tab-groups--setup)
    (auto-tab-groups--teardown)))

(define-minor-mode auto-tab-groups-echo-mode
  "Print the name in the echo area, when creating or switching tab groups."
  :global t
  :group 'auto-tab-groups)

(defun auto-tab-groups-group-name-project (&optional dir)
  "Return the tab group name for the project in DIR.
Use `project-name' if possible, otherwise fallback to the directory name."
  (if (stringp dir)
      (if-let ((project (project-current nil dir)))
          (project-name project)
        (file-name-nondirectory (directory-file-name dir)))
    (when-let ((project (project-current nil)))
      (project-name project))))

(defun auto-tab-groups-new-group (tab-group-name)
  "Create a new tab group with the name TAB-GROUP-NAME."
  (interactive (list(read-shell-command "Group Name: ")))
  (run-hooks 'auto-tab-groups-before-create-hook)
  (if (eq auto-tab-groups-new-choice 'group-scratch)
      (setq-local tab-bar-new-tab-choice (format "*%s-scratch*" tab-group-name))
    (setq-local tab-bar-new-tab-choice auto-tab-groups-new-choice))
  (tab-bar-new-tab)
  ;; HACK: When a new tab is created the previews buffers list seams to stay untouched,
  ;;       so we set it to nil here
  (when (stringp tab-bar-new-tab-choice)
    (set-window-prev-buffers (get-buffer-window) nil))
  (tab-bar-change-tab-group tab-group-name)
  (when auto-tab-groups-echo-mode
    (message "Created new tab group: %s" tab-group-name))
  (run-hooks 'auto-tab-groups-after-create-hook))

(provide 'auto-tab-groups)
;;; auto-tab-groups.el ends here
