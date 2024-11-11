;;; auto-tab-groups.el --- Simple auto tab group creator for specified commands -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, tabs

;;; Commentary:

;; This companion package provides the necessary glue code to make
;; `auto-tab-groups-mode' work together with `project.el', to mimic the
;; behavior of [[https://github.com/fritzgrabo/project-tab-groups][project-tab-groups.el]]

;;; Code:
(require 'project)
(require 'auto-tab-groups)

(defun auto-tab-groups-project--get-project-name (&optional dir)
  "Return the name for the project in DIR or the current project if DIR is nil."
  (when-let ((project (if (and dir (stringp dir)) (project-current nil dir) (project-current nil))))
    (project-name project)))

(defun auto-tab-groups-project--get-project-type (&optional dir)
  "Return the type of the project in DIR."
  (when-let* ((project (if (and dir (stringp dir)) (project-current nil dir)
                         (project-current nil)))
              (project-root (project-root project)))
    (if (file-remote-p project-root) ?T ?P)))

(defvar auto-tab-groups-project--create-commands
  '((project-prompt-project-dir project-switch-to-buffer) . auto-tab-groups-group-name-project))

(defvar auto-tab-groups-project--close-commands
  '(project-kill-buffers . auto-tab-groups-group-name-project))

(defun auto-tab-groups-project--project-kill-buffers-advice (orig-fun &rest args)
  (when-let* ((project (project-current t))
              (dir (project-root project)))
    (when (funcall orig-fun args) dir)))

(defun auto-tab-groups-project--setup ()
  "Perform configurations necessary for `auto-tab-groups-project-mode'."
  (advice-add #'project-kill-buffers :around #'auto-tab-groups-project--project-kill-buffers-advice)
  (auto-tab-groups--advice-add 'create auto-tab-groups-project--create-commands)
  (auto-tab-groups--advice-add 'close auto-tab-groups-project--close-commands))

(defun auto-tab-groups-project--teardown ()
  "Undo changes of `auto-tab-groups-project-mode'."
  (advice-remove #'project-kill-buffers #'auto-tab-groups-project--project-kill-buffers-advice)
  (auto-tab-groups--advice-remove 'create auto-tab-groups-project--create-commands)
  (auto-tab-groups--advice-remove 'close auto-tab-groups-project--close-commands))

;;;###autoload
(defun auto-tab-groups-group-name-project (&optional dir)
  "Return the tab group name for the project in DIR."
  (if-let* ((project-name (auto-tab-groups-project--get-project-name dir))
            (project-type (auto-tab-groups-project--get-project-type dir)))
      (format "[%c] %s" project-type project-name)))

;;;###autoload
(define-minor-mode auto-tab-groups-project-mode
  "Toggle automatic tab group management for project buffers."
  :global t
  :group 'auto-tab-groups
  (if auto-tab-groups-project-mode
      (auto-tab-groups-project--setup)
    (auto-tab-groups-project--teardown)))

(provide 'auto-tab-groups-project)
;;; auto-tab-groups.el ends here
