;;; auto-tab-groups-project.el --- Project integration for auto-tab-groups -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Version: 0.2
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

;; This companion package provides the necessary glue code to make
;; `auto-tab-groups-mode' work together with `project.el', to mimic the
;; behavior of [[https://github.com/fritzgrabo/project-tab-groups][project-tab-groups.el]]

;;; Code:
(require 'project)
(require 'auto-tab-groups)

(defun auto-tab-groups-project--get-project-name (dir)
  "Return the name for the project in DIR or the current project if DIR is nil."
  (when-let* ((project (if (and dir (stringp dir)) (project--find-in-directory dir))))
    (project-name project)))

(defun auto-tab-groups-project--get-project-type (dir)
  "Return the type of the project in DIR."
  (when-let* ((project (if (and dir (stringp dir)) (project-current nil dir)
                         (project-current nil)))
              (project-root (project-root project)))
    (if (file-remote-p project-root) ?T ?P)))

(defvar auto-tab-groups-project--create-commands
  '((project-prompt-project-dir project-prompt-project-name project-switch-to-buffer) . auto-tab-groups-project-group-name))

(defvar auto-tab-groups-project--close-commands
  '(project-kill-buffers . auto-tab-groups-project-group-name))

(defun auto-tab-groups-project--project-kill-buffers-advice (orig-fun &rest args)
  "Return the root of the current project when ORIG-FUN killed its buffers.
ORIG-FUN is `project-kill-buffers', ARGS its arguments.  The tab group
name function needs the directory, which is gone once the buffers are."
  (when-let* ((project (project-current t))
              (dir (project-root project)))
    (when (apply orig-fun args) dir)))

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
(defun auto-tab-groups-project-group-name (dir)
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
;;; auto-tab-groups-project.el ends here
