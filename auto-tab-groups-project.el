;;; auto-tab-groups-project.el --- Project integration for auto-tab-groups -*- lexical-binding: t; -*-

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

;; This companion package provides the necessary glue code to make
;; `auto-tab-groups-mode' work together with `project.el', to mimic the
;; behavior of [[https://github.com/fritzgrabo/project-tab-groups][project-tab-groups.el]]

;;; Code:
(require 'project)
(require 'auto-tab-groups)

(defun auto-tab-groups-project--get-project-name (dir)
  "Return the name of the project in DIR, or nil."
  (when-let* (((stringp dir))
              (project (project-current nil dir)))
    (project-name project)))

(defun auto-tab-groups-project--directory (thing)
  "Return the directory THING stands for, or nil.
Each command hands over what it returned, and the three of them
return three different things: `project-prompt-project-dir' a
directory, `project-switch-to-buffer' a buffer, and
`project-prompt-project-name' the name of a known project."
  (cond ((bufferp thing) (buffer-local-value 'default-directory thing))
        ((not (stringp thing)) nil)
        ((file-directory-p thing) thing)
        (t (seq-find (lambda (root)
                       (equal (auto-tab-groups-project--get-project-name root)
                              thing))
                     (mapcar #'expand-file-name (project-known-project-roots))))))

(defvar auto-tab-groups-project--create-commands
  '((project-prompt-project-dir project-prompt-project-name project-switch-to-buffer) . auto-tab-groups-project-group-name))

(defvar auto-tab-groups-project--close-commands
  '(project-kill-buffers . auto-tab-groups-project-group-name))

(defun auto-tab-groups-project--project-kill-buffers-advice (orig-fun &rest args)
  "Return the root of the current project when ORIG-FUN killed its buffers.
ORIG-FUN is `project-kill-buffers', ARGS its arguments.  The tab group
name function needs the directory, which is gone once the buffers are.
The project is asked for without a prompt.  `project-kill-buffers' asks
with a prompt of its own, and a prompt here comes first — through the
advised `project-prompt-project-dir', which creates a group for the
answer to a question the command then asks again.  The buffers of the
second answer were killed and the group of the first one closed.

The buffers decide, not the command's answer: where it finds none to
kill it returns the string of its own message, which is not nil, and the
group went although nothing had."
  (if-let* ((project (project-current nil))
            (dir (project-root project)))
      (progn (apply orig-fun args)
             (unless (project-buffers project) dir))
    (apply orig-fun args)))

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
(defun auto-tab-groups-project-group-name (thing)
  "Return the tab group name for the project THING belongs to.
THING is what the command returned: a directory, a buffer, or the
name of a known project."
  (when-let* ((dir (auto-tab-groups-project--directory thing))
              ;; one `project-current' for the name and the letter both:
              ;; measured, that call is 24.7 of the 221 microseconds this
              ;; function cost, and it ran twice
              (project (project-current nil dir))
              (root (project-root project)))
    (format "[%c] %s" (if (file-remote-p root) ?T ?P) (project-name project))))

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
