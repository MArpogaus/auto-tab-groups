;;; auto-tab-groups-eyecandy.el --- Modern tab bar style -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Your Name

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, tabs

;;; Commentary:

;; `auto-tab-groups-eyecandy' is a companion minor mode for `auto-tab-groups-mode'.
;; It gives the the tab bar more modern style, and allows to set icons for tab groups
;; depending on their name.

;;; Code:
(require 'tab-bar)
(require 'project)

(defgroup auto-tab-groups-eyecandy ()
  "Modern tab bar."
  :group 'auto-tab-groups)

(defcustom auto-tab-groups-eyecandy-icons '(("HOME" . ""))
  "Alist mapping commands to corresponding tab group names/functions.
Each element is a cons cell:
  - CAR: Command (symbol) or list of commands.
  - CDR: String (group name) or function returning a string."
  :type '(alist :key-type (choice symbol (repeat symbol))
                :value-type (choice string function)))

(defcustom auto-tab-groups-eyecandy-tab-height 25
  "Height of the tab bar tabs in pixels."
  :type 'number)

(defcustom auto-tab-groups-eyecandy-default-icon ""
  "Default icon displayed for tab groups."
  :type 'string)

(defun auto-tab-groups-eyecandy--get-bar-image (height width color)
  "Generate a rectangular bar image with HEIGHT, WIDTH, and COLOR."
  (if (and (image-type-available-p 'pbm) (display-graphic-p))
      (propertize
       " " 'display
       (create-image
        (concat (format "P1\n%i %i\n" width height) (make-string (* width height) ?1) "\n")
        'pbm t :scale 1 :foreground color :ascent 'center))
    (propertize "|" 'face (list :foreground color :background color))))

(defun auto-tab-groups-eyecandy--get-group-icon (tab-group-name)
  "Retrieve the icon for the given TAB-GROUP-NAME."
  (if-let ((tab-group-icon-or-func
            (cdr (seq-find (lambda (data)
                             (let ((tab-group-name-or-func (car data)))
                               (if (functionp tab-group-name-or-func)
                                   (funcall tab-group-name-or-func tab-group-name)
                                 (equal tab-group-name tab-group-name-or-func))))
                           auto-tab-groups-eyecandy-icons))))
      (if (functionp tab-group-icon-or-func)
          (funcall tab-group-icon-or-func tab-group-name)
        tab-group-icon-or-func)
    auto-tab-groups-eyecandy-default-icon))

(defun auto-tab-groups-eyecandy--tab-bar-tab-group-format-function (tab i &optional current-p)
  "Format the tab group name for TAB-BAR.
TAB is the tab object, I is the tab index, and CURRENT-P indicates if the tab is selected."
  (let* ((tab-group-name (funcall tab-bar-tab-group-function tab))
         (tab-group-face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive))
         (color (face-foreground (if current-p 'mode-line-emphasis 'shadow)))
         (group-sep (auto-tab-groups-eyecandy--get-bar-image auto-tab-groups-eyecandy-tab-height (if current-p 4 2) color))
         (group-icon (auto-tab-groups-eyecandy--get-group-icon tab-group-name)))
    (concat group-sep (propertize (concat " " group-icon " " tab-group-name " ") 'face tab-group-face))))

(defun auto-tab-groups-eyecandy--tab-bar-tab-name-format-function (tab i)
  "Format the tab name for TAB-BAR.
TAB is the tab object and I is the tab index."
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (concat (if current-p " " " ")
             (if tab-bar-tab-hints (format "%d " i) "")
             (alist-get 'name tab)
             (if (and tab-bar-close-button-show current-p)
                 tab-bar-close-button " "))
     'face (list :inherit 'tab-bar-tab :weight (if current-p 'bold 'normal)))))

(defun auto-tab-groups-eyecandy--setup ()
  "Setup advice for defined commands."
  (when (>= emacs-major-version 29)
    (require 'icons)
    (unless (iconp 'auto-tab-groups-eyecandy--tab-bar-new)
      (define-icon auto-tab-groups-eyecandy--tab-bar-new nil
        '((symbol "  " :face tab-bar-tab-inactive)
          (text " + "))
        "Icon for creating a new tab."
        :version "29.1"
        :help-echo "New tab"))
    (unless (iconp 'auto-tab-groups-eyecandy--tab-bar-close)
      (define-icon auto-tab-groups-eyecandy--tab-bar-close nil
        '((symbol " 󰅖 ")
          (text " x "))
        "Icon for closing the clicked tab."
        :version "29.1"
        :help-echo "Click to close tab"))
    (setq tab-bar-new-button (icon-string 'auto-tab-groups-eyecandy--tab-bar-new)
          tab-bar-close-button (propertize (icon-string 'auto-tab-groups-eyecandy--tab-bar-close)
                                           'close-tab t)))
  (setq tab-bar-format '(tab-bar-format-tabs-groups
                         tab-bar-format-new
                         tab-bar-format-align-right
                         tab-bar-format-global
                         tab-bar-format-menu-bar)
        tab-bar-separator ""
        tab-bar-auto-width nil
        tab-bar-close-button-show t
        tab-bar-tab-group-format-function #'auto-tab-groups-eyecandy--tab-bar-tab-group-format-function
        tab-bar-tab-name-format-function #'auto-tab-groups-eyecandy--tab-bar-tab-name-format-function))

(defun auto-tab-groups-eyecandy--teardown ()
  "Remove advice from defined commands."
  (when (>= emacs-major-version 29)
    (tab-bar--load-buttons))
  (dolist (s '(tab-bar-separator
               tab-bar-auto-width
               tab-bar-close-button-show
               tab-bar-tab-group-format-function
               tab-bar-tab-name-format-function
               tab-bar-format))
    (custom-reevaluate-setting s)))

;;;###autoload
(define-minor-mode auto-tab-groups-eyecandy-mode
  "Minor mode for automatic tab group management on command execution."
  :global t
  :group 'auto-tab-groups-eyecandy
  (if auto-tab-groups-eyecandy-mode
      (auto-tab-groups-eyecandy--setup)
    (auto-tab-groups-eyecandy--teardown)))

(defun auto-tab-groups-eyecandy-name-is-project (tab-group-name)
  "Return whether the specified TAB-GROUP-NAME corresponds to a project."
  (seq-find (lambda (dir)
              (when-let ((proj (project--find-in-directory dir))
                         (name (project-name proj)))
                (equal name tab-group-name)))
            (project-known-project-roots)))

(defun auto-tab-groups-eyecandy-group-icon-project (tab-group-name)
  "Determine the appropriate icon for the TAB-GROUP-NAME representing a project."
  (if-let ((project-root-dir (auto-tab-groups-eyecandy-name-is-project tab-group-name)))
      (if (file-remote-p project-root-dir) "" "")
    auto-tab-groups-eyecandy-default-icon))

(provide 'auto-tab-groups-eyecandy)
;;; auto-tab-groups-eyecandy.el ends here
