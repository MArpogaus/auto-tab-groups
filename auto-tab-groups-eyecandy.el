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

(defgroup auto-tab-groups-eyecandy ()
  "Modern tab bar."
  :group 'auto-tab-groups)

(defcustom auto-tab-groups-eyecandy-icons '(("HOME" . '(:style "suc" :icon "custom-emacs")))
  "Alist mapping commands to corresponding tab group names/functions.
Each element is a cons cell:
  - CAR: Command (symbol) or list of commands.
  - CDR: String (group name) or function returning a string."
  :type '(alist :key-type (choice symbol (repeat symbol))
                :value-type (choice string function)))

(defcustom auto-tab-groups-eyecandy-tab-height 25
  "Height of the tab bar tabs in pixels."
  :type 'number)

(defcustom auto-tab-groups-eyecandy-default-icon '(:style "oct" :icon "dot_fill")
  "Default icon displayed for tab groups."
  :type 'string)

(defcustom auto-tab-groups-eyecandy-tab-bar-group-name-format-function nil
  "Function to format the tab-group-name."
  :type 'function)

(defun auto-tab-groups-eyecandy--get-bar-image (height width color)
  "Generate a rectangular bar image with HEIGHT, WIDTH, and COLOR.

Thanks to doom-modeline for the idea:
https://github.com/seagle0128/doom-modeline/blob/ec6bc00ac035e75ad10b52e516ea5d95cc9e0bd9/doom-modeline-core.el#L1454C8-L1454C39"
  (if (and (image-type-available-p 'pbm) (display-graphic-p))
      (propertize
       " " 'display
       (create-image
        (concat (format "P1\n%i %i\n" width height) (make-string (* width height) ?1) "\n")
        'pbm t :foreground color :ascent 'center))
    (propertize "|" 'face (list :foreground color :background color))))

(defun auto-tab-groups-eyecandy--nerd-icon (icon-spec)
  "Return the nerd icon glyph for ICON-SPEC.

Inspired from nerd-icons-corfu: https://github.com/LuigiPiucco/nerd-icons-corfu/blob/721830b42b35e326a88b338fc53e4752f333fad2/nerd-icons-corfu.el#L113"
  (let* ((style (plist-get icon-spec :style))
         (icon (plist-get icon-spec :icon))
         (icon-fun (intern (concat "nerd-icons-" style "icon")))
         (icon-name (if (equal style "suc")
                        (concat "nf-" icon)
                      (concat "nf-"  style "-" icon))))
    (or (and (fboundp icon-fun) (funcall icon-fun icon-name)) "?")))

(defun auto-tab-groups-eyecandy--get-group-icon (tab-group-name)
  "Retrieve the icon for the given TAB-GROUP-NAME."
  (if-let ((tab-group-icon-spec
            (cdr (seq-find (lambda (data)
                             (let ((tab-group-name-re (car data)))
                               (string-match tab-group-name-re tab-group-name)))
                           auto-tab-groups-eyecandy-icons))))
      (if (listp tab-group-icon-spec)
          (auto-tab-groups-eyecandy--nerd-icon tab-group-icon-spec)
        tab-group-icon-spec)
    (auto-tab-groups-eyecandy--nerd-icon auto-tab-groups-eyecandy-default-icon)))

(defun auto-tab-groups-eyecandy--tab-bar-tab-group-format-function (tab _ &optional current-p)
  "Format the tab group name for TAB-BAR.
TAB is the tab object, I is the tab index,
 and CURRENT-P indicates if the tab is selected."
  (let* ((tab-group-name (funcall tab-bar-tab-group-function tab))
         (tab-group-face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive))
         (color (face-foreground (if current-p 'mode-line-emphasis 'shadow)))
         (group-sep (auto-tab-groups-eyecandy--get-bar-image auto-tab-groups-eyecandy-tab-height (if current-p 4 2) color))
         (group-icon (auto-tab-groups-eyecandy--get-group-icon tab-group-name))
         (tab-group-name-formatted (if (functionp auto-tab-groups-eyecandy-tab-bar-group-name-format-function)
                                       (funcall auto-tab-groups-eyecandy-tab-bar-group-name-format-function tab-group-name)
                                     tab-group-name)))
    (concat group-sep (propertize (concat " " group-icon " " tab-group-name-formatted " ") 'face tab-group-face))))

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
                         auto-tab-groups-new-group--tab-bar-format-new
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

(provide 'auto-tab-groups-eyecandy)
;;; auto-tab-groups-eyecandy.el ends here
