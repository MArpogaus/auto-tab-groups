;;; auto-tab-groups-eyecandy.el --- Modern tab bar style -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marcel Arpogaus

;; Author: Marcel Arpogaus <znepry.necbtnhf@tznvy.pbz>
;; Assisted-by: Claude:claude-opus-5
;; Version: 0.3
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

;; `auto-tab-groups-eyecandy' is a companion minor mode for `auto-tab-groups-mode'.
;; It gives the tab bar a more modern style and shows an icon for each tab
;; group, chosen by the group name.

;;; Code:
(require 'tab-bar)
(require 'icons)

(defgroup auto-tab-groups-eyecandy ()
  "Modern tab bar."
  :group 'auto-tab-groups)

(defcustom auto-tab-groups-eyecandy-icons
  '(("HOME" :style "suc" :icon "custom-emacs"))
  "Alist mapping tab group names to icons.
Each element is a cons cell:
  - CAR: Regular expression matched against the tab group name.
  - CDR: Either a string shown as is, or a nerd-icons specification,
         a plist with the keys `:style' and `:icon'.

The first matching element wins.  Group names without a match get
`auto-tab-groups-eyecandy-default-icon'."
  :type '(alist :key-type regexp
                :value-type (choice string (plist :key-type symbol
                                                  :value-type string))))

(defcustom auto-tab-groups-eyecandy-tab-height 25
  "Height of the tab bar tabs in pixels."
  :type 'number)

(defcustom auto-tab-groups-eyecandy-default-icon '(:style "oct" :icon "dot_fill")
  "Icon shown for tab groups that match no entry in the icon alist.
See `auto-tab-groups-eyecandy-icons' for the alist and for the
accepted icon values."
  :type '(choice string (plist :key-type symbol :value-type string)))

(defcustom auto-tab-groups-eyecandy-tab-bar-group-name-format-function nil
  "Function to format the tab-group-name."
  :type 'function)

(defun auto-tab-groups-eyecandy--format-spacer (&optional width)
  "Return a `tab-bar-format' item that inserts a space of WIDTH.
WIDTH is a factor of the normal space width, as in the `space-width'
display property.  It defaults to the normal width."
  (lambda ()
    ;; A terminal draws no fraction of a cell, and one `space-width'
    ;; item anywhere in the format leaves the whole bar row unpainted
    ;; there — reserved, and still showing whatever stood in it
    ;; before.  A plain space says the same thing in a tty.
    (if (display-graphic-p)
        (propertize " " 'display `(space-width ,width))
      " ")))

(defcustom auto-tab-groups-eyecandy-tab-bar-format
  `(tab-bar-format-tabs-groups
    auto-tab-groups-new-group--tab-bar-format-new
    tab-bar-format-align-right
    tab-bar-format-global
    ,(auto-tab-groups-eyecandy--format-spacer 0.1)
    tab-bar-format-menu-bar
    ,(auto-tab-groups-eyecandy--format-spacer 0.75))
  "List of tab bar items.  See `tab-bar-format' for datails."
  :type 'hook)

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

(defun auto-tab-groups-eyecandy--displayable-p (char)
  "Return non-nil when the selected frame has a glyph for CHAR.
`char-displayable-p' answers for the character set and not for the
font, so on a graphical frame ask the font instead.  Nerd font
glyphs live in the private use area, and without the font they draw
as a hex box."
  (if (display-graphic-p)
      (internal-char-font nil char)
    (char-displayable-p char)))

(defun auto-tab-groups-eyecandy--nerd-icon (icon-spec)
  "Return the nerd icon glyph for ICON-SPEC.

Inspired from nerd-icons-corfu: https://github.com/LuigiPiucco/nerd-icons-corfu/blob/721830b42b35e326a88b338fc53e4752f333fad2/nerd-icons-corfu.el#L113"
  (let* ((style (plist-get icon-spec :style))
         (icon (plist-get icon-spec :icon))
         (icon-fun (intern (concat "nerd-icons-" style "icon")))
         (icon-name (if (equal style "suc")
                        (concat "nf-" icon)
                      (concat "nf-"  style "-" icon))))
    (or (and (fboundp icon-fun)
             (let ((glyph (funcall icon-fun icon-name)))
               ;; nerd-icons answers with the glyph whether or not the
               ;; frame can draw it, and a nerd font glyph without the
               ;; font is a hex box in the tab bar.
               (and (> (length glyph) 0)
                    (auto-tab-groups-eyecandy--displayable-p (aref glyph 0))
                    glyph)))
        "?")))

(defun auto-tab-groups-eyecandy--icon (icon-spec)
  "Return ICON-SPEC as the string that shows in the tab bar.
A string stands for itself; a plist names a nerd icon."
  (if (listp icon-spec)
      (auto-tab-groups-eyecandy--nerd-icon icon-spec)
    icon-spec))

(defun auto-tab-groups-eyecandy--get-group-icon (tab-group-name)
  "Retrieve the icon for the given TAB-GROUP-NAME."
  (auto-tab-groups-eyecandy--icon
   (or (cdr (seq-find (lambda (data)
                        (string-match-p (car data) tab-group-name))
                      auto-tab-groups-eyecandy-icons))
       auto-tab-groups-eyecandy-default-icon)))

(defun auto-tab-groups-eyecandy--tab-bar-tab-group-format-function
    (tab _index &optional current-p)
  "Format the group name of TAB for the tab bar.
CURRENT-P is non-nil when TAB is the selected one."
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
     (concat (cond ((not current-p) " ")
                   ((auto-tab-groups-eyecandy--displayable-p #xeb70)
                    " ")
                   (t "\u203a "))
             (if tab-bar-tab-hints (format "%d " i) "")
             (alist-get 'name tab)
             (if (and tab-bar-close-button-show current-p)
                 tab-bar-close-button " "))
     'face (list :inherit 'tab-bar-tab :weight (if current-p 'bold 'normal)))))

(defun auto-tab-groups-eyecandy--setup ()
  "Setup advice for defined commands."
  (unless (iconp 'auto-tab-groups-eyecandy--tab-bar-new)
    (define-icon auto-tab-groups-eyecandy--tab-bar-new nil
      '((symbol "  " :face tab-bar-tab-inactive)
        (text " + "))
      "Icon for creating a new tab."
      :version "29.1"
      :help-echo "New tab"))
  (unless (iconp 'auto-tab-groups-eyecandy--tab-bar-close)
    (define-icon auto-tab-groups-eyecandy--tab-bar-close nil
      '((symbol " ✕ ")
        (text " x "))
      "Icon for closing the clicked tab."
      :version "29.1"
      :help-echo "Click to close tab"))
  (setq tab-bar-new-button (icon-string 'auto-tab-groups-eyecandy--tab-bar-new)
        tab-bar-close-button (propertize (icon-string 'auto-tab-groups-eyecandy--tab-bar-close)
                                         'close-tab t))
  (setq tab-bar-format auto-tab-groups-eyecandy-tab-bar-format
        tab-bar-separator ""
        tab-bar-auto-width nil
        tab-bar-tab-group-format-function #'auto-tab-groups-eyecandy--tab-bar-tab-group-format-function
        tab-bar-tab-name-format-function #'auto-tab-groups-eyecandy--tab-bar-tab-name-format-function))

(defun auto-tab-groups-eyecandy--teardown ()
  "Give the tab bar its stock look back."
  ;; There is no public way to restore the stock buttons; this is the
  ;; function that created them.
  (tab-bar--load-buttons)
  (dolist (s '(tab-bar-separator
               tab-bar-auto-width
               tab-bar-tab-group-format-function
               tab-bar-tab-name-format-function
               tab-bar-format))
    (custom-reevaluate-setting s)))

;;;###autoload
(define-minor-mode auto-tab-groups-eyecandy-mode
  "Give the tab bar a modern style, with an icon for each tab group."
  :global t
  :group 'auto-tab-groups-eyecandy
  (if auto-tab-groups-eyecandy-mode
      (auto-tab-groups-eyecandy--setup)
    (auto-tab-groups-eyecandy--teardown)))

(provide 'auto-tab-groups-eyecandy)
;;; auto-tab-groups-eyecandy.el ends here
