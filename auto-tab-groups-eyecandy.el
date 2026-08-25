;;; auto-tab-groups-eyecandy.el --- Modern tab bar style -*- lexical-binding: t; -*-

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

;; `auto-tab-groups-eyecandy' is a companion minor mode for `auto-tab-groups-mode'.
;; It gives the tab bar a more modern style and shows an icon for each tab
;; group, chosen by the group name.

;;; Code:
(require 'tab-bar)
(require 'icons)

(defgroup auto-tab-groups-eyecandy ()
  "Modern tab bar."
  :group 'auto-tab-groups)

(defvar auto-tab-groups-eyecandy--icons (make-hash-table :test #'equal)
  "The icon each group name answered, keyed on the name and the display.
A terminal and a graphic frame answer differently, and one session can
hold both.  `auto-tab-groups-eyecandy--forget' empties this where the
answer can change: another icon list, or a font arriving.")

(defun auto-tab-groups-eyecandy--forget (&rest _)
  "Forget the icons answered so far."
  (clrhash auto-tab-groups-eyecandy--icons))

(defun auto-tab-groups-eyecandy--set-icons (symbol value)
  "Set SYMBOL to VALUE and forget the icons answered before it changed."
  (set-default symbol value)
  (auto-tab-groups-eyecandy--forget))

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
                                                  :value-type string)))
  :set #'auto-tab-groups-eyecandy--set-icons)

(defcustom auto-tab-groups-eyecandy-tab-height 25
  "Height of the tab bar tabs in pixels."
  :type 'number)

(defcustom auto-tab-groups-eyecandy-default-icon '(:style "oct" :icon "dot_fill")
  "Icon shown for tab groups that match no entry in the icon alist.
See `auto-tab-groups-eyecandy-icons' for the alist and for the
accepted icon values."
  :type '(choice string (plist :key-type symbol :value-type string))
  :set #'auto-tab-groups-eyecandy--set-icons)

(define-obsolete-variable-alias
  'auto-tab-groups-eyecandy-tab-bar-group-name-format-function
  'auto-tab-groups-eyecandy-group-name-function "0.4")

(defcustom auto-tab-groups-eyecandy-group-name-function nil
  "Function that returns the name to show for a tab group.
It is called with the name of the group and returns the string that
goes into the tab bar.  Nil shows the name as it is."
  :type '(choice (const :tag "The name as it is" nil) function))

(defun auto-tab-groups-eyecandy-format-new-button ()
  "Return the tab bar button that makes a new tab group.
A `tab-bar-format' can name this function."
  `((add-tab menu-item ,tab-bar-new-button auto-tab-groups-new-group
             :help "New")))

(define-obsolete-function-alias
  'auto-tab-groups-new-group--tab-bar-format-new
  'auto-tab-groups-eyecandy-format-new-button "0.4")

(defun auto-tab-groups-eyecandy--spacer (width)
  "Return a space of WIDTH for the tab bar.
WIDTH is a factor of the normal width of a space, as in the
`space-width' display property.  A terminal draws no part of a cell,
and one `space-width' item in the format leaves the whole row of the
bar unpainted there.  The row then still shows what stood in it
before.  A plain space says the same thing in a terminal."
  (if (display-graphic-p)
      (propertize " " 'display `(space-width ,width))
    " "))

(defun auto-tab-groups-eyecandy--thin-spacer ()
  "Return a thin space for `auto-tab-groups-eyecandy-tab-bar-format'."
  (auto-tab-groups-eyecandy--spacer 0.1))

(defun auto-tab-groups-eyecandy--wide-spacer ()
  "Return a wide space for `auto-tab-groups-eyecandy-tab-bar-format'."
  (auto-tab-groups-eyecandy--spacer 0.75))

(defcustom auto-tab-groups-eyecandy-tab-bar-format
  '(tab-bar-format-tabs-groups
    auto-tab-groups-eyecandy-format-new-button
    tab-bar-format-align-right
    tab-bar-format-global
    auto-tab-groups-eyecandy--thin-spacer
    tab-bar-format-menu-bar
    auto-tab-groups-eyecandy--wide-spacer)
  "List of tab bar items.  See `tab-bar-format' for datails."
  :type 'hook)

(defun auto-tab-groups-eyecandy--tab-bar-format ()
  "Return `auto-tab-groups-eyecandy-tab-bar-format' for this display.
A terminal paints the row of the bar in the redisplay that turns the
bar on.  With the menu button in the format, a tab that changes before
that redisplay leaves the row blank for the rest of the session, and
neither `force-mode-line-update' nor `redraw-display' brings it back.
Measured with Emacs 30.2, in tmux 3.7 and under pyte.  A terminal
reaches the menu with \\[menu-bar-open] in any case."
  (if (display-graphic-p)
      auto-tab-groups-eyecandy-tab-bar-format
    (remq 'tab-bar-format-menu-bar auto-tab-groups-eyecandy-tab-bar-format)))

(defvar auto-tab-groups-eyecandy--bars (make-hash-table :test #'equal)
  "The bar images built so far, keyed on height, width and colour.
Nothing invalidates them: a theme brings another colour, which is
another key.")

(defun auto-tab-groups-eyecandy--get-bar-image (height width color)
  "Generate a rectangular bar image with HEIGHT, WIDTH, and COLOR.

Thanks to doom-modeline for the idea:
https://github.com/seagle0128/doom-modeline/blob/ec6bc00ac035e75ad10b52e516ea5d95cc9e0bd9/doom-modeline-core.el#L1454C8-L1454C39"
  (if (and (image-type-available-p 'pbm) (display-graphic-p))
      ;; Two of these exist in a session, one per width, and building one
      ;; means a fresh payload string and a spec the image cache then
      ;; compares whole: 14 microseconds a group, every redraw.
      (with-memoization (gethash (list height width color)
                                 auto-tab-groups-eyecandy--bars)
        (propertize
         " " 'display
         (create-image
          (concat (format "P1\n%i %i\n" width height) (make-string (* width height) ?1) "\n")
          'pbm t :foreground color :ascent 'center)))
    ;; A face attribute of nil is not "leave it alone", it is an error
    ;; that the display logs on each redisplay.  A separator without a
    ;; color wears no face and takes the one of the line it sits in.
    (propertize "|" 'face (and color (list :foreground color
                                           :background color)))))

(defun auto-tab-groups-eyecandy--drawable-p (string)
  "Return non-nil if this frame will draw the first character of STRING.

The question is asked of STRING, so the face STRING carries is part of
it.  That is the whole point: a nerd-icons glyph names
`nerd-icons-font-family\=' of its own, and asking instead whether the
buffer\='s default font has the character answers for a font that draws
nothing here.  Measured in a buffer where `mixed-pitch-mode\=' remaps
`default\=' to a proportional family — every Org buffer, for a reader who
turns that on — the same character resolves to that family, so every
nerd glyph reads as missing and every icon falls back to its plain
candidate.

`font-at\=' answers with the font that will really draw the character:
the one the face names, the one a fallback finds for it, or nil where
none will.  A string with no face of its own asks the frame\='s font,
which is what the tab bar draws in."
  (and (stringp string)
       (not (string-empty-p string))
       (if (display-graphic-p)
           (font-at 0 nil string)
         (char-displayable-p (aref string 0)))))

(defun auto-tab-groups-eyecandy--glyph (&rest candidates)
  "Return the first of CANDIDATES this frame can draw.
The last one is the answer where it can draw none of them, so keep a
plain character there.  nerd-icons answers with a glyph whether or not
the frame has the font, and a nerd font glyph without the font is a hex
box."
  (or (seq-find #'auto-tab-groups-eyecandy--drawable-p (butlast candidates))
      (car (last candidates))))

(defun auto-tab-groups-eyecandy--nerd-icon (icon-spec)
  "Return the nerd icon glyph for ICON-SPEC.

Inspired from nerd-icons-corfu: https://github.com/LuigiPiucco/nerd-icons-corfu/blob/721830b42b35e326a88b338fc53e4752f333fad2/nerd-icons-corfu.el#L113"
  (let* ((style (plist-get icon-spec :style))
         (icon (plist-get icon-spec :icon))
         (icon-fun (intern (concat "nerd-icons-" style "icon")))
         (icon-name (if (equal style "suc")
                        (concat "nf-" icon)
                      (concat "nf-"  style "-" icon))))
    (auto-tab-groups-eyecandy--glyph (and (fboundp icon-fun)
                                          (funcall icon-fun icon-name))
                                     "?")))

(defun auto-tab-groups-eyecandy--icon (icon-spec)
  "Return ICON-SPEC as the string that shows in the tab bar.
A string stands for itself; a plist names a nerd icon."
  (if (listp icon-spec)
      (auto-tab-groups-eyecandy--nerd-icon icon-spec)
    icon-spec))

(defun auto-tab-groups-eyecandy--get-group-icon (tab-group-name)
  "Retrieve the icon for the given TAB-GROUP-NAME.
The answer is kept.  The tab bar is built again on every command, and
finding an icon walks the table of its nerd-icons style — 6880 entries
for the material design one, measured at 26 microseconds, for every
group, every time.

The patterns are matched with the case rules of the reader who wrote
them, not with those of whatever buffer redisplay happens to be in:
`string-match-p' honours a buffer-local `case-fold-search', so \"HOME\"
claimed \"[P] homelab\" in most buffers and not in others."
  (let ((key (cons tab-group-name (display-graphic-p))))
    (with-memoization (gethash key auto-tab-groups-eyecandy--icons)
      (auto-tab-groups-eyecandy--icon
       (or (cdr (seq-find (lambda (data)
                            (let ((case-fold-search nil))
                              (string-match-p (car data) tab-group-name)))
                          auto-tab-groups-eyecandy-icons))
           auto-tab-groups-eyecandy-default-icon)))))

(defun auto-tab-groups-eyecandy--tab-bar-tab-group-format-function
    (tab _index &optional current-p)
  "Format the group name of TAB for the tab bar.
CURRENT-P is non-nil when TAB is the selected one."
  (let* ((tab-group-name (funcall tab-bar-tab-group-function tab))
         (tab-group-face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive))
         ;; With the default face at the end of the chain: a terminal
         ;; theme often leaves `mode-line-emphasis' without a
         ;; foreground of its own, and nil is not a color.
         (color (face-foreground (if current-p 'mode-line-emphasis 'shadow)
                                 nil 'default))
         (group-sep (auto-tab-groups-eyecandy--get-bar-image auto-tab-groups-eyecandy-tab-height (if current-p 4 2) color))
         (group-icon (auto-tab-groups-eyecandy--get-group-icon tab-group-name))
         (tab-group-name-formatted
          (if (functionp auto-tab-groups-eyecandy-group-name-function)
              (funcall auto-tab-groups-eyecandy-group-name-function
                       tab-group-name)
            tab-group-name)))
    (concat group-sep (propertize (concat " " group-icon " " tab-group-name-formatted " ") 'face tab-group-face))))

(defun auto-tab-groups-eyecandy--tab-bar-tab-name-format-function (tab i)
  "Format the tab name for TAB-BAR.
TAB is the tab object and I is the tab index."
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (concat (if current-p
                 (auto-tab-groups-eyecandy--glyph "󰅂 " " " "› ")
               " ")
             (if tab-bar-tab-hints (format "%d " i) "")
             (alist-get 'name tab)
             ;; `tab-bar-close-button-show' is four-valued, and reading
             ;; it as a boolean showed the button on the selected tab for
             ;; `non-selected', which is the other way round.
             (if (memq tab-bar-close-button-show
                       (if current-p '(t selected) '(t non-selected)))
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
  (add-hook 'after-setting-font-hook #'auto-tab-groups-eyecandy--forget)
  (setq tab-bar-new-button (icon-string 'auto-tab-groups-eyecandy--tab-bar-new)
        tab-bar-close-button (propertize (icon-string 'auto-tab-groups-eyecandy--tab-bar-close)
                                         'close-tab t))
  (setq tab-bar-format (auto-tab-groups-eyecandy--tab-bar-format)
        tab-bar-separator ""
        tab-bar-auto-width nil
        tab-bar-tab-group-format-function #'auto-tab-groups-eyecandy--tab-bar-tab-group-format-function
        tab-bar-tab-name-format-function #'auto-tab-groups-eyecandy--tab-bar-tab-name-format-function))

(defun auto-tab-groups-eyecandy--teardown ()
  "Give the tab bar its stock look back."
  (remove-hook 'after-setting-font-hook #'auto-tab-groups-eyecandy--forget)
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
