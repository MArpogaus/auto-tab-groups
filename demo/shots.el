;; -*- lexical-binding: t; -*-
;; Draws the picture of the tab bar for the README; see README.org in
;; this directory.  It runs the configuration example of the README,
;; so the picture shows what that example gives you.
;;
;; Needs nerd-icons on the load path and a nerd font on the system.
(add-to-list 'load-path (expand-file-name ".." (file-name-directory
                                                load-file-name)))
(require 'nerd-icons)
(require 'auto-tab-groups)
(require 'auto-tab-groups-eyecandy)
(setq inhibit-startup-screen t ring-bell-function #'ignore)
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(blink-cursor-mode -1)
(let ((font (seq-find (lambda (name) (find-font (font-spec :name name)))
                      '("FiraCode Nerd Font" "Symbols Nerd Font"
                        "Source Code Pro" "DejaVu Sans Mono"))))
  (when font (set-frame-font (format "%s 13" font) nil t)))

(defconst shots-directory
  (expand-file-name "../img" (file-name-directory load-file-name))
  "Where the pictures land.")

(defconst shots-height-file "/tmp/auto-tab-groups-shot.txt"
  "Where the height of the tab bar lands, for the crop.")

(defconst shots-width 815
  "The width of the picture, which is the width of the recording.")

;;; The configuration example of the README, as it stands there.

(setq auto-tab-groups-eyecandy-tab-height 25)

(setq auto-tab-groups-eyecandy-icons
      '(("HOME"       . (:style "suc" :icon "custom-emacs"))
        ("dirvish"    . (:style "suc" :icon "custom-folder_oct"))
        ("denote"     . (:style "md"  :icon "notebook_edit"))
        ("customize"  . (:style "cod" :icon "settings"))
        ("^\\[P\\] *" . (:style "oct" :icon "repo"))
        ("^\\[T\\] *" . (:style "cod" :icon "remote"))))

(setq auto-tab-groups-eyecandy-group-name-function
      (lambda (tab-group-name)
        (if (string-match "^\\[.\\] *" tab-group-name)
            (substring tab-group-name (match-end 0))
          tab-group-name)))

(defvar shots-groups
  '(("HOME" . "*scratch*")
    ("[P] auto-tab-groups" . "eyecandy.el")
    ("dirvish" . "~/notes")
    ("denote" . "a-note.org"))
  "The groups in the picture, and the tab in each of them.")

(defun shots--tabs ()
  "Give the frame one tab for each group in `shots-groups'."
  (tab-bar-mode 1)
  ;; The first group is the one the frame starts in.  Each of the
  ;; others gets a tab of its own, after it.
  (tab-bar-rename-tab (cdar shots-groups))
  (tab-bar-change-tab-group (caar shots-groups))
  (pcase-dolist (`(,group . ,tab) (cdr shots-groups))
    (tab-bar-new-tab)
    (tab-bar-rename-tab tab)
    (tab-bar-change-tab-group group))
  ;; and the picture is taken in the first one
  (tab-bar-select-tab 1))

(defun shots--write (name)
  "Export the frame as NAME, and note how tall its tab bar is."
  (force-mode-line-update t)
  (redisplay t)
  (write-region (format "%s %d\n" name (tab-bar-height nil t))
                nil shots-height-file t 'quiet)
  (let ((coding-system-for-write 'binary))
    (write-region (x-export-frames nil 'png) nil
                  (expand-file-name name shots-directory) nil 'quiet)))

(defun shots--run ()
  (set-frame-size (selected-frame) shots-width 140 t)
  (write-region "" nil shots-height-file nil 'quiet)
  (shots--tabs)
  ;; The tab bar as Emacs draws it, with the groups in it.
  (setq tab-bar-format '(tab-bar-format-tabs-groups tab-bar-format-add-tab))
  (shots--write "tab-bar-plain.png")
  ;; And the same frame with the eyecandy mode on.
  (load-theme 'modus-operandi t)
  (auto-tab-groups-eyecandy-mode 1)
  (shots--write "tab-bar.png")
  (kill-emacs 0))

(run-with-timer
 1.0 nil
 (lambda ()
   (condition-case err (shots--run)
     (error (write-region (format "shots: %S\n" err) nil
                          "/tmp/auto-tab-groups-shot-error.txt" nil 'quiet)
            (kill-emacs 1)))))
