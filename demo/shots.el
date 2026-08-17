;; -*- lexical-binding: t; -*-
;; Draws the picture of the tab bar for the README; see README.org in
;; this directory.  Needs nerd-icons on the load path and a nerd font
;; on the system.
(add-to-list 'load-path (expand-file-name ".." (file-name-directory
                                                load-file-name)))
(require 'nerd-icons)
(require 'auto-tab-groups)
(require 'auto-tab-groups-eyecandy)
(setq inhibit-startup-screen t ring-bell-function #'ignore)
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(blink-cursor-mode -1)
(let ((font (seq-find (lambda (name) (find-font (font-spec :name name)))
                      '("Source Code Pro" "DejaVu Sans Mono"
                        "Noto Sans Mono" "Liberation Mono"))))
  (when font (set-frame-font (format "%s 13" font) nil t)))

(defconst shots-file
  (expand-file-name "../img/tab-bar.png" (file-name-directory load-file-name))
  "Where the picture lands.")

(defconst shots-height-file "/tmp/auto-tab-groups-shot.txt"
  "Where the height of the tab bar lands, for the crop.")

(defvar shots-groups
  '(("HOME"            . (:style "suc" :icon "custom-emacs"))
    ("auto-tab-groups" . (:style "oct" :icon "repo"))
    ("dirvish"         . (:style "suc" :icon "custom-folder_oct"))
    ("denote"          . (:style "cod" :icon "notebook"))
    ("ai-cli"          . (:style "cod" :icon "terminal"))
    ("my group"        . nil))
  "The groups in the picture, and the icon of each.")

(defun shots--run ()
  (set-frame-size (selected-frame) 1040 120 t)
  (setq auto-tab-groups-eyecandy-icons
        (seq-filter #'cdr shots-groups))
  (tab-bar-mode 1)
  (auto-tab-groups-eyecandy-mode 1)
  ;; The first group is the one the frame starts in.  Each of the
  ;; others gets a tab of its own, after it.
  (tab-bar-rename-tab "*HOME-scratch*")
  (tab-bar-change-tab-group (caar shots-groups))
  (dolist (group (mapcar #'car (cdr shots-groups)))
    (tab-bar-new-tab)
    (tab-bar-rename-tab group)
    (tab-bar-change-tab-group group))
  ;; and the picture is taken in the first one
  (tab-bar-select-tab 1)
  (force-mode-line-update t)
  (redisplay t)
  (write-region (format "%d\n" (tab-bar-height nil t)) nil
                shots-height-file nil 'quiet)
  (let ((coding-system-for-write 'binary))
    (write-region (x-export-frames nil 'png) nil shots-file nil 'quiet))
  (kill-emacs 0))

(run-with-timer
 1.0 nil
 (lambda ()
   (condition-case err (shots--run)
     (error (write-region (format "shots: %S\n" err) nil
                          "/tmp/auto-tab-groups-shot-error.txt" nil 'quiet)
            (kill-emacs 1)))))
