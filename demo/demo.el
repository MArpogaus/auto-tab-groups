;;; demo.el --- records img/demo.gif  -*- lexical-binding: t; -*-

;; The animation is taken with the minimal configuration of the README
;; -- a group per project, and the mode -- plus one rule for customize
;; buffers, and modern-tab-bar for the look of the bar, as the README
;; shows it.  The theme is modus-operandi.  What follows the
;; configuration is presentation: a frame of a fixed size, a font, and
;; the scripted session.
;;
;;     Xvfb :99 -screen 0 1280x900x24 &
;;     DISPLAY=:99 emacs -Q -L path/to/nerd-icons -l demo/demo.el
;;
;; The frames land in demo/frames/; demo/README.org says how they become
;; the GIF.

;;; Code:
(dolist (p '("auto-tab-groups" "modern-tabs"))
  (add-to-list 'load-path (concat "/home/marcel/.emacs.d/packages/" p)))
(require 'auto-tab-groups)
(require 'auto-tab-groups-project)
(require 'modern-tab-bar)
;; The icons of the groups: give -Q the directory of nerd-icons with -L.
(require 'nerd-icons)

;;;; The configuration of the README
(setopt auto-tab-groups-create-commands '((customize-group . "customize"))
        auto-tab-groups-close-commands '((Custom-buffer-done "customize" :always-close t)))
(auto-tab-groups-project-mode)
(auto-tab-groups-mode)
;; the look of the bar
(setopt modern-tab-bar-icons '(("HOME"       . (:style "suc" :icon "custom-emacs"))
                               ("dirvish"    . (:style "suc" :icon "custom-folder_oct"))
                               ("denote"     . (:style "md"  :icon "notebook_edit"))
                               ("customize"  . (:style "cod" :icon "settings"))
                               ("^\\[P\\] *" . (:style "oct" :icon "repo"))
                               ("^\\[T\\] *" . (:style "cod" :icon "remote")))
        modern-tab-bar-group-name-function
        (lambda (group-name)
          (if (string-match "^\\[.\\] *" group-name)
              (substring group-name (match-end 0))
            group-name))
        modern-tab-bar-new-command #'auto-tab-groups-new-group)
(modern-tab-bar-mode)
(tab-bar-mode)

;;;; Presentation
(setq inhibit-startup-screen t ring-bell-function #'ignore)
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq-default cursor-type 'bar)
(load-theme 'modus-operandi t)
(let ((font (seq-find (lambda (name) (find-font (font-spec :name name)))
                      '("Source Code Pro" "FiraCode Nerd Font"
                        "DejaVu Sans Mono" "Liberation Mono"))))
  (when font (set-frame-font (format "%s 13" font) nil t)))
;; `project-switch-project' asks which command to run in the project.
(setq project-switch-commands #'project-find-file)

;;;; The session
(defconst demo--dir (expand-file-name "frames/" (file-name-directory
                                                 (or load-file-name buffer-file-name))))
(defconst demo--projects '("/home/marcel/.emacs.d/packages/auto-side-windows/"
                           "/home/marcel/.emacs.d/packages/modern-tabs/"))
(defvar demo--frame 0)
(defun demo--snap ()
  "Capture one frame.  Every frame is 0.1 s of the animation."
  (cl-incf demo--frame)
  (let ((coding-system-for-write 'binary))
    (write-region (x-export-frames nil 'png) nil
                  (format "%sf%04d.png" demo--dir demo--frame) nil 'quiet)))
(defun demo--hold (seconds)
  "Show the current state for SECONDS."
  (dotimes (_ (round (* 10 seconds)))
    (redisplay t)
    (demo--snap)
    (sit-for 0.02)))
(defun demo--say (text seconds)
  "Put TEXT in the echo area and hold the frame for SECONDS."
  (let ((message-log-max nil))
    (message "%s" text)
    (demo--hold seconds)
    (message nil)))
(defvar demo--answers nil)
(defun demo--answering (answers thunk)
  "Answer minibuffer prompts from ANSWERS while THUNK runs."
  (setq demo--answers answers)
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) (pop demo--answers))))
    (funcall thunk)))

(defun demo ()
  (dolist (dir demo--projects)
    (project-remember-project (project-current nil dir)))
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (erase-buffer)
  (insert ";; auto-tab-groups\n"
          ";;\n"
          ";; Tabs come and go with what you work on: every project\n"
          ";; gets its own tab group, commands can open groups of\n"
          ";; their own, and closing the work closes the group.\n")
  (goto-char (point-min))
  (redisplay t)
  (make-directory demo--dir t)
  (demo--hold 3.0)
  ;; 1. switch to a project: a group appears
  (demo--say "C-x p p  project-switch-project" 1.5)
  (demo--answering (list (car demo--projects) "auto-side-windows.el")
                   (lambda () (call-interactively #'project-switch-project)))
  (message nil)
  (demo--hold 4.0)
  ;; 2. a second project: another group
  (demo--answering (list (cadr demo--projects) "modern-tab.el")
                   (lambda () (call-interactively #'project-switch-project)))
  (message nil)
  (demo--hold 4.0)
  ;; 3. groups switch like tabs
  (demo--say "groups switch like tabs" 1.5)
  (tab-bar-switch-to-tab "*scratch*")
  (message nil)
  (demo--hold 3.0)
  ;; 4. a command with a group of its own
  (demo--say "M-x customize-group  has a group of its own" 1.5)
  (demo--answering '("tab-bar")
                   (lambda () (call-interactively #'customize-group)))
  (message nil)
  (demo--hold 4.0)
  ;; 5. finishing the work closes the group
  (demo--say "finishing the work closes the group" 1.5)
  (call-interactively #'Custom-buffer-done)
  (message nil)
  (demo--hold 3.0)
  ;; 6. killing the project buffers closes its group
  (tab-bar-switch-to-tab "auto-side-windows.el")
  (demo--say "C-x p k  project-kill-buffers" 2.0)
  (let ((default-directory (car demo--projects)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (call-interactively #'project-kill-buffers)))
  (message nil)
  (demo--hold 3.5)
  (write-region (format "frames=%d tabs=%S\n" demo--frame
                        (mapcar (lambda (tab) (alist-get 'group tab))
                                (funcall tab-bar-tabs-function)))
                nil (expand-file-name "done" demo--dir))
  (kill-emacs 0))
(run-with-timer 1.0 nil
                (lambda ()
                  (set-frame-size (selected-frame) 1120 640 t)
                  (condition-case err (demo)
                    (error (write-region (format "ERROR %S" err) nil
                                         (expand-file-name "failed" demo--dir))
                           (kill-emacs 1)))))
;;; demo.el ends here
