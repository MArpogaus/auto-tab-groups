;; -*- lexical-binding: t; -*-
(dolist (p '("auto-tab-groups"))
  (add-to-list 'load-path (concat "/home/marcel/.emacs.d/packages/" p)))
(require 'auto-tab-groups)
(require 'modern-tab-bar)
(require 'auto-tab-groups-project)
(require 'project)
(setq inhibit-startup-screen t ring-bell-function #'ignore)
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(set-frame-font "Source Code Pro 13" nil t)
(blink-cursor-mode -1)
(setq-default cursor-type 'bar)

(defun demo--icon (&rest candidates)
  (or (seq-find (lambda (c) (internal-char-font nil (aref c 0))) candidates)
      (car (last candidates))))

(defun demo--config ()
  (setq modern-tab-bar-indicator-height 26
        modern-tab-bar-icons
        `(("HOME" . ,(demo--icon "⌂" "λ"))
          ("^\\[P\\] *" . ,(demo--icon "▸" ">"))
          ("customize" . ,(demo--icon "☰" "=")))
        modern-tab-bar-default-icon (demo--icon "●" "*")
        modern-tab-bar-group-name-function
        (lambda (n) (if (string-match "^\\[.\\] *" n)
                        (substring n (match-end 0))
                      n))
        auto-tab-groups-create-commands
        '(((customize-group) . "customize"))
        auto-tab-groups-close-commands
        '((Custom-buffer-done "customize" :ignore-result t))
        project-switch-commands #'project-find-file)
  (auto-tab-groups-project-mode 1)
  (modern-tab-bar-mode 1)
  (auto-tab-groups-mode 1)
  (tab-bar-mode 1)
  (dolist (d '("/home/marcel/.emacs.d/packages/pycell/"
               "/home/marcel/.emacs.d/packages/auto-side-windows/"))
    (project-remember-project (project-current nil d))))

(defvar demo--frame 0)
(defun demo--snap ()
  (cl-incf demo--frame)
  (let ((coding-system-for-write 'binary))
    (write-region (x-export-frames nil 'png) nil
                  (format "/tmp/demo-atg/frames/f%04d.png" demo--frame)
                  nil 'quiet)))
(defun demo--hold (seconds)
  (dotimes (_ (round (* 10 seconds)))
    (redisplay t)
    (demo--snap)
    (sit-for 0.02)))
(defvar demo--answer-list nil)
(defun demo--with-answers (answers thunk)
  "Answer minibuffer prompts from ANSWERS while THUNK runs."
  (setq demo--answer-list answers)
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) (pop demo--answer-list))))
    (funcall thunk)))

(defun demo ()
  (demo--config)
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
  (make-directory "/tmp/demo-atg/frames" t)
  (demo--hold 3.0)
  ;; 1. switch to a project: a group appears
  (demo--with-answers '("/home/marcel/.emacs.d/packages/pycell/" "pycell.el")
                      (lambda () (call-interactively #'project-switch-project)))
  (message nil)
  (demo--hold 4.0)
  ;; 2. a second project: another group
  (demo--with-answers '("/home/marcel/.emacs.d/packages/auto-side-windows/"
                        "auto-side-windows.el")
                      (lambda () (call-interactively #'project-switch-project)))
  (message nil)
  (demo--hold 4.0)
  ;; 3. groups switch like tabs
  (tab-bar-switch-to-tab "*scratch*")
  (message nil)
  (demo--hold 3.0)
  ;; 4. a command with a group of its own
  (demo--with-answers '("tab-bar")
                      (lambda () (call-interactively #'customize-group)))
  (message nil)
  (demo--hold 4.0)
  ;; 5. finishing the work closes the group
  (call-interactively #'Custom-buffer-done)
  (message nil)
  (demo--hold 3.0)
  ;; 6. killing the project buffers closes its group
  (tab-bar-switch-to-tab "pycell.el")
  (demo--hold 2.0)
  (let ((default-directory "/home/marcel/.emacs.d/packages/pycell/"))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (call-interactively #'project-kill-buffers)))
  (message nil)
  (demo--hold 3.5)
  (write-region (format "frames=%d tabs=%S\n" demo--frame
                        (mapcar (lambda (tab) (alist-get 'group tab))
                                (funcall tab-bar-tabs-function)))
                nil "/tmp/demo-atg/done")
  (kill-emacs 0))
(run-with-timer 1.0 nil
                (lambda ()
                  (set-frame-size (selected-frame) 1120 640 t)
                  (condition-case err (demo)
                    (error (write-region (format "ERROR %S" err) nil
                                         "/tmp/demo-atg/failed")
                           (kill-emacs 1)))))
