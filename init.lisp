(require :bt-semaphore)
(require :external-program)
(require :swank)
(require :cl-ppcre)
(require :local-time)
(local-time:reread-timezone-repository)
(in-package :stumpwm)
(defvar *inner* (sb-ext:posix-getenv "STUMPWM_INNER"))
(defvar *swank-running* nil)
(defvar *swank-port* 4005)
(defvar *group-names* '("1" "2" "3" "4" "5" "6" "7"))
(defvar *message-semaphore* (bt:make-semaphore :count 1))
(defvar *winner-map* (make-sparse-keymap))
(defvar *desktop-history* '())
(set-font "-*-clean-*-*-*-*-12-*-*-*-*-*-*-*")

(set-bg-color "#121212")
(set-fg-color "#ffffea")
(if *inner*
    (set-bg-color "#8a1212"))
(set-border-color "#fffAfA")
(set-unfocus-color "#B8B8B8")
(set-focus-color "#222444")
(setq *colors*
      '("#282828" "#AE1818"
        "#18A038" "#EAEA14"
        "#1818AE" "#EAAEE9"
        "#58AE0E" "#EAEAEA"))
(setf *mode-line-background-color* (format nil "#~x" (screen-bg-color (current-screen))))
(setf *mode-line-border-color*     (format nil "#~x" (screen-border-color (current-screen))))
(setf *mode-line-border-width* 1)
(setf *mode-line-foreground-color* (format nil "#~x" (screen-fg-color (current-screen))))
(setf *mode-line-position* :top)
(setf *mode-line-timeout* 0.1)

(set-prefix-key (if *inner* (kbd "C-y") (kbd "C-t")))
(set-module-dir
 (pathname-as-directory (concat (getenv "HOME") "/.stumpwm.d/modules")))
(set-transient-gravity :left)
(setf
 *float-window-border*          1
 *float-window-title-height*    1
 *hidden-window-color*          "^(:fg \"#887888\")"
 *input-window-gravity*         :left
 *maxsize-border-width*         1
 *message-window-gravity*       :left
 *message-window-padding*       1
 *mode-line-highlight-template* "^(:bg \"#9A3232\")^(:fg \"#887888\")~A^n"
 *mode-line-pad-x*              20
 *mode-line-pad-y*              1
 *new-window-preferred-frame*   '(:empty :unfocused :last)
 *normal-border-width*          1
 *top-level-error-action*       :abort
 *transient-border-width*       1
 *window-border-style*          :tight
 )
(setf *group-format*  "^B%n°^b %s %q (%20r)")
(setf *mouse-focus-policy* :sloppy)
(setf *banish-pointer-to* :frame)
(setf *screen-mode-line-format* (list "^B%n°^b %v " " ^> " '(:eval (root-name))))
(setf *time-format-string-default* "%a %b %e %Y %l:%M %P")
(setf *window-format* "%n^(:fg \"#9a9a9a\")%s%m^n %c - %10t")

(run-shell-command "wmname LG3D")
(run-shell-command "pkill dst ; dst")

(define-key *groups-map* (kbd "/") "grouplist")
(define-key *groups-map* (kbd "w") "groups")
(define-key *root-map* (kbd "a") "report")
(define-key *root-map* (kbd "b") "report")
(define-key *root-map* (kbd "C-c") "exec emacsclient -c -e '(shell)'")
(define-key *root-map* (kbd "C-g") "only")
(define-key *root-map* (kbd "C-m") "toggle-mouse")
(define-key *root-map* (kbd ":") "colon")
(define-key *root-map* (kbd "C-o") "other")
(define-key *root-map* (kbd "C-/") "pull-from-everywhere")
(define-key *root-map* (kbd "C-r") "remove")
(define-key *root-map* (kbd "C-s") "hsplit")
(define-key *root-map* (kbd "C-SPC") "rotate-windows")
(define-key *root-map* (kbd "C-t") "send-raw-key")
(define-key *root-map* (kbd "C-v") "vsplit")
(define-key *root-map* (kbd "c") '*winner-map*)
(define-key *root-map* (kbd "d") "dump")
(define-key *root-map* (kbd "e")   "exec emacsclient -c")
(define-key *root-map* (kbd "[") "exchange-direction left")
(define-key *root-map* (kbd "]") "exchange-direction right")
(define-key *root-map* (kbd "m") "mark")
(define-key *root-map* (kbd "M") "pull-marked")
(define-key *root-map* (kbd "!") "shell-exec")
(define-key *root-map* (kbd "u") "restore")
(define-key *root-map* (kbd "/") "windowlist-everywhere")
(define-key *root-map* (kbd "*") "flatten-floats")
(define-key *root-map* (kbd "=") "balance-frame")
(define-key *top-map* (kbd "C-M-Delete") "lock")
(define-key *top-map* (kbd "C-M-Left") "gprev")
(define-key *top-map* (kbd "C-M-Right") "gnext")
(define-key *top-map* (kbd "C-M-S-Left") "gprev-with-window")
(define-key *top-map* (kbd "C-M-S-Right") "gnext-with-window")
(define-key *top-map* (kbd "M-ISO_Left_Tab") "prev")
(define-key *top-map* (kbd "M-SPC") "shell-exec")
(define-key *top-map* (kbd "M-TAB") "fnext")
(define-key *top-map* (kbd "M-/") "windowlist-everywhere")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "lower-volume")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "raise-volume")
(define-key *winner-map* (kbd "c") "dump-desktop")
(define-key *winner-map* (kbd "Left") "winner-undo")
(define-key *winner-map* (kbd "Right") "winner-redo")
(define-key *winner-map* (kbd "r") "restore-from-file")

(defun root-name ()
  (let* ((screen (current-screen))
         (selwin (screen-focus-window (current-screen)))
         (root (screen-root screen)))
    (utf8-to-string
     (xlib:get-property root
                        :wm_name
                        :result-type '(vector (unsigned-byte 8))))))

(defun window-pid (window)
  (car (window-property window '_NET_WM_PID)))
(let ((groups (sort-groups (current-screen))))
  (dotimes (n (length *group-names*))
    (if-let ((group (nth n groups)) (name (nth n *group-names*)))
      (%grename name group)
      (gnewbg name))))

(defcommand swank-off () ()
  "Turn off swank."
  (if *swank-running*
      (progn
        (swank:stop-server *swank-port*)
        (echo-string
         (current-screen)
         "Stopping swank.")
        (setf *swank-running* nil))))

(defcommand swank-on () ()
  "Turn on swank."
  (unless *swank-running*
    (progn
      (swank-loader:init)
      (swank:create-server
       :port *swank-port*
       :style swank:*communication-style*
       :dont-close t)
      (message
       "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm).")
      (setf *swank-running* t))))

(defcommand pull-from-everywhere (&optional (fmt *window-format*)) (:rest)
  ""
  (let ((pulled-window (select-window-from-menu
                        (all-windows)
                        fmt)))
    (when pulled-window
      (move-window-to-group pulled-window (current-group))
      (pull-window pulled-window))))

(defcommand windowlist-everywhere (&optional (fmt *window-format*)) ()
  "All windows."
  (if-let ((window-list (all-windows)))
    (if-let ((window (select-window-from-menu window-list fmt)))
      (let ((group (window-group window)))
        (progn
          (switch-to-group group)
          (group-focus-window group window)))
      (throw 'error :abort))
    (message "No Managed Windows")))

(defcommand swank () ()
  "Toggle the swank server on/off. "
  (if *swank-running* (swank-off) (swank-on)))

(defun clock (&key (where "America/New_York"))
  (let (
        (now (local-time:now))
        (timezone (local-time:find-timezone-by-location-name where))
        )
    (local-time:format-timestring
     nil now
     :timezone timezone
     :format local-time:+rfc-1123-format+
     )
    )
  )
(defcommand report () ()
  (message
   (format nil "~{~A~^~&~}"
      (list
       (clock :where "America/New_York")
       (clock :where "America/Asuncion")
       (clock :where "Europe/Moscow")))))

(defcommand toggle-modeline () ()
  "Toggle mode-line. "
  (progn
    (toggle-mode-line (current-screen) (current-head))
    (redisplay)))

(defcommand lock () ()
  (stumpwm:run-shell-command "physlock"))

(defcommand raise-volume () ()
  (stumpwm:run-shell-command "emacsclient -e '(emms-volume-raise)'"))
(defcommand lower-volume () ()
  (stumpwm:run-shell-command "emacsclient -e '(emms-volume-lower)'"))

(defun window-group-name (window)
  "Group name of a window."
  (group-name (window-group window)))
(defun group-size (group)
  "Amount of windows in group."
  (length  (group-windows group)))
(defun group-resume (group)
  "Amount of windows in group."
  (format nil "~{~A~^, ~}" (mapcar #'window-class (group-windows group))))

(push '(#\q window-group-name) *window-formatters*)
(push '(#\P window-pid) *window-formatters*)
(push '(#\q group-size) *group-formatters*)
(push '(#\r group-resume) *group-formatters*)

(defcommand toggle-mouse () ()
  (banish)
  (run-shell-command "~/.scripts/toggle-mouse"))

(defcommand dump () ()
  "Dump current desktop."
  (message (format nil "Dumped ~a" (length *desktop-history*)))
  (let ((current (dump-desktop))
        (last (first *desktop-history*)))
    (if (not (equalp current last))
        (push (dump-desktop) *desktop-history*))))

(defcommand restore () ()
  "Dump current desktop."
  (progn
    (message (format nil "Restored ~a" (length *desktop-history*)))
    (restore-desktop (pop *desktop-history*))))

(defvar *default-commands*
  '(stumpwm:only
    stumpwm:pull-from-windowlist
    stumpwm:pull-hidden-next
    stumpwm:pull-hidden-other
    stumpwm:pull-hidden-previous
    stumpwm:pull-marked
    stumpwm:pull-window-by-number
    stumpwm:next
    stumpwm:next-in-frame
    stumpwm:next-urgent
    stumpwm:prev
    stumpwm:prev-in-frame
    stumpwm:select-window
    stumpwm:select-from-menu
    stumpwm:select-window-by-name
    stumpwm:select-window-by-number
    stumpwm::pull
    stumpwm::remove
    stumpwm:iresize
    stumpwm:vsplit
    stumpwm:hsplit
    stumpwm:move-window
    stumpwm:move-windows-to-group
    stumpwm:move-window-to-group
    stumpwm::delete
    stumpwm::kill
    stumpwm:fullscreen))



(defun shift-windows-forward (frames window)
  (when frames
    (let ((frame (car frames)))
      (shift-windows-forward (cdr frames)
                             (frame-window frame))
      (when window
        (pull-window window frame)))))

(defcommand rotate-windows () ()
  "Rotate windows"
  (let* ((frames (group-frames (current-group)))
         (window (frame-window (car (last frames)))))
    (shift-windows-forward frames window)))


(defun shell-command (command)
  "Run a shell command and display output to screen.
    This must be used in a functional side-effects-free style! If a program does not
    exit of its own accord, Stumpwm might hang!"
  (echo-string (current-screen) (run-shell-command command t)))

(define-stumpwm-type :shell (input prompt)
  (let ((*input-history* *input-shell-history*))
    (unwind-protect
         (or (argument-pop-rest input)
             (completing-read (current-screen)
                              (if (symbolp prompt) (funcall prompt) prompt)
                              'complete-program))
      (setf *input-shell-history* *input-history*))))

(defun ai/prompt (&rest args)
  "Prompt for shell commands."
  (format nil "~a~&* " (run-shell-command "xclip -o | head"  t)))

(defcommand shell-exec (command) ((:shell ai/prompt))
  (if (and command (not (equal command "")))
      (let* ((cmd (cl-ppcre:regex-replace-all "{}" command "`xclip -o`"))
             (proc (external-program:start "/bin/sh" (list "-c" cmd) :output :stream))
             (pid (external-program:process-id proc)))
        (message (format nil "`~a` started with pid ~a" cmd (external-program:process-id proc)))
        (bt:make-thread
         (lambda ()
           (let ((output (external-program:process-output-stream proc)))
             (with-open-stream (stdout output)
               (let ((lines
                       (loop
                         repeat 50
                         for line = (read-line stdout nil nil)
                         while line
                         collect line
                         finally (close stdout))))
                 (message
                  (format nil
                          "`~a` (~a) finished with ~a~&~{|~A~^~&~}"
                          cmd
                          pid
                          (sb-ext:process-exit-code (sb-ext:process-wait proc))
                          lines
                          ))
                 ))))))))

(defun focus-group-report (current last)
  (bt:make-thread
   (lambda ()
     (sleep 0.3)
     (echo-windows))))

(defun message-only (&rest xs)
  (bt:wait-on-semaphore *message-semaphore*)
  (sleep .1)
  (apply 'message xs)
  (sleep 1)
  (bt:signal-semaphore *message-semaphore*))

(defun message-async (&rest xs)
  (bt:make-thread
   (lambda ()
     (apply 'message-only xs))))

(defun fmt-window-status (window)
  (let ((group (window-group window)))
    (cond ((eq window (group-current-window group))
           #\+)
          ((and (typep (second (group-windows group)) 'window)
                (eq window (second (group-windows group))))
           #\-)
          (t #\ ))))

(defun fmt-window-marked (window)
  (if (window-marked window)
      #\~
      #\Space))

(defun root-click-handle (screen button x y)
  (message "root"))

(defun new-window-handle (win)
  (if (equal (window-class win) "mpv")
      (let ((screen (window-screen win))
            (group (window-group win)))
        (float-window win group))))

(define-remapped-keys
    '(("(Firefox|Chrome)" ("C-a"   . "C-t"))
      ("(Wfica)"          ("M-ESC" . "M-TAB"))))

(clear-window-placement-rules)
(define-frame-preference "2"
  (0 t t :class "Chromium")
  (1 t t :class "Wfica"))

(define-frame-preference "1"
  (0 t t :class "Emacs")
  (1 t t :class "Firefox" :create t))

(defun move-pointer (curr prev)
  (banish-pointer))

(add-hook *focus-window-hook* 'move-pointer)
(add-hook *root-click-hook* 'root-click-handle)
(add-hook *new-window-hook* 'new-window-handle)
(add-hook *post-command-hook*
          (lambda (command)
            (when (member command *default-commands*)
              (dump))))
(add-hook *focus-group-hook* 'focus-group-report)

(turn-on-mode-line-timer)
(toggle-modeline)
(defun run-elisp (code &key (frame t))
  (shell-exec (format nil "emacsclient ~A -e '~a'" (if frame "-c" "") code)))

(defcommand elisp (command) ((:shell ai/prompt))
  (run-elisp command))

(defcommand elisp-no-frame (command) ((:shell ai/prompt))
  (run-elisp command :frame nil))

(defvar *execute-map* (make-sparse-keymap))
(define-key *root-map*    (kbd "e") '*execute-map*)
(define-key *execute-map* (kbd "e") "elisp (ibuffer)")
(define-key *execute-map* (kbd "t") "elisp (vterm)")
(define-key *execute-map* (kbd "s") "elisp (shell)")
(define-key *execute-map* (kbd "d") "elisp (dired default-directory)")
(define-key *execute-map* (kbd "f") "exec firefox -P")
(define-key *execute-map* (kbd "c") "exec chromium")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "raise-volume")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "lower-volume")
(defcommand raise-volume () ()
  (elisp-no-frame "(emms-volume-raise)"))
(defcommand lower-volume () ()
  (elisp-no-frame "(emms-volume-lower)"))
