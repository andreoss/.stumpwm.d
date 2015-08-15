(require :bt-semaphore)
(require :external-program)
(require :swank)
(in-package :stumpwm)

(defvar *swank-running* nil)
(defvar *swank-port* 4005)
(defvar *group-names* '("1" "2" "3" "4" "5" "6" "7"))

(setq *colors*
      '("#181818"
        "#AE1818"
        "#18AE18"
        "#EAEA14"
        "#1818AE"
        "#EAAEE9"
        "#18AEAE"
        "#EAEAEA"))
(set-font "-*-ttyp0-medium-*-*-*-12-*-*-*-*-*-*-*")
(set-fg-color "#181818")
(set-bg-color "#EAEAEA")
(set-border-color "#8A8A8A")
(set-focus-color "#EAEAEA")
(set-unfocus-color "#181818")
(set-prefix-key (kbd "C-t"))

(run-shell-command "wmname LG3D")
(run-shell-command "pkill dst ; dst")

(setf
 *float-window-border*       1
 *float-window-title-height* 1
 *hidden-window-color*                "^(:fg \"#887888\")"
 *input-window-gravity*               :center
 *top-level-error-action*             :abort:
 *maxsize-border-width*               1
 *message-window-padding*             1
 *mode-line-highlight-template*       "^(:bg \"#9a3232\")^(:fg \"#887888\")~A^n"
 *mode-line-pad-x*                    20
 *mode-line-pad-y*                    1
 *new-window-preferred-frame*         '(:empty :unfocused :last)
 *normal-border-width*                1
 *transient-border-width*             1
 *window-border-style*                :tight
 *message-window-gravity*             :center
 )

(setf *mode-line-border-width* 1)
(setf *mode-line-background-color* (format nil "#~x" (screen-bg-color (current-screen))))
(setf *mode-line-foreground-color* (format nil "#~x" (screen-fg-color (current-screen))))
(setf *mode-line-border-color*     (format nil "#~x" (screen-border-color (current-screen))))
(setf *mode-line-position* :bottom)
(setf *mode-line-timeout* 0.1)
(setf *screen-mode-line-format*
      (list "^B%n°^b %v " " ^> "
            '(:eval (root-name)))
      )

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

(set-module-dir
 (pathname-as-directory (concat (getenv "HOME") "/.stumpwm.d/modules")))

(set-transient-gravity :left)

(defcommand report () ()
  (message
   (format nil "~{~A~^| ~}"
           (list
            (stumpwm:run-shell-command "uptime | sed 's/.*:[ ]*//'" t)
            (stumpwm:run-shell-command "TZ=America/New_York date" t)
            (stumpwm:run-shell-command "TZ=Europe/Moscow date +'                %X %Z'" t)
            (stumpwm:run-shell-command "acpi" t)))))


(defcommand toggle-modeline () ()
  "Toggle mode-line. "
  (progn
    (toggle-mode-line (current-screen) (current-head))
    (redisplay)))

(defcommand lock () ()
  (stumpwm:run-shell-command "physlock"))
(setf *time-format-string-default* "%a %b %e %Y %l:%M%P")
(define-key *top-map* (kbd "C-M-Delete") "lock")
(define-key *top-map* (kbd "M-/") "windowlist-everywhere")
(define-key *top-map* (kbd "M-TAB") "fnext")
(define-key *top-map* (kbd "M-SPC") "shell-exec")
(define-key *top-map* (kbd "M-ISO_Left_Tab") "prev")
(define-key *top-map* (kbd "C-M-Left") "gprev")
(define-key *top-map* (kbd "C-M-Right") "gnext")
(define-key *top-map* (kbd "C-M-S-Left") "gprev-with-window")
(define-key *top-map* (kbd "C-M-S-Right") "gnext-with-window")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "raise-volume")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "lower-volume")
(define-key *top-map* (kbd "C-M-S-Right") "gnext-with-window")

(defcommand raise-volume () ()
  (stumpwm:run-shell-command "emacsclient -e '(emms-volume-raise)'"))
(defcommand lower-volume () ()
  (stumpwm:run-shell-command "emacsclient -e '(emms-volume-lower)'"))

(define-key *root-map* (kbd ":") "colon")
(define-key *root-map* (kbd "[") "exchange-direction left")
(define-key *root-map* (kbd "]") "exchange-direction right")
(define-key *root-map* (kbd "/") "windowlist-everywhere")
(define-key *root-map* (kbd "a") "report")
(define-key *root-map* (kbd "b") "report")
(define-key *root-map* (kbd "C-/") "pull-from-everywhere")
(define-key *root-map* (kbd "C-c") "exec emacsclient -c -e '(shell)'")
(define-key *root-map* (kbd "e")   "exec emacsclient -c")
(define-key *root-map* (kbd "C-o") "other")
(define-key *root-map* (kbd "C-r") "remove")
(define-key *root-map* (kbd "C-s") "hsplit")
(define-key *root-map* (kbd "C-t") "send-raw-key")
(define-key *root-map* (kbd "C-v") "vsplit")
(define-key *root-map* (kbd "m") "mark")
(define-key *root-map* (kbd "M") "pull-marked")

(defun window-group-name (window)
  "Group name of a window."
  (group-name (window-group window)))
(push '(#\q window-group-name) *window-formatters*)
(push '(#\P window-pid) *window-formatters*)
(setf *window-format* "%n^(:fg \"#9a9a9a\")%s%m^n %c - %10t")

(defun group-size (group)
  "Amount of windows in group."
  (length  (group-windows group)))

(defun group-resume (group)
  "Amount of windows in group."
  (format nil "~{~A~^, ~}" (mapcar #'window-class (group-windows group))))

(push '(#\q group-size) *group-formatters*)
(push '(#\r group-resume) *group-formatters*)

(setf *group-format*  "^B%n°^b %s %q (%20r)")
(setf *time-format-string-default* "%a %b %e %Y %l:%M %P")
(defvar *winner-map* (make-sparse-keymap))
(define-key *root-map*   (kbd "c") '*winner-map*)
(define-key *winner-map* (kbd "c") "dump-desktop")
(define-key *winner-map* (kbd "r") "restore-from-file")
(define-key *winner-map* (kbd "Left") "winner-undo")
(define-key *winner-map* (kbd "Right") "winner-redo")

(setf *mouse-focus-policy* :sloppy)

(load-module "urgentwindows")
(add-hook *urgent-window-hook* 'really-raise-window)

(define-remapped-keys
    '(
      ("(Firefox|Chrome)" ("C-a"   . "C-t"))
      ("(Wfica)"          ("M-ESC" . "M-TAB"))
      )
  )

(defvar *desktop-history* '())

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
    ;;stumpwm:balance-frames
    stumpwm::delete
    stumpwm::kill
    stumpwm:fullscreen))

(add-hook *post-command-hook*
          (lambda (command)
            (when (member command *default-commands*)
              (dump))))

(define-key *root-map* (kbd "d") "dump")
(define-key *root-map* (kbd "u") "restore")
(define-key *root-map* (kbd "C-g") "only")

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


(define-key *root-map* (kbd "C-SPC") "rotate-windows")
(define-key *groups-map* (kbd "/") "grouplist")
(define-key *groups-map* (kbd "w") "groups")

(defun shell-command (command) "Run a shell command and display output to screen.
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
  ""
  (format nil "~a~&* " (run-shell-command "xclip -o" t)))

(defcommand shell-exec (command) ((:shell ai/prompt))
  (if (and command (not (equal command "")))
      (let* ((proc (external-program:start "/bin/sh" (list "-c" command) :output :stream))
             (pid (external-program:process-id proc)))
        (message (format nil "`~a` started with pid ~a" command (external-program:process-id proc)))
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
                          command
                          pid
                          (sb-ext:process-exit-code (sb-ext:process-wait proc))
                          lines
                            ))))))))))


(define-key *root-map* (kbd "!") "shell-exec")
(defcommand toggle-mouse () ()
  (banish)
  (run-shell-command "~/.scripts/toggle-mouse"))

(define-key *root-map* (kbd "C-m") "toggle-mouse")

(defun focus-group-report (current last)
  (bt:make-thread
   (lambda ()
     (sleep 0.3)
     (echo-windows))))

(add-hook *focus-group-hook* 'focus-group-report)

(defun focus-window-report (current last)
  (unless (equal current last)
    (let ((frame (window-frame current)))
      (warp-pointer (group-screen (window-group current))
                    (+ (frame-x frame) (/ (frame-width frame) 2))
                    (+ (frame-y frame) (/ (frame-height frame) 2))))))

(add-hook *focus-window-hook* 'focus-window-report)

(defvar *message-semaphore* (bt:make-semaphore :count 1))
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
           #\✓)
          ((and (typep (second (group-windows group)) 'window)
                (eq window (second (group-windows group))))
           #\×)
          (t #\ ))))

(defun fmt-window-marked (window)
  (if (window-marked window)
      #\✓
      #\Space))

(turn-on-mode-line-timer)
(toggle-modeline)



(clear-window-placement-rules)
(define-frame-preference "2"
  (0 t t :class "Chromium")
  (1 t t :class "Wfica")
  )

(define-frame-preference "1"
  (0 t t :class "Emacs")
  (1 t t :class "Firefox" :create t)
  )

(add-hook *root-click-hook* 'root-click-handle)
(defun root-click-handle (screen button x y)
  (message "root"))

(add-hook *new-window-hook* 'new-window-handle)
(defun new-window-handle (win)
  (if (equal (window-class win) "mpv")
      (let ((screen (window-screen win))
            (group (window-group win)))
        (float-window win group))))
