;; -*-lisp-*-
;;
;; Here is a sample .stumpwmrc file

(in-package :stumpwm)

;; change the prefix key to something else
(set-prefix-key (kbd "s-SPC"))

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

(defcommand toggle-bar () ()
  "toggle bar command"
  (stumpwm:toggle-mode-line (current-screen)
                          (current-head)))

(defun get-battery-charge ()
  (let ((raw-battery (run-shell-command "acpi | cut -d, -f2 | tail -c 4 | head -c 3" t)))
    (substitute #\Space #\Newline raw-battery)))

(defun show-battery-state ()
  (let ((raw-battery (run-shell-command "acpi | cut -d: -f2 | cut -d, -f1" t)))
    (substitute #\Space #\Newline raw-battery)))

(defun get-volume ()
  (string-trim
   (string #\newline)
   (run-shell-command "amixer sget Master | awk '/^ +Front L/{print $5}'" t)))

(defun get-time ()
  (run-shell-command "date +%R" t))

(setq groups-num '(1 2 3 4 5 6))
(setq group-names '("Web" "Emacs" "Office" "Video" "Pictures"))
(setq background "/home/slamko/echurch/background/x0wdvy9056g61.png")

(defun spawn-groups ()
  (loop for g in (cdr groups-num)
        do (gnew (nth (- g 2) group-names))))

(defun startup ()
  (progn
    (toggle-bar)
    (spawn-groups)
    (run-shell-command "exec pulseaudio --start &")
    (run-shell-command (concatenate 'string "feh --bg-scale " background " &"))
    (run-shell-command "exec emacs --daemon &")))

(startup)

(define-key *top-map* (kbd "s-Return") "exec st")
(define-key *top-map* (kbd "s-p") "exec rofi -show run")
(define-key *top-map* (kbd "s-b") "exec brave-browser-stable")
(define-key *top-map* (kbd "s-g") "gnew")
(define-key *top-map* (kbd "s-s") "gselect")
(define-key *top-map* (kbd "s-c") "delete")
(define-key *top-map* (kbd "s-h") "resize-direction left")
(define-key *top-map* (kbd "s-l") "resize-direction right")
(define-key *top-map* (kbd "s-=") "exec amixer set Master 3%+")
(define-key *top-map* (kbd "s--") "exec amixer set Master 3%-")

(setf *mode-line-foreground-color* "white")
(setf *mode-line-timeout* 60)
(setf stumpwm:*screen-mode-line-format*
    (list "%g "
            " ^> "                      ; remaining elements become left-aligned
            "   Vol:"   '(:eval (get-volume))
            "  BAT:["  '(:eval (get-battery-charge)) "] "
            '(:eval (get-time))))

(loop for n in groups-num
      do (define-key *top-map*
             (kbd (concatenate 'string "s-" (write-to-string n)))
           (concatenate 'string "gselect " (write-to-string n))))

(loop for n in groups-num
      do (define-key *root-map*
             (kbd (concatenate 'string "s-" (write-to-string n)))
           (concatenate 'string "gmove " (write-to-string n))))

(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-k") "fnext")
(define-key *top-map* (kbd "s-j") "fprev")
(define-key *top-map* (kbd "s-TAB") "next")
(define-key *top-map* (kbd "s-;") "colon")

(define-key *top-map* (kbd "s-m") "exec eclient")
(define-key *root-map* (kbd "s-SPC") "exec /home/slamko/.local/bin/sw_lang")
(define-key *root-map* (kbd "S-M") "exec /home/slamko/.local/bin/sw_lang")

(define-key *root-map* (kbd "Return") "exec st")
(define-key *root-map* (kbd "ESC") "abort") 
(define-key *root-map* (kbd "o") "only") 
(define-key *root-map* (kbd "m") "toggle-bar") 
(define-key *root-map* (kbd "R") "restart-hard") 
(define-key *root-map* (kbd "r") "restart-soft") 
;; Browse somewhere
(define-key *top-map* (kbd "s-b") "exec brave-browser-stable")
;; Ssh somewhere
(define-key *root-map* (kbd "p") "exec rofi -show run")
;; Lock screen
(define-key *root-map* (kbd "C-l") "exec xlock")

(setq *message-window-gravity* :center
      *input-window-gravity* :center
      *window-border-style* :thin
      *message-window-padding* 10
      *message-window-y-padding* 5
      *maxsize-border-width* 2
      *normal-border-width* 2
      *transient-border-width* 0
      *float-window-border* 2
      *float-window-title-height* 2
      *mouse-focus-policy* :click)

;; Web jump (works for DuckDuckGo and Imdb)
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
    (nsubstitute #\+ #\Space search)
    (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "duckduckgo" "brave-browser-stable https://duckduckgo.com/?q=")
(make-web-jump "imdb" "firefox http://www.imdb.com/find?q=")

;; Message window font
(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")

;;; Define window placement policy...

;; Clear rules
(clear-window-placement-rules)

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.

(define-frame-preference "Scratch"
  ;; frame raise lock (lock AND raise == jumpto)
  (1 t nil :class "st")
  (1 t nil :class "alacritty"))

(define-frame-preference "Ardour"
  (0 t   t   :instance "ardour_editor" :type :normal)
  (0 t   t   :title "Ardour - Session Control")
  (0 nil nil :class "XTerm")
  (1 t   nil :type :normal)
  (1 t   t   :instance "ardour_mixer")
  (2 t   t   :instance "jvmetro")
  (1 t   t   :instance "qjackctl")
  (3 t   t   :instance "qjackctl" :role "qjackctlMainForm"))

(define-frame-preference "Shareland"
  (0 t   nil :class "XTerm")
  (1 nil t   :class "aMule"))

;; (define-frame-preference "Emacs"
  ;; (1 t t :restore "emacs-editing-dump" :title "...xdvi")
  ;; (0 t t :create "emacs-dump" :class "Emacs"))
