;;; eldoro.el -- A pomodoro timer/tracker that works with org-mode. -*- lexical-binding: t -*-

;; Copyright (C) 2012-2014 Peter Jones <pjones@pmade.com>
;;
;; Author: Peter Jones <pjones@pmade.com>
;; URL: https://github.com/pjones/eldoro
;; Version: 0.1.0
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Eldoro is a simple Pomodoro timer and tracker for Emacs.  You
;; define your tasks in an org-mode buffer then start Eldoro which
;; helps you see your estimates and pomodori, along with a clock and
;; notification system.

;;; License:
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:
(eval-when-compile
  (require 'org)
  (require 'org-clock))

(defgroup eldoro nil
  "A pomodoro timer that works with `org-mode'."
  :version "0.1.0"
  :prefix "eldoro-"
  :group 'applications)

(defcustom eldoro-work-time 25
  "The number of minutes that a pomodoro working block takes."
  :type 'integer
  :group 'eldoro)

(defcustom eldoro-short-break 5
  "The number of minutes that a short break lasts."
  :type 'integer
  :group 'eldoro)

(defcustom eldoro-long-break 20
  "The number of minutes that a long break lasts."
  :type 'integer
  :group 'eldoro)

(defcustom eldoro-long-break-after 4
  "The number of pomodori after which a long break is taken."
  :type 'integer
  :group 'eldoro)

(defcustom eldoro-show-help t
  "Whether to show a short help message in the Eldoro buffer or not."
  :type 'boolean
  :group 'eldoro)

(defcustom eldoro-current-task-prompt " > "
  "The string to place in front of the active task."
  :type 'string
  :group 'eldoro)

(defcustom eldoro-date-format "%A, %B %d, %Y"
  "The date format used for pomodoro statistics."
  :type 'string
  :group 'eldoro)

(defcustom eldoro-notify-function 'org-notify
  "A function to call to notify the user that a pomodoro or break
has expired.  The function should take a single argument, a
string to display to the user."
  :type 'function
  :group 'eldoro)

(defcustom eldoro-pomodoro-end-msg
  "The current pomodoro for \"%s\" has ended.  Time for a break!"
  "A notification message shown when a pomodoro has ended.  The
string is run through `format' with one string argument, the
title of the current task."
  :type 'string
  :group 'eldoro)

(defcustom eldoro-break-end-msg
  "The current break has ended.  Get back to work!"
  "A notification message shown when a break has ended.  The
string is run through `format' with one string argument, the
title of the current task."
  :type 'string
  :group 'eldoro)

(defcustom eldoro-use-org-clock nil
  "If non-nil, start the org mode clock when starting a pomodoro
and stop it for an interruption or break."
  :type 'boolean
  :group 'eldoro)

(defcustom eldoro-record-in-properties t
  "If non-nil, record the number of pomodori and interruptions
into the source org buffer using properties."
  :type 'boolean
  :group 'eldoro)

(defcustom eldoro-estimate-property "ELDORO_ESTIMATE"
  "The name of the `org-mode' property in which to read pomodoro
estimates."
  :type 'string
  :group 'eldoro)

(defcustom eldoro-pomodoro-property "ELDORO_POMODORI"
  "The name of the `org-mode' property in which to store pomodoro
counts."
  :type 'string
  :group 'eldoro)

(defcustom eldoro-interruption-property "ELDORO_INTERRUPTIONS"
  "The name of the `org-mode' property in which to store
interruption counts."
  :type 'string
  :group 'eldoro)

(defgroup eldoro-faces nil
  "Customize the appearance of Eldoro."
  :prefix "eldoro-"
  :group 'faces
  :group 'eldoro)

(defface eldoro-header
  '((t :inherit header-line))
  "Face for the header lines in the Eldoro buffer."
  :group 'eldoro-faces)

(defface eldoro-active-task
  '((t :inherit highlight))
  "Face for the active task in Eldoro."
  :group 'eldoro-faces)

;;; Internal Variables.
(defvar eldoro-buffer-name "*Eldoro*"
  "The name of the buffer used to show pomodoros.")

(defvar eldoro--show-help nil)
(defvar eldoro--start-time nil)
(defvar eldoro--source-marker nil)
(defvar eldoro--active-marker nil)
(defvar eldoro--countdown-type nil)
(defvar eldoro--countdown-start nil)
(defvar eldoro--pomodori 0)
(defvar eldoro--breaks 0)
(defvar eldoro--interrupts 0)
(defvar eldoro--leave-point 0)
(defvar eldoro--first-task 0)
(defvar eldoro--timer nil)
(defvar eldoro--sent-notification nil)
(defvar eldoro--skip-update nil)

(defvar eldoro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b")   'bury-buffer)
    (define-key map (kbd "g")   'eldoro-update)
    (define-key map (kbd "h")   'eldoro-toggle-help)
    (define-key map (kbd "i")   'eldoro-interruption)
    (define-key map (kbd "q")   'eldoro-quit)
    (define-key map (kbd "r")   'eldoro-reset-statistics)
    (define-key map (kbd "s")   'eldoro-stop-clock)
    (define-key map (kbd "TAB") 'eldoro-jump-to-heading)
    (define-key map (kbd "RET") 'eldoro-next-action)
    map)
  "Keymap used in Eldoro mode.")

;;;###autoload
(defun eldoro (&optional force-reset)
  "Start Eldoro on the current `org-mode' heading.  If Eldoro is
already running bring its buffer forward.

If Eldoro has already been started and this function is called
from an `org-mode' buffer, prompt for permission to reset the
Eldoro tasks.  With a prefix argument force a reset without
prompting."
  (interactive)
  (cond
   ;; We're in an org buffer and we're allowed to reset.
   ((and (string= major-mode "org-mode")
         (or (not eldoro--source-marker) force-reset
             (y-or-n-p "Reset Eldoro from this org buffer? ")))
    (if eldoro--countdown-start (eldoro-stop-clock))
    (eldoro-reset-vars)
    (save-excursion
      (org-back-to-heading)
      (setq eldoro--source-marker (point-marker)))
    (unless (> (eldoro-children-count) 0)
      (error "This heading doesn't have any children"))
    (switch-to-buffer (get-buffer-create eldoro-buffer-name))
    (eldoro-mode))
   ;; There's an Eldoro buffer already.
   ((get-buffer eldoro-buffer-name)
    (switch-to-buffer eldoro-buffer-name))
   ;; No Eldoro buffer, and we're not in an org-mode buffer.
   (t
    (error "Eldoro mode should be started from an org-mode heading"))))

(define-derived-mode eldoro-mode fundamental-mode "Eldoro"
  "A major mode for showing pomodoro timers.

Eldoro works with tasks defined in an `org-mode' buffer.  When
you start Eldoro from within an `org-mode' buffer it will gather
all of headings which are children of the heading that point is
currently on.  You can then start and stop pomodoro and break
timers from within the Eldoro buffer.

## Getting Started

First, create an `org-mode' outline that looks something like
this:

~~~
* Tasks I Want To Do
** Write angry letter to Congress
** Find a bug in OpenSSL
** Upload compromising photos to FB
~~~

Now, move point to the first-level heading and start Eldoro with
the `eldoro' interactive function.  Move point to a task you want
to work on and press RET.

To switch to a break, just press RET again.  If someone
interrupts you, press i.

![Screenshot](http://www.pmade.com/static/images/2014/a5fa5925980289be7d83d3e8dbe31e1c.png)

## Reporting

By default, Eldoro writes some basic statistics into `org-mode'
properties.  If you want to compare the number of pomodori from
day to day make sure you create new headings in the `org-mode'
buffer every day.  Eldoro does not currently record timestamps
with its statistics.  It would be nice if there was a better Org
API for logging Eldoro statistics into a drawer."
  :group 'eldoro
  (setq buffer-read-only t
        truncate-lines t)
  (buffer-disable-undo)
  (eldoro-timer-stop)
  (setq eldoro--timer (run-at-time nil 10 'eldoro-update)))

(defun eldoro-update ()
  "Update the Eldoro buffer."
  (interactive)
  (unless eldoro--skip-update
    (when (not (string= (format-time-string "%Y%m%d" eldoro--start-time)
                        (format-time-string "%Y%m%d")))
      (setq eldoro--start-time (current-time))
      (eldoro-really-reset-counters))
    (let ((buffer (get-buffer eldoro-buffer-name)))
      (if (not buffer) (progn (eldoro-timer-stop) (eldoro-reset-vars))
        (with-current-buffer buffer
          (let ((buffer-read-only nil)) (eldoro-draw-buffer)))))
    (if (and eldoro--countdown-start (<= (eldoro-remaining) 0))
        (eldoro-send-notification))))

(defun eldoro-quit ()
  "Stop the current timer and kill the Eldoro buffer."
  (interactive)
  (when (y-or-n-p "Really quit Eldoro? ")
    (if eldoro--countdown-start (eldoro-stop-clock))
    (eldoro-timer-stop)
    (eldoro-reset-vars)
    (kill-buffer eldoro-buffer-name)))

(defun eldoro-next-action ()
  "Start the next appropriate clock (pomodoro or break)."
  (interactive)
  (let ((eldoro--skip-update t)
        (old eldoro--countdown-type)
        (marker (eldoro-task-p))
        (old-marker eldoro--active-marker))
    (if (not marker) (error "Please move point to a task first"))

    ;; Clock is running, figure out what to do.
    (when eldoro--countdown-start
      (eldoro-stop-clock)

      ;; If we're on the same task that was previously running then
      ;; reset the counter type so that starting the clock switches to
      ;; a break.
      (if (and marker (equal marker old-marker))
          (setq eldoro--countdown-type old)))

    ;; Restart the clock;
    (eldoro-start-clock))

  ;; Now update the display.
  (eldoro-update))

(defun eldoro-start-clock ()
  "Start a pomodoro or break clock for the task at point."
  (interactive)
  (if eldoro--countdown-start (error "A task is still in progress"))
  (let ((marker (eldoro-task-p)))
    (if (not marker) (error "Please move point to a task first"))
    (setq eldoro--active-marker marker
          eldoro--countdown-start (float-time)
          eldoro--countdown-type (eldoro-next-clock-type)
          eldoro--sent-notification nil)
    (if (eq eldoro--countdown-type 'work) (eldoro-org-clock-start)))
  (eldoro-update))

(defun eldoro-stop-clock (&optional interruption)
  "Stop the current pomodoro/break clock.  With a prefix argument
abort the current pomodoro due to an interruption."
  (interactive "P")
  (if (not eldoro--countdown-start) (error "No task is in progress"))
  (eldoro-org-clock-stop)
  (let ((restarting nil))
    (cond
     ;; Stop working (not an interruption).
     ((and (eq eldoro--countdown-type 'work) (not interruption))
      (eldoro-record-pomodoro)
      (setq eldoro--pomodori (1+ eldoro--pomodori)))
     ;; Restart work timer due to an interruption.
     ((eq eldoro--countdown-type 'work)
      (eldoro-record-interruption)
      (setq eldoro--interrupts (1+ eldoro--interrupts)))
     ;; Stop during a break (not an interruption).
     ((and (eq eldoro--countdown-type 'break) (not interruption))
      (setq eldoro--breaks (1+ eldoro--breaks)))
     ;; Restart a break due to an interruption.
     ((eq eldoro--countdown-type 'break)
      (setq eldoro--countdown-start (float-time)
            eldoro--sent-notification nil
            restarting t)))
    (unless restarting
      (setq eldoro--countdown-start nil
            eldoro--countdown-type nil
            eldoro--active-marker nil))
    (eldoro-update)))

(defun eldoro-interruption ()
  "Abort the current pomodoro due to an interruption and start a
new pomodoro."
  (interactive)
  (if eldoro--countdown-start (eldoro-stop-clock t)))

(defun eldoro-jump-to-heading ()
  "With point on an Eldoro task, make the source org buffer
current and jump to the matching heading."
  (interactive)
  (let ((marker (eldoro-task-p)))
    (if (not marker) (error "Please move point to an Eldoro task"))
    (switch-to-buffer (marker-buffer marker))
    (goto-char (marker-position marker))))

(defun eldoro-reset-statistics (&optional force)
  "Reset the counters used to track pomodori, breaks, and
interruptions.  With a prefix argument don't prompt for
confirmation."
  (interactive "P")
  (when (or force (y-or-n-p "Really reset Eldoro counters? "))
    (eldoro-really-reset-counters)
    (eldoro-update)))

(defun eldoro-toggle-help ()
  "Toggle whether or not a brief help message is displayed in the
Eldoro buffer."
  (interactive)
  (setq eldoro--show-help (not eldoro--show-help))
  (eldoro-update))

;;;-------------------------------------------------------------------------
;;; Internal Functions.
;;;-------------------------------------------------------------------------

;; Silence a compiler warning
(declare-function org-clock-in "org-clock")
(declare-function org-clock-out "org-clock")

(defun eldoro-map-tree (eldoro-fun)
  "Call ELDORO-FUN for each child in the org source tree."
  (let ((start-level))
    (with-current-buffer (marker-buffer eldoro--source-marker)
      (save-excursion
        (goto-char (marker-position eldoro--source-marker))
        (setq start-level (funcall outline-level)) ; Why funcall?
        (org-map-tree
         (lambda () (if (/= start-level (funcall outline-level))
                        (funcall eldoro-fun))))))))

(defun eldoro-children-count ()
  "Return the number of child headings in the org doc."
  (let ((children 0))
    (eldoro-map-tree (lambda () (setq children (1+ children))))
    children))

(defun eldoro-task-p ()
  "Return nil if point isn't on a Eldoro task, otherwise returns
the marker associated with the task at point."
  (with-current-buffer eldoro-buffer-name
    (get-text-property (point) 'eldoro-src)))

(defun eldoro-minutes-as-string (minutes)
  (if (= (abs minutes) 1) "minute" "minutes"))

(defun eldoro-remaining-string (&optional countdown)
  (let* ((time (or countdown eldoro--countdown-start))
         (min (eldoro-remaining time))
         (ajd (if (>= min 0) " remaining" " too long"))
         (clock (number-to-string (abs min))))
    (concat clock " " (eldoro-minutes-as-string min) ajd)))

(defun eldoro-remaining (&optional countdown)
  (setq countdown (or countdown eldoro--countdown-start))
  (round (- (eldoro-duration)
            (/ (- (float-time) countdown) 60))))

(defun eldoro-duration ()
  "Return the number of minutes the clock should run for."
  (cond
   ((eq eldoro--countdown-type 'work)
    eldoro-work-time)
   ((and (eq eldoro--countdown-type 'break)
         (/= eldoro--pomodori 0)
         (= (% eldoro--pomodori eldoro-long-break-after) 0))
    eldoro-long-break)
   (t eldoro-short-break)))

(defun eldoro-timer-stop ()
  "Stop the internal Emacs timer."
  (if eldoro--timer (setq eldoro--timer (cancel-timer eldoro--timer))))

(defun eldoro-reset-vars ()
  "Reset all internal variables tied to a given org file."
  (if eldoro--countdown-start (eldoro-stop-clock))
  (if (not eldoro--start-time) (setq eldoro--start-time (current-time)))
  (setq eldoro--show-help eldoro-show-help
        eldoro--countdown-type nil
        eldoro--countdown-start nil
        eldoro--sent-notification nil
        eldoro--leave-point 0
        eldoro--source-marker nil
        eldoro--active-marker nil))

(defun eldoro-really-reset-counters ()
  (setq eldoro--pomodori 0
        eldoro--breaks 0
        eldoro--interrupts 0))

(defun eldoro-next-clock-type ()
  (cond
   ((eq eldoro--countdown-type 'work)  'break)
   ((eq eldoro--countdown-type 'break) 'work)
   (t 'work)))

(defun eldoro-draw-buffer ()
  "Write the contents of the Eldoro buffer."
  (let ((buf (get-buffer eldoro-buffer-name))
        (size-before (buffer-size))
        (eldoro--leave-point (point))
        (eldoro--first-task 0))
    (erase-buffer)
    (eldoro-draw-stats)
    (if eldoro--show-help (eldoro-draw-help))
    (insert (propertize (concat (eldoro-parent-task-heading) ":")
                        'face 'eldoro-header))
    (insert "\n\n")
    (eldoro-map-tree 'eldoro-draw-heading)
    (set-buffer-modified-p nil)
    (setq eldoro--leave-point
          (if (= 0 size-before) eldoro--first-task
            (+ eldoro--leave-point (- (buffer-size) size-before))))
    (goto-char eldoro--leave-point)
    (dolist (w (window-list))
      (if (eq (window-buffer w) buf)
          (set-window-point w eldoro--leave-point)))))

(defun eldoro-draw-stats ()
  (let ((indent (make-string (length eldoro-current-task-prompt) ? ))
        (clock (and eldoro--countdown-start (eldoro-remaining-string)))
        (pomodori (number-to-string eldoro--pomodori))
        (breaks (number-to-string eldoro--breaks))
        (interrupts (number-to-string eldoro--interrupts)))
    (insert (propertize (concat "Pomodoro statistics for "
                                (format-time-string eldoro-date-format) ":")
                        'face 'eldoro-header))
    (insert "\n\n")
    (if eldoro--countdown-start
        (cond
         ((eq eldoro--countdown-type 'work)
          (insert (concat indent " Pomodoro Timer: " clock "\n")))
         ((eq eldoro--countdown-type 'break)
          (insert (concat indent "    Break Timer: " clock "\n")))))
    (insert (concat indent "       Pomodori: " pomodori "\n"))
    (insert (concat indent "         Breaks: " breaks "\n"))
    (insert (concat indent "  Interruptions: " interrupts "\n"))
    (insert "\n")))

(defun eldoro-draw-help ()
  (let ((help (substitute-command-keys "\\{eldoro-mode-map}"))
        (indent (make-string (length eldoro-current-task-prompt) ? ))
        (offset 0))
    (while (string-match "^\\([^[:space:]]\\)" help offset)
      (setq help (replace-match (concat indent "\\1") t nil help))
      (setq offset (+ 2 offset)))
    (insert (propertize "Eldoro Help:" 'face 'eldoro-header))
    (insert (concat "\n\n" help))))

(defun eldoro-draw-heading ()
  (let* ((heading (substring-no-properties (org-get-heading t t)))
         (mark (point-marker))
         (prompt (make-string (length eldoro-current-task-prompt) ? ))
         (estimate (eldoro-get-org-prop eldoro-estimate-property "0" mark))
         (done (eldoro-get-org-prop eldoro-pomodoro-property "0" mark))
         (stats (format "[%02d/%02d] "
                        (string-to-number done)
                        (string-to-number estimate)))
         task active)
    (if (equal mark eldoro--active-marker)
        (setq prompt eldoro-current-task-prompt active t))
    (setq task (concat prompt stats heading "\n"))
    (put-text-property 0 (length task) 'eldoro-src mark task)
    (with-current-buffer eldoro-buffer-name
      (if (= eldoro--first-task 0) (setq eldoro--first-task (point)))
      (if active (insert (propertize task 'face 'eldoro-active-task))
        (insert task)))))

(defun eldoro-at-marker (marker fun)
  "Move to MARKER and apply FUN."
  (setq marker (or marker eldoro--active-marker))
  (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (funcall fun))))

(defun eldoro-get-task-heading (&optional marker)
  "Return the heading text for the heading at MARKER or at the
active marker if MARKER is nil."
  (eldoro-at-marker
   marker
   (lambda () (substring-no-properties (org-get-heading t t)))))

(defun eldoro-parent-task-heading ()
  "Return the heading text for the task Eldoro was started on."
  (eldoro-get-task-heading eldoro--source-marker))

(defun eldoro-active-task-heading ()
  "Return the heading text for the active task."
  (eldoro-get-task-heading eldoro--active-marker))

(defun eldoro-record-pomodoro ()
  "Increment the number of pomodori for the active task."
  (if eldoro-record-in-properties
      (eldoro-inc-org-prop eldoro-pomodoro-property)))

(defun eldoro-record-interruption ()
  "Increment the number of interruptions for the active task."
  (if eldoro-record-in-properties
      (eldoro-inc-org-prop eldoro-interruption-property)))

(defun eldoro-get-org-prop (name &optional missing marker)
  "Return the value for the given property.  If the property is
missing return the value of MISSING.  By default the property is
looked up on the active org heading unless MARKER is given."
  (eldoro-at-marker
   marker (lambda () (or (org-entry-get (point) name) missing))))

(defun eldoro-set-org-prop (name value &optional marker)
  "Set the property NAME to VALUE for MARKER or the active
heading."
  (eldoro-at-marker
   marker (lambda () (org-entry-put (point) name value))))

(defun eldoro-inc-org-prop (name &optional marker)
  (let* ((s (eldoro-get-org-prop name "0" marker))
         (n (1+ (string-to-number s))))
    (eldoro-set-org-prop name (number-to-string n) marker)))

(defun eldoro-org-clock-start ()
  "Start the `org-mode' clock for the active heading."
  (when eldoro-use-org-clock
    (eldoro-at-marker
     eldoro--active-marker (lambda () (org-clock-in)))))

(defun eldoro-org-clock-stop ()
  "Stop a running `org-mode' clock."
  (when (and eldoro-use-org-clock eldoro--countdown-start)
    (eldoro-at-marker
     eldoro--active-marker (lambda () (org-clock-out t)))))

(defun eldoro-send-notification ()
  "Send a notification that a pomodoro or break ended."
  (when (and (not eldoro--sent-notification) eldoro-notify-function)
    (setq eldoro--sent-notification t)
    (let ((msg (if (eq eldoro--countdown-type 'work)
                   eldoro-pomodoro-end-msg
                 eldoro-break-end-msg)))
      (funcall eldoro-notify-function
               (format msg (eldoro-active-task-heading))))))

(provide 'eldoro)
;;; eldoro.el ends here
