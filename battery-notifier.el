;;; battery-notifier.el --- package used to notifiy when battery capacity is low

;; This file is not part of Emacs

;; Copyright (C) 2021 by Jason Johson
;; Copyright (C) 2021-2021 by Jason Johnson
;; Author:          Jason Johnson (jason@fullsteamlabs.com)
;; Maintainer:      Jason Johnson (jason@fullsteamlabs.com)
;; Created:         May 15, 2021
;; Keywords:        battery
;; Package-Version: 20210515.0001
;; Package-Commit: ed7abf23bdd076de803bb37c7870a0e5f9ee7520
;; URL: https://github.com/jasonmj/battery-notifier
;; Version: 0.02

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  Simple package to notify when battery capacity is low.  Allows
;;  for configuration of battery device, notification capacity threshold,
;;  suspend capacity threshold, notification function and suspend function.
;;  This is a global minor mode.

;;; Installation:
;;
;;  1. Place battery-notifier.el somewhere on your Emacs load path.
;;  2. Add (require 'battery-notifier) to your .emacs
;;  3. Add (battery-notifier-mode 1) to your .emacs
;;
;;  battery-notifier.el is also available in Melpa.  See
;;  https://github.com/melpa/melpa#usage for information on using
;;  Melpa.  Then you can run M-x package-install battery-notifier to install
;;  it.

;;; Usage:
;;
;;  M-x `battery-notifer-mode'
;;      Toggles battery-notifier-mode on & off.  Optional arg turns
;;      battery-notifier-mode on if arg is a positive integer.

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are
;;  welcome.  Please create issues or send pull requests via Github at
;;  https://github.com/jasonmj/battery-notifier.

;;; Change Log:
;;
;;  See https://github.com/jasonmj/battery-notifer/commits/main

;;; Code:

;;; **************************************************************************
;;; ***** mode definition
;;; **************************************************************************

;;;###autoload
(define-minor-mode battery-notifier-mode
  "Toggle use of 'battery-notifier-mode'.
   This global minor mode sends notifications when battery capacity is low
   and suspends the computer when battery is critically low."
  :lighter " enabled"
  :init-value nil
  :keymap nil
  :global t

  (battery-notifier-watch))

;;; **************************************************************************
;;; ***** utility functions
;;; **************************************************************************

(defvar battery-notifier-device "BAT0"
  "The device to check capacity for displaying notifications.")

(defvar battery-notifier-notification-function 'message
  "The function to use when displaying low battery notifications.")

(defvar battery-notifier-suspend-function (lambda()(call-process-shell-command "systemctl suspend"))
  "The function to use when suspending the computer.")

(defvar battery-notifier-threshold 25
  "The threshold below which battery notifications should be sent.")

(defvar battery-notifier-suspend-threshold 15
  "The threshold below which the computer should suspend.")

(defun battery-notifier-get-device-capacity()
  "Checks the current capacity of the specified device."
  (string-to-number
   (shell-command-to-string
    (concat "cat /sys/class/power_supply/" battery-notifier-device "/capacity"))))

(defun battery-notifier-get-device-status()
  "Checks the current status of the specified device."
  (string-trim (shell-command-to-string
    (concat "cat /sys/class/power_supply/" battery-notifier-device "/status"))))

(defun battery-notifier-check()
  (unless (bound-and-true-p battery-notifier-mode) (cancel-timer battery-notifier-watcher))
  (let ((battery-capacity (battery-notifier-get-device-capacity))
        (battery-status (battery-notifier-get-device-status)))
    (if (and (< battery-capacity battery-notifier-threshold) (equal battery-status "Discharging"))
        (funcall battery-notifier-notification-function
                 (concat "Low Battery: " (number-to-string battery-capacity) "%")))
    (if (and (< battery-capacity battery-notifier-suspend-threshold) (equal battery-status "Discharging"))
        (funcall battery-notifier-suspend-function))))

(defun battery-notifier-watch()
  (battery-notifier-check)
  (setq battery-notifier-watcher
        (run-with-idle-timer 30 t 'battery-notifier-check)))

(provide 'battery-notifier)

;;; battery-notifier.el ends here
