### Description
A simple package to notify when battery capacity is low and suspend the computer when battery capacity is critically low.  Allows for configuration of notification capacity threshold, suspend capacity threshold, notification function, and suspend function.

### Getting Started
Either clone from this GitHub repository or install from MELPA (recommended).

If you've cloned the repository, follow these instructions:
1. Place battery-notifier.el somewhere on your Emacs load path.
2. Add `(require 'battery-notifier)` to your Emacs configuration.
3. Add `(battery-notifier-mode 1)` to your Emacs configuration.

### Customization
There are several options for customizing this package:

#### Capacity Thresholds
The `battery-notifier-capacity-low-threshold` variable defaults to **15%**.

The `battery-notifier-capacity-critical-threshold` variable defaults to **5%**.

If you would like to change either of these, simply add one or both of:

``` emacs-lisp
(setq battery-notifier-capacity-low-threshold 15)
(setq battery-notifier-capacity-critical-threshold 5)
```

to your Emacs configuration, replacing the default values with your own.

#### Notification Function
Battery Notifier comes with a customized version of `alert` that displays notifications with a high severity level (to get your attention).

If you would like to define your own notification function, you can do so by adding this to your Emacs configuration:

``` emacs-lisp
(setq battery-notifier-notification-function 'my-notification-function)
```

Note: Battery Notifier will pass a string argument to the notification function.

#### Capacity Critical Hook
Battery Notifier includes a hook to perform specified action(s) when the batter reaches the critical threshold.

For example, you may want to suspend the computer with an action similar to this:

``` emacs-lisp
(add-hook 'battery-notifier-capacity-critical-hook
          (lambda () (call-process-shell-command "systemctl suspend")))
```

For MacOS you may want something like this:

``` emacs-lisp
(add-hook 'battery-notifier-capacity-critical-hook
          (lambda () (call-process-shell-command "pmset sleepnow")))
```

Of course, you could also design some other action(s) to take at critical capacity. It's up to you!

#### Battery Check Interval
By default, Battery Notifier checks the battery state every 30 seconds. If you would like to modify this interval, you can add your own definition by including the following in your Emacs configuration:

``` emacs-lisp
(setq battery-notifier-timer-interval 30)
```
