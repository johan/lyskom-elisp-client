;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: option-edit.el,v 44.124 2010-05-13 18:14:12 byers Exp $
;;;;; Copyright (C) 1991-2002  Lysator Academic Computer Association.
;;;;;
;;;;; This file is part of the LysKOM Emacs LISP client.
;;;;; 
;;;;; LysKOM is free software; you can redistribute it and/or modify it
;;;;; under the terms of the GNU General Public License as published by 
;;;;; the Free Software Foundation; either version 2, or (at your option) 
;;;;; any later version.
;;;;; 
;;;;; LysKOM is distributed in the hope that it will be useful, but WITHOUT
;;;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;;;; for more details.
;;;;; 
;;;;; You should have received a copy of the GNU General Public License
;;;;; along with LysKOM; see the file COPYING.  If not, write to
;;;;; Lysator, c/o ISY, Linkoping University, S-581 83 Linkoping, SWEDEN,
;;;;; or the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, 
;;;;; MA 02139, USA.
;;;;;
;;;;; Please mail bug reports to bug-lyskom@lysator.liu.se. 
;;;;;
;;;; ================================================================
;;;; ================================================================
;;;;
;;;; File: option-edit.el
;;;;
;;;; Customization for LysKOM
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: option-edit.el,v 44.124 2010-05-13 18:14:12 byers Exp $\n"))

(lyskom-external-function widget-default-format-handler)
(lyskom-external-function popup-mode-menu)

;;; ======================================================================
;;; Require Per Abrahamsens widget package, version 0.991 or later.
;;;

;;; ======================================================================
;;; Global variables

(defvar lyskom-widgets nil
  "List of widgets in the customize buffer.")

;(setq lyskom-customize-buffer-format '([kom-format-html-authors]))

(defvar lyskom-customize-buffer-format
  '("\n"
    (lyskom bold centered)
    "\n\n"
    lyskom-doc
    "\n\n"
    [kom-customize-format]
    "\n\n"
    section
    (look-and-feel-misc bold centered)
    section
    "\n"
    [kom-default-language]
    [kom-show-namedays]
    [kom-show-week-number]
    [kom-print-relative-dates]
    [kom-print-seconds-in-time-strings]
    "\n"
    [kom-extended-status-information]
    [kom-auto-list-faqs]
    [kom-auto-review-faqs]
    "\n"
    [kom-auto-confirm-new-conferences]
    "\n"
    [kom-idle-hide]
    [kom-relogin-behaviour]
    [kom-relogin-inhibit-commands]
    [kom-remember-password]
    [kom-show-where-and-what]
    [kom-show-since-and-when]
    "\n"
    [kom-highlight-conferences]
    [kom-friends]
    [kom-morons]
    [kom-person-list-1]
    [kom-person-list-2]
    [kom-person-list-3]
    [kom-person-list-4]
    "\n"
    [kom-show-unread-in-frame-title]
    [kom-unread-mode-line-type]
    [kom-presence-messages-in-echo-area]
    [kom-presence-messages-in-buffer]
    "\n"
    [kom-text-no-prompts]
    [kom-keyboard-menu-immediate-selection]
    [kom-page-before-command]
    [kom-deferred-printing]
    [kom-max-buffer-size]
    [kom-trim-buffer-minimum]
    [kom-bury-buffers]
    [kom-keep-alive-interval]
    [kom-lost-session-notification]
    [kom-show-sync-messages]
    "\n"
    [kom-agree-text]
    [kom-mercial]
    "\n"
    [kom-user-prompt-format]
    [kom-user-prompt-format-executing]
    [kom-anonymous-prompt-format]
    [kom-anonymous-prompt-format-executing]
    [kom-enabled-prompt-format]
    [kom-enabled-prompt-format-executing]
    "\n\n"
    section
    (window-locations bold centered)
    section
    "\n"
    (windows-where bold centered)
    "\n"
    windows-doc
    "\n\n"
    [kom-customize-in-window]
    [kom-edit-filters-in-window]
    [kom-prioritize-in-window]
    [kom-list-membership-in-window]
    [kom-write-texts-in-window]
    [kom-view-commented-in-window]
    [kom-personal-messages-in-window]
    
    "\n\n"
    section
    (predefined-fonts bold centered)
    section
    "\n"
    [kom-active-face]
    [kom-url-face]
    [kom-me-face]
    [kom-highlight-face]
    [kom-subject-face]
    [kom-text-no-face]
    [kom-friends-face]
    [kom-morons-face]
    [kom-presence-face]
    [kom-first-line-face]
    [kom-warning-face]
    [kom-mark-face]
    [kom-dim-face]
    [kom-text-body-face]
    [kom-dashed-lines-face]
    [kom-async-text-body-face]
    [kom-async-dashed-lines-face]
    "\n\n"
    section
    (other-fonts bold centered)
    section
    "\n"
    [kom-active-highlight-face]
    [kom-active-strikethrough-face]
    "\n\n"
    section
    (audio-cues bold centered)
    section
    "\n"
    [kom-audio-player]
    "\n"
    (audio-cues-when bold centered)
    "\n"
    audio-cues-doc
    "\n\n"
    [kom-ding-on-priority-break]
    [kom-ding-on-new-letter]
    [kom-ding-on-wait-done]
    [kom-ding-on-no-subject]
    [kom-ding-on-common-messages]
    [kom-ding-on-group-messages]
    [kom-ding-on-personal-messages]
    "\n"
    [kom-silent-ansaphone]
    "\n\n"
    section
    (reading bold centered)
    section
    "\n"
    [kom-default-session-priority]
    [kom-print-number-of-unread-on-entrance]
    [kom-allow-incompleteness]
    [kom-permissive-completion]
    [kom-complete-numbers-before-names]
    [kom-continuous-scrolling]
    [kom-inhibit-typeahead]
    [kom-confirm-add-recipients]
    "\n"
    [kom-higher-priority-breaks]
    [kom-review-priority]
    [kom-server-priority]
    [kom-server-priority-breaks]
    [kom-session-nickname]
    "\n"
    [kom-read-depth-first]
    [kom-read-related-first]
    [kom-reading-puts-comments-in-pointers-last]
    [kom-show-footnotes-immediately]
    [kom-follow-comments-outside-membership]
    [kom-follow-attachments]
    [kom-mark-read-texts-as-read-in-new-recipient]
    [kom-created-texts-are-read]
    [kom-created-texts-are-saved]
    [kom-review-uses-cache]
    [kom-review-marks-texts-as-read]
    "\n"
    [kom-saved-file-name]
    [kom-default-mark]
    [kom-symbolic-marks-alist]
    [kom-membership-default-message-flag]
    [kom-membership-default-priority]
    [kom-membership-default-placement]
    [kom-unsubscribe-makes-passive]
    [kom-postpone-default]
    "\n"
    [kom-autowrap]
    [kom-autowrap-timeout]
    [kom-show-author-at-end]
    [kom-show-creating-software]
    [kom-show-imported-importer]
    [kom-show-imported-external-recipients]
    [kom-show-imported-envelope-sender]
    [kom-show-imported-message-id]
    [kom-text-properties]
    [kom-fontify-text]
    [kom-smileys]
    [kom-w3-simplify-body]
    [kom-format-html-authors]
    [kom-format-show-images]
    "\n"
    [kom-max-overlays]
    [kom-highlight-first-line]
    [kom-highlight-dashed-lines]
    [kom-highlight-text-body]
    [kom-async-highlight-dashed-lines]
    [kom-async-highlight-text-body]
    "\n"
    [kom-dashed-lines]
    [kom-long-lines]
    [kom-text-footer-format]
    [kom-text-header-dash-length]
    [kom-text-footer-dash-length]
    "\n"
    [kom-truncate-threshold]
    [kom-truncate-show-lines]
    "\n"
    [kom-ignore-message-senders]
    [kom-ignore-message-recipients]
    [kom-show-personal-message-date]
    [kom-filter-outgoing-messages]
    "\n\n"
    section
    (writing bold centered)
    section
    "\n"
    [kom-edit-hide-add-button]
    [kom-cite-string]
    [kom-ispell-dictionary]
    "\n"
    sending-doc
    "\n\n"
    [kom-confirm-multiple-recipients]
    [kom-check-commented-author-membership]
    [kom-dont-check-commented-authors]
    [kom-check-for-new-comments]

    "\n\n"
    section
    (urls bold centered)
    section
    "\n"
    [kom-text-links]
    [kom-url-transformation-rules]
    [kom-url-viewer-preferences]
    [kom-windows-browser-command]
    [kom-mosaic-command]
    [kom-netscape-command]
    [kom-netscape-variant]
    [kom-galeon-command]
    [kom-lynx-terminal]
    [kom-lynx-xterm-command]
    [kom-lynx-terminal-command]


    "\n\n"
    section
    (personal-messages bold centered)
    section
    "\n"
    [kom-show-personal-messages-in-buffer]
    [kom-pop-personal-messages]
    [kom-default-message-recipient]
    "\n"
    (automatic-replies bold centered)
    "\n"
    [kom-ansaphone-record-messages]
    [kom-ansaphone-show-messages]
    "\n"
    [kom-ansaphone-default-reply]
    "\n"
    [kom-ansaphone-replies]
    "\n\n"
    section
    (remote-control bold centered)
    section
    "\n"
    [kom-remote-control]
    [kom-self-control]
    "\n"
    [kom-remote-controllers]
    "\n\n"
    section
    (hooks bold centered)
    section
    "\n"
    [kom-login-hook]
    [kom-do-when-done]
    ))

;;; ======================================================================
;;;     User functions
;;;

(defun lyskom-customize-apply ()
  "Set the variables in the LysKOM buffer according to their values in the
customize buffer but do not save them to the server."
  (interactive)
  (let ((tmp lyskom-widgets))
    (save-current-buffer
      (set-buffer lyskom-buffer)
      (when (listp kom-dont-read-saved-variables)
        (setq kom-dont-read-saved-variables nil))
      (while tmp
        (set (car (car tmp))
             (widget-value (cdr (car tmp))))
        (when (not (widget-value (widget-get (cdr (car tmp)) 
                                             ':lyskom-storage-widget)))
          (set-default (car (car tmp)) (widget-value (cdr (car tmp))))
          (when (and (listp kom-dont-read-saved-variables)
                     (or (memq (car (car tmp)) lyskom-elisp-variables)
                         (lyskom-flag-global-variable-from-elisp (car (car tmp)))))
            (setq kom-dont-read-saved-variables 
                  (cons (car (car tmp)) kom-dont-read-saved-variables))))
        (setq tmp (cdr tmp))))))

(eval-when-compile (defvar save-options-init-file nil))

(defun lyskom-customize-send ()
  "Save variables to the server"
  (save-current-buffer
    (set-buffer lyskom-buffer)
    (lyskom-save-options (current-buffer)
                         (lyskom-get-string 'saving-settings)
                         (lyskom-get-string 'saving-settings-done)
                         (lyskom-get-string 'could-not-save-options))
    (let ((var-list nil))
      (mapc
       (lambda (e)
         (when (and (vectorp e)
                    (symbolp (elt e 0))
                    (or 
                     (and (not (memq (elt e 0) lyskom-elisp-variables))
                          (not (lyskom-flag-global-variable-from-elisp (elt e 0))))
                     (memq (elt e 0) kom-dont-read-saved-variables))
                    (boundp (elt e 0)))
           (setq var-list (cons (cons (elt e 0)
                                      (symbol-value (elt e 0)))
                                var-list))))
       lyskom-customize-buffer-format)

      (let* ((actual-save-options-init-file
              (or (and (boundp 'save-options-init-file)
                       save-options-init-file)
                  (and (not (equal user-init-file ""))
                       user-init-file)
                  (and (eq system-type 'ms-dos)
                       (concat "~" (user-login-name) "/_emacs"))
                  (concat "~" (user-login-name) "/.emacs")))
             (init-output-buffer (find-file-noselect
                                  actual-save-options-init-file))
             (init-output-marker nil))

        (save-current-buffer
          (set-buffer init-output-buffer)
          ;;
          ;; Find and delete the previously saved data, and position to write.
          ;;
          (goto-char (point-min))
          (if (re-search-forward "^;;; LysKOM Settings *\n" nil 'move)
              (let ((p (match-beginning 0)))
                (goto-char p)
                (or (re-search-forward
                     "^;;; End of LysKOM Settings *\\(\n\\|\\'\\)"
                     nil t)
                    (error "can't find END of saved state in .emacs"))
                (delete-region p (match-end 0)))
            (goto-char (point-max))
            (insert "\n"))
          (setq init-output-marker (point-marker)))

    (let ((standard-output init-output-marker))
      (princ ";;; LysKOM Settings\n")
      (princ ";;; =====================\n")
      (mapc (lambda (x)
                (princ (format "(setq-default %S %s%S)\n" 
                               (car x)
                               (cond ((eq (cdr x) t) "")
                                     ((null (cdr x)) "")
                                     ((symbolp (cdr x)) "'")
                                     ((listp (cdr x)) "'")
                                     (t ""))
                               (cdr x))))
       var-list)
      (princ ";;; ============================\n")
      (princ ";;; End of LysKOM Settings\n"))

    (set-marker init-output-marker nil)
    (save-current-buffer
      (set-buffer init-output-buffer)
      (save-buffer))
    ))))

(defun lyskom-customize-save ()
  "Apply changes and save them to the server."
  (interactive)
  (lyskom-customize-apply)
  (lyskom-customize-send))

(defun lyskom-customize-quit ()
  "Quit the customize buffer without saving"
  (interactive)
  (let ((buf (current-buffer)))
    (lyskom-undisplay-buffer buf)
    (kill-buffer buf)))

(defun lyskom-customize-save-and-quit ()
  "Save and quit the customize buffer"
  (interactive)
  (lyskom-customize-apply)
  (lyskom-customize-send)
  (lyskom-customize-quit))

(defun lyskom-customize-help ()
  (interactive)
  (message (lyskom-get-string 'customize-help)))

(defun lyskom-widget-click (event)
  (interactive "e")
  (let ((pos (lyskom-event-point event)))
    (if (and pos (widget-at pos))
        (widget-button-click event)
      (popup-mode-menu))))

(defun lyskom-customize-mode ()
  "Major mode for editing LysKOM settings.
See the top of the buffer for localized help.

These commands work outside editable text fields.
\\[lyskom-customize-save-and-quit] to save and quit,
\\[lyskom-customize-save-and-quit] to save and quit,
\\[lyskom-customize-save] to save without quitting,
\\[lyskom-customize-quit] to quit without saving.
\\[widget-forward] moves to the next setting
\\[widget-button-press] changes the value
Press C-h m for a complete list of commands.

Documentation:  [?] Show documentation    [!] Hide documentation
Lists etc.   :  [INS] Add a line   [DEL] Remove a line   [*] Modify

If the box before the name of the setting is selected, the setting will
be saved in the server. Otherwise it will be saved in your .emacs.

Click prompts with the middle mouse button to change values and bring 
up menus.

All key bindings:
\\{lyskom-customize-map}
"
  (kill-all-local-variables)
  (use-local-map lyskom-customize-map)
  (make-local-variable 'lyskom-widgets)
  (setq lyskom-widgets nil)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (condition-case nil
      (lyskom-copy-face kom-active-face 'widget-button-face)
    (error nil))
  (mapc 'lyskom-custom-insert lyskom-customize-buffer-format)
  (widget-setup)
  (mapc (function
           (lambda (variable)
             (widget-value-set (cdr variable)
                               (save-current-buffer
                                 (set-buffer lyskom-buffer)
                                 (symbol-value (car variable))))))
          lyskom-widgets)
  (widget-setup)
  (setq mode-name "LysKOM Settings")
  (setq major-mode 'lyskom-customize-mode)
  )

(defun kom-customize ()
  "Open the customize buffer"
  (interactive)
  (let ((buf (lyskom-get-buffer-create 'customize
                                       (lyskom-format
                                        (lyskom-custom-string 'buffer-name)
                                        lyskom-server-name)
                                       t)))
    (unwind-protect
        (progn
          (lyskom-start-of-command 'kom-customize)
          (sit-for 0)
          (save-current-buffer
            (set-buffer buf)
            (lyskom-customize-mode))
           (lyskom-display-buffer buf))
      (save-current-buffer
        (set-buffer lyskom-buffer)
        (lyskom-end-of-command)))
    (goto-char (point-min))))


;;; ============================================================
;;; Non-user functions
;;;

(defun lyskom-custom-insert (w)
  (cond ((symbolp w)
         (widget-insert (substitute-command-keys (lyskom-custom-string w))))
        ((listp w)
         (let ((start (point))
               (end nil)
               (inhibit-read-only t))
           (widget-insert (lyskom-custom-string (car w)))
           (setq end (point))
           (mapc (lambda (fn)
                   (funcall (intern (concat "lyskom-custom-insert-"
                                            (symbol-name fn)))
                            start end))
                   (cdr w))))
        ((stringp w)
         (widget-insert w))
        ((vectorp w)
         (setq lyskom-widgets (cons (cons (aref w 0)
                                          (lyskom-create-widget (aref w 0)))
                                    lyskom-widgets)))))

;;UNUSED: lyskom-custom-insert-bold
(defun lyskom-custom-insert-bold (s e)
  (add-text-properties s e (list 'face 'bold 'end-closed nil)))

;;UNUSED: lyskom-custom-insert-centered
(defun lyskom-custom-insert-centered (s e)
  (save-excursion (goto-char s)
                  (center-line 1)))


(defun lyskom-custom-get-value (var)
  (save-current-buffer
    (set-buffer lyskom-buffer)
    (symbol-value var)))

(defun lyskom-custom-string (s)
  (lyskom-get-string s 'lyskom-custom-strings))

(defvar lyskom-custom-variables-missing '(kom-permanent-filter-list
                                          kom-quit-hook 
                                          lyskom-last-known-conf-no
                                          lyskom-last-known-pers-no
                                          kom-presence-messages
                                          kom-dont-complain-about-missing-presentation
					  kom-respect-ancient-content-types
					  kom-read-last-text-hook-conf-list
					  kom-list-summary-hook-conf-list
                                          lyskom-settings-version)
  "List of user variables not in lyskom-custom-variables.")

(defvar lyskom-custom-variables
  '(
    (kom-max-overlays (choice ((number nil
                                       :tag limited-number
                                       :format "%[%t%] (%v)")
                               (const (unlimited-number nil)))))
    (kom-highlight-first-line (toggle (on off)))
    (kom-highlight-dashed-lines (toggle (on off)))
    (kom-highlight-text-body (toggle (on off)))
    (kom-async-highlight-dashed-lines (toggle (on off)))
    (kom-async-highlight-text-body (toggle (on off)))
    (kom-personal-messages-in-window (open-window))
    (kom-write-texts-in-window (open-window))
    (kom-list-membership-in-window (open-window))
    (kom-edit-filters-in-window (open-window))
    (kom-prioritize-in-window (open-window))
    (kom-customize-in-window (open-window))
    (kom-view-commented-in-window (open-window))
    (kom-edit-filters-in-window (open-window))
    (kom-list-membership-in-window (open-window))
    (kom-customize-format (choice ((const (long-format long))
                                   (const (short-format short)))))
    (kom-default-language (repeat (language-choice nil)))
    (kom-user-prompt-format (string))
    (kom-user-prompt-format-executing (string))
    (kom-anonymous-prompt-format (string))
    (kom-anonymous-prompt-format-executing (string))
    (kom-enabled-prompt-format (string))
    (kom-enabled-prompt-format-executing (string))
    (kom-cite-string (string))
    (kom-created-texts-are-read (toggle (yes no)))
    (kom-mark-read-texts-as-read-in-new-recipient (toggle (yes no)))
    (kom-review-uses-cache (toggle (yes no)))
    (kom-review-marks-texts-as-read (toggle (yes no)))
    (kom-created-texts-are-saved (choice ((const (no nil))
                                          (file nil :tag to-file))))
    (kom-default-mark (choice ((number (0 255) 
                                       :tag selected-mark
                                       :format "%[%t%] (%v)"
                                       :size 0)
                               (const (ask nil)))))
    (kom-symbolic-marks-alist (repeat (mark-association
                                       nil
                                       :tag symbolic-mark-association
                                       :mark-key-prompt symbolic-mark-name
                                       :mark-value-prompt mark-type-to-assoc)))
    (kom-reading-puts-comments-in-pointers-last (toggle (after before)))
    (kom-autowrap (choice ((const (on t))
                           (const (off nil))
                           (number nil :tag max-text-length))))
    (kom-autowrap-timeout (choice ((const (no-timeout nil))
                                   (number nil :tag autowrap-timeout))))
    (kom-auto-list-faqs (toggle (on off)))
    (kom-auto-review-faqs (toggle (on off)))
    (kom-dashed-lines (toggle (on off)))
    (kom-show-author-at-end (toggle (on off)))

    (kom-truncate-threshold (choice ((const (off nil))
				     (number nil :tag truncate-threshold-lines))))
    (kom-truncate-show-lines (number nil))

    (kom-print-number-of-unread-on-entrance (toggle (yes no)))
    (kom-show-where-and-what (toggle (yes no)))
    (kom-show-since-and-when (toggle (yes no)))
    (kom-remember-password (toggle (yes no)))
    (kom-idle-hide (number))
    (kom-show-footnotes-immediately (toggle (yes no)))
    (kom-follow-comments-outside-membership (toggle (yes no)))
    (kom-read-depth-first (toggle (depth-first time-order)))
    (kom-read-related-first (choice ((const (off nil))
				     (const (ancestor oldest-ancestor))
				     (const (relative oldest-relative)))))
    (kom-continuous-scrolling  (toggle (on off)))
    (kom-deferred-printing (toggle (on off)))
    (kom-higher-priority-breaks (choice ((const (express-break express))
                                         (const (break t))
                                         (const (no-break nil)))))
    (kom-server-priority (number))
    (kom-server-priority-breaks (choice ((const (express-server-break express))
                                         (const (express-server-letters-break express-letters))
                                         (const (server-break t))
                                         (const (letters-server-break letters))
                                         (const (after-conf-server-break after-conf))
                                         (const (after-conf-letters-server-break after-conf-letters))
                                         (const (when-done-server-break when-done))
                                         (const (no-server-break nil)))))
    (kom-session-nickname (choice ((const (automatic-session-name nil))
				   (string nil 
					   :tag given-session-name
					   :help-echo select-buffer))))			 

    (kom-relogin-inhibit-commands (repeat (command nil :tag command)))
    (kom-login-hook (repeat (command nil :tag command)))
    (kom-do-when-done (repeat (choice ((command nil :tag command)
                                       (kbd-macro nil :tag kbd-macro))
                                      :tag execute
                                      :help-echo select-what-to-execute
                                      :format "%[%t%] %v")))
    (kom-page-before-command (choice ((const (page-none nil))
                                      (const (page-all t))
                                      (repeat (command nil
                                                       :tag command)
                                              :tag page-some
                                              :menu-tag page-some
                                              :value (kom-view-next-text)))
                                     ))
    (kom-permissive-completion (noggle (on off)))
    (kom-membership-default-priority
     (choice ((const (ask-every-time ask))
              (const (ask-every-time nil))
              (number (0 255)
                      :tag fixed-priority 
                      :help-echo select-priority
                      :format "%[%t%] (%v)"
                      :size 0))))
    (kom-membership-default-message-flag
     (choice ((const (ask-every-time ask))
              (const (yes t))
              (const (no nil)))))
    (kom-show-personal-messages-in-buffer
     (choice ((const (messages-in-lyskom-buffer t))
              (const (discard-messages nil))
              (string nil :tag in-named-buffer :help-echo select-buffer))))
    (kom-pop-personal-messages (choice ((const (yes t))
                                        (const (yes-dont-pop yes))
                                        (const (no nil)))))
    (kom-ding-on-new-letter (ding))
    (kom-ding-on-priority-break (ding))
    (kom-ding-on-wait-done (ding))
    (kom-ding-on-common-messages (ding))
    (kom-ding-on-group-messages (ding))
    (kom-ding-on-personal-messages (ding))
    (kom-ding-on-no-subject (ding))
    (kom-audio-player (file))
    (kom-default-message-recipient 
     (choice ((const (everybody-rcpt everybody))
              (const (group-rcpt group))
              (const (sender-rcpt sender))
              (const (last-recipient-recpt last-recipient)))))
    (kom-filter-outgoing-messages (noggle (yes no)))
    (kom-highlight-conferences
     (repeat (cons ((choice ((const (other-persons t))
			     (const (morons kom-morons))
                             (const (friends kom-friends))
                             (const (me lyskom-pers-no))
                             (const (i-am-supervisor lyskom-highlight-i-am-supervisor))
                             (const (has-no-presentation lyskom-highlight-has-no-presentation))
			     (const (pers-list-1 kom-person-list-1))
			     (const (pers-list-2 kom-person-list-2))
			     (const (pers-list-3 kom-person-list-3))
			     (const (pers-list-4 kom-person-list-4))
			     (symbol nil :tag other-list)
			     (person nil :tag name :lyskom-predicate (pers))
                             (repeat (person nil :tag name) 
                                     :indent 12
                                     :tag conflist
                                     :lyskom-predicate (pers conf)
                                     :menu-tag conflist)
                             )
                            :tag highlight-conferences
			    :format "%[%t%] %v\n"
                            )
                    (face (kom-active-face
                           kom-active-highlight-face
                           kom-active-strikethrough-face
                           kom-friends-face
                           kom-morons-face
                           kom-me-face) :tag highlight-face))
                   :format "%v"
                   )
             ))

    (kom-presence-messages-in-buffer 
     (choice ((const (always-show-presence t))
	      (const (never-show-presence nil))
	      (symbol nil :tag obsolete-setting)
	      (repeat (cons ((choice ((const (other-persons t))
				      (const (me lyskom-pers-no))
				      (const (morons kom-morons))
				      (const (friends kom-friends))
				      (const (pers-list-1 kom-person-list-1))
				      (const (pers-list-2 kom-person-list-2))
				      (const (pers-list-3 kom-person-list-3))
				      (const (pers-list-4 kom-person-list-4))
				      (symbol nil :tag other-list)
				      (person nil :tag name :lyskom-predicate (pers))
				      (repeat (person nil :tag name) 
					      :indent 12
					      :tag conflist
					      :lyskom-predicate (pers)
					      :menu-tag conflist)
				      )
				     :tag some-persons
				     :format "%[%t%] %v\n"
				     )
			     (toggle (yes no) :tag show-presence))
			    )
		      :tag detailed-setting
		      ))))

    (kom-presence-messages-in-echo-area
     (choice ((const (always-show-presence t))
	      (const (never-show-presence nil))
	      (symbol nil :tag obsolete-setting)
	      (repeat (cons ((choice ((const (other-persons t))
				      (const (me lyskom-pers-no))
				      (const (morons kom-morons))
				      (const (friends kom-friends))
				      (const (pers-list-1 kom-person-list-1))
				      (const (pers-list-2 kom-person-list-2))
				      (const (pers-list-3 kom-person-list-3))
				      (const (pers-list-4 kom-person-list-4))
				      (symbol nil :tag other-list)
				      (person nil :tag name :lyskom-predicate (pers))
				      (repeat (person nil :tag name) 
					      :indent 12
					      :tag conflist
					      :lyskom-predicate (pers)
					      :menu-tag conflist)
				      )
				     :tag some-persons
				     :format "%[%t%] %v\n"
				     )
			     (toggle (yes no) :tag show-presence))
			    )
		      :tag detailed-setting
		      ))))

    (kom-friends (repeat (person nil :tag name)))
    (kom-morons (repeat (person nil :tag name)))
    (kom-person-list-1 (repeat (person nil :tag name)))
    (kom-person-list-2 (repeat (person nil :tag name)))
    (kom-person-list-3 (repeat (person nil :tag name)))
    (kom-person-list-4 (repeat (person nil :tag name)))
    (kom-url-viewer-preferences (repeat (url-viewer nil :tag viewer-program)))
    (kom-windows-browser-command (file))
    (kom-mosaic-command (choice ((file nil :tag ext-simple-command :format "%[%t:%] %v")
                                     (repeat (string nil :tag ext-complex-component :format "%[%t:%] %v")
                                             :tag ext-complex-command
                                             :menu-tag ext-complex-command))))
    (kom-netscape-command (choice ((file nil :tag ext-simple-command :format "%[%t:%] %v")
                                     (repeat (string nil :tag ext-complex-component :format "%[%t:%] %v")
                                             :tag ext-complex-command
                                             :menu-tag ext-complex-command))))
    (kom-galeon-command (choice ((file nil :tag ext-simple-command :format "%[%t:%] %v")
                                     (repeat (string nil :tag ext-complex-component :format "%[%t:%] %v")
                                             :tag ext-complex-command
                                             :menu-tag ext-complex-command))))
    (kom-lynx-terminal (choice ((const (lynx-xterm xterm))
                                (const (lynx-emacs terminal)))))
    (kom-lynx-terminal-command (file))
    (kom-lynx-xterm-command  (choice ((file nil :tag ext-simple-command :format "%[%t:%] %v")
                                     (repeat (string nil :tag ext-complex-component :format "%[%t:%] %v")
                                             :tag ext-complex-command
                                             :menu-tag ext-complex-command))))
    (kom-confirm-multiple-recipients
     (choice ((const (dont-check nil))
              (const (check-before-open before))
              (const (check-before-send after)
                     :match (lambda (w v) (and v (not (eq v 'before))))))))
    (kom-check-commented-author-membership (toggle (yes no)))
    (kom-check-for-new-comments (toggle (yes no)))
    (kom-inhibit-typeahead (noggle (yes no)))
    (kom-max-buffer-size (choice ((const (no-size-limit nil))
                                  (number nil 
                                          :tag max-size-in-bytes
                                          :help-echo select-buffer-size))))

    (kom-ansaphone-record-messages (toggle (yes no)))
    (kom-ansaphone-show-messages (toggle (yes no)))
    (kom-ansaphone-default-reply (string nil :format "%[%t%]\n%v"))
    (kom-remote-control (toggle (on off)))
    (kom-remote-controllers (repeat (person nil :tag name)))
    (kom-self-control (toggle (yes no)))
    (kom-ispell-dictionary (ispell-dictionary))
    (kom-show-namedays (choice ((const (off nil))
                                (const (default-namedays t))
                                (repeat (nameday nil :format "%[%t%]: `%v'" 
						     :tag specific-namedays)
                                        :tag several-name-lists
                                        :menu-tag several-name-lists)
                                (nameday nil :tag specific-namedays))))

    (kom-show-week-number (toggle (on off)))

    (kom-relogin-behaviour (choice ((const (off nil))
				    (const (ask ask))
				    (const (on t)))))

    (kom-membership-default-placement (choice ((const (last last))
                                               (const (first first))
                                               (number nil
                                                       :tag specific-placement
                                                       ))))
    (kom-show-imported-importer (toggle (on off)))
    (kom-show-imported-envelope-sender (toggle (on off)))
    (kom-show-imported-external-recipients (toggle (on off)))
    (kom-show-imported-message-id (toggle (on off)))
    (kom-agree-text (choice ((string nil :tag a-string)
                             (repeat (string nil
                                             :tag a-string
                                             :format "%[%t%] `%v'")
                                     :tag some-string
                                     :menu-tag some-string))))
    (kom-silent-ansaphone (noggle (on off)))
    (kom-default-session-priority (choice ((const (unspecified nil))
                                           (number nil
                                                   :tag priority))))
    (kom-unsubscribe-makes-passive (toggle (on off)))
    (kom-review-priority (choice ((const (same-as-conf nil))
                                  (number nil :tag fixed-priority))))
    (kom-show-creating-software (toggle (on off)))
    (kom-text-footer-format (choice ((const (default-format nil))
                                     (string nil :tag custom-format))))
    (kom-long-lines (toggle (on off)))
    (kom-postpone-default (number))
    (kom-allow-incompleteness (toggle (on off)))
    (kom-smileys (toggle (on off)))
    (kom-ignore-message-senders (repeat (person nil :tag name)))
    (kom-ignore-message-recipients (repeat (person nil 
                                                   :tag name
                                                   :lyskom-predicate (pers conf))))
    (kom-text-header-dash-length (number))
    (kom-text-footer-dash-length (number))
    (kom-show-personal-message-date (toggle (on off)))
    (kom-mercial (string))
    (kom-w3-simplify-body (toggle (on off)))
    (kom-format-html-authors 
     (repeat (cons ((choice ((const (other-persons t))
			     (const (me lyskom-pers-no))
			     (const (morons kom-morons))
			     (const (friends kom-friends))
			     (const (pers-list-1 kom-person-list-1))
			     (const (pers-list-2 kom-person-list-2))
			     (const (pers-list-3 kom-person-list-3))
			     (const (pers-list-4 kom-person-list-4))
			     (symbol nil :tag other-list)
			     (person nil :tag name   
					 :format "%[[*]%] %v"
					 :lyskom-predicate (pers))
			     (repeat (person nil :tag name) 
				     :indent 12
				     :tag conflist
				     :lyskom-predicate (pers)
				     :menu-tag conflist)
			     )
			    :tag some-persons
			    :format "%[%t%] %v\n"
			    )
		    (toggle (yes no) :tag format-html))
		   )
	     :tag detailed-setting
	     ))
    (kom-format-show-images (toggle (on off)))
    (kom-bury-buffers (toggle (on off)))
    (kom-ansaphone-replies (ansaphone))
    (kom-complete-numbers-before-names (toggle (on off)))
    (kom-keep-alive-interval (number))
    (kom-lost-session-notification (choice ((const (off nil))
					    (const (session-lost-all-buffers all-buffers))
					    (const (session-lost-beep beep)))))
    (kom-text-no-prompts (repeat (cons ((command nil :tag command :format "%[%t%]: %v\n")
                                        (toggle (yes no)  :tag prompt-for-text-no :format "%[%t%]: %v")
                                        ))))
    (kom-saved-file-name (file))
    (kom-follow-attachments (toggle (yes no)))
    (kom-show-unread-in-frame-title (toggle (yes no)))

    (kom-unread-mode-line-type (choice ((const (simple-unread-mode-line nil))
					(const (explicit-unread-mode-line t)))))

    (kom-confirm-add-recipients (toggle (yes no)))
    (kom-trim-buffer-minimum (number))
    (kom-dont-check-commented-authors (repeat (person nil :tag name)))
    (kom-print-relative-dates (toggle (yes no)))
    (kom-print-seconds-in-time-strings (toggle (yes no)))
    (kom-extended-status-information (choice ((const (yes t))
                                              (repeat (cons ((choice ((const (extended-conf-info server))
                                                                      (const (extended-pers-info pers))
                                                                      (const (extended-server-info server))
                                                                      (const (extended-read-faq-info read-faq))
                                                                      (const (extended-statistics-info raw-server-stats))
                                                                      (const (extended-all-other-info t))
                                                                      )
                                                                     :tag extended-info-type
                                                                     :format "%[%t%] %v\n")
                                                             (toggle (yes no) :tag extended-info-show)))
                                                      :tag specific-extended-info 
                                                      :menu-tag specific-extended-info))))
    (kom-edit-hide-add-button (noggle (yes no)))
    (kom-keyboard-menu-immediate-selection (noggle (yes no)))
    (kom-url-transformation-rules (repeat (cons ((string nil :tag url-transform-regexp :format "%[%t%] %v\n")
                                                 (string nil :tag url-transform-newtext)))))
    (kom-text-links (repeat (cons ((choice ((person nil :tag link-specific-rcpt :lyskom-predicate (pers conf))
                                            (const (all-conferences t)))
                                           :tag link-pattern-for
                                           :format "%[%t%]: %v")
                                   (repeat (list ((string nil :tag link-pattern :format "%[%t%]: %v\n")
                                                  (string nil :tag link-replace :format "%[%t%]: %v\n")
                                                  (number (0 9) :tag link-highlight-match :format "%[%t%]: %v\n")
                                                  (toggle (yes no) :tag link-fold-case :format "%[%t%]: %v"))
                                                 ) :indent 12 :tag "")
                                   ))))
    (kom-url-face (face))
    (kom-text-no-face (face))
    (kom-active-face (face))
    (kom-active-highlight-face (face))
    (kom-active-strikethrough-face (face))
    (kom-highlight-face (face))
    (kom-me-face (face))
    (kom-friends-face (face))
    (kom-morons-face (face))
    (kom-subject-face (face))
    (kom-presence-face (face))
    (kom-first-line-face (face))
    (kom-warning-face (face))
    (kom-mark-face (face))
    (kom-dim-face (face))
    (kom-text-body-face (face t))
    (kom-dashed-lines-face (face t))
    (kom-async-text-body-face (face t))
    (kom-async-dashed-lines-face (face t))
    (kom-show-sync-messages (toggle (yes no)))
    (kom-netscape-variant (choice ((const (default-netscape nil))
                                   (const (new-window-netscape new-window))
                                   (const (new-tab-netscape new-tab)))))
    (kom-auto-confirm-new-conferences (toggle (yes no)))
    (kom-text-properties (toggle (yes no)))
    (kom-fontify-text (toggle (yes no)))
))

(defvar lyskom-widget-functions 
  '((toggle . lyskom-toggle-widget)
    (noggle . lyskom-toggle-widget-inverse)
    (ding   . lyskom-ding-widget)
    (choice . lyskom-choice-widget)
    (string . lyskom-string-widget)
    (symbol . lyskom-symbol-widget)
    (number . lyskom-number-widget)
    (const .  lyskom-item-widget)
    (repeat . lyskom-repeat-widget)
    (cons . lyskom-cons-widget)
    (list . lyskom-list-widget)
    (face . lyskom-face-widget)
    (nameday . lyskom-nameday-widget)
    (kbd-macro . lyskom-kbd-macro-widget)
    (url-viewer . lyskom-url-viewer-widget)
    (ispell-dictionary . lyskom-ispell-dictionary-widget)
    (open-window . lyskom-open-window-widget)
    (command . lyskom-command-widget)
    (person . lyskom-person-widget)
    (language-choice . lyskom-language-widget)
    (file . lyskom-file-widget)
    (ext-command . lyskom-external-command-widget)
    (ansaphone . lyskom-ansaphone-reply-widget)
    (mark-association . lyskom-mark-association-widget)
))

(defun lyskom-make-menu-tag (str)
  "Make a menu tag from the string STR."
  (if (string-match "\\(.*\\):\\s-*" str)
      (match-string 1 str)
    str))

(defun lyskom-create-widget (variable)
  (let* ((el (assq variable lyskom-custom-variables))
         (dummy (or el (error "Unknown variable: %S" variable)))
         (spec (lyskom-widget-convert-specification (car (cdr el)) variable))
         (tag-sym (intern (concat (symbol-name variable) "-tag")))
         (doc-sym (intern (concat (symbol-name variable) "-doc")))
         (help-sym (intern (concat (symbol-name variable) "-help")))
         (value (save-current-buffer (set-buffer lyskom-buffer)
                                     (symbol-value variable)))
         (storage-widget nil))

    (lyskom-ignore value help-sym dummy)       ; Are they ever used?

    (setq storage-widget
	  (prog1
	      (widget-create 'checkbox
			     ':value (and (not (memq variable kom-dont-read-saved-variables))
					  (or (memq variable lyskom-elisp-variables)
					      (lyskom-flag-global-variable-from-elisp variable)))
			     ':args (list variable)
			     ':format "%[%v%]"
			     ':help-echo (lyskom-custom-string 'variable-type-help))
	    (widget-insert " ")))

    (setq spec 
          (cons (car spec)
                (append
                 (list ':tag
                       (lyskom-custom-string tag-sym)
                       ':menu-tag
                       (lyskom-make-menu-tag (lyskom-custom-string tag-sym))
                       ':value
                       (lyskom-custom-get-value variable)
                       ':help-echo
                       (lyskom-format 
                        (lyskom-custom-string 'default-help-echo)
                        (symbol-name variable))
                       ':lyskom-storage-widget storage-widget
                       )
                 (cdr spec))))

    (let ((widget (apply 'widget-create spec)))
      (condition-case nil
          (progn
            (if (string= "" (lyskom-custom-string doc-sym))
                (progn (widget-insert (lyskom-format " %#1@(%#2s)" 
                                                     `(face ,kom-dim-face)
                                                     (symbol-name variable)))
                       (widget-insert "\n"))
              (widget-insert "  ")
              (widget-create 'lyskom-widget-help
                             ':value (lyskom-default-value 'kom-customize-format)
                             ':help-echo
                             (if (eq (lyskom-default-value 'kom-customize-format)
                                     'long)
                                 (lyskom-custom-string 'hide-doc)
                               (lyskom-custom-string 'show-doc))
                             ':args
                             (list (list 'long
                                         "\n%s\n\n"
                                         (lyskom-custom-string doc-sym)
                                         (lyskom-custom-string 'hide-doc)
                                         "-")
                                   (list 'short 
                                         "%s"
                                         ""
                                         (lyskom-custom-string 'show-doc)
                                         "?"))
                             ':format (lyskom-format "%%[[%%T]%%] %[%#1@(%#2s)%]\n%%D"
                                                     `(face ,kom-dim-face)
                                                     (symbol-name variable)))))
        (error (widget-insert "\n")))
        widget)))


(defun lyskom-widget-convert-specification (spec var)
  "Convert a LysKOM widget specification to something widget-create accepts"
  (let ((convertfn (assq (car spec) lyskom-widget-functions)))
    (if (null convertfn)
        (error "Unknown widget type: %S" spec))
    (nconc (funcall (cdr convertfn)
                    (car spec)
                    (car (cdr spec))
                    (cdr (cdr spec))
                    var)
           (lyskom-widget-convert-props spec))))

(defun lyskom-widget-convert-props (spec)
  "Convert widget properties to a format that widget-create likes"
  (let ((propl (nthcdr 2 spec))
        (result))
    (if (/= 0 (% (length propl) 2))
        (error "Widget property list has odd length: %S" spec))
    (while propl
      (setq result (cons (car propl) result))
      (setq propl (cdr propl))
      (setq result
            (cons (cond ((symbolp (car propl))
                         (cond ((eq (car propl) t) t)
                               ((eq (car propl) nil) nil)
                               (t (lyskom-custom-string (car propl)))))
                        ((and (consp (car propl))
                              (eq 'quote (car (car propl))))
                         (car (cdr (car propl))))
                        (t (car propl)))
                  result))
      (setq propl (cdr propl)))
    (nreverse result)))


(defun lyskom-build-simple-widget-spec (type defaults propl)
  (let (new-props)
    (while defaults
      (unless (plist-member propl (car defaults))
        (setq new-props (cons (car defaults)
                              (cons (car (cdr defaults)) new-props))))
      (setq defaults (cdr (cdr defaults))))
    (cons type new-props)))


(defun lyskom-file-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec 'file
                                   '(:format "%[%t%] %v" :size 0)
                                   propl))

(defun lyskom-person-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec 'lyskom-name nil propl))

(defun lyskom-mark-association-widget (type &optional args propl ar)
  (lyskom-build-simple-widget-spec 'lyskom-mark-association nil propl))

(defun lyskom-command-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec 'lyskom-command nil propl))

(defun lyskom-kbd-macro-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec 'lyskom-kbd-macro
                                   (list ':macro-buffer lyskom-buffer)
                                   propl))

(defun lyskom-item-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec 'item
                                   (list ':format "%t"
                                         ':tag (lyskom-custom-string 
                                                (elt args 0))
                                         ':value (elt args 1))
                                   propl))

(defun lyskom-language-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'menu-choice
   (list ':format "%[%t%] %v"
         ':case-fold t
         ':tag (lyskom-custom-string 'language)
         ':args
         (mapcar
          (lambda (x)
            (list 'item
                  ':tag (lyskom-language-name (car x))
                  ':format "%t"
                  ':value (elt x 0)))
          lyskom-languages))
   propl))

(defun lyskom-nameday-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'menu-choice
   (list ':format "%[%t%] %v"
         ':case-fold t
         ':args
         (mapcar
          (lambda (x)
            (list 'item
                  ':tag (nameday-data->name (cdr x))
                  ':format "%t"
                  ':value (nameday-data->code (cdr x))))
          lyskom-namedays))
   propl))

(defvar ispell-dictionary-alist)
(defun lyskom-ispell-dictionary-widget (type &optional args propl var)
  (let ((tmp-dictionary-alist nil))
    (condition-case nil 
        (progn (require 'ispell)
               (setq tmp-dictionary-alist ispell-dictionary-alist))
      (error (when (null ispell-dictionary-alist)
               (setq tmp-dictionary-alist '("american" "brasiliano"
                                            "british" "castellano"
                                            "castellano8" "czech" 
                                            "dansk" "deutsch"
                                            "deutsch8" "english" 
                                            "esperanto" "esperanto-tex"
                                            "francais7" "francais" 
                                            "francais-tex" "nederlands"
                                            "nederlands8" "norsk"
                                            "norsk7-tex" "polish"
                                            "russian" "svenska")))))
    (lyskom-build-simple-widget-spec 
     'menu-choice
     (list ':format "%[%t%] %v"
           ':case-fold nil
           ':args
           (cons (list 'item
                       ':tag "ispell-dictionary"
                       ':format "%t"
                       ':value nil)
                 (delq nil
                       (mapcar 
                        (lambda (x)
                          (and (car x)
                               (list 'item
                                     ':tag (car x)
                                     ':format "%t"
                                     ':value (car x))))
                        tmp-dictionary-alist))))
     propl)))

(defun lyskom-face-widget (type &optional args propl var)
  (let ((wargs (append (mapcar (lambda (f)
                                 (list 'item 
                                       :tag (symbol-name f)
                                       :value f
                                       :format "%t"))
                               (or
                                (and (listp args) args)
                                (cdr (assq var lyskom-predefined-faces))))
                       (list (list 'symbol
                                   ':size 30
                                   ':tag (lyskom-custom-string 
                                          'other-face))))))
    (when (eq args t)
      (setq wargs 
            (append (list (list 'item
                                :tag (lyskom-custom-string 'generated-face)
                                :value nil
                                :format "%t"))
                    wargs)))
    (if wargs
        (lyskom-build-simple-widget-spec 'menu-choice
                                         (list ':format "%[%t%] %v"
                                               ':args wargs)
                                         propl)
      (lyskom-build-simple-widget-spec 'symbol
                                       (list ':format "%[%t%] %v"
                                             ':size 30
                                             ':args wargs)
                                       propl))))

(defun lyskom-url-viewer-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'menu-choice 
   (list ':format "%[%v%]"
         ':case-fold t
         ':help-echo (lyskom-custom-string 'select-url-viewer)
         ':args
         (list (list 'item
                     ':tag (lyskom-custom-string 'no-viewer)
                     ':format "%t"
                     ':value nil)
               (list 'item 
                     ':tag (lyskom-custom-string 'default-viewer)
                     ':format "%t"
                     ':value "default")
               (list 'item
                     ':tag (lyskom-custom-string 'windows-viewer)
                     ':format "%t"
                     ':value "windows")
               (list 'item
                     ':tag (lyskom-custom-string 'netscape-viewer)
                     ':format "%t"
                     ':value "netscape")
               (list 'item
                     ':tag (lyskom-custom-string 'emacs-w3-viewer)
                     ':format "%t"
                     ':value "w3")
               (list 'item 
                     ':tag (lyskom-custom-string 'emacs-general-viewer)
                     ':format "%t"
                     ':value "emacs")
               (list 'item
                     ':tag (lyskom-custom-string 'emacs-dired-viewer)
                     ':format "%t"
                     ':value "dired")
               (list 'item 
                     ':tag (lyskom-custom-string 'emacs-mail-viewer)
                     ':format "%t"
                     ':value "mail-mode")
               (list 'item
                     ':tag (lyskom-custom-string 'emacs-telnet-viewer)
                     ':format "%t"
                     ':value "telnet-mode")
               (list 'item
                     ':tag (lyskom-custom-string 'mosaic-viewer)
                     ':format "%t"
                     ':value "mosaic")
               (list 'item
                     ':tag (lyskom-custom-string 'lynx-viewer)
                     ':format "%t"
                     ':value "lynx")
	       (list 'item
                     ':tag (lyskom-custom-string 'galeon-viewer)
                     ':format "%t"
                     ':value "galeon")))
   propl))

(defun lyskom-open-window-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'menu-choice
   (list ':case-fold t
         ':format "%[%t%] %v"
         ':args
         (list (list 'item
                     ':tag (lyskom-custom-string 'other-window)
                     ':format "%t"
                     ':value 'other)
               (list 'item 
                     ':tag (lyskom-custom-string 'other-frame)
                     ':format "%t"
                     ':value 'other-frame)
               (list 'item 
                     ':tag (lyskom-custom-string 'new-frame)
                     ':format "%t"
                     ':value 'new-frame)
               (list 'item
                     ':tag (lyskom-custom-string 'lyskom-window)
                     ':format "%t"
                     ':value nil)
               (list 'editable-field
                     ':tag (lyskom-custom-string 'window-on-buffer)
                     ':format "%[%t%]: `%v'"
                     ':value ""
                     ':size 0)))
   propl))

(defun lyskom-ding-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'menu-choice
   (list  ':case-fold t
          ':format "%[%t%] %v"
          ':args
          (list (list 'item 
                      ':tag (lyskom-custom-string 'turned-off)
                      ':value 0
                      ':format "%t"
                      ':match '(lambda (w v) (eq v 0)))
                (list 'lyskom-number
                      ':tag (lyskom-custom-string 'number-of-times)
                      ':help-echo (lyskom-custom-string 'select-number)
                      ':value 1
                      ':format "%[%t%]: (%v)"
                      ':size 0
                      ':min-value 1
                      ':max-value 255)
                (list 'lyskom-string
                      ':tag (lyskom-custom-string 'sound-file)
                      ':help-echo (lyskom-custom-string 'select-audio-file)
                      ':size 0)
                (list 'editable-list
                      ':format "%[%t%]\n%v%i"
                      ':tag (lyskom-custom-string 'specific-spec)
                      ':menu-tag (lyskom-custom-string 'specific-spec)
                      ':args
                      `((cons :format "%v" 
                              :value (1 0)
                              :args
                              ((menu-choice
                                :case-fold t
                                :format "%[%t%]: %v\n"
                                :tag ,(lyskom-custom-string 'conf-or-person)
                                :args
                                ((lyskom-name :lyskom-predicate (pers conf)
                                              :tag ,(lyskom-custom-string 'conf-or-person))
                                 (item :tag ,(lyskom-custom-string 'other-persons)
                                       :value t
                                       :format "%t\n")))
                               (menu-choice
                                :case-fold t
                                :format "%[%t%]: %v\n"
                                :tag ,(lyskom-custom-string 'ding)
                                :args
                                ((item :tag ,(lyskom-custom-string 'turned-off)
                                       :value nil
                                       :format "%t"
                                       :match (lambda (w v) (or (null v) (eq v 0))))
                                 (lyskom-number :tag ,(lyskom-custom-string 'number-of-times)
                                                :help-echo ,(lyskom-custom-string 'select-number)
                                                :value 1
                                                :format "%[%t%]: (%v)"
                                                :size 0
                                                :min-value 1
                                                :max-value 255)
                                 (lyskom-string :tag ,(lyskom-custom-string 'sound-file)
                                                :help-echo ,(lyskom-custom-string 'select-audio-file)
                                                :size 0))))))
                      )
                ))
   propl))

(defun lyskom-toggle-widget-inverse (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'menu-choice
   (list ':case-fold t
         ':format "%[%t%] %v"
         ':args
         (list (list 'item ':tag (lyskom-custom-string (elt args 0))
                     ':value nil
                     ':format "%t")
               (list 'item 
                     ':tag (lyskom-custom-string (elt args 1))
                     ':value t
                     ':match '(lambda (w v) v)
                     ':format "%t")))
   propl))

(defun lyskom-toggle-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'menu-choice
   (list ':case-fold t
         ':format "%[%t%] %v"
         ':args
         (list (list 'item
                     ':tag (lyskom-custom-string (elt args 0))
                     ':value t
                     ':format "%t"
                     ':match '(lambda (w v) v))
               (list 'item ':tag (lyskom-custom-string (elt args 1))
                     ':value nil
                     ':format "%t")))
   propl))


(defun lyskom-repeat-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'editable-list 
   (list ':format "%[%t%]\n%v%i"
	 ':entry-format "%i %d %v\n"
         ':args
         (list (lyskom-widget-convert-specification args var)))
   propl))

(defun lyskom-cons-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'cons
   (list ':format "%v"
         ':tag ""
         ':args (list (lyskom-widget-convert-specification (elt args 0) var)
                      (lyskom-widget-convert-specification (elt args 1) var)))
   propl))

(defun lyskom-list-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'list
   (list ':format "%v"
         ':tag ""
         ':args (mapcar (lambda (x) (lyskom-widget-convert-specification x var)) args))
   propl))

(defun lyskom-choice-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'menu-choice
   (list ':case-fold t
         ':format "%[%t%] %v"
         ':args
         (mapcar (lambda (x) (lyskom-widget-convert-specification x var)) args))
   propl))


(defun lyskom-string-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'lyskom-string 
   (list ':size 0 
         ':format "%[%t%] `%v'")
   propl))

(defun lyskom-symbol-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'symbol 
   (list ':size 0 
         ':format "%[%t%] `%v'")
   propl))


(defun lyskom-number-widget (type &optional args propl var)
  (if args
      (lyskom-build-simple-widget-spec
       'lyskom-number
       (list ':min-value (elt args 0)
             ':max-value (elt args 1)
	     ':value 0
             ':size 0)
       propl)
    (lyskom-build-simple-widget-spec
     'lyskom-number
     (list ':size 0 ':value 0)
     propl)))

;;;
;;; The ansaphone reply widget (whew!)

(defun lyskom-ansaphone-reply-widget (type &optional args propl var)
  (lyskom-build-simple-widget-spec
   'editable-list
   (list ':format "%t:\n%v%i\n"
         ':args
         `((group
            :args ((menu-choice :tag ,(lyskom-custom-string 'ar-message-type)
                                :format "%[%t%]: %v"
                                :args
                                ((item :value personal :tag ,(lyskom-custom-string 'ar-personal))
                                 (item :value group :tag ,(lyskom-custom-string 'ar-group))
                                 (item :value common :tag ,(lyskom-custom-string 'ar-alarm))
                                 (item :value nil :tag ,(lyskom-custom-string 'ar-any-type))))
                   (menu-choice :tag ,(lyskom-custom-string 'ar-sender)
                                :args
                                ((item :value nil :tag ,(lyskom-custom-string 'ar-any-sender))
                                 (editable-list :tag ,(lyskom-custom-string 'ar-specified-sender)
                                                :menu-tag ,(lyskom-custom-string 'ar-specified-sender)
                                                :format "%t:\n%v%i\n"
                                                :indent 14
                                                :args
                                                ((lyskom-name 
                                                  :format "%[%t%]: %v"
                                                  :tag ,(lyskom-custom-string 'ar-person))))))
                   (menu-choice :tag ,(lyskom-custom-string 'ar-recipient)
                                :args
                                ((item :value nil :tag ,(lyskom-custom-string 'ar-any-recipient))
                                 (editable-list :tag ,(lyskom-custom-string 'ar-specified-recipient)
                                                :menu-tag ,(lyskom-custom-string 'ar-specified-recipient)
                                                :format "%t:\n%v%i\n"
                                                :indent 14
                                                :args
                                                ((lyskom-name :tag ,(lyskom-custom-string 'ar-pers-or-conf)
                                                              :format "%[%t%]: %v"
                                                              :lyskom-predicate (pers conf))))))
                   (menu-choice :tag ,(lyskom-custom-string 'ar-message-text)
                                :args
                                ((item :value nil :tag ,(lyskom-custom-string 'ar-any-message))
                                 (lyskom-string :tag ,(lyskom-custom-string 'ar-matching-regexp)
                                                :size 0
                                                :format "%[%t%] `%v'\n")))
                   (menu-choice :tag ,(lyskom-custom-string 'ar-reply)
                                :args
                                ((item :value nil :tag ,(lyskom-custom-string 'ar-no-reply))
                                 (lyskom-string :tag ,(lyskom-custom-string 'ar-reply-text)
                                                :size 0
                                                :format "%[%t%] `%v'\n")))))))
   propl))


;;; ======================================================================
;;; Generic LysKOM widget functions
;;;

(defun lyskom-widget-value-get (widget)
  (widget-get widget ':value))

(defun lyskom-widget-value-delete (widget)
  (set-marker (widget-get widget ':value-from) nil)
  (set-marker (widget-get widget ':value-to) nil))
  

(defun lyskom-widget-value-to-external (widget value)
  value)

(defun lyskom-widget-value-to-internal (widget value)
  value)

(defun lyskom-widget-invalid-value (widget)
  (widget-put widget ':error 
              (lyskom-format (lyskom-custom-string 'invalid-value)
                             (widget-value widget)))
  widget)


;;;
;;; The Person Widget
;;;

(defun lyskom-widget-name-action (widget &optional event)
  (widget-value-set widget
                    (save-current-buffer
                      (set-buffer lyskom-buffer)
                      (lyskom-read-conf-no
                       (lyskom-custom-string
                        (cond ((widget-get widget ':lyskom-prompt)
                               (widget-get widget ':lyskom-prompt))
                              ((and 
                                (memq 'pers (widget-get widget
                                                        ':lyskom-predicate))
                                (memq 'conf (widget-get widget
                                                        ':lyskom-predicate)))
                               'which-conf-or-person)
                              ((memq 'pers (widget-get widget
                                                       ':lyskom-predicate))
                               'which-person)
                              ((memq 'conf (widget-get widget
                                                       ':lyskom-predicate))
                               'which-conf)
                              (t 'which-name)))
                       (widget-get widget ':lyskom-predicate)
                       nil "" t)))
  (widget-setup))
               
(defun lyskom-widget-name-value-create (widget)
  (let* ((size (widget-get widget ':size))
         (value (widget-get widget ':value))
         (from (point))
         (string (save-current-buffer
                   (set-buffer lyskom-buffer)
                   (cond ((or (null value) (eq 0 value)) "")
                         (t (or (conf-stat->name
                                 (blocking-do 'get-conf-stat value))
                                (lyskom-format
                                 (lyskom-custom-string 'some-person)
                                 value)))))))
    (if (null size)
        (insert string)
      (insert string)
      (if (< (lyskom-string-width value) size)
          (insert-char ?\  (- size (lyskom-string-width value)))))

    (widget-put widget ':value-from (copy-marker from))
    (widget-put widget ':value-to (copy-marker (point)))
    (set-marker-insertion-type (widget-get widget ':value-to) nil)
    (insert " ")))

(defun lyskom-widget-name-match (widget value)
  (and (numberp value)
       (>= value 0)))

(defun lyskom-widget-name-validate (widget)
  (let ((value (widget-value widget)))
    (if (and (numberp value)
             (>= value 0))
        nil
      (lyskom-widget-invalid-value widget))))

(define-widget 'lyskom-name 'default
  "A LysKOM person"
  ':format "%[[*]%] %v"
  ':default-help-echo 'change-this-name
  ':value 0
  ':lyskom-predicate '(pers)
  ':action 'lyskom-widget-name-action
  ':value-create 'lyskom-widget-name-value-create
  ':value-delete 'lyskom-widget-value-delete
  ':value-get 'lyskom-widget-value-get
  ':value-to-external 'lyskom-widget-value-to-external
  ':value-to-internal 'lyskom-widget-value-to-internal
  ':match 'lyskom-widget-name-match
  ':validate 'lyskom-widget-name-validate
)

(define-widget 'lyskom-name-list 'editable-list
  "A list of LysKOM commands"
  ':entry-format "%i %d %v"
  ':args '(lyskom-name))


;;;
;;; The Command Widget
;;;

(defun lyskom-widget-command-action (widget &optional event)
  (widget-value-set widget
		    (save-current-buffer
		      (set-buffer lyskom-buffer)
		      (lyskom-read-extended-command nil (concat 
                                                         (widget-get widget ':tag)
                                                         ": "))))
  (widget-setup))
               
(defun lyskom-widget-command-value-create (widget)
  (let* ((size (widget-get widget ':size))
         (value (widget-get widget ':value))
         (from (point))
         (string (save-current-buffer
                   (set-buffer lyskom-buffer)
                   (cond ((null value) "")
                         (t (or (lyskom-try-get-string value 'lyskom-command)
                                (lyskom-format
                                 (lyskom-custom-string 'unknown-command)
                                 (symbol-name value))))))))
    (if (null size)
        (insert string)
      (insert string)
      (if (< (lyskom-string-width value) size)
          (insert-char ?\  (- size (lyskom-string-width value)))))

    (widget-put widget ':value-from (copy-marker from))
    (widget-put widget ':value-to (copy-marker (point)))
    (set-marker-insertion-type (widget-get widget ':value-to) nil)
    (insert " ")))

(defun lyskom-widget-command-match (widget value)
  (symbolp value))

(defun lyskom-widget-command-validate (widget)
  (if (symbolp (widget-value widget))
      nil
    (lyskom-widget-invalid-value widget)))
  

(define-widget 'lyskom-command 'default
  "A LysKOM command"
  ':format "%[%t%] %v"
  ':default-help-echo 'select-command
  ':value 'kom-display-time
  ':action 'lyskom-widget-command-action
  ':value-create 'lyskom-widget-command-value-create
  ':value-delete 'lyskom-widget-value-delete
  ':value-get 'lyskom-widget-value-get
  ':value-to-external 'lyskom-widget-value-to-external
  ':value-to-internal 'lyskom-widget-value-to-internal
  ':match 'lyskom-widget-command-match
  ':validate 'lyskom-widget-command-validate
)

(define-widget 'lyskom-command-list 'editable-list
  "A list of LysKOM commands"
  ':entry-format "%i %d %v"
  ':args '(lyskom-command))



;;;
;;; A new String widget
;;;

(defun lyskom-widget-string-action (widget &optional event)
  (let ((tmp (lyskom-read-from-minibuffer 
              (format "%s " (widget-get widget ':tag))
              (widget-value widget))))
    (widget-value-set widget tmp)
    (widget-setup)))


(define-widget 'lyskom-string 'editable-field
  "A string"
  ':format "%[%t%] %v"
  ':action 'lyskom-widget-string-action)

;;;
;;; A Number widget
;;;

(defun lyskom-widget-number-action (widget &optional event)
  (let ((min (widget-get widget ':min-value))
        (max (widget-get widget ':max-value)))
    (widget-value-set
     widget
     (if (and min max)
         (lyskom-read-num-range min max
                                (widget-get widget ':tag)
                                t
                                (widget-value widget))
       (lyskom-read-number (widget-get widget ':tag)
                           (widget-value widget))))
    (widget-setup)))

(defun lyskom-widget-number-value-to-external (widget value)
  (cond ((stringp value) (lyskom-string-to-number value))
	((numberp value) value)
	((null value) 0)
	(t (error "unable to convert settings value"))))

(defun lyskom-widget-number-value-to-internal (widget value)
  (cond ((stringp value) value)
	((numberp value) (format "% 4d" value))
	((null value) "")
	(t (format "%S" value))))

(defun lyskom-widget-number-validate (widget)
  (if (numberp (widget-value widget))
      nil
    (lyskom-widget-invalid-value widget)))

(defun lyskom-widget-number-match (widget value)
  (if (and (widget-get widget ':max-value)
           (widget-get widget ':min-value))
      (and (numberp value)
           (>= value (widget-get widget ':min-value))
           (<= value (widget-get widget ':max-value)))
    (numberp value)))


(define-widget 'lyskom-number 'editable-field
  "A number"
  ':format "%[%t%] %v"
  ':action 'lyskom-widget-number-action
  ':value-to-external 'lyskom-widget-number-value-to-external
  ':value-to-internal 'lyskom-widget-number-value-to-internal
  ':match 'lyskom-widget-number-match
  ':validate 'lyskom-widget-number-validate)


;;;
;;; Keyboard macro widget
;;;

(defun lyskom-widget-kbd-macro-match (widget value)
  (or (stringp value)
      (vectorp value)))

(defun lyskom-widget-kbd-macro-validate (widget)
  (if (lyskom-widget-kbd-macro-match widget (widget-value widget))
      nil
    (lyskom-widget-invalid-value widget)))

(defun lyskom-widget-kbd-macro-value-create (widget)
  (let* ((size (widget-get widget ':size))
         (value (widget-get widget ':value))
         (from (point))
         (string (mapconcat 'single-key-description (append value nil) " ")))
    (if (null size)
        (insert string)
      (insert string)
      (if (< (lyskom-string-width string) size)
          (insert-char ?\  (- size (lyskom-string-width string)))))

    (widget-put widget ':value-from (copy-marker from))
    (widget-put widget ':value-to (copy-marker (point)))
    (set-marker-insertion-type (widget-get widget ':value-to) nil)
    (insert " ")))

;;;
;;; This is a truly disgusting piece of work. In Gnu Emacs it's not
;;; possible to simply read a keyboard macro in a recursive command
;;; loop and have the command loop end when macro definition time is
;;; up, so we start a macro definition, make the normal end-kbd-macro
;;; keys just exit the recursive command loop, go recursive and when
;;; the recursive loop is over, any which way, we see if a macro was
;;; defined and reset the keyboard bindings.
;;;

(defun lyskom-widget-end-kbd-macro ()
  (interactive)
  (exit-recursive-edit))

(defun lyskom-widget-kbd-macro-action (widget &optional event)
  (let ((tmp last-kbd-macro)
        (value nil))
    (save-window-excursion
      (unwind-protect
          (progn
            (mapc (function
                     (lambda (k)
                       (define-key global-map k 'lyskom-widget-end-kbd-macro)))
                    (where-is-internal 'end-kbd-macro))
            (if (widget-get widget ':macro-buffer)
                (pop-to-buffer (widget-get widget ':macro-buffer)))
            (start-kbd-macro nil)
            (recursive-edit))
        (mapc (function
                 (lambda (k)
                   (define-key global-map k 'end-kbd-macro)))
                (where-is-internal 'lyskom-widget-end-kbd-macro))
        (condition-case e
            (end-kbd-macro)
          (error (setq last-kbd-macro nil)))))
    (setq value
          (prog1 last-kbd-macro
            (setq last-kbd-macro tmp)))
    (if value
        (progn
          (widget-value-set widget value)
          (widget-setup)))))


(define-widget 'lyskom-kbd-macro 'default
  "A Keyboard Macro"
  ':action 'lyskom-widget-kbd-macro-action
  ':value ""
  ':format "%[%t%] %v"
  ':validate 'lyskom-widget-kbd-macro-validate
  ':value-create 'lyskom-widget-kbd-macro-value-create
  ':value-delete 'lyskom-widget-value-delete
  ':value-get 'lyskom-widget-value-get
  ':value-to-external 'lyskom-widget-value-to-external
  ':value-to-internal 'lyskom-widget-value-to-internal
  ':match 'lyskom-widget-kbd-macro-match)


;;;
;;; Mark association widget
;;;

(defun lyskom-widget-mark-association-action (widget &optional event)
  (widget-value-set
   widget
   (cons
    (lyskom-read-string (widget-get widget ':mark-key-prompt)
                        (car (widget-value widget)))
    (lyskom-read-num-range 0
                           255
                           (widget-get widget ':mark-value-prompt)
                           t
                           (cdr (widget-value widget)))))
  (widget-setup))

(defun lyskom-widget-mark-association-match (widget value)
  (and (consp value)
       (let ((s (car value))
             (i (cdr value)))
         (and (stringp s)
              (> (length s) 0)
              (integerp i)
              (>= i 0)
              (<= i 255)))))

(defun lyskom-widget-mark-association-validate (widget)
  (let ((value (widget-value widget)))
    (if (lyskom-widget-mark-association-match widget value)
        nil
      (lyskom-widget-invalid-value widget))))

(defun lyskom-widget-mark-association-value-create (widget)
  (let ((from (point))
        (value (widget-value widget)))
    (insert (format "%s <--> %s"
                    (car value)
                    (if (and (string= (car value) "...")
                             (= (cdr value) 0))
                        "..."
                      (cdr value))))
    (widget-put widget ':value-from (copy-marker from))
    (widget-put widget ':value-to (copy-marker (point)))))

(define-widget 'lyskom-mark-association 'default
  "A mark association."
  :tag "Mark association"
  :format "%[%t%] %v"
  :value '("..." . 0)
  :default-help-echo 'change-this-name
  :match 'lyskom-widget-mark-association-match
  :validate 'lyskom-widget-mark-association-validate
  :action 'lyskom-widget-mark-association-action
  :value-create 'lyskom-widget-mark-association-value-create
  :value-delete 'lyskom-widget-value-delete
  :value-get 'lyskom-widget-value-get
  :value-to-external 'lyskom-widget-value-to-external
  :value-to-internal 'lyskom-widget-value-to-internal)

;;;
;;; Help widget
;;;

(defun lyskom-widget-help-action (widget &optional event)
  (let* ((value (widget-get widget ':value))
         (spec (assq value (widget-get widget ':args)))
         (syms (car (cdr (memq spec (widget-get widget ':args))))))
    (if (null syms)
        (setq syms (car (widget-get widget ':args))))
    (widget-put widget ':help-echo (elt syms 3))
    (widget-value-set widget (car syms))
    (widget-setup)))


(defun lyskom-widget-help-format-handler (widget escape)
  (let* ((value (widget-get widget ':value))
         (spec  (assq value (widget-get widget ':args))))
    (cond ((eq escape ?D)
           (if (widget-get widget ':indent)
               (insert-char ?  (widget-get widget ':indent)))
           (insert (format (elt spec 1) (elt spec 2))))
          ((eq escape ?T)
           (if (widget-get widget ':indent)
               (insert-char ?  (widget-get widget ':indent)))
           (insert (elt spec 4)))
          (t (widget-default-format-handler widget escape)))))


(define-widget 'lyskom-widget-help 'push-button
  "A help widget"
  ':format "%[[%t]%] %D"
  ':action 'lyskom-widget-help-action
  ':format-handler 'lyskom-widget-help-format-handler)
