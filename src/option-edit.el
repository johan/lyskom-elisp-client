;;;;;
;;;;; $Id: option-edit.el,v 44.4 1996-10-27 15:57:44 davidk Exp $
;;;;; Copyright (C) 1991, 1996  Lysator Academic Computer Association.
;;;;;
;;;;; This file is part of the LysKOM server.
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
;;;; File: <File name>
;;;;
;;;; <Description>
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: option-edit.el,v 44.4 1996-10-27 15:57:44 davidk Exp $\n"))

;;; ======================================================================
;;; Require Per Abrahamsens widget package, version 0.991 or later.
;;;

(require 'widget)
(require 'widget-edit)

;;; ======================================================================
;;; Global variables

(defvar lyskom-widgets nil
  "List of widgets in the customize buffer.")

(defvar lyskom-customize-buffer-format
  '((lyskom bold centered)
    "\n\n"
    lyskom-doc
    "\n\n"
    section
    (look-and-feel-misc bold centered)
    section
    "\n"
    [kom-friends]
    [kom-deferred-printing]
    [kom-prompt-format]
    [kom-dashed-lines]
    [kom-continuous-scrolling]
    [kom-permissive-completion]
    [lyskom-prompt-text]
    [lyskom-prompt-executing-default-command-text]
    [kom-inhibit-typeahead]
    [kom-max-buffer-size]
    [kom-page-before-command]
    [kom-audio-player]
    [kom-idle-hide]
    [kom-presence-messages]
    [kom-presence-messages-in-buffer]
    [kom-show-where-and-what]
    [kom-customize-in-window]
    [kom-customize-format]
    
    "\n\n"
    section
    (reading bold centered)
    section
    "\n"
    [kom-read-depth-first]
    [kom-higher-priority-breaks]
    [kom-ding-on-priority-break]
    [kom-ding-on-new-letter]
    [kom-created-texts-are-read]
    [kom-show-footnotes-immediately]
    [kom-follow-comments-outside-membership]
    [kom-reading-puts-comments-in-pointers-last]
    [kom-print-number-of-unread-on-entrance]
    [kom-ding-on-wait-done]
    [kom-default-mark]
    [kom-membership-default-priority]
    [kom-prioritize-in-window]

    "\n\n"
    section
    (writing bold centered)
    section
    "\n"
    [kom-cite-string]
    [kom-write-texts-in-window]
    [kom-confirm-multiple-recipients]
    [kom-check-commented-author-membership]
    [lyskom-ding-on-no-subject]

    "\n\n"
    section
    (urls bold centered)
    section
    "\n"
    [kom-url-viewer-preferences]
    [kom-mosaic-command]
    [kom-netscape-command]

    "\n\n"
    section
    (personal-messages bold centered)
    section
    "\n"
    [kom-show-personal-messages-in-buffer]
    [kom-pop-personal-messages]
    [kom-default-message-recipient]
    "\n"
    (audio-cues bold centered)
    "\n"
    [kom-ding-on-common-messages]
    [kom-ding-on-group-messages]
    [kom-ding-on-personal-messages]

    "\n"
    (automatic-replies bold centered)
    "\n"
    [kom-ansaphone-record-messages]
    [kom-ansaphone-show-messages]
    [kom-ansaphone-default-reply]

    [lyskom-filter-outgoing-messages]
    
    "\n\n"
    section
    (remote-control bold centered)
    section
    "\n"
    [kom-remote-control]
    [kom-remote-controllers]
    [kom-self-control]

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
    (save-excursion
      (set-buffer lyskom-buffer)
      (while tmp
        (set (car (car tmp))
             (widget-value (cdr (car tmp))))
        (setq tmp (cdr tmp))))))

(defun lyskom-customize-send ()
  "Save variables to the server"
  (save-excursion
    (set-buffer lyskom-buffer)
    (lyskom-save-options (current-buffer)
                         (lyskom-get-string 'saving-settings)
                         (lyskom-get-string 'saving-settings-done)
                         (lyskom-get-string 'could-not-save-options))))

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

(defun kom-customize ()
  "Open the customize buffer"
  (interactive)
  (let ((buf (lyskom-associate-buffer 
              (get-buffer-create (lyskom-format
                                  (lyskom-custom-string 'buffer-name)
                                  lyskom-server-name)))))
    (unwind-protect
        (progn
          (lyskom-start-of-command 'kom-customize)
           (save-excursion
             (set-buffer buf)
             (lyskom-protect-environment
              (kill-all-local-variables))
             (make-local-variable 'lyskom-widgets)
             (setq lyskom-widgets nil)
             (let ((inhibit-read-only t))
               (erase-buffer))
             (condition-case err
                 (copy-face 'kom-active-face 'widget-button-face))
             (mapcar 'lyskom-custom-insert lyskom-customize-buffer-format)
             (use-local-map lyskom-customize-map)
             (widget-setup)
             (mapcar (function
                      (lambda (variable)
                        (widget-value-set (cdr variable)
                                          (save-excursion
                                            (set-buffer lyskom-buffer)
                                            (symbol-value (car variable))))))
                     lyskom-widgets)
             (widget-setup))
           (lyskom-display-buffer buf
                                  'kom-customize-in-window
                                  'kom-dont-restore-window-after-customize))
      (save-excursion
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
           (mapcar (function
                    (lambda (fn)
                      (funcall (intern (concat "lyskom-custom-insert-"
                                               (symbol-name fn)))
                               start end)))
                   (cdr w))))
        ((stringp w)
         (widget-insert w))
        ((vectorp w)
         (setq lyskom-widgets (cons (cons (aref w 0)
                                          (lyskom-create-widget (aref w 0)))
                                    lyskom-widgets)))))

(defun lyskom-custom-insert-bold (s e)
  (add-text-properties s e (list 'face 'bold)))

(defun lyskom-custom-insert-centered (s e)
  (save-excursion (goto-char s)
                  (center-line 1)))


(defun lyskom-custom-get-value (var)
  (save-excursion
    (set-buffer lyskom-buffer)
    (symbol-value var)))

(defun lyskom-custom-string (s)
  (lyskom-get-string s 'lyskom-custom-strings))


(defvar lyskom-custom-variables
  '((kom-emacs-knows-iso-8859-1 (toggle (yes no)))
    (kom-write-texts-in-window (open-window))
    (kom-prioritize-in-window (open-window))
    (kom-customize-in-window (open-window))
    (kom-customize-format (choice ((const (long-format long))
                                   (const (short-format short)))))
    (kom-prompt-format (string))
    (kom-cite-string (string))
    (kom-created-texts-are-read (toggle (yes no)))
    (kom-dont-restore-window-after-editing (noggle (yes no)))
    (kom-dont-restore-window-after-customize (noggle (yes no)))
    (kom-dont-restore-window-after-prioritize (noggle (yes no)))
    (kom-default-mark (choice ((number (0 255) 
                                       :tag selected-mark
                                       :format "%[%t%] (%v)"
                                       :size 0)
                               (const (ask nil)))))
    (kom-reading-puts-comments-in-pointers-last (toggle (before after)))
    (kom-dashed-lines (toggle (on off)))
    (kom-print-number-of-unread-on-entrance (toggle (yes no)))
    (kom-presence-messages      (choice ((const (on t))
              (repeat (person nil :tag name)
                      :tag some-persons
                      :menu-tag some-persons))))
    (kom-presence-messages-in-buffer
     (choice ((const (on t))
              (repeat (person nil :tag name)
                      :tag some-persons
                      :menu-tag some-persons))))
    (kom-show-where-and-what (toggle (yes no)))
    (kom-idle-hide (number))
    (kom-show-footnotes-immediately (toggle (yes no)))
    (kom-follow-comments-outside-membership (toggle (yes no)))
    (kom-read-depth-first (toggle (depth-first time-order)))
    (kom-continuous-scrolling  (toggle (on off)))
    (kom-deferred-printing (toggle (on off)))
    (kom-higher-priority-breaks (choice ((const (express-break express))
                                         (const (break t))
                                         (const (no-break nil)))))
    (kom-login-hook (repeat (command nil :tag command)))
    (kom-do-when-done (repeat (choice ((command nil :tag command)
                                       (kbd-macro nil :tag kbd-macro))
                                      :tag execute
                                      :format "%[%t%] %v")))
    (kom-page-before-command (choice ((const (page-none nil))
                                      (const (page-all t))
                                      (repeat (command nil
                                                       :tag command)
                                              :tag page-some
                                              :menu-tag page-some
                                              :value (kom-view-next-text)))
                                     :format "%[%t%]: %v"))
    (kom-permissive-completion (noggle (on off)))
    (kom-membership-default-priority
     (choice ((const (ask-every-time nil))
              (number (0 255)
                      :tag fixed-priority 
                      :format "%[%t%] (%v)"
                      :size 0))))
    (lyskom-prompt-text (string))
    (lyskom-prompt-executing-default-command-text (string))
    (kom-show-personal-messages-in-buffer
     (choice ((const (messages-in-lyskom-buffer t))
              (const (discard-messages nil))
              (string nil :tag in-named-buffer))))
    (kom-pop-personal-messages (toggle (yes no)))
    (kom-ding-on-new-letter (ding))
    (kom-ding-on-priority-break (ding))
    (kom-ding-on-wait-done (ding))
    (kom-ding-on-common-messages (ding))
    (kom-ding-on-group-messages (ding))
    (kom-ding-on-personal-messages (ding))
    (lyskom-ding-on-no-subject (ding))
    (kom-audio-player (file))
    (kom-default-message-recipient 
     (choice ((const (everybody-rcpt everybody))
              (const (group-rcpt group))
              (const (sender-rcpt sender)))))
    (lyskom-filter-outgoing-messages (noggle (yes no)))
    (kom-friends (repeat (person nil :tag name)))
    (kom-url-viewer-preferences (repeat (url-viewer nil :tag viewer-program)))
    (kom-mosaic-command (file))
    (kom-netscape-command (file))
    (kom-confirm-multiple-recipients
     (choice ((const (dont-check nil))
              (const (check-before-open before))
              (const (check-before-send after)
                     :match (lambda (w v) (and v (not (eq v 'before))))))))
    (kom-check-commented-author-membership (toggle (yes no)))
    (kom-inhibit-typeahead (noggle (yes no)))
    (kom-max-buffer-size (choice ((const (no-size-limit nil))
                                  (number nil :tag max-size-in-bytes))))

    (kom-ansaphone-record-messages (toggle (yes no)))
    (kom-ansaphone-show-messages (toggle (yes no)))
    (kom-ansaphone-default-reply (string nil :format "%[%t%]:\n%v"))
    (kom-remote-control (toggle (on off)))
    (kom-remote-controllers (repeat (person nil :tag name)))
    (kom-self-control (toggle (yes no)))

))

(defvar lyskom-widget-functions 
  '((toggle . lyskom-toggle-widget)
    (noggle . lyskom-toggle-widget-inverse)
    (ding   . lyskom-ding-widget)
    (choice . lyskom-choice-widget)
    (string . lyskom-string-widget)
    (number . lyskom-number-widget)
    (const .  lyskom-item-widget)
    (repeat . lyskom-repeat-widget)
    (kbd-macro . lyskom-kbd-macro-widget)
    (url-viewer . lyskom-url-viewer-widget)
    (open-window . lyskom-open-window-widget)
    (command . lyskom-command-widget)
    (person . lyskom-person-widget)
    (file . lyskom-file-widget)
))

(defun lyskom-create-widget (variable)
  (let* ((el (assq variable lyskom-custom-variables))
         (dummy (or el (error "Unknown variable: %S" variable)))
         (spec (lyskom-widget-convert-specification (car (cdr el))))
         (tag-sym (intern (concat (symbol-name variable) "-tag")))
         (doc-sym (intern (concat (symbol-name variable) "-doc")))
         (help-sym (intern (concat (symbol-name variable) "-help")))
         (value (save-excursion (set-buffer lyskom-buffer)
                                (symbol-value variable))))
    (setq spec 
          (cons (car spec)
                (append
                 (list ':tag
                       (lyskom-custom-string tag-sym)
                       ':value
                       (lyskom-custom-get-value variable)
                       ;; ':doc
                       ;; (lyskom-custom-string doc-sym)
                       )
                 (cdr spec))))
    (let ((widget (apply 'widget-create spec)))
      (widget-insert "  ")
      (widget-create 'lyskom-widget-help
                     ':value kom-customize-format
                     ':args
                     (list (list 'long
                                 "\n%s\n\n"
                                 (lyskom-custom-string doc-sym)
                                 "!")
                           (list 'short 
                                 "%s"
                                 ""
                                 "?"))
                     :format "%[[%T]%]\n%D")
      widget)))


(defun lyskom-widget-convert-specification (spec)
  "Convert a LysKOM widget specification to something widget-create accepts"
  (let ((convertfn (assq (car spec) lyskom-widget-functions)))
    (if (null convertfn)
        (error "Unknown widget type: %S" spec))
    (nconc (funcall (cdr convertfn)
                    (car spec)
                    (car (cdr spec))
                    (car (cdr (cdr spec))))
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


(defun lyskom-file-widget (type &optional args propl)
  (list 'file ':format "%[%t%]: %v" :size 0))

(defun lyskom-person-widget (type &optional args propl)
  (list 'lyskom-name))

(defun lyskom-command-widget (type &optional args propl)
  (list 'lyskom-command))

(defun lyskom-kbd-macro-widget (type &optional args propl)
  (list 'lyskom-kbd-macro ':macro-buffer lyskom-buffer))

(defun lyskom-item-widget (type &optional args propl)
  (list 'item
        ':format "%t"
        ':tag (lyskom-custom-string (elt args 0))
        ':value (elt args 1)))

(defun lyskom-url-viewer-widget (type &optional args propl)
  (list 'menu-choice 
        ':format "%v\n"
        ':case-fold t
        ':args
        (list (list 'item
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
                    ':value "mosaic"))))

(defun lyskom-open-window-widget (type &optional args propl)
  (list 'menu-choice
        ':case-fold t
        ':args
        (list (list 'item
                    ':tag (lyskom-custom-string 'other-window)
                    ':format "%t"
                    ':value 'other)
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
                    ':format "%t"
                    ':format "%[%t%]: `%v'"
                    ':value ""
                    ':size 0))))

(defun lyskom-ding-widget (type &optional args propl)
  (list 'menu-choice
        ':case-fold t
        ':args
        (list (list 'item 
                    ':tag (lyskom-custom-string 'turned-off)
                    ':value nil
                    ':format "%t"
                    ':match '(lambda (w v) (or (null v) (eq v 0))))
              (list 'lyskom-number
                    ':tag (lyskom-custom-string 'number-of-times)
                    ':value "1"
                    ':format "%[%t%]: (%v)"
                    ':size 0
                    ':min-value 1
                    ':max-value 255)
              (list 'lyskom-string
                    ':tag (lyskom-custom-string 'sound-file)
                    ':size 0))))

(defun lyskom-toggle-widget-inverse (type &optional args propl)
  (list 'menu-choice
        ':case-fold t
        ':args
        (list (list 'item ':tag (lyskom-custom-string (elt args 0))
                    ':value nil
                    ':format "%t")
              (list 'item 
                    ':tag (lyskom-custom-string (elt args 1))
                    ':value t
                    ':match '(lambda (w v) v)
                    ':format "%t"))))

(defun lyskom-toggle-widget (type &optional args propl)
  (list 'menu-choice
        ':case-fold t
        ':args
        (list (list 'item
                    ':tag (lyskom-custom-string (elt args 0))
                    ':value t
                    ':format "%t"
                    ':match '(lambda (w v) v))
              (list 'item ':tag (lyskom-custom-string (elt args 1))
                    ':value nil
                    ':format "%t"))))


(defun lyskom-repeat-widget (type &optional args propl)
  (list 'editable-list 
        ':format "%[%t%]:\n%v%i"
        ':args
        (list (lyskom-widget-convert-specification args))))

(defun lyskom-choice-widget (type &optional args propl)
  (list 'menu-choice
        ':case-fold t
        ':format "%[%t%]: %v"
        ':args
        (mapcar 'lyskom-widget-convert-specification args)))


(defun lyskom-string-widget (type &optional args propl)
  (list 'lyskom-string ':size 0 ':format "%[%t%]: `%v'"))


(defun lyskom-number-widget (type &optional args propl)
  (if args
      (list 'lyskom-number
            ':min-value (elt args 0)
            ':max-value (elt args 1)
            ':size 0)
    (list 'lyskom-number
          ':size 0)))



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
                    (save-excursion
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
                       nil
                       nil
                       t)))
  (widget-setup))
               
(defun lyskom-widget-name-value-create (widget)
  (let* ((size (widget-get widget ':size))
         (value (widget-get widget ':value))
         (from (point))
         (string (save-excursion
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
      (if (< (length value) size)
          (insert-char ?\  (- size (length value)))))

    (widget-put widget ':value-from (copy-marker from))
    (widget-put widget ':value-to (copy-marker (point)))
    (set-marker-insertion-type (widget-get widget ':value-to) nil)
    (if (null size)
        (insert ?\n)
      (insert ?\ ))))

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
		    (save-excursion
		      (set-buffer lyskom-buffer)
		      (lyskom-read-extended-command)))
  (widget-setup))
               
(defun lyskom-widget-command-value-create (widget)
  (let* ((size (widget-get widget ':size))
         (value (widget-get widget ':value))
         (from (point))
         (string (save-excursion
                   (set-buffer lyskom-buffer)
                   (cond ((null value) "")
                         (t (or (lyskom-get-string value 'lyskom-command)
                                (lyskom-format
                                 (lyskom-custom-string 'unknown-command)
                                 (symbol-name value))))))))
    (if (null size)
        (insert string)
      (insert string)
      (if (< (length value) size)
          (insert-char ?\  (- size (length value)))))

    (widget-put widget ':value-from (copy-marker from))
    (widget-put widget ':value-to (copy-marker (point)))
    (set-marker-insertion-type (widget-get widget ':value-to) nil)
    (if (null size)
        (insert ?\n)
      (insert ?\ ))))

(defun lyskom-widget-command-match (widget value)
  (symbolp value))

(defun lyskom-widget-command-validate (widget)
  (if (symbolp (widget-value widget))
      nil
    (lyskom-widget-invalid-value widget)))
  

(define-widget 'lyskom-command 'default
  "A LysKOM command"
  ':format "%[%t%]: %v"
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
  (let ((tmp (read-from-minibuffer 
              (format "%s: " (widget-get widget ':tag))
              (cons (widget-value widget) 1))))
    (widget-value-set widget tmp)
    (widget-setup)))


(define-widget 'lyskom-string 'editable-field
  "A string"
  ':format "%[%t%]: %v"
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
                                (concat (widget-get widget ':tag) ": ")
                                t
                                (widget-value widget))
       (lyskom-read-number (concat (widget-get widget ':tag) ": ")
                           (widget-value widget))))
    (widget-setup)))

(defun lyskom-widget-number-value-to-external (widget value)
  (string-to-int value))

(defun lyskom-widget-number-value-to-internal (widget value)
  (int-to-string value))

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
  ':format "%[%t%]: %v"
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
      (if (< (length string) size)
          (insert-char ?\  (- size (length string)))))

    (widget-put widget ':value-from (copy-marker from))
    (widget-put widget ':value-to (copy-marker (point)))
    (set-marker-insertion-type (widget-get widget ':value-to) nil)
    (if (null size)
        (insert ?\n)
      (insert ?\ ))))

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
            (mapcar (function
                     (lambda (k)
                       (define-key global-map k 'lyskom-widget-end-kbd-macro)))
                    (where-is-internal 'end-kbd-macro))
            (if (widget-get widget ':macro-buffer)
                (pop-to-buffer (widget-get widget ':macro-buffer)))
            (start-kbd-macro nil)
            (recursive-edit))
        (mapcar (function
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
  ':format "%[%t%]: %v"
  ':validate 'lyskom-widget-kbd-macro-validate
  ':value-create 'lyskom-widget-kbd-macro-value-create
  ':value-delete 'lyskom-widget-value-delete
  ':value-get 'lyskom-widget-value-get
  ':value-to-external 'lyskom-widget-value-to-external
  ':value-to-internal 'lyskom-widget-value-to-internal
  ':match 'lyskom-widget-kbd-macro-match)


(defun lyskom-widget-help-action (widget &optional event)
  (let* ((value (widget-get widget ':value))
         (syms (car (cdr (memq (assq value (widget-get widget ':args))
                               (widget-get widget ':args))))))
    (if (null syms)
        (setq syms (car (widget-get widget ':args))))
    (widget-value-set widget (car syms))
    (widget-setup)))


(defun lyskom-widget-help-format-handler (widget escape)
  (let* ((value (widget-get widget ':value))
         (spec  (assq value (widget-get widget ':args))))
    (cond ((eq escape ?D)
           (if (widget-get widget :indent)
               (insert-char ?  (widget-get widget :indent)))
           (insert (format (elt spec 1) (elt spec 2))))
          ((eq escape ?T)
           (if (widget-get widget :indent)
               (insert-char ?  (widget-get widget :indent)))
           (insert (elt spec 3)))
          (t (widget-default-format-handler widget escape)))))


(define-widget 'lyskom-widget-help 'push-button
  "A help widget"
  ':format "%[[%t]%] %D"
  ':action 'lyskom-widget-help-action
  ':format-handler 'lyskom-widget-help-format-handler)
