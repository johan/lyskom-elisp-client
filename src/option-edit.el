

;;;
;;; +++ The following is an UGLY HACK that will have to be removed
;;;     before release!
;;;

(eval-when-compile (setq load-path (cons "." load-path)))
(require 'widget)
(require 'widget-edit)

(defvar lyskom-widgets nil
  "List of widgets in the customize buffer.")

(defvar lyskom-customize-map nil
  "Keymap for the customize buffer")

(defvar kom-customize-in-window nil)
(defvar kom-customize-dont-restore-after-quit nil)

(defvar kom-dont-restore-window-after-customize nil
  "*Non-nil means don't restore the window configuration after customizing
LysKOM.")

(defvar lyskom-customize-window-configuration nil)

(if lyskom-customize-map
    nil
  (setq lyskom-customize-map (make-sparse-keymap))
  (set-keymap-parent lyskom-customize-map global-map)
  (define-key lyskom-customize-map "\t" 'widget-forward)
  (define-key lyskom-customize-map "\M-\t" 'widget-backward)
  (define-key lyskom-customize-map "\C-m" 'widget-button-press)
  (define-key lyskom-customize-map [mouse-2] 'widget-button-click)
  (define-key lyskom-customize-map "\C-c\C-c" 'lyskom-customize-save-and-quit)
  (define-key lyskom-customize-map "\C-c\C-k" 'lyskom-customize-quit)
  (define-key lyskom-customize-map "\C-c\C-s" 'lyskom-customize-save)
  (define-key lyskom-customize-map "\C-c\C-a" 'lyskom-customize-apply)
)

;;;
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
  (kill-buffer (current-buffer))
  (if kom-dont-restore-window-after-customize
      (bury-buffer)
    (set-window-configuration lyskom-customize-window-configuration)))

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
              (get-buffer-create "*LysKOM Customize*"))))
    (setq lyskom-customize-window-configuration (current-window-configuration))
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
                                  'kom-customize-dont-restore-after-quit))
      (save-excursion
        (set-buffer lyskom-buffer)
        (lyskom-end-of-command)))
    (goto-char (point-min))))

;;; ============================================================
;;; Non-user functions
;;;


(defun lyskom-custom-insert (w)
  (cond ((symbolp w)
         (widget-insert (lyskom-custom-string w)))
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
  (lyskom-get-string s lyskom-custom-strings))

(defvar lyskom-customize-buffer-format
  '((lyskom bold centered)
    "\n\n"
    lyskom-doc
    "\n\n"
    section
    (look-and-feel-misc bold centered)
    section
    "\n"
    [kom-page-before-command]
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
    [kom-show-where-and-what]
    [kom-customize-in-window]
    
    "\n\n"
    section
    (reading bold centered)
    section
    "\n"
    [kom-read-depth-first]
    [kom-higher-priority-breaks]
    [kom-ding-on-priority-break]
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
    [kom-dont-restore-window-after-editing]
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


(defvar lyskom-custom-variables
  '((kom-emacs-knows-iso-8859-1 (toggle (yes no)))
    (kom-write-texts-in-window (open-window))
    (kom-prioritize-in-window (open-window))
    (kom-customize-in-window (open-window))
    (kom-prompt-format (string))
    (kom-cite-string (string))
    (kom-created-texts-are-read (toggle (yes no)))
    (kom-dont-restore-window-after-editing (noggle (yes no)))
    (kom-default-mark (choice ((number (0 255) 
                                       :tag selected-mark
                                       :format "%[%t%] (%v)\n"
                                       :size 1)
                               (const (ask nil)))))
    (kom-reading-puts-comments-in-pointers-last (toggle (before after)))
    (kom-dashed-lines (toggle (on off)))
    (kom-print-number-of-unread-on-entrance (toggle (yes no)))
    (kom-presence-messages (toggle (on off)))
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
                                              :value (kom-view-next-text)))))
    (kom-permissive-completion (noggle (on off)))
    (kom-membership-default-priority
     (choice ((const (ask-every-time nil))
              (number (0 255)
                      :tag fixed-priority 
                      :format "%[%t%] (%v)\n"
                      :size 1))))
    (lyskom-prompt-text (string))
    (lyskom-prompt-executing-default-command-text (string))
    (kom-show-personal-messages-in-buffer
     (choice ((const (messages-in-lyskom-buffer t))
              (const (discard-messages nil))
              (string nil :tag in-named-buffer))))
    (kom-pop-personal-messages (toggle (yes no)))
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
         (value (save-excursion (set-buffer lyskom-buffer)
                                (symbol-value variable))))
    (setq spec 
          (cons (car spec)
                     (cons ':tag 
                           (cons (lyskom-custom-string tag-sym)
                                 (cons ':value 
                                       (cons (lyskom-custom-get-value variable)
                                             (cdr spec)))))))
    (let ((widget (apply 'widget-create spec)))
      (widget-insert "\n")
      (widget-insert (lyskom-custom-string doc-sym))
      (widget-insert "\n\n")
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
  (list 'file ':format "%[%t%]: %v"))

(defun lyskom-person-widget (type &optional args propl)
  (list 'lyskom-name))

(defun lyskom-command-widget (type &optional args propl)
  (list 'lyskom-command))

(defun lyskom-kbd-macro-widget (type &optional args propl)
  (list 'lyskom-kbd-macro))

(defun lyskom-item-widget (type &optional args propl)
  (list 'item
        ':tag (lyskom-custom-string (elt args 0))
        ':value (elt args 1)))

(defun lyskom-url-viewer-widget (type &optional args propl)
  (list 'menu-choice ':args
        (list (list 'item
                    ':tag (lyskom-custom-string 'netscape-viewer)
                    ':value "netscape")
              (list 'item
                    ':tag (lyskom-custom-string 'emacs-w3-viewer)
                    ':value "w3")
              (list 'item 
                    ':tag (lyskom-custom-string 'emacs-general-viewer)
                    ':value "emacs")
              (list 'item
                    ':tag (lyskom-custom-string 'emacs-dired-viewer)
                    ':value "dired")
              (list 'item 
                    ':tag (lyskom-custom-string 'emacs-mail-viewer)
                    ':value "mail-mode")
              (list 'item
                    ':tag (lyskom-custom-string 'emacs-telnet-viewer)
                    ':value "telnet-mode")
              (list 'item
                    ':tag (lyskom-custom-string 'mosaic-viewer)
                    ':value "mosaic"))))

(defun lyskom-open-window-widget (type &optional args propl)
  (list 'menu-choice
        ':args
        (list (list 'item
                    ':tag (lyskom-custom-string 'other-window)
                    ':value 'other)
              (list 'item 
                    ':tag (lyskom-custom-string 'new-frame)
                    ':value 'new-frame)
              (list 'item
                    ':tag (lyskom-custom-string 'lyskom-window)
                    ':value nil)
              (list 'editable-field
                    ':tag (lyskom-custom-string 'window-on-buffer)
                    ':format "%[%t%]: %v"
                    ':value ""))))

(defun lyskom-ding-widget (type &optional args propl)
  (list 'menu-choice
        ':args
        (list (list 'item 
                    ':tag (lyskom-custom-string 'turned-off)
                    ':value nil
                    ':match '(lambda (w v) (or (null v) (eq v 0))))
              (list 'lyskom-number
                    ':tag (lyskom-custom-string 'number-of-times)
                    ':value "1"
                    ':format "%[%t%]: (%v)\n"
                    ':size 1
                    ':min-value 1
                    ':max-value 255)
              (list 'lyskom-string
                    ':tag (lyskom-custom-string 'sound-file)))))

(defun lyskom-toggle-widget-inverse (type &optional args propl)
  (list 'menu-choice 
      ':args
      (list (list 'item ':tag (lyskom-custom-string (elt args 0)) ':value nil)
            (list 'item 
                  ':tag (lyskom-custom-string (elt args 1))
                  ':value t
                  ':match '(lambda (w v) v)))))

(defun lyskom-toggle-widget (type &optional args propl)
  (list 'menu-choice 
      ':args
      (list (list 'item
                  ':tag (lyskom-custom-string (elt args 0))
                  ':value t
                  ':match '(lambda (w v) v))
            (list 'item ':tag (lyskom-custom-string (elt args 1)) ':value nil))))


(defun lyskom-repeat-widget (type &optional args propl)
  (list 'editable-list 
        ':format "%[%t%]:\n%v%i"
        ':args
        (list (lyskom-widget-convert-specification args))))

(defun lyskom-choice-widget (type &optional args propl)
  (list 'menu-choice ':args
        (mapcar 'lyskom-widget-convert-specification args)))


(defun lyskom-string-widget (type &optional args propl)
  (list 'lyskom-string))


(defun lyskom-number-widget (type &optional args propl)
  (if args
      (list 'lyskom-number
            ':min-value (elt args 0)
            ':max-value (elt args 1))
    (list 'lyskom-number)))



(defvar lyskom-custom-strings
  '(
    ;;
    ;; Widget strings
    ;;

    (which-person . "Ange en person: ")
    (which-conf . "Ange ett m�te: ")
    (which-conf-or-person . "Ange en person eller ett m�te: ")
    (which-name . "Ange ett namn: ")
    (some-person . "Person %#1d")

    (unknown-command . "Ok�nt kommando (%#1s)")

    ;;
    ;; Strings that are used in types and so forth
    ;;

    (other-window . "N�got annat f�nster")
    (other-frame . "N�gon annan frame")
    (new-frame . "I en ny frame")
    (lyskom-window . "LysKOM-buffertens f�nster")
    (window-on-buffer . "Ett f�nster som visar bufferten")
    (on . "P�")
    (off . "Av")
    (yes . "Ja ")
    (no . "Nej")
    (turned-off . "Avslaget")
    (number-of-times . "N�gra g�nger")
    (sound-file . "Genom att spela en ljudfil")
    (selected-mark . "Markering")
    (ask . "Fr�ga varje g�ng")
    (before . "F�re texten")
    (after . "Efter texten")
    (depth-first . "I kommentarsordning")
    (time-order . "I tidsordning")
    (express-break . "Omedelbart efter de har skapats")
    (break . "Efter aktuell kommentarskedja")
    (no-break . "Efter aktuellt m�te")
    (command . "Kommando")
    (command-list . "Kommandolista")
    (name . "Namn")
    (page-none . "Aldrig")
    (page-all . "F�re varje kommando")
    (page-some . "F�re f�ljande kommandon")
    (ask-every-time . "Fr�ga varje g�ng")
    (fixed-priority . "Fast prioritet")
    (messages-in-lyskom-buffer . "I LysKOM-bufferten")
    (discard-messages . "Ingenstans - kasta bort dem!")
    (in-named-buffer . "I namngiven buffert")
    (everybody-rcpt . "Alla")
    (group-rcpt . "Mottagaren f�r senaste gruppmeddelande")
    (sender-rcpt . "Avs�ndaren f�r senaste meddelandet")
    (viewer-program . "WWW-l�sare")
    (netscape-viewer . "Netscape Navigator (alla)")
    (emacs-w3-viewer . "Emacs W3-mode (HTTP, Goper, FTP)")
    (emacs-general-viewer . "Emacs (FTP, Telnet, Mail)")
    (emacs-dired-viewer . "Emacs Dired (FTP)")
    (emacs-mail-viewer . "Emacs Mail-mode (Mail)")
    (emacs-telnet-viewer . "Emacs Telnet-mode (telnet)")
    (mosaic-viewer . "NCSA Mosaic (alla)")
    (dont-check . "Ingen bekr�ftelse")
    (check-before-open . "Bekr�fta innan inl�gget skrivs")
    (check-before-send . "Bekr�fta innan inl�gget s�nds")
    (no-size-limit . "Ingen begr�nsning")
    (max-size-in-bytes . "Begr�nsning (i bytes)")
    (execute . "Utf�r")
    (kbd-macro . "Tangentbordmakro")
    (command . "Kommando")
    (enter-kbd-macro . "Mata in tangentbordsmakro. Avsluta med %#1s")

    ;;
    ;; Misc doc strings
    ;;

    (lyskom . "Inst�llningar f�r LysKOM")
    (lyskom-doc . "")

    (section . "------------------------------------------------------------------------------\n")
    (look-and-feel-misc . "Diverse utseende och beteende\n")
    (reading .            "Beteende vid l�sning\n")
    (writing .            "Beteende vid skrivning\n")
    (urls .               "Hantering av URLer\n")
    (personal-messages .  "Hantering av personliga meddelanden\n")
    (remote-control .     "Fj�rrstyrning av LysKOM\n")
    (hooks .              "Hook-funktioner\n")

    (audio-cues .         "Ljudsignaler\n")
    (automatic-replies .  "Automatiskt svar\n")


    ;;
    ;; Doc strings for variables
    ;;

    (kom-emacs-knows-iso-8859-1-doc . "\
  P�slaget betyder att Emacs f�rv�ntas f�rst� ISO-8859-1. Avslaget inneb�r att
  swascii accepteras i kommandonamn med mera. Skall vara p�slaget.")

    (kom-bury-buffers-doc . "\
  Best�mmer hur bufferten hanteras n�r man g�r till ett annat KOM med N�sta
  LysKOM och liknande funktioner. P�slaget inneb�r att den aktuella bufferten
  l�ggs sist i buffertlistan n�r man byter LysKOM.")

    (kom-write-texts-in-window-doc . "\
  Best�mmer i vilket f�nster nya texter skrivs. Samma f�nster inneb�r att
  texter skrivs i samma f�nster som LysKOM-bufferten. Annat f�nster inneb�r att
  texter skrivs i ett annat f�nster; Emacs kan skapa ett nytt f�nster f�r
  �ndam�let. Genom att ange ett buffertnamn kommer texter alltid att skrivas
  i ett f�nster som f�r tillf�llet visar den namngivna bufferten.")

    (kom-prioritize-in-window-doc . "\
  Best�mmer i vilket f�nster man prioriterar om m�ten. Samma f�nster inneb�r
  att texter skrivs i samma f�nster som LysKOM-bufferten. Annat f�nster 
  inneb�r att texter skrivs i ett annat f�nster; Emacs kan skapa ett nytt 
  f�nster f�r �ndam�let. Genom att ange ett buffertnamn kommer texter alltid 
  att skrivas i ett f�nster som f�r tillf�llet visar den namngivna bufferten.")

    (kom-customize-in-window-doc . "\
  Best�mmer i vilket f�nster man g�r dessa inst�llningar. Samma f�nster inneb�r
  att texter skrivs i samma f�nster som LysKOM-bufferten. Annat f�nster 
  inneb�r att texter skrivs i ett annat f�nster; Emacs kan skapa ett nytt 
  f�nster f�r �ndam�let. Genom att ange ett buffertnamn kommer texter alltid 
  att skrivas i ett f�nster som f�r tillf�llet visar den namngivna bufferten.")

    (kom-prompt-format-doc . "\
  Format f�r LysKOM-prompten. Texten `%s' i denna str�ng ers�tts med den
  riktiga LysKOM-prompten. Till exempel g�r v�rdet `Johan, %s' att alla 
  prompter kommer att skrivas ut med texten `Johan, ' f�re.")
    
    (kom-cite-string-doc . "\
  Text som s�tts in f�re varje rad i ett citerat inl�gg.")

    (kom-created-texts-are-read-doc . "\
  P�slaget inneb�r att texter man sj�lv skapas automatiskt l�smarkeras. 
  Avslaget inneb�r att man f�r l�sa texter man sj�lv skapar.")

    (kom-dont-restore-window-after-editing-doc . "\
  Om detta �r p�slaget s� �terst�lls f�nsterkonfigurationen efter man har
  skickat in ett inl�gg till hur den s�g ut innan man b�rjade skriva 
  inl�gget.")

    (kom-default-mark-doc . "\
  Det markeringsv�rde som anv�nds f�r nya markeringar. Om inget markerings-
  v�rde �r valt fr�gar LysKOM varje g�ng man markerar en text. Till�tna
  v�rden �r mellan 0 och 255")

    (kom-reading-puts-comments-in-pointers-last-doc . "\
  Best�mmer om kommentarsl�nkar visas f�re eller efter en text.")

    (kom-dashed-lines-doc . "\
  Streckade linjer visas runt inl�ggstexten om detta �r p�slaget.")

    (kom-print-number-of-unread-on-entrance-doc . "\
  Om detta �r p�slaget s� visas antalet ol�sta i ett m�te n�r man g�r till 
  m�tet.")

    (kom-presence-messages-doc . "\
  Best�mmer om information om att andra personer har loggat in, loggat ut eller
  bytt namn skall visas. Normalt visas informationen i minibufferten, men det
  g�r att f� informationen i LysKOM-bufferten genom att pilla p� en annan
  variabel.")

    (kom-presence-messages-in-buffer-doc . "\
  Best�mmer om information om att personer har loggat in, loggat ut eller
  bytt namn skall visas i LysKOM-bufferten eller minibufferten. P�slaget 
  betyder att informationen visas i LysKOM-bufferten.")

    (kom-show-where-and-what-doc . "\
  Information i vilkalistan om varifr�n en person �r inloggad och vad personen
  g�r visas bara om detta �r p�slaget.")

    (kom-idle-hide-doc . "\
  I vilkalistan visas per default enbart de sessioner som har varit aktiva
  under den senaste tiden. Denna inst�llning best�mmer hur m�nga minuter
  en session f�r ha varit stilla och �nd� visas i vilkalistan.")

    (kom-show-footnotes-immediately-doc . "\
  Fotnoter visas normalt p� samma s�tt som kommentarer, men om detta �r
  p�slaget s� visas fotnoter direkt efter inl�gget de h�r till.")

    (kom-follow-comments-outside-membership-doc . "\
  Om detta �r p�slaget s� f�ljer LysKOM-klienten en kommentarskedja �ven om
  kommentarernas mottagare �r ett m�te man inte �r med i.")

    (kom-read-depth-first-doc . "\
  Det g�r att l�sa inl�gg i den ordning de skapades eller i den ordning de
  ligger i kommentarstr�det. Det naturliga �r oftast att l�sa i 
  kommentarsordning.")

    (kom-continuous-scrolling-doc . "\
  P�slaget inneb�r att klienten scrollar bufferten medan ny text s�tts in, s�
  man ser det senaste som har satts in. Detta fungerar bra om man har en
  snabb terminal, men kan vara v�rt att sl� av om terminalen �r l�ngsam, och
  scrollning i Emacs tar tid.")

    (kom-deferred-printing-doc . "\
  Om detta �r p�slaget s� kan LysKOM f�rdr�ja utskrifter av vissa saker, 
  som person- och m�tesnamn, vilket snabbar upp tiden mellan kommandon. ")

    (kom-higher-priority-breaks-doc . "\
  N�r texter kommer in till m�ten som har h�gre prioritet �n det man l�ser
  kan klienten l�ta en l�sa dessa texter omedelbart, efter den aktuella
  kommentarskedjan eller efter man har l�st ut m�tet.")

    (kom-login-hook-doc . "\
  Kommandon som skall k�ras n�r man loggar in, innan n�gon inmatning fr�n
  tangentbordet tas emot.")

    (kom-do-when-done-doc . "\
  Kommandon som skall k�ras n�r man har l�st ut alla inl�gg.")

    (kom-page-before-command-doc . "\
  LysKOM-bufferten kan scrollas f�re alla kommandon s� att text som s�tts in
  av kommandot hamnar �verst i f�nstret. Denna variabel talar om f�re vilka
  kommandon detta skall ske.")

    (kom-permissive-completion-doc . "\
  Om detta �r p�slaget s� kommer TAB bara att fylla ut namn p� inloggade
  personer n�r kommandot bara kan utf�ras p� inloggade personer (till
  exempel Status (f�r) session och S�nda messelande.) Om det �r avslaget
  kommer TAB att fylla ut �ven med namn p� personer som inte �r inloggade.")

    (kom-membership-default-priority-doc . "\
  Detta best�mmer hur prioriteten p� m�ten man g�r med i s�tts. Om det �r
  en siffra mellan 0 och 255 s� kommer nya medlemskap att f� den prioriteten.
  I annat fall kommer klienten att fr�ga f�r varje nytt medlemskap vilken
  prioritet det skall ha.")

    (lyskom-prompt-text-doc . "\
  Denna text s�tts efter den aktuella prompten, men byts ut om man trycker
  SPC f�r att k�ra kommandot i prompten.")

    (lyskom-prompt-executing-default-command-text-doc . "\
  Denna text s�tts efter prompten om man k�r kommandot i prompten.")

    (kom-show-personal-messages-in-buffer-doc . "\
  Denna inst�llning best�mmer var personliga, gruppmeddelanden och allm�nna
  meddelanden visas. Meddelanden kan antingen visas i LysKOM-bufferten, 
  kastas bort helt eller visas i en namngiven buffert.")

    (kom-pop-personal-messages-doc . "\
  Om personliga meddelanden skall visas i en egen buffert och denna inst�llning
  ocks� �r p�slagen s� kommer den bufferten att visas s� fort ett meddelande
  anl�nder.")

    (kom-ding-on-priority-break-doc . "\
  N�r det kommer ett inl�gg med h�gre prioritet �n det man f�r tillf�llet
  l�ser, och LysKOM vill bryta l�sordningen f�r att visa detta inl�gg s�
  best�mmer denna inst�llning hur detta signaleras med ljud.")

    (kom-ding-on-wait-done-doc . "\
  N�r man har gjort V�nta och det kommer ett inl�gg best�mmer denna inst�llning
  hur h�ndelsen signaleras med ljud.")

    (kom-ding-on-common-messages-doc . "\
  Denna int�llning best�mmer hur ljud anv�nds f�r att signalera att det har
  kommit ett allm�nt meddelande.")

    (kom-ding-on-group-messages-doc . "\
  Denna int�llning best�mmer hur ljud anv�nds f�r att signalera att det har
  kommit ett gruppmeddelande.")

    (kom-ding-on-personal-messages-doc . "\
  Denna int�llning best�mmer hur ljud anv�nds f�r att signalera att det har
  kommit ett personligt meddelande.")

    (lyskom-ding-on-no-subject-doc . "\
  Denna inst�llning best�mmer hur ljud anv�nds f�r att varna att man har
  gl�mt att skriva en �renderad.")

    (kom-audio-player-doc . "\
  Om man vill att LysKOM skall spela ljudfiler i st�llet f�r att pipa vid 
  olika tillf�llen m�ste denna inst�llning peka ut ett program som klarar
  att spela ljudfilerna. Programmet skall ta ett argument: namnet p� filen
  som skall spelas.")

    (kom-default-message-recipient-doc . "\
  Denna inst�llning best�mmer vem som kommer att vara defaultmottagare f�r
  personliga meddelande man skickar. Alternativen som finns �r att meddelanden
  per default �r alm�nna, att avs�ndaren f�r det senast mottagna meddelandet
  skall vara default eller att mottagaren f�r det senaste gruppmeddelandet
  (eller avs�ndaren av det senaste personliga eller alm�nna meddelandet) skall
  vara mottagare.")

    (lyskom-filter-outgoing-messages-doc . "\
  Om denna inst�llning �r p�slagen s� kommer meddelanden som skickas 
  automatiskt, till exempel automatiska svar och svar p� fj�rkotrollf�rs�k
  �ven att visas som om man hade skickat det manuellt.")

    (kom-friends-doc . "\
  Namnen p� personerna i denna lista kommer att visas med ett speciellt 
  utseende i LysKOM-bufferten.")

    (kom-url-viewer-preferences-doc . "\
  Denna inst�llning best�mmer vilken WWW-l�sare som i f�rsta hand skall 
  anv�ndas f�r att �ppna URLer som f�rekommer i LysKOM. Om den f�rsta l�saren 
  i listan inte klarar den typ av URL som man f�rs�ker �ppna s� anv�nds n�sta
  l�sare och s� vidare.")

    (kom-mosaic-command-doc . "\
  Denna inst�llning talar om vilket kommando som skall anv�ndas f�r att starta
  Mosaic.")

    (kom-netscape-command-doc . "\
  Denna inst�llning anger kommandot f�r att k�ra Netscape.")

    (kom-confirm-multiple-recipients-doc . "\
  N�r man f�rs�ker skriva ett inl�gg med mer �n en mottagare kan LysKOM be om
  en bekr�ftelse att alla mottagare skall vara med. Detta f�r att man skall
  undvika att skicka inl�gg till mottagare som inte l�ngre �r relevanta f�r
  inl�ggets inneh�ll. Kontrollen kan antingen inte ske alls, ske innan man
  b�rjar skriva inl�gget eller precis n�r man skall skicka in det.")

    (kom-check-commented-author-membership-doc . "\
  Om detta �r p�slaget kommer LysKOM att kontrollera att f�rfattaren av
  inl�gg man kommenterar �r medlem i n�gon av kommentarens mottagare. Om
  s� inte �r fallet f�r man fr�gan om denne skall l�ggas till som mottagare.")

    (kom-inhibit-typeahead-doc . "\
  Normalt sparas tangenttryckningar som g�rs medan klienten �r upptagen, och
  utf�rs n�r det blir m�jligt. Om denna inst�llning �r avslagen s� kommer
  LysKOM enbart att utf�ra kommandon som ges n�r klienten inte arbetar.")

    (kom-max-buffer-size-doc . "\
  Det �r m�jligt att begr�nsa LysKOM-buffertens storlek genom att ange hur
  stor den f�r bli i den h�r variabeln. Om bufferten blir f�r stor kommer
  information fr�n buffertens b�rjan att tas bort.")

    (kom-ansaphone-record-messages-doc . "\
  LysKOM kan \"spela in\" meddelanden som kommer n�r funktionen f�r automatiskt
  svar �r p�slagen. Denna inst�llning best�mmer om s� sker eller inte.")

    (kom-ansaphone-show-messages-doc . "\
  Om denna inst�llning �r p�slagen kommer LysKOM att visa inkomna personliga
  meddelanden �ven om automatiskt svar �r p�slaget.")

    (kom-ansaphone-default-reply-doc . "\
  Autmatsvararen skickar detta meddelande om inget annat meddelande har
  specificerats via n�gon annan mekanism.")

    (kom-remote-control-doc . "\
  P�slagen inneb�r att det g�r att fj�rrstyra klienten. Fj�rrstyrningen �r
  dock begr�nsad till vissa personer.")

    (kom-remote-controllers-doc . "\
  Personerna i denna lista f�r fj�rrstyra klienten.")

    (kom-self-control-doc . "\
  Om detta �r p�slaget f�r anv�ndaren som �r inloggad styra klienten fr�n
  en annan session. Detta �r ett alternativ till att l�gga in sig sj�lv i
  listan �ver till�tna fj�rrstyrare.")


    ;;
    ;; Tags for variables
    ;;
    
    (kom-emacs-knows-iso-8859-1-tag . "Emacs f�rst�r ISO-8859-1")
    (kom-bury-buffers-tag . "Begrav buffertar n�r man byter LysKOM")
    (kom-write-texts-in-window-tag . "Skriv texter i")
    (kom-prioritize-in-window-tag . "Prioritera m�ten i")
    (kom-customize-in-window-tag . "St�ll in LysKOM i")
    (kom-prompt-format-tag . "Promptformat")
    (kom-cite-string-tag . "Citatmarkering")
    (kom-created-texts-are-read-tag . "L�smarkera skapade texter")
    (kom-dont-restore-window-after-editing-tag . 
       "�terst�ll f�nster efter editering")
    (kom-default-mark-tag . "Defaultmarkering")
    (kom-reading-puts-comments-in-pointers-last-tag . "Kommentarsl�nkar visas")
    (kom-dashed-lines-tag . "Streckade linjer kring inl�ggstexten")
    (kom-print-number-of-unread-on-entrance-tag . 
       "Visa antalet ol�sta n�r man g�r till ett m�te")
    (kom-presence-messages-tag . "N�rvaromeddelanden p� eller av")
    (kom-presence-messages-in-buffer-tag . "N�rvaromeddelanden i LysKOM-bufferten")
    (kom-show-where-and-what-tag . 
       "Visa varifr�n personer �r inloggade och vad de g�r")
    (kom-idle-hide-tag . 
       "Antal minuter en session f�r vara inaktiv och �nd� visas")
    (kom-show-footnotes-immediately-tag .
       "Visa fotnoter omedelbart")
    (kom-follow-comments-outside-membership-tag .
       "F�lj kommentarskedjor utanf�r medlemskap")
    (kom-read-depth-first-tag . "L�sordning")
    (kom-continuous-scrolling-tag . "Omedelbar scrollning")
    (kom-deferred-printing-tag . "F�rdr�jda utskrifter")
    (kom-higher-priority-breaks-tag . "L�s prioriterade texter")
    (kom-login-hook-tag . "Kommandon som k�rs vid login")
    (kom-do-when-done-tag . "Kommandon som k�rs efter allt �r utl�st")
    (kom-page-before-command-tag . "Rensa sk�rmen")
    (kom-permissive-completion-tag . "Petig utfyllnad av namn")
    (kom-membership-default-priority-tag . "Prioritet f�r nya medlemskap")
    (lyskom-prompt-text-tag . "Text som avslutar aktuell prompt")
    (lyskom-prompt-executing-default-command-text-tag . "Text att avsluta prompten med n�r man trycker SPC")
    (kom-show-personal-messages-in-buffer-tag . "Personliga, grupp och allm�nna meddelanden visas")
    (kom-pop-personal-messages-tag . "Ploppa upp en buffert med personliga meddelanden n�r de kommer")
    (kom-ding-on-priority-break-tag . "Pip n�r det kommer prioriterade inl�gg")
    (kom-ding-on-wait-done-tag . "Pip n�r man har v�ntat klart")
    (kom-ding-on-common-messages-tag . "Pip n�r det kommer allm�nna meddelanden")
    (kom-ding-on-group-messages-tag . "Pip n�r det kommer gruppmeddelanden")
    (kom-ding-on-personal-messages-tag . "Pip n�r det kommer personliga meddelanden")
    (lyskom-ding-on-no-subject-tag . "Pip n�r man gl�mmer att skriva en �renderad")
    (kom-audio-player-tag . "Ljudspelarprogram")
    (kom-default-message-recipient-tag . "Defaultmottagare f�r meddelanden")
    (lyskom-filter-outgoing-messages-tag . "Visa automatiska meddelanden")
    (kom-friends-tag . "V�nner och bekanta")
    (kom-url-viewer-preferences-tag . "�ppna URLer med f�ljande program")
    (kom-mosaic-command-tag . "Kommando f�r att starta NCSA Mosaic")
    (kom-netscape-command-tag . "Kommando f�r att starta Netscape Navigator")
    (kom-confirm-multiple-recipients-tag . "Bekr�fta multipla mottagare")
    (kom-check-commented-author-membership-tag . "Kontrollerade den kommenterade f�rfattarens medlemskap")
    (kom-inhibit-typeahead-tag . "Buffra tangenttryckningar")
    (kom-max-buffer-size-tag . "Maximal buffertstorlek")
    (kom-ansaphone-record-messages-tag . "Spela in meddelanden i automatsvareren")
    (kom-ansaphone-show-messages-tag . "Visa meddelanden medan automatsvararen �r p�slagen")
    (kom-ansaphone-default-reply-tag . "Svarsmeddelande")
    (kom-remote-control-tag . "Fj�rrstyrning")
    (kom-remote-controllers-tag . "Till�tna fj�rrstyrare")
    (kom-self-control-tag . "Till�t fj�rrstyrning av mig sj�lv")

    )
)


;;;
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
                       (widget-get widget ':lyskom-predicate))))
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
    (and (numberp value)
         (>= value 0))))

(define-widget 'lyskom-name 'default
  "A LysKOM person"
  ':format "%[[*]%] %v"
  ':value 0
  '':lyskom-predicate '(pers)
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
                    (car-safe
                     (save-excursion
                       (set-buffer lyskom-buffer)
                       (lyskom-read-extended-command))))
  (widget-setup))
               
(defun lyskom-widget-command-value-create (widget)
  (let* ((size (widget-get widget ':size))
         (value (widget-get widget ':value))
         (from (point))
         (string (save-excursion
                   (set-buffer lyskom-buffer)
                   (cond ((null value) "")
                         (t (or (car-safe (cdr-safe
                                           (assq value lyskom-commands)))
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
    (symbolp (widget-value widget)))

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
  (numberp (widget-value widget)))

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
  ':value-get 'lyskom-widget-value-get
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
  (lyskom-widget-kbd-macro-match widget (widget-value widget)))

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
            (pop-to-buffer "kom.lysator.liu.se")
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
