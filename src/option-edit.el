

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
    (which-conf . "Ange ett möte: ")
    (which-conf-or-person . "Ange en person eller ett möte: ")
    (which-name . "Ange ett namn: ")
    (some-person . "Person %#1d")

    (unknown-command . "Okänt kommando (%#1s)")

    ;;
    ;; Strings that are used in types and so forth
    ;;

    (other-window . "Något annat fönster")
    (other-frame . "Någon annan frame")
    (new-frame . "I en ny frame")
    (lyskom-window . "LysKOM-buffertens fönster")
    (window-on-buffer . "Ett fönster som visar bufferten")
    (on . "På")
    (off . "Av")
    (yes . "Ja ")
    (no . "Nej")
    (turned-off . "Avslaget")
    (number-of-times . "Några gånger")
    (sound-file . "Genom att spela en ljudfil")
    (selected-mark . "Markering")
    (ask . "Fråga varje gång")
    (before . "Före texten")
    (after . "Efter texten")
    (depth-first . "I kommentarsordning")
    (time-order . "I tidsordning")
    (express-break . "Omedelbart efter de har skapats")
    (break . "Efter aktuell kommentarskedja")
    (no-break . "Efter aktuellt möte")
    (command . "Kommando")
    (command-list . "Kommandolista")
    (name . "Namn")
    (page-none . "Aldrig")
    (page-all . "Före varje kommando")
    (page-some . "Före följande kommandon")
    (ask-every-time . "Fråga varje gång")
    (fixed-priority . "Fast prioritet")
    (messages-in-lyskom-buffer . "I LysKOM-bufferten")
    (discard-messages . "Ingenstans - kasta bort dem!")
    (in-named-buffer . "I namngiven buffert")
    (everybody-rcpt . "Alla")
    (group-rcpt . "Mottagaren för senaste gruppmeddelande")
    (sender-rcpt . "Avsändaren för senaste meddelandet")
    (viewer-program . "WWW-läsare")
    (netscape-viewer . "Netscape Navigator (alla)")
    (emacs-w3-viewer . "Emacs W3-mode (HTTP, Goper, FTP)")
    (emacs-general-viewer . "Emacs (FTP, Telnet, Mail)")
    (emacs-dired-viewer . "Emacs Dired (FTP)")
    (emacs-mail-viewer . "Emacs Mail-mode (Mail)")
    (emacs-telnet-viewer . "Emacs Telnet-mode (telnet)")
    (mosaic-viewer . "NCSA Mosaic (alla)")
    (dont-check . "Ingen bekräftelse")
    (check-before-open . "Bekräfta innan inlägget skrivs")
    (check-before-send . "Bekräfta innan inlägget sänds")
    (no-size-limit . "Ingen begränsning")
    (max-size-in-bytes . "Begränsning (i bytes)")
    (execute . "Utför")
    (kbd-macro . "Tangentbordmakro")
    (command . "Kommando")
    (enter-kbd-macro . "Mata in tangentbordsmakro. Avsluta med %#1s")

    ;;
    ;; Misc doc strings
    ;;

    (lyskom . "Inställningar för LysKOM")
    (lyskom-doc . "")

    (section . "------------------------------------------------------------------------------\n")
    (look-and-feel-misc . "Diverse utseende och beteende\n")
    (reading .            "Beteende vid läsning\n")
    (writing .            "Beteende vid skrivning\n")
    (urls .               "Hantering av URLer\n")
    (personal-messages .  "Hantering av personliga meddelanden\n")
    (remote-control .     "Fjärrstyrning av LysKOM\n")
    (hooks .              "Hook-funktioner\n")

    (audio-cues .         "Ljudsignaler\n")
    (automatic-replies .  "Automatiskt svar\n")


    ;;
    ;; Doc strings for variables
    ;;

    (kom-emacs-knows-iso-8859-1-doc . "\
  Påslaget betyder att Emacs förväntas förstå ISO-8859-1. Avslaget innebär att
  swascii accepteras i kommandonamn med mera. Skall vara påslaget.")

    (kom-bury-buffers-doc . "\
  Bestämmer hur bufferten hanteras när man går till ett annat KOM med Nästa
  LysKOM och liknande funktioner. Påslaget innebär att den aktuella bufferten
  läggs sist i buffertlistan när man byter LysKOM.")

    (kom-write-texts-in-window-doc . "\
  Bestämmer i vilket fönster nya texter skrivs. Samma fönster innebär att
  texter skrivs i samma fönster som LysKOM-bufferten. Annat fönster innebär att
  texter skrivs i ett annat fönster; Emacs kan skapa ett nytt fönster för
  ändamålet. Genom att ange ett buffertnamn kommer texter alltid att skrivas
  i ett fönster som för tillfället visar den namngivna bufferten.")

    (kom-prioritize-in-window-doc . "\
  Bestämmer i vilket fönster man prioriterar om möten. Samma fönster innebär
  att texter skrivs i samma fönster som LysKOM-bufferten. Annat fönster 
  innebär att texter skrivs i ett annat fönster; Emacs kan skapa ett nytt 
  fönster för ändamålet. Genom att ange ett buffertnamn kommer texter alltid 
  att skrivas i ett fönster som för tillfället visar den namngivna bufferten.")

    (kom-customize-in-window-doc . "\
  Bestämmer i vilket fönster man gör dessa inställningar. Samma fönster innebär
  att texter skrivs i samma fönster som LysKOM-bufferten. Annat fönster 
  innebär att texter skrivs i ett annat fönster; Emacs kan skapa ett nytt 
  fönster för ändamålet. Genom att ange ett buffertnamn kommer texter alltid 
  att skrivas i ett fönster som för tillfället visar den namngivna bufferten.")

    (kom-prompt-format-doc . "\
  Format för LysKOM-prompten. Texten `%s' i denna sträng ersätts med den
  riktiga LysKOM-prompten. Till exempel gör värdet `Johan, %s' att alla 
  prompter kommer att skrivas ut med texten `Johan, ' före.")
    
    (kom-cite-string-doc . "\
  Text som sätts in före varje rad i ett citerat inlägg.")

    (kom-created-texts-are-read-doc . "\
  Påslaget innebär att texter man själv skapas automatiskt läsmarkeras. 
  Avslaget innebär att man får läsa texter man själv skapar.")

    (kom-dont-restore-window-after-editing-doc . "\
  Om detta är påslaget så återställs fönsterkonfigurationen efter man har
  skickat in ett inlägg till hur den såg ut innan man började skriva 
  inlägget.")

    (kom-default-mark-doc . "\
  Det markeringsvärde som används för nya markeringar. Om inget markerings-
  värde är valt frågar LysKOM varje gång man markerar en text. Tillåtna
  värden är mellan 0 och 255")

    (kom-reading-puts-comments-in-pointers-last-doc . "\
  Bestämmer om kommentarslänkar visas före eller efter en text.")

    (kom-dashed-lines-doc . "\
  Streckade linjer visas runt inläggstexten om detta är påslaget.")

    (kom-print-number-of-unread-on-entrance-doc . "\
  Om detta är påslaget så visas antalet olästa i ett möte när man går till 
  mötet.")

    (kom-presence-messages-doc . "\
  Bestämmer om information om att andra personer har loggat in, loggat ut eller
  bytt namn skall visas. Normalt visas informationen i minibufferten, men det
  går att få informationen i LysKOM-bufferten genom att pilla på en annan
  variabel.")

    (kom-presence-messages-in-buffer-doc . "\
  Bestämmer om information om att personer har loggat in, loggat ut eller
  bytt namn skall visas i LysKOM-bufferten eller minibufferten. Påslaget 
  betyder att informationen visas i LysKOM-bufferten.")

    (kom-show-where-and-what-doc . "\
  Information i vilkalistan om varifrån en person är inloggad och vad personen
  gör visas bara om detta är påslaget.")

    (kom-idle-hide-doc . "\
  I vilkalistan visas per default enbart de sessioner som har varit aktiva
  under den senaste tiden. Denna inställning bestämmer hur många minuter
  en session får ha varit stilla och ändå visas i vilkalistan.")

    (kom-show-footnotes-immediately-doc . "\
  Fotnoter visas normalt på samma sätt som kommentarer, men om detta är
  påslaget så visas fotnoter direkt efter inlägget de hör till.")

    (kom-follow-comments-outside-membership-doc . "\
  Om detta är påslaget så följer LysKOM-klienten en kommentarskedja även om
  kommentarernas mottagare är ett möte man inte är med i.")

    (kom-read-depth-first-doc . "\
  Det går att läsa inlägg i den ordning de skapades eller i den ordning de
  ligger i kommentarsträdet. Det naturliga är oftast att läsa i 
  kommentarsordning.")

    (kom-continuous-scrolling-doc . "\
  Påslaget innebär att klienten scrollar bufferten medan ny text sätts in, så
  man ser det senaste som har satts in. Detta fungerar bra om man har en
  snabb terminal, men kan vara värt att slå av om terminalen är långsam, och
  scrollning i Emacs tar tid.")

    (kom-deferred-printing-doc . "\
  Om detta är påslaget så kan LysKOM fördröja utskrifter av vissa saker, 
  som person- och mötesnamn, vilket snabbar upp tiden mellan kommandon. ")

    (kom-higher-priority-breaks-doc . "\
  När texter kommer in till möten som har högre prioritet än det man läser
  kan klienten låta en läsa dessa texter omedelbart, efter den aktuella
  kommentarskedjan eller efter man har läst ut mötet.")

    (kom-login-hook-doc . "\
  Kommandon som skall köras när man loggar in, innan någon inmatning från
  tangentbordet tas emot.")

    (kom-do-when-done-doc . "\
  Kommandon som skall köras när man har läst ut alla inlägg.")

    (kom-page-before-command-doc . "\
  LysKOM-bufferten kan scrollas före alla kommandon så att text som sätts in
  av kommandot hamnar överst i fönstret. Denna variabel talar om före vilka
  kommandon detta skall ske.")

    (kom-permissive-completion-doc . "\
  Om detta är påslaget så kommer TAB bara att fylla ut namn på inloggade
  personer när kommandot bara kan utföras på inloggade personer (till
  exempel Status (för) session och Sända messelande.) Om det är avslaget
  kommer TAB att fylla ut även med namn på personer som inte är inloggade.")

    (kom-membership-default-priority-doc . "\
  Detta bestämmer hur prioriteten på möten man går med i sätts. Om det är
  en siffra mellan 0 och 255 så kommer nya medlemskap att få den prioriteten.
  I annat fall kommer klienten att fråga för varje nytt medlemskap vilken
  prioritet det skall ha.")

    (lyskom-prompt-text-doc . "\
  Denna text sätts efter den aktuella prompten, men byts ut om man trycker
  SPC för att köra kommandot i prompten.")

    (lyskom-prompt-executing-default-command-text-doc . "\
  Denna text sätts efter prompten om man kör kommandot i prompten.")

    (kom-show-personal-messages-in-buffer-doc . "\
  Denna inställning bestämmer var personliga, gruppmeddelanden och allmänna
  meddelanden visas. Meddelanden kan antingen visas i LysKOM-bufferten, 
  kastas bort helt eller visas i en namngiven buffert.")

    (kom-pop-personal-messages-doc . "\
  Om personliga meddelanden skall visas i en egen buffert och denna inställning
  också är påslagen så kommer den bufferten att visas så fort ett meddelande
  anländer.")

    (kom-ding-on-priority-break-doc . "\
  När det kommer ett inlägg med högre prioritet än det man för tillfället
  läser, och LysKOM vill bryta läsordningen för att visa detta inlägg så
  bestämmer denna inställning hur detta signaleras med ljud.")

    (kom-ding-on-wait-done-doc . "\
  När man har gjort Vänta och det kommer ett inlägg bestämmer denna inställning
  hur händelsen signaleras med ljud.")

    (kom-ding-on-common-messages-doc . "\
  Denna intällning bestämmer hur ljud används för att signalera att det har
  kommit ett allmänt meddelande.")

    (kom-ding-on-group-messages-doc . "\
  Denna intällning bestämmer hur ljud används för att signalera att det har
  kommit ett gruppmeddelande.")

    (kom-ding-on-personal-messages-doc . "\
  Denna intällning bestämmer hur ljud används för att signalera att det har
  kommit ett personligt meddelande.")

    (lyskom-ding-on-no-subject-doc . "\
  Denna inställning bestämmer hur ljud används för att varna att man har
  glömt att skriva en ärenderad.")

    (kom-audio-player-doc . "\
  Om man vill att LysKOM skall spela ljudfiler i stället för att pipa vid 
  olika tillfällen måste denna inställning peka ut ett program som klarar
  att spela ljudfilerna. Programmet skall ta ett argument: namnet på filen
  som skall spelas.")

    (kom-default-message-recipient-doc . "\
  Denna inställning bestämmer vem som kommer att vara defaultmottagare för
  personliga meddelande man skickar. Alternativen som finns är att meddelanden
  per default är almänna, att avsändaren för det senast mottagna meddelandet
  skall vara default eller att mottagaren för det senaste gruppmeddelandet
  (eller avsändaren av det senaste personliga eller almänna meddelandet) skall
  vara mottagare.")

    (lyskom-filter-outgoing-messages-doc . "\
  Om denna inställning är påslagen så kommer meddelanden som skickas 
  automatiskt, till exempel automatiska svar och svar på fjärkotrollförsök
  även att visas som om man hade skickat det manuellt.")

    (kom-friends-doc . "\
  Namnen på personerna i denna lista kommer att visas med ett speciellt 
  utseende i LysKOM-bufferten.")

    (kom-url-viewer-preferences-doc . "\
  Denna inställning bestämmer vilken WWW-läsare som i första hand skall 
  användas för att öppna URLer som förekommer i LysKOM. Om den första läsaren 
  i listan inte klarar den typ av URL som man försöker öppna så används nästa
  läsare och så vidare.")

    (kom-mosaic-command-doc . "\
  Denna inställning talar om vilket kommando som skall användas för att starta
  Mosaic.")

    (kom-netscape-command-doc . "\
  Denna inställning anger kommandot för att köra Netscape.")

    (kom-confirm-multiple-recipients-doc . "\
  När man försöker skriva ett inlägg med mer än en mottagare kan LysKOM be om
  en bekräftelse att alla mottagare skall vara med. Detta för att man skall
  undvika att skicka inlägg till mottagare som inte längre är relevanta för
  inläggets innehåll. Kontrollen kan antingen inte ske alls, ske innan man
  börjar skriva inlägget eller precis när man skall skicka in det.")

    (kom-check-commented-author-membership-doc . "\
  Om detta är påslaget kommer LysKOM att kontrollera att författaren av
  inlägg man kommenterar är medlem i någon av kommentarens mottagare. Om
  så inte är fallet får man frågan om denne skall läggas till som mottagare.")

    (kom-inhibit-typeahead-doc . "\
  Normalt sparas tangenttryckningar som görs medan klienten är upptagen, och
  utförs när det blir möjligt. Om denna inställning är avslagen så kommer
  LysKOM enbart att utföra kommandon som ges när klienten inte arbetar.")

    (kom-max-buffer-size-doc . "\
  Det är möjligt att begränsa LysKOM-buffertens storlek genom att ange hur
  stor den får bli i den här variabeln. Om bufferten blir för stor kommer
  information från buffertens början att tas bort.")

    (kom-ansaphone-record-messages-doc . "\
  LysKOM kan \"spela in\" meddelanden som kommer när funktionen för automatiskt
  svar är påslagen. Denna inställning bestämmer om så sker eller inte.")

    (kom-ansaphone-show-messages-doc . "\
  Om denna inställning är påslagen kommer LysKOM att visa inkomna personliga
  meddelanden även om automatiskt svar är påslaget.")

    (kom-ansaphone-default-reply-doc . "\
  Autmatsvararen skickar detta meddelande om inget annat meddelande har
  specificerats via någon annan mekanism.")

    (kom-remote-control-doc . "\
  Påslagen innebär att det går att fjärrstyra klienten. Fjärrstyrningen är
  dock begränsad till vissa personer.")

    (kom-remote-controllers-doc . "\
  Personerna i denna lista får fjärrstyra klienten.")

    (kom-self-control-doc . "\
  Om detta är påslaget får användaren som är inloggad styra klienten från
  en annan session. Detta är ett alternativ till att lägga in sig själv i
  listan över tillåtna fjärrstyrare.")


    ;;
    ;; Tags for variables
    ;;
    
    (kom-emacs-knows-iso-8859-1-tag . "Emacs förstår ISO-8859-1")
    (kom-bury-buffers-tag . "Begrav buffertar när man byter LysKOM")
    (kom-write-texts-in-window-tag . "Skriv texter i")
    (kom-prioritize-in-window-tag . "Prioritera möten i")
    (kom-customize-in-window-tag . "Ställ in LysKOM i")
    (kom-prompt-format-tag . "Promptformat")
    (kom-cite-string-tag . "Citatmarkering")
    (kom-created-texts-are-read-tag . "Läsmarkera skapade texter")
    (kom-dont-restore-window-after-editing-tag . 
       "Återställ fönster efter editering")
    (kom-default-mark-tag . "Defaultmarkering")
    (kom-reading-puts-comments-in-pointers-last-tag . "Kommentarslänkar visas")
    (kom-dashed-lines-tag . "Streckade linjer kring inläggstexten")
    (kom-print-number-of-unread-on-entrance-tag . 
       "Visa antalet olästa när man går till ett möte")
    (kom-presence-messages-tag . "Närvaromeddelanden på eller av")
    (kom-presence-messages-in-buffer-tag . "Närvaromeddelanden i LysKOM-bufferten")
    (kom-show-where-and-what-tag . 
       "Visa varifrån personer är inloggade och vad de gör")
    (kom-idle-hide-tag . 
       "Antal minuter en session får vara inaktiv och ändå visas")
    (kom-show-footnotes-immediately-tag .
       "Visa fotnoter omedelbart")
    (kom-follow-comments-outside-membership-tag .
       "Följ kommentarskedjor utanför medlemskap")
    (kom-read-depth-first-tag . "Läsordning")
    (kom-continuous-scrolling-tag . "Omedelbar scrollning")
    (kom-deferred-printing-tag . "Fördröjda utskrifter")
    (kom-higher-priority-breaks-tag . "Läs prioriterade texter")
    (kom-login-hook-tag . "Kommandon som körs vid login")
    (kom-do-when-done-tag . "Kommandon som körs efter allt är utläst")
    (kom-page-before-command-tag . "Rensa skärmen")
    (kom-permissive-completion-tag . "Petig utfyllnad av namn")
    (kom-membership-default-priority-tag . "Prioritet för nya medlemskap")
    (lyskom-prompt-text-tag . "Text som avslutar aktuell prompt")
    (lyskom-prompt-executing-default-command-text-tag . "Text att avsluta prompten med när man trycker SPC")
    (kom-show-personal-messages-in-buffer-tag . "Personliga, grupp och allmänna meddelanden visas")
    (kom-pop-personal-messages-tag . "Ploppa upp en buffert med personliga meddelanden när de kommer")
    (kom-ding-on-priority-break-tag . "Pip när det kommer prioriterade inlägg")
    (kom-ding-on-wait-done-tag . "Pip när man har väntat klart")
    (kom-ding-on-common-messages-tag . "Pip när det kommer allmänna meddelanden")
    (kom-ding-on-group-messages-tag . "Pip när det kommer gruppmeddelanden")
    (kom-ding-on-personal-messages-tag . "Pip när det kommer personliga meddelanden")
    (lyskom-ding-on-no-subject-tag . "Pip när man glömmer att skriva en ärenderad")
    (kom-audio-player-tag . "Ljudspelarprogram")
    (kom-default-message-recipient-tag . "Defaultmottagare för meddelanden")
    (lyskom-filter-outgoing-messages-tag . "Visa automatiska meddelanden")
    (kom-friends-tag . "Vänner och bekanta")
    (kom-url-viewer-preferences-tag . "Öppna URLer med följande program")
    (kom-mosaic-command-tag . "Kommando för att starta NCSA Mosaic")
    (kom-netscape-command-tag . "Kommando för att starta Netscape Navigator")
    (kom-confirm-multiple-recipients-tag . "Bekräfta multipla mottagare")
    (kom-check-commented-author-membership-tag . "Kontrollerade den kommenterade författarens medlemskap")
    (kom-inhibit-typeahead-tag . "Buffra tangenttryckningar")
    (kom-max-buffer-size-tag . "Maximal buffertstorlek")
    (kom-ansaphone-record-messages-tag . "Spela in meddelanden i automatsvareren")
    (kom-ansaphone-show-messages-tag . "Visa meddelanden medan automatsvararen är påslagen")
    (kom-ansaphone-default-reply-tag . "Svarsmeddelande")
    (kom-remote-control-tag . "Fjärrstyrning")
    (kom-remote-controllers-tag . "Tillåtna fjärrstyrare")
    (kom-self-control-tag . "Tillät fjärrstyrning av mig själv")

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
