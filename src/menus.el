;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: menus.el,v 44.32 2004-04-13 14:39:51 jhs Exp $
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
;;;; File: menus.el
;;;; Author: Niels Möller
;;;;
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: menus.el,v 44.32 2004-04-13 14:39:51 jhs Exp $\n"))

(lyskom-external-function set-buffer-menubar)
(lyskom-external-function popup-menu)
(lyskom-external-function add-submenu)


(def-kom-var lyskom-current-menu-category nil
  "Category of menus currently used in buffer"
  local)


(defvar lyskom-menu-template
  '((menu read
          ((item kom-view-next-text)
           (item kom-list-news)
           (item kom-list-summary)
           (item kom-go-to-conf)
           (hline)
           (menu review
                 ((item kom-view)
                  (item kom-review-next)
                  (item kom-review-last-normally-read)
                  (item kom-review-noconversion)
                  (item kom-review-converted)
                  (item kom-review-mail-headers)
                  (hline)
                  (item kom-find-root)
                  (item kom-view-commented-text)
                  (item kom-review-comments)
		  (item kom-review-cross-references)
                  (item kom-review-tree)
                  (item kom-find-root-review)
                  (hline)
                  (item kom-review-by-to)
                  (item kom-review-first)
                  (item kom-review-all)
                  (item kom-review-more)
                  (hline)
                  (item kom-make-review-mark-as-read)
                  (item kom-make-review-not-mark-as-read)
                  (hline)
                  (item kom-review-stack)
                  (item kom-review-backward)
                  (item kom-review-clear)))
           (menu unread
                 ((item kom-mark-unread)
                  (item kom-unread-last-normally-read)
                  (hline)
                  (item kom-unread-root)
                  (item kom-unread-commented-text)
                  (item kom-unread-comments)
                  (item kom-unread-tree)
                  (item kom-unread-root-review)
                  (hline)
                  (item kom-unread-by-to)
                  (item kom-unread-first)
                  (item kom-unread-all)
                  (item kom-unread-more)
                  (hline)
                  (item kom-unread-presentation)
                  (item kom-unread-faq)
                  (item kom-unread-server-faq)))
           (menu marks
                 ((item kom-mark-text)
                  (item kom-unmark-text)
                  (hline)
                  (item kom-list-marks)
                  (item kom-review-marked-texts)
                  (item kom-review-all-marked-texts)
                  (hline)
                  (item kom-unread-marked-texts)
                  (item kom-unread-all-marked-texts)))
           (menu filters
                 ((item kom-filter-author)
                  (item kom-filter-subject)
                  (item kom-filter-text)
                  (item kom-filter-recipient)
                  (hline)
                  (item kom-list-filters)
                  (item kom-filter-edit)))
           (hline)
           (item kom-save-text)
           (item kom-save-text-body)
           (hline)
           (item kom-compare-texts)
           (item kom-diff-texts)
           (hline)
           (item kom-postpone)
           (item kom-jump)
           (item kom-super-jump)
           (item kom-set-unread)))

    (menu write
          ((item kom-write-text)
           (item kom-send-letter)
           (item kom-write-comment)
           (item kom-private-answer)
           (item kom-write-footnote)
           (hline)
           (menu receivers
                 ((item kom-will-person-read-text)
                  (hline)
                  (item kom-add-recipient)
                  (item kom-add-copy)
                  (item kom-add-bcc)
                  (item kom-sub-recipient)
                  (hline)
                  (item kom-move-text)
                  (item kom-move-text-tree)))
           (menu commented
                 ((item kom-add-comment)
                  (item kom-sub-comment)
                  (item kom-add-footnote)
                  (item kom-sub-footnote)))
           (menu aux-items 
                 ((item kom-add-cross-reference)
                  (item kom-add-private-answer)
                  (item kom-add-no-comments)
                  (item kom-add-request-confirm)
                  (item kom-fast-reply)
                  (item kom-agree)))
           (hline)
           (item kom-next-kom)
           (item kom-next-unread-kom)
           (item kom-previous-kom)
           (hline)
           (item kom-set-session-priority)
           (hline)
           (item kom-send-message)))

    (menu conference
          ((item kom-go-to-conf)
           (item kom-go-to-next-conf)
           (hline)
           (item kom-review-presentation)
           (item kom-status-conf)
           (item kom-review-faq)
           (item kom-list-faqs)
           (hline)
           (item kom-list-conferences)
           (item kom-list-re)
           (item kom-list-new-conferences)
           (hline)
           (menu conf-admin
                 ((item kom-change-presentation)
                  (item kom-set-presentation)
                  (item kom-remove-presentation)
                  (hline)
                  (item kom-change-name)
                  (item kom-change-supervisor)
                  (item kom-set-garb-nice)
                  (item kom-set-super-conf)
                  (item kom-set-permitted-submitters)
                  (item kom-change-conf-type)
                  (item kom-limit-import)
                  (hline)
                  (item kom-change-conf-motd)
                  (item kom-set-motd-text)
                  (item kom-unset-conf-motd)
                  (hline)
                  (item kom-change-conf-faq)
                  (item kom-add-faq)
                  (item kom-del-faq)
                  (hline)
                  (item kom-sub-member)
                  (hline)
                  (item kom-create-conf)
                  (item kom-delete-conf)
                  (item kom-list-created-conferences)))
           (hline)
           (item kom-add-self)
           (item kom-sub-self)
           (item kom-add-member)
           (item kom-prioritize)))

    (menu person
          ((item kom-who-is-on)
           (item kom-who-is-on-in-conference)
           (item kom-who-is-present-in-conference)
           (item kom-who-is-on-and-friend)
           (hline)
           (item kom-list-persons)
           (item kom-list-re)
           (item kom-list-new-persons)
           (hline)
           (item kom-review-presentation)
           (item kom-status-person)
           (item kom-status-session)
           (item kom-list-created-conferences)
           (item kom-is-person-member-of-conference)
           (item kom-will-person-read-text)
           (hline)
           (menu membership
                 ((item kom-add-self)
                  (item kom-sub-self)
                  (hline)
                  (item kom-join-all-conferences)
                  (item kom-leave-all-conferences)
                  (hline)
                  (item kom-change-priority)
                  (item kom-change-message-flag)
                  (item kom-prioritize)))
           (menu pers-admin
                 ((item kom-change-presentation)
                  (item kom-change-name)
                  (item kom-change-parenthesis)
                  (item kom-change-password)
                  (item kom-redirect-comments)
                  (item kom-change-privileges)))
           (hline)
           (item kom-who-am-i)))

    (menu server
          ((item kom-status-server)
           (item kom-review-server-faq)
           (item kom-list-server-faqs)
           (hline)
           (menu server-admin
                 ((item kom-change-server-faq)
                  (item kom-add-server-faq)
                  (item kom-del-server-faq)
                  (hline)
                  (item kom-set-motd)
                  (item kom-remove-motd)
                  (hline)
                  (item kom-recommend-conference)
                  (hline)
                  (item kom-enable-adm-caps)
                  (item kom-disable-adm-caps)
                  (hline)
                  (item kom-shutdown-server)
                  (item kom-sync-database)))
           ))


    (menu other
          ((item kom-help)
           (menu language
                 ((item kom-change-global-language)
                  (item kom-change-local-language)))
           (item kom-where-is)
           (item kom-display-time)
           (item kom-calculate)
           (hline)
           (item kom-customize)
           (item kom-copy-options)
           (item kom-save-options)
           (item kom-show-user-area)
           (hline)
           (menu remote-control
                 ((item kom-remote-autoreply)
                  (item kom-remote-set-message)
                  (item kom-remote-list-messages)
                  (item kom-remote-erase-messages)
                  (item kom-remote-quit)))
           (menu autoreply
                 ((item kom-toggle-auto-reply)
                  (item kom-change-auto-reply)
                  (item kom-list-messages)
                  (item kom-erase-messages)))
           (hline)
           (item kom-keep-alive)
           (item kom-stop-keep-alive)
           (hline)
           (item kom-recover)
           (item kom-start-anew)
           (item kom-force-logout)
           (item kom-quit))))

  "The menus used in LysKOM.")

(defvar lyskom-popup-menu-template
  (` (menu lyskom ((,@ lyskom-menu-template))))
  "Popup-menu in the backgrouond of the LysKOM window")


(defvar lyskom-edit-menu-template
  '((menu lyskom
	  ((item kom-ispell-message)
           (item kom-edit-send)
	   (hline receiver-separator)
	   (item kom-edit-add-recipient)
	   (item kom-edit-add-copy)
           (item kom-edit-add-bcc)
           (item kom-edit-move-text)
           (item kom-edit-add-cross-reference)
           (hline special-separator)
           (item kom-edit-add-no-comments)
           (item kom-edit-add-personal-comments)
           (item kom-edit-add-read-confirm-request)
	   (hline comment-separator)
	   (item kom-edit-show-commented)
	   (hline send-separator)
	   (item kom-edit-quit))))
  "The menus for editing LysKOM messages.")

(defvar lyskom-menu-list '((lyskom-mode . lyskom-menu)
                           (lyskom-edit-mode . lyskom-edit-menu))
  "List of menu sets in LysKOM")

(defvar lyskom-menu nil
  "A keymap describing the LysKOM top menu.")

(defvar lyskom-edit-menu nil
  "A keymap the LysKOM menu in the edit buffer.")

(defvar lyskom-popup-menu nil
  "A keymap the LysKOM menu in the edit buffer.")

(defun lyskom-menu-guess-shortcuts (keymap &optional prefix result force)
  (lyskom-traverse-keymap
   (lambda (key binding)
     (when binding
       (let ((force (or force (assq key lyskom-swedish-bindings-reverse))))
         (if (keymapp binding)
             (setq result
                   (lyskom-menu-guess-shortcuts
                    (cond ((symbolp binding) (symbol-value binding))
                          (t binding))
                    (append prefix (list key))
                    result
                    force))
           (when force
             (setq key `(,@prefix ,key))
             (if (assq binding result)
                 (setcdr (assq binding result)
                         (cons (apply 'vector key)
                               (cdr (assq binding result))))
               (setq result (cons (cons binding 
                                        (list (apply 'vector key)))
                                  result))))))))
   keymap)
  result)

(defun lyskom-build-menus ()
  "Create menus according to LYSKOM-MENUS"
  (lyskom-xemacs-or-gnu (lyskom-build-menus-xemacs)
                        (lyskom-build-menus-gnu)))


(defun lyskom-build-menus-xemacs ()
  (setq lyskom-edit-menu (lyskom-define-menu-xemacs lyskom-edit-menu-template))
  (setq lyskom-popup-menu (lyskom-define-menu-xemacs
                               lyskom-popup-menu-template))
  (setq lyskom-menu (lyskom-define-menu-xemacs lyskom-menu-template)))


(defun lyskom-build-menus-gnu ()
  "Rebuilds the LysKOM menus"
  (setq lyskom-menu (make-sparse-keymap))
  (setq lyskom-edit-menu (make-sparse-keymap))
  (setq lyskom-popup-menu (make-sparse-keymap))
  (lyskom-define-menu-gnu lyskom-menu lyskom-menu-template)
  (lyskom-define-menu-gnu lyskom-edit-menu lyskom-edit-menu-template)
  (lyskom-define-menu-gnu lyskom-popup-menu
                          (list lyskom-popup-menu-template))
  (setq lyskom-popup-menu (lookup-key lyskom-popup-menu [lyskom])))

(defun lyskom-define-menu-xemacs (menus &optional specials)
  (let ((specials (or specials
                      (lyskom-menu-guess-shortcuts (current-local-map)))))
    (cond ((null (car menus)))
          ((listp (car menus))          ; Menu bar
           (mapcar (lambda (x) (lyskom-define-menu-xemacs x specials)) menus))
          ((eq (car menus) 'menu)       ; A menu
           (let ((menu-title (car (cdr menus)))
                 (menu-items (car (cdr (cdr menus)))))
             (cons (lyskom-get-menu-string menu-title)
                   (mapcar
		    (lambda (item)
		      (let ((type (car item))
			    (symbol (car (cdr item))))
			(cond ((eq type 'item)
			       (let ((shortcut nil))
				 (when (assq symbol specials)
				   (unless (lyskom-traverse key (cdr (assq symbol specials))
							    (unless (condition-case nil
									(eq (lookup-key (current-local-map) 
											key) symbol)
								      (error nil))
							      (lyskom-traverse-break t)))
				     (setq shortcut
					   (mapconcat 
					    (lambda (key)
					      (if (assq key lyskom-swedish-bindings-reverse)
						  (symbol-name 
						   (cdr (assq key lyskom-swedish-bindings-reverse)))
						(single-key-description key)))
					    (car (cdr (assq symbol specials)))
					    " "))))
				 (if shortcut
				     (vector (lyskom-get-menu-string symbol)
					     symbol
					     ':active t
					     ':keys shortcut)
				   (vector (lyskom-get-menu-string symbol) symbol ':active t))))
			      ((eq type 'hline)
			       "--:shadowEtchedIn")
			      ((eq type 'menu)
			       (lyskom-define-menu-xemacs item specials))
			      (t (error "Bad menu item: %S" item)))))
		    menu-items))))

          (t nil))))

(defun lyskom-define-menu-gnu (map menus &optional specials)
  (let ((specials (or specials
                      (lyskom-menu-guess-shortcuts (current-local-map)))))
    (when menus
      (lyskom-define-menu-gnu map (cdr menus) specials)
      (let ((type (car (car menus)))
            (symbol (car (cdr (car menus)))))
        (cond ((eq 'hline type)
               (define-key map (vector (lyskom-gensym)) '("--")))
              ((eq 'menu type)
               (let* ((name (lyskom-get-menu-string symbol))
                      (submap (make-sparse-keymap name)))
                 (define-key map (vector symbol)
                   (cons name submap))
                 (lyskom-define-menu-gnu submap
                                         (car (cdr (cdr (car menus))))
                                         specials)))
              ((eq 'item type)
               (let ((shortcut nil))
                 (when (assq symbol specials)
                   (unless (lyskom-traverse key (cdr (assq symbol specials))
                             (unless (condition-case nil
                                         (eq (lookup-key (current-local-map) 
                                                         key) symbol)
                                       (error nil))
                               (lyskom-traverse-break t)))
                       (setq shortcut
                             (mapconcat 
                              (lambda (key)
                                (if (assq key lyskom-swedish-bindings-reverse)
                                    (symbol-name 
                                     (cdr (assq key lyskom-swedish-bindings-reverse)))
                                  (single-key-description key)))
                              (car (cdr (assq symbol specials)))
                              " "))))
                 (if shortcut
                     (progn (when (memq window-system '(win32 mswindows w32))
                              (setq shortcut (encode-coding-string shortcut 'iso-8859-1)))
                            (define-key map (vector symbol)
                              `(menu-item ,(lyskom-get-menu-string symbol) ,symbol
                                          :keys ,shortcut)))
                   (define-key map (vector symbol)
                     (cons (lyskom-get-menu-string symbol) symbol)))))
              (t (error "Menu description invalid in lyskom-define-menu")))))))


(defun lyskom-get-menu-category (menu-category)
  (symbol-value (cdr (assq menu-category lyskom-menu-list))))

(defun lyskom-update-menus ()
  (lyskom-build-menus)
  (when (and (boundp 'lyskom-current-menu-category)
             lyskom-current-menu-category)
    (mapcar (lambda (mc)
	      (lyskom-set-menus mc (current-local-map)))
            lyskom-current-menu-category)))

(defun lyskom-set-menus (menu-category  keymap)
  (lyskom-xemacs-or-gnu (lyskom-set-menus-xemacs menu-category)
                        (lyskom-set-menus-gnu menu-category keymap)))

(defun lyskom-set-menus-gnu (menu-category keymap)
  "Update the menus"
  (define-key keymap [menu-bar] (lyskom-get-menu-category menu-category))
  (make-local-variable 'lyskom-current-menu-category)
  (if (not (boundp 'lyskom-current-menu-category))
      (setq lyskom-current-menu-category (list menu-category))
    (add-to-list 'lyskom-current-menu-category menu-category)))

(eval-when-compile (defvar default-menubar nil))

(defun lyskom-set-menus-xemacs (menu-category)
  "Update the menus"
  (make-local-variable 'current-menubar)
  (make-local-variable 'lyskom-current-menu-category)
  (set-buffer-menubar default-menubar)
  (mapcar (function
           (lambda (menu)
             (add-submenu nil menu)))
          (lyskom-get-menu-category menu-category))
  (setq lyskom-current-menu-category (list menu-category)))

;;;
;;; This function would have been completely unnecessary if Gnu Emacs
;;; didn't carry around an ancient version of popup-menu that is
;;; completely incompatible with XEmacs version of the same function. 
;;; Sometimes I hate elisp.
;;;

(defun lyskom-do-popup-menu (menu event)
  "Pop up a menu"
  (lyskom-xemacs-or-gnu 
   (popup-menu menu event)
   (let* ((result (nreverse (x-popup-menu (or event t)
                                          (list menu)))))
     (cond ((null result))
           ((listp (car result)) 
            (apply (car (car result))
                   (cdr (car result))))
           ((commandp (car result))
            (call-interactively (car result)))
           ((functionp (car result))
            (funcall (car result)))
           (t nil)))))


(defun lyskom-background-menu (pos event)
  "Pop up a menu with LysKOM commands and execute the selected command."
  (let* ((menu lyskom-popup-menu)
	 (result (lyskom-do-popup-menu menu event)))))




;;;;(Återse) Baklänges		   Addera extra kopiemottagare
;;;;Addera kommentar		   Addera medlem
;;;;Addera mottagare		   Automatsvar
;;;;Avmarkera (inlägg)		   Bli medlem i möte
;;;;Börja med nytt namn		   Endast läsa senaste
;;;;Filtrera författare		   Filtrera innehåll
;;;;Filtrera ärende			   Fjärrkontrollera automatsvar
;;;;Fjärrkontrollera avsluta	   Fjärrkontrollera lista meddelanden
;;;;Fjärrkontrollera radera meddelanden
;;;;Fjärrkontrollera ändra svarsmeddelande
;;;;Flytta inlägg			   Fotnot till inlägg
;;;;Få skäll			   Få uppmuntran
;;;;Gå till möte			   Gå till nästa möte
;;;;Hjälp				   Hoppa över alla kommentarer
;;;;Kasta ut en session		   Kommentera föregående inlägg
;;;;Kommentera inlägget		   Ladda ner fil
;;;;Ladda upp fil			   Lista (med) regexpar
;;;;Lista filarean			   Lista filter
;;;;Lista klienter			   Lista meddelanden
;;;;Lista medlemsskap		   Lista möten
;;;;Lista nyheter			   Lista personer
;;;;Lista ärenden			   Läsa nästa inlägg
;;;;Långsamma kommandon		   Markera (inlägg)
;;;;Personligt svar			   Personligt svar på föregående inlägg
;;;;Prioritera möten		   Radera inlägg
;;;;Radera meddelanden		   Se tiden
;;;;Skapa möte			   Skicka brev
;;;;Skriva ett inlägg		   Sluta
;;;;Snabba kommandon		   Spara text (på fil)
;;;;Spara variabler			   Status (för) möte
;;;;Status (för) person		   Status (för) session
;;;;Subtrahera kommentar		   Subtrahera mottagare
;;;;Superhoppa			   Sända meddelande
;;;;Sätt lapp på dörren		   Sätt läsnivå
;;;;Ta bort lapp på dörren		   Uppskjuta läsning
;;;;Uteslut medlem			   Utplåna
;;;;Utträda ur möte			   Var (är) jag
;;;;Vilka är inloggade		   Visa user-arean
;;;;Vänta på ett inlägg		   Ändra filter
;;;;Ändra livslängd			   Ändra lösenord
;;;;Ändra mötestyp			   Ändra namn
;;;;Ändra organisatör		   Ändra presentation
;;;;Ändra supermöte			   Ändra svarsmeddelande
;;;;Ändra tillåtna författare	   Ändra variabler
;;;;Återse alla			   Återse alla kommentarer
;;;;Återse alla kommentarer rekursivt  Återse alla markerade
;;;;Återse det föregående kommenterade
;;;;Återse det kommenterade		   Återse första
;;;;Återse hoppa			   Återse igen
;;;;Återse inlägg			   Återse lista
;;;;Återse markerade		   Återse nästa
;;;;Återse omodifierat		   Återse presentation
;;;;Återse senaste			   Återse träd
;;;;Återse urinlägget		   Återstarta kom
;;;;Övergå till administratörsmod

(eval-and-compile (provide 'lyskom-menus))

;;; menus.el ends here
