(setq lyskom-menus
      '((menu lyskom
	      ((menu read ((item kom-view-next-text)
			   hline
			   (item kom-view-commented-text)
			   (item kom-view-previous-commented-text)
			   (item kom-review-comments)
			   (item kom-review-tree)
			   (item kom-find-root)))
	       (menu dont-read((item kom-jump)
			       (item kom-set-unread)))
	       (menu write ((item kom-write-text)
			    (item kom-send-letter)
			    (item kom-write-comment)
			    (item kom-comment-previous)
			    hline
			    (item kom-send-message)))
	       (menu move ((item kom-go-to-conf)
			   (item kom-go-to-next-conf)))
	       (menu info ((item kom-who-is-on)
			   (item kom-list-news)
			   hline
			   (item kom-status-person)
			   (item kom-status-conf)
			   (item kom-review-presentation)
			   hline
			   (item kom-list-conferences)
			   (item kom-list-persons)))))))

(setq lyskom-menu-names
      '((lyskom . "Lyskom")
	(read . "L�s")
	(dont-read . "Hoppa")
	(write . "Skriv")
	(move . "G�")
	(info . "Om")))

(setq lyskom-menu-map (make-sparse-keymap))

(defun lyskom-build-menus ()
  (define-key lyskom-mode-map [menu-bar]
    lyskom-menu-map)
  (lyskom-define-menu lyskom-menu-map lyskom-menus))

(defun lyskom-define-menu (map menus)
  (when menus
    (lyskom-define-menu map (cdr menus))
    (cond ((eq 'hline (car menus))
	   ; ignore
	   t)
	  ((eq 'menu (car (car menus)))
	   (let* ((symbol (car (cdr (car menus))))
		  (name (lyskom-get-string symbol lyskom-menu-names))
		  (submenu (car (cdr (cdr (car menus)))))
		  (submap (make-sparse-keymap name)))
	     (define-key map (vector symbol)
	       (cons name submap))
	     (lyskom-define-menu submap submenu)))
	  ((eq 'item (car (car menus)))
	   (let* ((symbol (car (cdr (car menus))))
		  (name (car (lyskom-get-string symbol lyskom-commands))))
	     (define-key map (vector symbol)
	       (cons name symbol))))
	  (t (error "Menu description invalid in lyskom-define-menu")))))

		   
	      



;;;;(�terse) Bakl�nges		   Addera extra kopiemottagare
;;;;Addera kommentar		   Addera medlem
;;;;Addera mottagare		   Automatsvar
;;;;Avmarkera (inl�gg)		   Bli medlem i m�te
;;;;B�rja med nytt namn		   Endast l�sa senaste
;;;;Filtrera f�rfattare		   Filtrera inneh�ll
;;;;Filtrera �rende			   Fj�rrkontrollera automatsvar
;;;;Fj�rrkontrollera avsluta	   Fj�rrkontrollera lista meddelanden
;;;;Fj�rrkontrollera radera meddelanden
;;;;Fj�rrkontrollera �ndra svarsmeddelande
;;;;Flytta inl�gg			   Fotnot till inl�gg
;;;;F� sk�ll			   F� uppmuntran
;;;;G� till m�te			   G� till n�sta m�te
;;;;Hj�lp				   Hoppa �ver alla kommentarer
;;;;Kasta ut en session		   Kommentera f�reg�ende inl�gg
;;;;Kommentera inl�gget		   Ladda ner fil
;;;;Ladda upp fil			   Lista (med) regexpar
;;;;Lista filarean			   Lista filter
;;;;Lista klienter			   Lista meddelanden
;;;;Lista medlemsskap		   Lista m�ten
;;;;Lista nyheter			   Lista personer
;;;;Lista �renden			   L�sa n�sta inl�gg
;;;;L�ngsamma kommandon		   Markera (inl�gg)
;;;;Personligt svar			   Personligt svar p� f�reg�ende inl�gg
;;;;Prioritera m�ten		   Radera inl�gg
;;;;Radera meddelanden		   Se tiden
;;;;Skapa m�te			   Skicka brev
;;;;Skriva ett inl�gg		   Sluta
;;;;Snabba kommandon		   Spara text (p� fil)
;;;;Spara variabler			   Status (f�r) m�te
;;;;Status (f�r) person		   Status (f�r) session
;;;;Subtrahera kommentar		   Subtrahera mottagare
;;;;Superhoppa			   S�nda meddelande
;;;;S�tt lapp p� d�rren		   S�tt l�sniv�
;;;;Ta bort lapp p� d�rren		   Uppskjuta l�sning
;;;;Uteslut medlem			   Utpl�na
;;;;Uttr�da ur m�te			   Var (�r) jag
;;;;Vilka �r inloggade		   Visa user-arean
;;;;V�nta p� ett inl�gg		   �ndra filter
;;;;�ndra livsl�ngd			   �ndra l�senord
;;;;�ndra m�testyp			   �ndra namn
;;;;�ndra organisat�r		   �ndra presentation
;;;;�ndra superm�te			   �ndra svarsmeddelande
;;;;�ndra till�tna f�rfattare	   �ndra variabler
;;;;�terse alla			   �terse alla kommentarer
;;;;�terse alla kommentarer rekursivt  �terse alla markerade
;;;;�terse det f�reg�ende kommenterade
;;;;�terse det kommenterade		   �terse f�rsta
;;;;�terse hoppa			   �terse igen
;;;;�terse inl�gg			   �terse lista
;;;;�terse markerade		   �terse n�sta
;;;;�terse omodifierat		   �terse presentation
;;;;�terse senaste			   �terse tr�d
;;;;�terse urinl�gget		   �terstarta kom
;;;;�verg� till administrat�rsmod
