;;; =================================================================
;;; LysKOM buttons
;;; Author: David Byers
;;; Text property mouse-2-action is executed with point and
;;; text property mouse-2-arg as arguments when called by
;;; kom-mouse-2
;;;

(defun kom-mouse-2 (click)
  (interactive "e")
  (let* ((start (event-start click))
         (window (car start))
         (pos (car (cdr start)))
         (action nil)
         (arg nil))
    (select-window window)
    (goto-char pos)
    (if (setq action (get-text-property pos 'mouse-2-action))
        (funcall action pos (get-text-property pos 'mouse-2-arg)))))


(defun lyskom-button-transform-text (text)
  (let ((blist kom-text-buttons)
	(start 0)
	(el nil)
	(props nil))
    (while blist
      (setq el (car blist))
      (while (string-match (elt el 0) text start)
	(if (elt el 1)
	    (setq props 
		  (lyskom-default-button 
			 (elt el 1)
			 (substring text
				    (match-beginning 0)
				    (match-end 0))))
	  (setq props
		(append (list 'mouse-2-action
			      (elt el 2)
			      'mouse-2-arg 
			      (elt el 3))
			(elt el 4))))
	(add-text-properties (match-beginning 0)
			     (match-end 0)
			     props
			     text)
	(setq props nil)
	(setq start (match-end 0)))
      (setq blist (cdr blist)))
    text))

	

(defun lyskom-default-button (type arg)
  (cond ((eq type 'conf) 
	 (let ((persno (if (boundp 'kom-buffer)
			   (save-excursion (set-buffer kom-buffer)
					   lyskom-pers-no)
			 (if (boundp lyskom-pers-no)
			     lyskom-pers-no
			   0))))
	   (list 'face 
		 (cond ((and (integerp arg) (= arg persno)) 'kom-me-face)
		       ((and (lyskom-conf-stat-p arg)
			     (= (conf-stat->conf-no arg) persno)) 'kom-me-face)
		       (t 'kom-active-face))
		 'mouse-2-action 'lyskom-button-view-pres
		 'mouse-2-arg 
		 (cons (if (stringp arg) (string-to-int arg) arg)
		       (if (boundp 'kom-buffer)
			   kom-buffer
			 (current-buffer)))
		 'mouse-face 'kom-highlight-face)))
	((eq type 'text) (list 'mouse-face 'kom-highlight-face
			       'mouse-2-action 'lyskom-button-view-text
			       'mouse-2-arg 			
			       (cons 
				(if (stringp arg)
				    (string-to-int arg)
				  arg)
				     (if (boundp 'kom-buffer)
					 kom-buffer
				       (current-buffer)))
			       'face 'kom-active-face))
	((eq type 'pers) 
	 (let ((persno (if (boundp 'kom-buffer)
			   (save-excursion (set-buffer kom-buffer)
					   lyskom-pers-no)
			 (if (boundp lyskom-pers-no)
			     lyskom-pers-no
			   0))))
	   (list 'face 
		 (cond ((and (integerp arg) (= arg persno)) 'kom-me-face)
		       ((and (lyskom-conf-stat-p arg)
			     (= (conf-stat->conf-no arg) persno)) 'kom-me-face)
		       (t 'kom-active-face))
		 'mouse-2-action 'lyskom-button-view-pres
		 'mouse-2-arg 
		 (cons arg 
		       (if (boundp 'kom-buffer)
			   kom-buffer
			 (current-buffer)))
		 'mouse-face 'kom-highlight-face)))))

;;;========================================
;;; Button actions
;;;

(defun lyskom-button-view-text (pos args)
  (let ((arg (car args))
	(buf (cdr args)))
    (if (not (buffer-name buf))
	(error "Selecting deleted buffer: %s" buf)
      (pop-to-buffer buf)
      (lyskom-start-of-command 'kom-button-view-text)
      (unwind-protect
	  (let ((text-no (cond ((integerp arg) arg)
			       ((lyskom-text-stat-p arg) 
				(text-stat->text-no arg))
			       (t -1))))
	    (cond ((= -1 text-no) (signal 'lyskom-internal-error
					  (list 'lyskom-button-view-text
						": argument error")))
		  (t (lyskom-view-text text-no t))))
	(lyskom-end-of-command)))))
  
  
(defun lyskom-button-goto-conf (pos args)
  (let ((arg (car args))
	(buf (cdr args)))
    (if (not (buffer-name buf))
	(error "Selecting deleted buffer: %s" buf)
      (pop-to-buffer buf)
      (lyskom-start-of-command 'kom-button-goto-conf)
      (unwind-protect
	  (lyskom-go-to-conf-handler arg)
	(lyskom-end-of-command)))))

(defun lyskom-button-stat-conf (pos args)
  (let ((arg (car args))
	(buf (cdr args)))
    (if (not (buffer-name buf))
	(error "Selecting deleted buffer: %s" buf)
      (pop-to-buffer buf)
      (kom-status-conf arg))))
      
(defun lyskom-button-view-pres (pos args)
  (let ((arg (car args))
	(buf (cdr args)))
    (if (not (buffer-name buf))
	(error "Selecting deleted buffer: %s" buf)
      (pop-to-buffer buf)
      (unwind-protect
	  (lyskom-start-of-command 'kom-button-view-pres)
	(let ((pres-no (cond ((integerp arg) arg)
			     ((lyskom-conf-stat-p arg) (conf-stat->presentation arg))
			     (t -1)))
	      (conf-name (cond ((integerp arg)
				(lyskom-format "%#1:M" arg))
			       ((lyskom-conf-stat-p arg) (conf-stat->name arg))
			       (t (lyskom-get-string 'the-conf)))))
	  (cond ((= 0 pres-no) (lyskom-format-insert 
				'has-no-presentation conf-name))
		((= -1 pres-no) (signal 'lyskom-internal-error
					(list 'lyskom-button-view-pres
					      ": argument error")))
		(t (lyskom-view-text pres-no t))))
	(lyskom-end-of-command)))))




;;;
;;; Create buttons
;;;

(defun lyskom-make-button (start end action arg)
  "Create a text button from START to END with action ACTION and 
argument ARG. If END is nil, assume (point-max)."
  (add-text-properties start
                       (or end (point-max))
                       (list 'mouse-face 'highlight
                             'mouse-2-action action
                             'mouse-2-arg arg)))

;;;
;;; Insert text number. This function also adds necessary properties.
;;; Most LysKOM functions do this on their own and don't come here.
;;;

(defun lyskom-insert-text-no (no)
  "Insert a text number and add a button to the text."
  (let ((start (point-max))            ; See lyskom-insert in lyskom-rest.el
        (string (format "%d" no)))
    (lyskom-insert string)
    (if lyskom-emacs19-p
        (add-text-properties start 
                             (+ start (length string))
                             (list 'mouse-face 'highlight
                                   'mouse-2-action 'lyskom-button-view-text
                                   'mouse-2-arg no)))))

(defun lyskom-insert-button (string action arg)
  "lyskom-insert STRING
Add a button with action ACTION and argument ARG."
  (let ((start (point-max)))
    (lyskom-insert string)
    (if lyskom-emacs19-p
        (add-text-properties start (point-max)
                             (list 'mouse-face 'highlight
                                   'mouse-2-action action
                                   'mouse-2-arg arg)))))


(defun lyskom-add-buttons (start end args)
  "Add buttons to the buffer from START to END. ARGS is a list of pattern, action and argument"
  (if lyskom-emacs19-p
      (let (arg bstart bend
                (case-fold-search t))
        (while args
          (setq arg (car args)
                args (cdr args))
          (save-excursion
            (goto-char start)
            (while (re-search-forward (car arg) end t)
              (setq bstart (or (match-beginning 1) (match-beginning 0))
                    bend (or (match-end 1) (match-end 0)))
              (add-text-properties bstart
                                   bend
                                   (list 'mouse-face 'highlight
                                         'mouse-2-action (car (cdr arg))
                                         'mouse-2-arg (car (cdr (cdr arg)))))
              (goto-char (match-end 0))))))))
               


(defun lyskom-insert-with-button (string &rest args)
  "lyskom-insert STRING
The first occurrence of TEXT will have a mouse-2 button with action
ACTION and argument ARG added."
  (let ((start (point-max))
        action text arg)
    (lyskom-insert string)
    (if lyskom-emacs19-p
        (while args
          (setq text (car args)
                action (car (cdr args))
                arg (car (cdr (cdr args)))
                args (cdr (cdr (cdr args))))
          (string-match text string)
          (if (match-beginning 0)
              (add-text-properties
               (+ start (match-beginning 0))
               (+ start (match-end 0))
               (list 'mouse-face 'highlight
                     'mouse-2-action action
                     'mouse-2-arg arg)))))))

(defun lyskom-insert-before-prompt-with-buttons (string &rest args)
  "Insert STRING just before the prompt of if no prompt then just buffers.
If prompt on screen then do the scroll if necessary.
The strings buffered are printed before the prompt by lyskom-print-prompt."
  (cond
   ((and lyskom-executing-command
         (not lyskom-is-waiting)
         (not (eq lyskom-is-waiting t)))
    ;; Don't insert the string until the current command is finished.
    (if (null lyskom-to-be-printed-before-prompt)
        (setq lyskom-to-be-printed-before-prompt (lyskom-queue-create)))
    (lyskom-queue-enter lyskom-to-be-printed-before-prompt string))
   (t
    (goto-char (point-max))
    (let* ((window (get-buffer-window (current-buffer)))
           (pv (and window
                    (pos-visible-in-window-p (point) window))))
      (beginning-of-line)
      (let ((buffer-read-only nil)
            (start (point))
            arg action text)
        (insert (if kom-emacs-knows-iso-8859-1
                    string
                  (iso-8859-1-to-swascii string)))
        (if lyskom-emacs19-p
            (while args
              (setq text (car args)
                    action (car (cdr args))
                    arg (car (cdr (cdr args)))
                    args (cdr (cdr (cdr args))))
              (string-match text string)
              (if (match-beginning 0)
                  (add-text-properties
                   (+ start (match-beginning 0))
                   (+ start (match-end 0))
                   (list 'mouse-face 'highlight
                         'mouse-2-action action
                         'mouse-2-arg arg))))))
      (goto-char (point-max))
      (if (and pv
               (eq window (selected-window))
               (not (pos-visible-in-window-p (point) window)))
          (recenter -1))
      (if window
          (if (pos-visible-in-window-p (point) window)
                nil
              (goto-char (window-start window))
              (end-of-line (1- (window-height window)))))))))


(defun insert-with-button (string text action arg)
  "Insert STRING at point.
The first occurrence of TEXT will have a mouse-2 button with action
ACTION and argument ARG added."
  (let ((start (point)))
    (insert string)
    (if lyskom-emacs19-p
        (progn
          (string-match text string)
          (if (match-beginning 0)
              (add-text-properties
               (+ start (match-beginning 0))
               (+ start (match-end 0))
               (list 'mouse-face 'highlight
                     'mouse-2-action action
                     'mouse-2-arg arg)))))))



;;;
;;;	LysKOM URL Management
;;;

(defun lyskom-get-url-manager (protocol)
  (let ((managers kom-url-managers)
	(preferences kom-url-viewer-preferences)
	(result nil))
    (while (and preferences (not result))
      (setq managers kom-url-managers)
      (while (and managers (not result))
	(if (and (string-match (car (car managers))
			       (car preferences))
		 (string-match (car (cdr (car managers)))
			       protocol))
	    (setq result (car managers)))
	(setq managers (cdr managers)))
      (setq preferences (cdr preferences)))
    result))



(defun lyskom-button-follow-url (pos arg)
  (let* ((url (buffer-substring 
	       (previous-single-property-change pos 'mouse-face)
	       (next-single-property-change pos 'mouse-face)))
	 protocol
	 url-manager)
    (string-match ":" url)
    (setq protocol (substring url 0 (match-beginning 0)))
    (setq url-manager (lyskom-get-url-manager protocol))
    (if (null url-manager)
	(lyskom-error "Can't find URL viewer"))
    (goto-char (point-max))
    (funcall (elt url-manager 3) url url-manager)))



(defun lyskom-url-manager-starting (manager)
  (lyskom-message (lyskom-format (lyskom-get-string 'starting-program)
				 (elt manager 2))))


(defun lyskom-view-url-w3 (url manager)
  (w3-fetch url))

(defun lyskom-view-url-dired (url manager)
  
  (if (not (and (string-match
		 "\\(file\\|ftp\\)://\\([^/:]*\\)\\(:[0-9]*\\)?\\(/\\|$\\)"
		 url)
		(match-beginning 0)
		(match-beginning 1)
		(match-beginning 2)))
      (lyskom-error "Bad URL"))
  (let ((host (substring url (match-beginning 2) (match-end 2)))
	(path (substring url (match-end 0)))
	(user "anonymous"))

    (if (and (string-match "\\([^@]*\\)@" host)
	     (match-beginning 1))
	(progn
	  (setq user (substring host (match-beginning 1) (match-end 1)))
	  (setq host (substring host (match-end 0)))))
    (find-file (concat "/" user "@" host ":/" path))))


(defun lyskom-view-url-telnet (url manager)
  (if (not (and (string-match 
		 "telnet:\\(//\\)?\\([^/:]*\\)\\(:[0-9]*\\)?\\(/.*$\\|$\\)"
		 url)
		(match-beginning 0)
		(match-beginning 2)))
      (lyskom-error "Bad URL"))
  (let ((host (substring url (match-beginning 2) (match-end 2)))
	(port (if (match-beginning 3)
		  (substring url (1+ (match-beginning 3)) (match-end 3))
		""))
	(user ""))
    (if (and (string-match "\\([^@]*\\)@" host)
	     (match-beginning 1))
	(progn
	  (setq user (substring host (match-beginning 1) (match-end 1)))
	  (setq host (substring host (match-end 0)))))
    (telnet (concat host " " port))))

(defun lyskom-view-url-mailmode (url manager)
  (if (not (and (string-match "mailto:\\([^@]+@.*\\)$" url)
		(match-beginning 1)))
      (lyskom-error "Bad URL"))
  (mail nil (substring url (match-beginning 1) (match-end 1))))


(defun lyskom-view-url-netscape (url manager)
  (let ((pid -1))
    (start-process "netscape"
                   nil
                   kom-netscape-command
                   "-remote" 
                   (format "openUrl(%s)" url))
    lyskom-url-manager-starting manager))


(defun lyskom-view-url-mosaic (url manager)
  (let ((pid -1)
        tempbuffer
        (filename "/tmp/Mosaic."))
    (if (file-exists-p (expand-file-name "~/.mosaicpid"))
        (save-excursion
          (set-buffer 
           (setq tempbuffer (get-buffer-create " *kom*-mosaicpid")))
          (insert-file-contents (expand-file-name "~/.mosaicpid"))
          (setq pid (read tempbuffer))
          (delete-region (point-min) (point-max))
          (insert "newwin\n")
          (insert url)
          (insert "\n")
          (setq filename (concat filename (number-to-string pid)))
          (write-region (point-min) (point-max) filename nil 'x)
          (kill-buffer tempbuffer)
          (if (= -1 (signal-process pid 30))
              (start-process "mosaic"
                             (current-buffer)
                             kom-mosaic-command 
                             url)
	    (lyskom-url-manager-starting manager)))
      (save-excursion
        (start-process "mosaic"
                       (current-buffer)
                       kom-mosaic-command 
                       url)
	(lyskom-url-manager-starting manager)))))




