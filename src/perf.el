;; Included from fields.el

;; (provide 'fields)

(require 'lyskom)

(defvar lyskom-prof-template "\

 Cache performance

   text:       	       	[text]
   text-stat:  	       	[text-stat]
   conf-stat:  	       	[conf-stat]
   uconf-stat: 	       	[uconf-stat]
   pers-stat:           [pers-stat]
   static-session-info: [static-session-info]

 Server calls

   queue 0: [queuelen-0]  [queuenames-0]
	 3: [queuelen-3]  [queuenames-3]
	 6: [queuelen-6]  [queuenames-6]
	 9: [queuelen-9]  [queuenames-9]

   sent:     [sent]    returned: [returned]   pending:  [pending-calls]
   async:    [async]    errors:   [errors]

 Lisp functions

   call: [in-call]
	 [apo] [blocking-do]")

(defvar lyskom-prof-buffer nil)
(defvar lyskom-prof-fields nil)
(defvar lyskom-prof-frame nil)

(defun fields-new (string)
  (let ((fields nil))
    (while (string-match "\\[" string)
      (insert (substring string 0 (match-beginning 0)))
      (setq string (substring string (match-end 0)))
      (cond ((= (elt string 0) ?\[)
	     (insert "[")
	     (setq string (substring string 1)))
	    ((string-match "\\([^]]+\\)\\]" string)
	     (setq fields (cons (list (intern (match-string 1 string))
				      (point-marker)
				      nil)
				fields))
	     (setq string (substring string (match-end 0))))))
    (insert string)
    (cons (current-buffer) fields)))


(defun fields-replace (fields item string)
  (save-excursion
    (set-buffer (car fields))
    (let ((place (assq item (cdr fields))))
      (if (not place) (error "Unknown item"))
      (goto-char (nth 1 place))
      (insert string)
      (if (nth 2 place)
	  (delete-region (point) (nth 2 place))
	(setcar (cdr (cdr place)) (point-marker)))
      (if (zerop (length string))
	  (setcar (cdr (cdr place)) nil)))))


(require 'advice)

;;; Cache statistics

(defvar lyskom-caches-stat '(text text-stat pers-stat uconf-stat conf-stat
				  static-session-info))

(defun cache-hit (what)
  (put what 'cache-hits (1+ (get what 'cache-hits)))
  (fields-replace lyskom-prof-fields what (cache-format what)))

(defun cache-miss (what)
  (put what 'cache-misses (1+ (get what 'cache-misses)))
  (fields-replace lyskom-prof-fields what (cache-format what)))

(defun cache-prefetch-hit (what)
  (put what 'cache-prefetch-hits (1+ (get what 'cache-prefetch-hits)))
  (fields-replace lyskom-prof-fields what (cache-format what)))

(defun cache-prefetch-miss (what)
  (put what 'cache-prefetch-misses (1+ (get what 'cache-prefetch-misses)))
  (fields-replace lyskom-prof-fields what (cache-format what)))

(defun cache-format (what)
  (let* ((hits (get what 'cache-hits))
	 (misses (get what 'cache-misses))
	 (phits (get what 'cache-prefetch-hits))
	 (pmisses (get what 'cache-prefetch-misses))
	 (total (float (+ hits misses)))
	 (hitrate (if (zerop total) 0 (* (/ hits total) 100))))
    (format "%d hits, %d misses (%d%%)   prefetch: %d/%d"
	    hits misses hitrate phits pmisses)))

(defun lyskom-prof-init-cache ()
  (mapcar
   (lambda (cache)
     (put cache 'cache-hits 0)
     (put cache 'cache-misses 0)
     (put cache 'cache-prefetch-hits 0)
     (put cache 'cache-prefetch-misses 0)
     (let ((cache-fun (intern (concat "cache-get-" (symbol-name cache)))))
       (eval
	`(defadvice ,cache-fun (after stat activate)
	   "Collect statistics about cache hits."
	   (if lyskom-inhibit-prefetch
	       (if ad-return-value
		   (cache-prefetch-hit ',cache)
		 (cache-prefetch-miss ',cache))
	     (if ad-return-value
		 (cache-hit ',cache)
	       (cache-miss ',cache)))
	   ad-return-value))))
   lyskom-caches-stat))


;;; Queue statistics


(defvar queue-sizes (make-vector 10 0))
(defvar queue-names (make-vector 10 nil))
(defvar lyskom-prof-sent 0)
(defvar lyskom-prof-returned 0)
(defvar lyskom-prof-async 0)
(defvar lyskom-prof-errors 0)

(defun lyskom-send-packet (kom-queue string)
  "Send a packet to the server.
Add info on lyskom-pending-calls. Update lyskom-ref-no.
Args: KOM-QUEUE STRING."

  ;; Queue it
  (let* ((pri (lyskom-queue-priority kom-queue))
	 (sz (1+ (aref queue-sizes pri))))
    (lyskom-queue-enter (aref lyskom-output-queues pri)
			(cons lyskom-ref-no string))
    (aset queue-sizes pri sz)
    (fields-replace lyskom-prof-fields
		    (intern (concat "queuelen-" (int-to-string pri)))
		    (format "%d" sz))
    (setq lyskom-pending-calls
	  (cons (cons lyskom-ref-no kom-queue)
		lyskom-pending-calls))
    (++ lyskom-ref-no)
    
    ;; Send something from the output queues
    (sit-for 0) ;; (if (= pri 9) 0.5 0))
    (lyskom-check-output-queues)))

(defun lyskom-check-output-queues ()
  "Check for pending calls to the server.
Send calls from queues with higher priority first, and make sure that at
most lyskom-max-pending-calls are sent to the server at the same time."
  (catch 'done
    (let ((i 9))
      (while (and lyskom-ok-to-send-new-calls
		  (< lyskom-number-of-pending-calls
		     lyskom-max-pending-calls))
	(while (lyskom-queue-isempty (aref lyskom-output-queues i))
	  (-- i)
	  (if (< i 0) (throw 'done nil)))
	(let ((entry (lyskom-queue-delete-first
		      (aref lyskom-output-queues i)))
	      (sz (1- (aref queue-sizes i))))
	  (++ lyskom-number-of-pending-calls)
	  (aset queue-sizes i sz)
	  (fields-replace lyskom-prof-fields
			  (intern (concat "queuelen-" (int-to-string i)))
			  (format "%d" sz))
	  (fields-replace lyskom-prof-fields 'pending-calls
			  (int-to-string lyskom-number-of-pending-calls))
	  (lyskom-process-send-string
	   lyskom-proc
	   (concat (car entry) (cdr entry) "\n"))
	  (++ lyskom-prof-sent)
	  (fields-replace lyskom-prof-fields 'sent
			  (int-to-string lyskom-prof-sent))))))
  (sit-for 0))


(defun lyskom-prof-set-queue-priority (queue-name priority)
  (let* ((oldpri (get queue-name 'lyskom-queue-priority))
	 (noldpri (or oldpri 0)))
    (aset queue-names noldpri (delq queue-name (aref queue-names noldpri)))
    (fields-replace lyskom-prof-fields
		    (intern (concat "queuenames-" (int-to-string noldpri)))
		    (mapconcat 'symbol-name (aref queue-names noldpri) ",")))
  (aset queue-names priority (cons queue-name (aref queue-names priority)))
  (fields-replace lyskom-prof-fields
		  (intern (concat "queuenames-" (int-to-string priority)))
		  (mapconcat 'symbol-name (aref queue-names priority) ",")))
   

(defadvice lyskom-set-queue-priority (before stat activate)
  (lyskom-prof-set-queue-priority queue-name priority)
  (sit-for 0))


(defadvice lyskom-add-new-queue (before stat activate)
  (let ((pri (get queue-name 'lyskom-queue-priority)))
    (when (null pri)
      (ding)
      (message "Queue %S has no priority" queue-name))
    (lyskom-prof-set-queue-priority queue-name (or pri 0))))


(defadvice lyskom-decrease-pending-calls (after stat activate compile)
  (fields-replace lyskom-prof-fields
		  'pending-calls
		  (int-to-string lyskom-number-of-pending-calls))
  (sit-for 0))


;; Server responses

(defadvice lyskom-parse-success (after stat activate)
  (++ lyskom-prof-returned)
  (fields-replace lyskom-prof-fields 'returned
		  (int-to-string lyskom-prof-returned)))

(defadvice lyskom-parse-async (after stat activate)
  (++ lyskom-prof-async)
  (fields-replace lyskom-prof-fields 'async
		  (int-to-string lyskom-prof-async)))

(defadvice lyskom-parse-error (after stat activate)
  (++ lyskom-prof-errors)
  (fields-replace lyskom-prof-fields 'errors
		  (int-to-string lyskom-prof-errors)))

;; Misc


(defadvice lyskom-prefetch-map-handler (around stat activate)
  (fields-replace lyskom-prof-fields 'in-call "lyskom-prefetch-map-handler")
  (sit-for 0)
  ad-do-it
  (fields-replace lyskom-prof-fields 'in-call "")
  (sit-for 0))


(defadvice blocking-do (around stat activate)
  (fields-replace lyskom-prof-fields 'blocking-do
		  (concat "blocking-do " (symbol-name command)))
  (sit-for 0)
  ad-do-it
  (fields-replace lyskom-prof-fields 'blocking-do "")
  (sit-for 0))


(defadvice lyskom-accept-process-output (around stat activate protect)
  (fields-replace lyskom-prof-fields 'apo "apo")
  (sit-for 0)
  ad-do-it
  (fields-replace lyskom-prof-fields 'apo "   ")
  (sit-for 0.1))


;; Initialazation


(defun lyskom-prof-init ()
  (interactive)
  (setq lyskom-prof-buffer (get-buffer-create "LysKOM profile"))
  (set-buffer lyskom-prof-buffer)
  (buffer-disable-undo)
  (erase-buffer)
  (setq lyskom-prof-fields (fields-new lyskom-prof-template))
  (setq lyskom-prof-sent 0
	lyskom-prof-returned 0
	lyskom-prof-async 0
	lyskom-prof-errors 0)
  (lyskom-prof-init-cache)
  (if window-system
      (progn
	(when (not (frame-live-p lyskom-prof-frame))
	  (setq lyskom-prof-frame (make-frame '((height . 27)))))
	(select-frame lyskom-prof-frame)
	(display-buffer lyskom-prof-buffer)
	(goto-char 0))
    (let ((w (selected-window)))
      (split-window nil 8)
      (set-window-buffer w lyskom-prof-buffer)
      (select-window w))))

(lyskom-prof-init)
