;; Included from fields.el

;; (provide 'fields)

(require 'lyskom)

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
;; (require 'fields)

(defvar lyskom-caches-stat '(text text-stat pers-stat uconf-stat conf-stat
				  static-session-info))

(mapcar (lambda (cache)
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
	lyskom-caches-stat)

(defun cache-hit (what)
  (put what 'cache-hits (1+ (get what 'cache-hits)))
  (fields-replace cache-info-fields what (cache-format what)))

(defun cache-miss (what)
  (put what 'cache-misses (1+ (get what 'cache-misses)))
  (fields-replace cache-info-fields what (cache-format what)))

(defun cache-prefetch-hit (what)
  (put what 'cache-prefetch-hits (1+ (get what 'cache-prefetch-hits)))
  (fields-replace cache-info-fields what (cache-format what)))

(defun cache-prefetch-miss (what)
  (put what 'cache-prefetch-misses (1+ (get what 'cache-prefetch-misses)))
  (fields-replace cache-info-fields what (cache-format what)))

(defun cache-format (what)
  (let* ((hits (get what 'cache-hits))
	 (misses (get what 'cache-misses))
	 (phits (get what 'cache-prefetch-hits))
	 (pmisses (get what 'cache-prefetch-misses))
	 (total (float (+ hits misses)))
	 (hitrate (if (zerop total) 0 (* (/ hits total) 100))))
    (format "%d hits, %d misses (%d%%)   prefetch: %d/%d"
	    hits misses hitrate phits pmisses)))


(defvar queue-sizes (make-vector 10 0))

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
    (fields-replace cache-info-fields
		    (intern (concat "queue" (int-to-string pri)))
		    (format "%d" sz)))
  (setq lyskom-pending-calls
	(cons (cons lyskom-ref-no kom-queue)
	      lyskom-pending-calls))
  (++ lyskom-ref-no)
  
  ;; Send something from the output queues
  (lyskom-check-output-queues)
  (sit-for 0))

(defun lyskom-check-output-queues ()
  "Check for pending calls to the server.
Send calls from queues with higher priority first, and make sure that at
most lyskom-max-pending-calls are sent to the server at the same time."
  (catch 'done
    (let ((i 9))
      (while (< lyskom-number-of-pending-calls
		lyskom-max-pending-calls)
	(while (lyskom-queue-isempty (aref lyskom-output-queues i))
	  (-- i)
	  (if (< i 0) (throw 'done nil)))
	(let ((entry (lyskom-queue-delete-first
		      (aref lyskom-output-queues i)))
	      (sz (1- (aref queue-sizes i))))
	  (++ lyskom-number-of-pending-calls)
	  (aset queue-sizes i sz)
	  (fields-replace cache-info-fields
			  (intern (concat "queue" (int-to-string i)))
			  (format "%d" sz))
	  (lyskom-process-send-string
	   lyskom-proc
	   (concat (car entry) (cdr entry) "\n")))))))

;; (defadvice lyskom-check-output-queues (before stat activate)
;;   (fields-replace cache-info-fields
;; 		     'pending-calls
;; 		     (int-to-string lyskom-number-of-pending-calls)))

(defvar cache-info-template "\
  text:       	       [text]
  text-stat:  	       [text-stat]
  conf-stat:  	       [conf-stat]
  uconf-stat: 	       [uconf-stat]
  pers-stat:           [pers-stat]
  static-session-info: [static-session-info]
  pending calls:       [pending-calls]

 Queues  0: [queue0]
	 1: [queue1]
         2: [queue2]
         3: [queue3]
         4: [queue4]
         5: [queue5]
         6: [queue6]
         7: [queue7]
         8: [queue8]
         9: [queue9]")

(defvar cache-info-buffer nil)
(defvar cache-info-fields nil)

(progn
  (setq cache-info-buffer (generate-new-buffer "LysKOM cache"))
  (set-buffer cache-info-buffer)
  (erase-buffer)
  (setq cache-info-fields (fields-new cache-info-template))
  (let ((w (selected-window)))
    (split-window nil 8)
    (set-window-buffer w cache-info-buffer)
    (select-window w)))
