(require 'advice)
(require 'fields)

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

(defvar cache-info-template "\
  text:       	       [text]
  text-stat:  	       [text-stat]
  conf-stat:  	       [conf-stat]
  uconf-stat: 	       [uconf-stat]
  pers-stat:           [pers-stat]
  static-session-info: [static-session-info]")

(defvar cache-info-buffer nil)
(defvar cache-info-fields nil)

(progn
  (setq cache-info-buffer (generate-new-buffer "LysKOM cache"))
  (set-buffer cache-info-buffer)
  (erase-buffer)
  (setq cache-info-fields (fields-new cache-info-template))
  (let ((w (selected-window)))
    (split-window nil 7)
    (set-window-buffer w cache-info-buffer)
    (select-window w)))
