(defun lyskom-news-document-command ()
  (interactive)
  (let* ((cn (intern (thing-at-point 'symbol)))
         (doc (documentation cn))
         (sn (let ((lyskom-language 'sv)) (lyskom-command-name cn)))
         (en (let ((lyskom-language 'en)) (lyskom-command-name cn)))
         (sb (mapcar 'key-description (where-is-internal cn lyskom-sv-mode-map)))
         (eb (mapcar 'key-description (where-is-internal cn lyskom-en-mode-map)))
         (fill-prefix "   "))
    (lyskom-insert-at-point (lyskom-format "\
** %#2s: %[%#1F%#3s%]

   Swedish name: %#4s%#6?b%[ [%#6s]%]%[%]
   English name: %#5s%#7?b%[ [%#7s]%]%[%]

" 
                           72 cn (or doc "No documentation") sn en 
                           (and sb (mapconcat 'identity sb ", "))
                           (and eb (mapconcat 'identity eb ", "))))))

