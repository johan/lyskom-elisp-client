(autoload 'lyskom "lyskom-elisp-client" "Start a LysKOM session.
Optional arguments: HOST, USERNAME and PASSWORD.

See lyskom-mode for details." t)
(if (and (not (string-match "XEmacs" emacs-version))
         (or (and (= emacs-major-version 19) (>= emacs-minor-version 30))
             (>= emacs-major-version 20)))
    (let ((tools-map (lookup-key global-map [menu-bar tools])))
      (define-key-after tools-map [lyskom] '("LysKOM" . lyskom)
        (if (>= emacs-major-version 20) 'gnus 'rmail))))
