;;;;;
;;;;; $Id: check-strings.el,v 44.0 1996-08-30 14:45:17 davidk Exp $
;;;;; Copyright (C) 1996  Lysator Academic Computer Association.
;;;;;
;;;;
;;;;  This file is used for checking swedish-strings.el and
;;;;  english-strings.el. Run using
;;;;
;;;;  emacs -batch -l check-strings.el -f lyskom-check-strings
;;;;
;;;;


(defvar swedish-strings)
(defvar swedish-missing-strings)
(defvar english-strings)
(defvar english-missing-strings)
(defvar swedish-commands)
(defvar swedish-missing-bindings)
(defvar english-commands)
(defvar english-missing-bindings)
(defvar swedish-bindings)
(defvar english-bindings)

(defvar keybindings nil)
(defvar keymaps nil)

(defun suppress-keymap (&rest args) nil)

(defmacro define-key (map key cmd)
  (` (progn (setq keymaps (cons (quote (, map)) keymaps))
            (x-define-key (, cmd)))))

(defun define-prefix-command (&rest args) nil)

(defun x-define-key (cmd)
  (if (not (assq cmd keybindings))
      (setq keybindings (cons (cons cmd nil) keybindings))))

(defun reset-keybindings ()
  (setq keybindings nil)
  (mapcar (function (lambda (x) (set x nil))) keymaps)
  (setq keymaps nil))

(defun do-check (var &optional missing)
  (do-check-strings (symbol-value (intern (concat "swedish-" 
                                                  (symbol-name var))))
                    "Swedish"
                    (symbol-value (intern (concat "english-" 
                                                  (symbol-name var))))
                    "English"
                    (if missing
                        (symbol-value
                         (intern
                          (concat "english-"
                                  (symbol-name missing))))))
  (do-check-strings (symbol-value (intern (concat "english-" 
                                                  (symbol-name var))))
                    "English"
                    (symbol-value (intern (concat "swedish-" 
                                                  (symbol-name var))))
                    "Swedish"
                    (if missing
                        (symbol-value (intern
                                       (concat "swedish-"
                                               (symbol-name missing)))))))



(defun lyskom-check-strings ()
  (load-file "vars.el")
  (load-file "swedish-strings.el")
  (setq swedish-strings lyskom-strings)
  (setq swedish-missing-strings lyskom-missing-strings)
  (setq swedish-commands lyskom-commands)
  (setq swedish-bindings keybindings)
  (setq swedish-missing-bindings lyskom-missing-bindings)
  (makunbound 'lyskom-strings)
  (makunbound 'lyskom-commands)
  (makunbound 'lyskom-missing-bindings)
  (makunbound 'lyskom-missing-strings)
  (reset-keybindings)
  (load-file "english-strings.el")
  (setq english-strings lyskom-strings)
  (setq english-missing-strings lyskom-missing-strings)
  (setq english-commands lyskom-commands)
  (setq english-bindings keybindings)
  (setq english-missing-bindings lyskom-missing-bindings)
  (message "Checking lyskom-strings")
  (do-check 'strings 'missing-strings)
  (message "Checking lyskom-commands")
  (do-check 'commands)
  (message "Checking keybindings")
  (do-check 'bindings 'missing-bindings)
)



(defun do-check-strings (l1 n1 l2 n2 &optional ignores)
  (while l1
    (if (and (not (assq (car (car l1)) l2))
             (not (memq (car (car l1)) ignores)))
        (message "%-25S present in %s, but not in %s"
                 (car (car l1))
                 n1
                 n2))
    (setq l1 (cdr l1))))
