;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: regexp.el,v 44.2 2003-01-05 21:37:08 byers Exp $
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
;;;; File: regexp.el
;;;;
;;;; Translate from various regexp synatxes
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: regexp.el,v 44.2 2003-01-05 21:37:08 byers Exp $\n"))

(defvar lyskom-re-converters 
  '((0 . nil)
    (1 . lyskom-re-convert-posix-ere)
    (10000 . identity)))

(defvar lyskom-re-cache nil)


(defun lyskom-re-cache-get (type regexp)
  "Return a cached converted regexp.
TYPE is the regexp syntax. REGEXP is the regular expression.
This function returns a cons of (REGEXP . CONVERTED) if the
regexp is in the cache."
  (assoc regexp (cdr (assq type lyskom-re-cache))))

(defun lyskom-re-cache-put (type regexp translated)
  (let ((a (assq type lyskom-re-cache)))
    (if a
        (setcdr a (cons (cons regexp translated) (cdr a)))
      (setq lyskom-re-cache
            (cons (cons type (list (cons regexp translated)))
                  lyskom-re-cache)))
    translated))


(defvar lyskom-re-have-chclass nil
  "Non-nil if Emacs regexps support [[: :]] character classes")
(defvar lyskom-re-have-backrefs (string-match "^\\(.\\)\\1$" "xx")
  "Non-nil if Emacs regexps support back references")


(def-komtype regex-parse (re pos stack result))

(defun lyskom-re-parse-posix-limit (parse)
  (cond
   ((eq (string-match "\\([0-9]+\\)}" (regex-parse->re parse)
                      (regex-parse->pos parse))
        (regex-parse->pos parse))
    (lyskom-re-parse-modify-last
     parse
     'LIMIT
     (cons (string-to-int (match-string 1 (regex-parse->re parse)))
           (string-to-int (match-string 1 (regex-parse->re parse))))))

   ((eq (string-match "\\([0-9]+\\)?,\\([0-9]+\\)?}" (regex-parse->re parse)
                      (regex-parse->pos parse))
        (regex-parse->pos parse))
    (let ((min (match-string 1 (regex-parse->re parse)))
          (max (match-string 2 (regex-parse->re parse))))
      (lyskom-re-parse-modify-last 
       parse
       'LIMIT 
       (cons (and min (string-to-int min))
             (and max (string-to-int max))))))

   (t (throw 'lyskom-re-parse 'syntax-error)))
  (set-regex-parse->pos parse (match-end 0)))

(defun lyskom-re-parse-posix-charclass (parse)
  (let ((start (regex-parse->pos parse)))

    ;; Skip over initial negation and rbrack

    (when (eq (string-match "^?\\]?" 
                            (regex-parse->re parse)
                            (regex-parse->pos parse))
              (regex-parse->pos parse))
      (set-regex-parse->pos (match-end 0)))

    ;; Now parse the rest of the expression

    (let ((done nil))
      (while (and (not done)
                  (< (regex-parse->pos parse) (length (regex-parse->re parse))))
        (cond
         ((eq (string-match "\\[[\\.=]"
                            (regex-parse->re parse) (regex-parse->pos parse))
              (regex-parse->pos parse))
          (throw 'lyskom-re-parse 'unsupported))
         ((eq (string-match "\\[:\\)"
                            (regex-parse->re parse) (regex-parse->pos parse))
              (regex-parse->pos parse))
          (cond ((not lyskom-re-have-chclass)
                 (throw 'lyskom-re-parse 'unsupported))
                ((eq (string-match "\\[:\\(.*?\\):\\]"
                                   (regex-parse->re parse)
                                   (regex-parse->pos parse))
                     (regex-parse->pos parse))
                 (set-regex-parse->pos (match-end 0)))
                (t (throw 'lyskom-re-parse 'syntax-error))))
         ((eq ?\] (aref (regex-parse->re parse) (regex-parse->pos parse)))
          (set-regex-parse->pos parse (1+ (regex-parse->pos parse)))
          (setq done t))
         (t nil))))

    (lyskom-re-parse-accumulate 
     parse
     (vector CLASS
             (substring (regex-parse->re parse)
                        start
                        (1- (regex-parse->pos parse)))))))


(defun lyskom-re-parse-start-subexpression (parse)
  (set-regex-parse->stack parse
                          (cons (regex-parse->result parse)
                                (regex-parse->stack parse)))
  (set-regex-parse->result parse nil))


(defun lyskom-re-parse-end-subexpression (parse)
  (let ((cur (regex-parse->result parse)))
    ;; Pop the old expression from the stack
    (set-regex-parse->result parse (car (regex-parse->stack parse)))
    (set-regex-parse->stack parse (cdr (regex-parse->stack parse)))
    (lyskom-re-parse-accumulate parse cur)))

(defun lyskom-re-parse-accumulate (parse what)
  (set-regex-parse->result parse (cons what (regex-parse->result parse))))

(defun lyskom-re-parse-modify-last (parse &rest what)
  (set-regex-parse->result 
   parse
   (cons (apply 'vector (append what (list (car (regex-parse->result parse)))))
         (cdr (regex-parse->result parse)))))

(defun lyskom-re-parse-posix-ere (parse)
  "Parse a POSIX ERE using parse state PARSE
Returns the modified parse."
  (let ((done nil))
    (while (and (not done)
                (< (regex-parse->pos parse) (length (regex-parse->re parse))))
      (let ((c (aref (regex-parse->re parse) (regex-parse->pos parse))))
        (set-regex-parse->pos parse (1+ (regex-parse->pos parse)))
        (cond

         ;; Start of a paren means we are about to parse a subexpression
         ;; So terminate the current literal stuff and 

         ((eq c ?\()
          (lyskom-re-parse-start-subexpression parse)
          (set-regex-parse->result parse (lyskom-re-parse-posix-ere parse))
          (lyskom-re-parse-end-subexpression parse))

         ;; We see an end paren when inside a subexpression.
         ;; Terminate the parse.

         ((and (eq c ?\)) (regex-parse->stack parse))
          (setq done t))

         ((eq c ?\{)
          (lyskom-re-parse-posix-limit parse))

         ((eq c ?\\)
          (if (< (regex-parse->pos parse) (length (regex-parse->re parse)))
              (progn
                (lyskom-re-parse-accumulate parse
                 (aref (regex-parse->re parse) (regex-parse->pos parse)))
                (set-regex-parse->pos parse (1+ (regex-parse->pos parse))))
            (throw 'lyskom-re-parse 'syntax-error)))

         ((and (eq c ?\|) (regex-parse->stack parse))
          (lyskom-re-parse-accumulate parse '|))

         ((eq c ?\[)
          (lyskom-re-parse-posix-charclass parse))

         ((eq c ?.)
          (lyskom-re-parse-accumulate parse '\.))

         ((eq c ?+)
          (lyskom-re-parse-modify-last parse '+))

         ((eq c ?\?)
          (lyskom-re-parse-modify-last parse '\?))

         ((eq c ?*)
          (lyskom-re-parse-modify-last parse '*))

         ((eq c ?^)
          (lyskom-re-parse-accumulate parse '^))

         ((eq c ?$)
          (lyskom-re-parse-accumulate parse '$))

         (t (lyskom-re-parse-accumulate parse c)))))

    (when (and (regex-parse->stack parse)
               (not done)
               (>= (regex-parse->pos parse)
                   (length (regex-parse->re parse))))
      (throw 'lyskom-re-parse 'syntax-error))

    (nreverse (regex-parse->result parse))))


(defun lyskom-re-convert-finalize-result (parse)
  (let ((acc nil)
        (result nil))
    (while parse
      (when (and (not (characterp (car parse))) acc)
        (setq result (cons (regexp-quote (apply 'string (nreverse acc)))
                           result)
              acc nil))
      (cond 
       ((characterp (car parse))
        (setq acc (cons (car parse) acc)))
       ((symbolp (car parse))
        (cond ((eq (car parse) '\.) (setq result (cons "." result)))
              ((eq (car parse) '\|) (setq result (cons "\\|" result)))
              (t (error "bug 1 in lyskom-re-convert-finalize-result"))))
       ((vectorp (car parse))
        (let ((op (elt (car parse) 0))
              (re (lyskom-re-convert-finalize-result
                   (list (elt (car parse) (1- (length (car parse))))))))
          (cond ((eq op '+)
                 (setq result (cons (concat re "+") result)))
                ((eq op '*)
                 (setq result (cons (concat re "*") result)))
                ((eq op '\?)
                 (setq result (cons (concat re "?") result)))
                ((eq op 'LIMIT)
                 (let ((min (car (elt (car parse) 1)))
                       (max (cdr (elt (car parse) 1))))
                   (setq result (cons (concat re 
                                              "\\{"
                                              (if min (int-to-string min) "")
                                              ","
                                              (if max (int-to-string max) "")
                                              "\\}")
                                      result))))
                (t (error "bug 2 in lyskom-re-convert-finalize-result"))))
        )
       ((listp (car parse))
        (setq result (cons (concat "\\("
                                   (lyskom-re-convert-finalize-result (car parse))
                                   "\\)")
                           result)))
       (t (error "bug 3 in lyskom-re-convert-finalize-result")))
      (setq parse (cdr parse)))
    (when acc
      (setq result (cons (regexp-quote (apply 'string (nreverse acc)))
                         result)
            acc nil))
    (apply 'concat (nreverse result))))


(defun lyskom-re-convert-posix-ere (regexp)
  (let ((result (lyskom-re-parse-posix-ere
                 (lyskom-create-regex-parse regexp 0 nil nil))))
    (when result ((lyskom-re-convert-finalize-result result)))))




(defun lyskom-re-convert-regexp (type regexp)
  "Convert a regular expression to Emacs syntax.
TYPE is the regexp type, an integer key in lyskom-re-converters.
REGEXP is the regular expression.

This function returns the converted regular expression, if
conversion is possible, nil if there is no converter or
a symbol if there was a problem converting the regexp."
  (let ((c (lyskom-re-cache-get type regexp)))
    (if c (cdr c)
      (let ((fn (cdr (assq type lyskom-re-converters))))
        (when fn 
          (lyskom-re-cache-put 
           type regexp 
           (catch 'lyskom-re-parse
             (funcall fn regexp))))))))
