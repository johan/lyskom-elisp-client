;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: stack-m.el,v 1.1 2004-07-11 23:01:53 byers Exp $
;;;;; Copyright (C) 2004  Lysator Academic Computer Association.
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
;;;; ----------------------------------------------------------------
;;;; This is a derivation of the standard elib stack-m.el with a
;;;; different prefix. Since elib is not maintained actively,
;;;; we provide a local copy for the LysKOM elisb client, in
;;;; which we can add bug and performance fixes more easily.
;;;; ----------------------------------------------------------------


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: stack-m.el,v 1.1 2004-07-11 23:01:53 byers Exp $\n"))

;;;; $Id: stack-m.el,v 1.1 2004-07-11 23:01:53 byers Exp $
;;;; This file implements a simple LIFO stack using macros.

;; Copyright (C) 1991-1995 Free Software Foundation

;; Author: Inge Wallin <inge@lysator.liu.se>
;; Maintainer: elib-maintainers@lysator.liu.se
;; Created: 12 May 1991
;; Keywords: extensions, lisp

;;;; This file is part of the GNU Emacs lisp library, Elib.
;;;;
;;;; GNU Elib is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; GNU Elib is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Elib; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA 02111-1307, USA
;;;; 
;;;; Author: Inge Wallin
;;;; 

;;; Commentary:

;;; The stack is implemented as a linked list with a tag 'STACK
;;; as the first element.  All entries and removals are done using
;;; destructive functions.
;;;
;;; This file implements the functions as macros for speed in compiled 
;;; code.
;;;


;;; Code:

;; Provide the function version and remove the macro version
;;; (provide 'lyskom-elib-stack)


;;; ================================================================


(defmacro lyskom-elib-stack-create ()
  "Create an empty lifo stack."
  (` (cons 'STACK nil)))


(defmacro lyskom-elib-stack-p (stack)
  "Return t if STACK is a stack, otherwise return nil."
  (` (eq (car-safe (, stack)) 'STACK)))


(defmacro lyskom-elib-stack-push (stack element)
  "Push an element onto the stack.
Args: STACK ELEMENT"
  (` (setcdr (, stack) (cons (, element) (cdr (, stack))))))


(defmacro lyskom-elib-stack-pop (stack)
  "Remove the topmost element from STACK and return it. 
If the stack is empty, return nil."
  (` (prog1
	 (car-safe (cdr (, stack)))
       (setcdr (, stack) (cdr-safe (cdr (, stack)))))))


(defmacro lyskom-elib-stack-empty (stack)
  "Return t if STACK is empty, otherwise return nil."
  (` (null (cdr (, stack)))))


(defmacro lyskom-elib-stack-top (stack)
  "Return the topmost element of STACK or nil if it is empty."
  (` (car-safe (cdr (, stack)))))


(defmacro lyskom-elib-stack-nth (stack n)
  "Return nth element of a stack, but don't remove it.
Args: STACK N
If the length of the stack is less than N, return nil.

The top stack element has number 0."
  (` (nth (, n) (cdr (, stack)))))


(defmacro lyskom-elib-stack-all (stack)
  "Return a list of all entries in STACK.
The element last pushed is first in the list."
  (` (cdr (, stack))))


(defmacro lyskom-elib-stack-copy (stack)
  "Return a copy of STACK.
All entries in STACK are also copied."
  (` (cons 'STACK (copy-sequence (cdr (, stack))))))


(defmacro lyskom-elib-stack-length (stack)
  "Return the number of elements on STACK."
  (` (length (cdr (, stack)))))


(defmacro lyskom-elib-stack-clear (stack)
  "Remove all elements from STACK."
  (` (setcdr (, stack) nil)))

;;; stack-m.el ends here
