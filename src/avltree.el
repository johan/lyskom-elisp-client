;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: avltree.el,v 1.1 2004-07-11 23:01:53 byers Exp $
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
;;;; This is a derivation of the standard elib avltree.el with a
;;;; different prefix. Since elib is not maintained actively,
;;;; we provide a local copy for the LysKOM elisb client, in
;;;; which we can add bug and performance fixes more easily.
;;;; ----------------------------------------------------------------


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: avltree.el,v 1.1 2004-07-11 23:01:53 byers Exp $\n"))

;;;; $Id: avltree.el,v 1.1 2004-07-11 23:01:53 byers Exp $
;;;; This file implements balanced binary trees, AVL-trees.

;; Copyright (C) 1991-1995 Free Software Foundation
;; Copyright (C) 2004 Lysator

;; Author: Inge Wallin <inge@lysator.liu.se>
;;	Thomas Bellman <bellman@lysator.liu.se>
;; Created: 10 May 1991
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
;;;; Initial author:	 Thomas Bellman	       
;;;;			 Lysator Computer Club 
;;;;		 	 Linkoping University  
;;;;		 	 Sweden		       
;;;;
;;;; Bugfixes and completion: Inge Wallin
;;;;


;;; Commentary:
;;;
;;; An AVL tree is a nearly-perfect balanced binary tree.  A tree
;;; consists of two cons cells, the first one holding the tag
;;; 'AVLTREE in the car cell, and the second one having the tree
;;; in the car and the compare function in the cdr cell.  The tree has
;;; a dummy node as its root with the real tree in the left pointer.
;;; 
;;; Each node of the tree consists of one data element, one left
;;; sub-tree and one right sub-tree.  Each node also has a balance
;;; count, which is the difference in depth of the left and right
;;; sub-trees. 
;;;

;;; Code:

;;; (require 'lyskom-elib-node)
;;; (require 'lyskom-elib-stack)
;;; (provide 'lyskom-elisb-avltree)


;;; ================================================================
;;;        Functions and macros handling an AVL tree node.

;;
;; The rest of the functions needed here can be found in
;; elib-node.el.
;;


(defmacro lyskom-elib-avl-node-create (left right data balance)

  ;; Create and return an avl-tree node.
  (` (vector (, left) (, right) (, data) (, balance))))


(defmacro lyskom-elib-avl-node-balance (node)

  ;; Return the balance field of a node.
  (` (aref (, node) 3)))


(defmacro lyskom-elib-avl-node-set-balance (node newbal)

  ;; Set the balance field of a node.
  (` (aset (, node) 3 (, newbal))))



;;; ================================================================
;;;       Internal functions for use in the AVL tree package

;;;
;;; The functions and macros in this section all start with `lyskom-elib-avl-'.
;;;


(defmacro lyskom-elib-avl-root (tree)

  ;; Return the root node for an avl-tree.  INTERNAL USE ONLY.
  (` (lyskom-elib-node-left (car (cdr (, tree))))))


(defmacro lyskom-elib-avl-dummyroot (tree)

  ;; Return the dummy node of an avl-tree.  INTERNAL USE ONLY.

  (` (car (cdr (, tree)))))


(defmacro lyskom-elib-avl-cmpfun (tree)

  ;; Return the compare function of AVL tree TREE.  INTERNAL USE ONLY.
  (` (cdr (cdr (, tree)))))


;; ----------------------------------------------------------------
;;                          Deleting data


(defun lyskom-elib-avl-del-balance1 (node branch)

  ;; Rebalance a tree and return t if the height of the tree has shrunk.
  (let* ((br (lyskom-elib-node-branch node branch))
	 p1
	 b1
	 p2
	 b2 
	 result)
    (cond
     ((< (lyskom-elib-avl-node-balance br) 0)
      (lyskom-elib-avl-node-set-balance br 0)
      t)

     ((= (lyskom-elib-avl-node-balance br) 0)
      (lyskom-elib-avl-node-set-balance br +1)
      nil)

     (t					; Rebalance
      (setq p1 (lyskom-elib-node-right br)
	    b1 (lyskom-elib-avl-node-balance p1))
      (if (>= b1 0)
	  ;; Single RR rotation
	  (progn
	    (lyskom-elib-node-set-right br (lyskom-elib-node-left p1))
	    (lyskom-elib-node-set-left p1 br)
	    (if (= 0 b1)
		(progn
		  (lyskom-elib-avl-node-set-balance br +1)
		  (lyskom-elib-avl-node-set-balance p1 -1)
		  (setq result nil))
	      (lyskom-elib-avl-node-set-balance br 0)
	      (lyskom-elib-avl-node-set-balance p1 0)
	      (setq result t))
	    (lyskom-elib-node-set-branch node branch p1)
	    result)

	;; Double RL rotation
	(setq p2 (lyskom-elib-node-left p1)
	      b2 (lyskom-elib-avl-node-balance p2))
	(lyskom-elib-node-set-left p1 (lyskom-elib-node-right p2))
	(lyskom-elib-node-set-right p2 p1)
	(lyskom-elib-node-set-right br (lyskom-elib-node-left p2))
	(lyskom-elib-node-set-left p2 br)
	(if (> b2 0)
	    (lyskom-elib-avl-node-set-balance br -1)
	  (lyskom-elib-avl-node-set-balance br 0))
	(if (< b2 0)
	    (lyskom-elib-avl-node-set-balance p1 +1)
	  (lyskom-elib-avl-node-set-balance p1 0))
	(lyskom-elib-node-set-branch node branch p2)
	(lyskom-elib-avl-node-set-balance p2 0)
	t)
      ))
    ))


(defun lyskom-elib-avl-del-balance2 (node branch)

  (let* ((br (lyskom-elib-node-branch node branch))
	 p1
	 b1
	 p2 
	 b2 
	 result)
    (cond
     ((> (lyskom-elib-avl-node-balance br) 0)
      (lyskom-elib-avl-node-set-balance br 0)
      t)

     ((= (lyskom-elib-avl-node-balance br) 0)
      (lyskom-elib-avl-node-set-balance br -1)
      nil)

     (t					; Rebalance
      (setq p1 (lyskom-elib-node-left br)
	    b1 (lyskom-elib-avl-node-balance p1))
      (if (<= b1 0)
	  ;; Single LL rotation
	  (progn
	    (lyskom-elib-node-set-left br (lyskom-elib-node-right p1))
	    (lyskom-elib-node-set-right p1 br)
	    (if (= 0 b1)
		(progn
		  (lyskom-elib-avl-node-set-balance br -1)
		  (lyskom-elib-avl-node-set-balance p1 +1)
		  (setq result nil))
	      (lyskom-elib-avl-node-set-balance br 0)
	      (lyskom-elib-avl-node-set-balance p1 0)
	      (setq result t))
	    (lyskom-elib-node-set-branch node branch p1)
	    result)

	;; Double LR rotation
	(setq p2 (lyskom-elib-node-right p1)
	      b2 (lyskom-elib-avl-node-balance p2))
	(lyskom-elib-node-set-right p1 (lyskom-elib-node-left p2))
	(lyskom-elib-node-set-left p2 p1)
	(lyskom-elib-node-set-left br (lyskom-elib-node-right p2))
	(lyskom-elib-node-set-right p2 br)
	(if (< b2 0)
	    (lyskom-elib-avl-node-set-balance br +1)
	  (lyskom-elib-avl-node-set-balance br 0))
	(if (> b2 0)
	    (lyskom-elib-avl-node-set-balance p1 -1)
	  (lyskom-elib-avl-node-set-balance p1 0))
	(lyskom-elib-node-set-branch node branch p2)
	(lyskom-elib-avl-node-set-balance p2 0)
	t)
      ))
    ))


(defun lyskom-elib-avl-do-del-internal (node branch q)

  (let* ((br (lyskom-elib-node-branch node branch)))
      (if (lyskom-elib-node-right br)
	  (if (lyskom-elib-avl-do-del-internal br +1 q)
	      (lyskom-elib-avl-del-balance2 node branch))
	(lyskom-elib-node-set-data q (lyskom-elib-node-data br))
	(lyskom-elib-node-set-branch node branch
			      (lyskom-elib-node-left br))
	t)))



(defun lyskom-elib-avl-do-delete (cmpfun root branch data)

  ;; Return t if the height of the tree has shrunk.
  (let* ((br (lyskom-elib-node-branch root branch)))
    (cond
     ((null br)
      nil)

     ((funcall cmpfun data (lyskom-elib-node-data br))
      (if (lyskom-elib-avl-do-delete cmpfun br 0 data)
	  (lyskom-elib-avl-del-balance1 root branch)))

     ((funcall cmpfun (lyskom-elib-node-data br) data)
      (if (lyskom-elib-avl-do-delete cmpfun br 1 data)
	  (lyskom-elib-avl-del-balance2 root branch)))

     (t
      ;; Found it.  Let's delete it.
      (cond
       ((null (lyskom-elib-node-right br))
	(lyskom-elib-node-set-branch root branch (lyskom-elib-node-left br))
	t)

       ((null (lyskom-elib-node-left br))
	(lyskom-elib-node-set-branch root branch (lyskom-elib-node-right br))
	t)

       (t
	(if (lyskom-elib-avl-do-del-internal br 0 br)
	    (lyskom-elib-avl-del-balance1 root branch)))))
     )))


;; ----------------------------------------------------------------
;;                           Entering data



(defun lyskom-elib-avl-enter-balance1 (node branch)

  ;; Rebalance a tree and return t if the height of the tree has grown.
  (let* ((br (lyskom-elib-node-branch node branch))
	 p1
	 p2
	 b2 
	 result)
    (cond
     ((< (lyskom-elib-avl-node-balance br) 0)
      (lyskom-elib-avl-node-set-balance br 0)
      nil)

     ((= (lyskom-elib-avl-node-balance br) 0)
      (lyskom-elib-avl-node-set-balance br +1)
      t)

     (t
      ;; Tree has grown => Rebalance
      (setq p1 (lyskom-elib-node-right br))
      (if (> (lyskom-elib-avl-node-balance p1) 0)
	  ;; Single RR rotation
	  (progn
	    (lyskom-elib-node-set-right br (lyskom-elib-node-left p1))
	    (lyskom-elib-node-set-left p1 br)
	    (lyskom-elib-avl-node-set-balance br 0)
	    (lyskom-elib-node-set-branch node branch p1))

	;; Double RL rotation
	(setq p2 (lyskom-elib-node-left p1)
	      b2 (lyskom-elib-avl-node-balance p2))
	(lyskom-elib-node-set-left p1 (lyskom-elib-node-right p2))
	(lyskom-elib-node-set-right p2 p1)
	(lyskom-elib-node-set-right br (lyskom-elib-node-left p2))
	(lyskom-elib-node-set-left p2 br)
	(if (> b2 0)
	    (lyskom-elib-avl-node-set-balance br -1)
	  (lyskom-elib-avl-node-set-balance br 0))
	(if (< b2 0)
	    (lyskom-elib-avl-node-set-balance p1 +1)
	  (lyskom-elib-avl-node-set-balance p1 0))
	(lyskom-elib-node-set-branch node branch p2))
      (lyskom-elib-avl-node-set-balance (lyskom-elib-node-branch node branch) 0)
      nil))
    ))


(defun lyskom-elib-avl-enter-balance2 (node branch)

  ;; Return t if the tree has grown.
  (let* ((br (lyskom-elib-node-branch node branch))
	 p1
	 p2 
	 b2)
    (cond
     ((> (lyskom-elib-avl-node-balance br) 0)
      (lyskom-elib-avl-node-set-balance br 0)
      nil)

     ((= (lyskom-elib-avl-node-balance br) 0)
      (lyskom-elib-avl-node-set-balance br -1)
      t)

     (t	
      ;; Balance was -1 => Rebalance
      (setq p1 (lyskom-elib-node-left br))
      (if (< (lyskom-elib-avl-node-balance p1) 0)
	  ;; Single LL rotation
	  (progn
	    (lyskom-elib-node-set-left br (lyskom-elib-node-right p1))
	    (lyskom-elib-node-set-right p1 br)
	    (lyskom-elib-avl-node-set-balance br 0)
	    (lyskom-elib-node-set-branch node branch p1))

	;; Double LR rotation
	(setq p2 (lyskom-elib-node-right p1)
	      b2 (lyskom-elib-avl-node-balance p2))
	(lyskom-elib-node-set-right p1 (lyskom-elib-node-left p2))
	(lyskom-elib-node-set-left p2 p1)
	(lyskom-elib-node-set-left br (lyskom-elib-node-right p2))
	(lyskom-elib-node-set-right p2 br)
	(if (< b2 0)
	    (lyskom-elib-avl-node-set-balance br +1)
	  (lyskom-elib-avl-node-set-balance br 0))
	(if (> b2 0)
	    (lyskom-elib-avl-node-set-balance p1 -1)
	  (lyskom-elib-avl-node-set-balance p1 0))
	(lyskom-elib-node-set-branch node branch p2))
      (lyskom-elib-avl-node-set-balance (lyskom-elib-node-branch node branch) 0)
      nil))
    ))


(defun lyskom-elib-avl-do-enter (cmpfun root branch data)

  ;; Return t if height of tree ROOT has grown.  INTERNAL USE ONLY.
  (let ((br (lyskom-elib-node-branch root branch)))
    (cond
     ((null br)
      ;; Data not in tree, insert it
      (lyskom-elib-node-set-branch root branch
			    (lyskom-elib-avl-node-create nil nil data 0))
      t)

     ((funcall cmpfun (lyskom-elib-node-data br) data)
      (and (lyskom-elib-avl-do-enter cmpfun
			      br
			      1 data)
	   (lyskom-elib-avl-enter-balance1 root branch)) )

    ((funcall cmpfun data (lyskom-elib-node-data br))
      (and (lyskom-elib-avl-do-enter cmpfun
			      br
			      0 data)
	   (lyskom-elib-avl-enter-balance2 root branch)))

     (t
      (lyskom-elib-node-set-data br data)
      nil))))


;; ----------------------------------------------------------------


(defun lyskom-elib-avl-mapc (map-function root)
  ;; Apply MAP-FUNCTION to all nodes in the tree starting with ROOT.
  ;; The function is applied in-order.
  ;;
  ;; Note: MAP-FUNCTION is applied to the node and not to the data itself.
  ;; INTERNAL USE ONLY.

  (let ((node root)
	(stack (lyskom-elib-stack-create))
	(go-left t))
    (lyskom-elib-stack-push stack nil)
    (while node
      (if (and go-left
	       (lyskom-elib-node-left node))
	  (progn				   ; Do the left subtree first.
	    (lyskom-elib-stack-push stack node)
	    (setq node (lyskom-elib-node-left node)))
	(funcall map-function node)		   ; Apply the function...
	(if (lyskom-elib-node-right node)		   ; and do the right subtree.
	    (setq node (lyskom-elib-node-right node)
		  go-left t)
	  (setq node (lyskom-elib-stack-pop stack)
		go-left nil))))))


(defun lyskom-elib-avl-do-copy (root)
  ;; Copy the tree with ROOT as root.
  ;; Highly recursive. INTERNAL USE ONLY.
  (if (null root) 
      nil
    (lyskom-elib-avl-node-create (lyskom-elib-avl-do-copy (lyskom-elib-node-left root))
			  (lyskom-elib-avl-do-copy (lyskom-elib-node-right root))
			  (lyskom-elib-node-data root)
			  (lyskom-elib-avl-node-balance root))))



;;; ================================================================
;;;       The public functions which operate on AVL trees.


(defun lyskom-avltree-create (compare-function)
  "Create an empty avl tree.
COMPARE-FUNCTION is a function which takes two arguments, A and B,
and returns non-nil if A is less than B, and nil otherwise."
  (cons 'AVLTREE
	(cons (lyskom-elib-avl-node-create nil nil nil 0)
	      compare-function)))


(defun lyskom-avltree-p (obj)
  "Return t if OBJ is an avl tree, nil otherwise."
  (eq (car-safe obj) 'AVLTREE))


(defun lyskom-avltree-compare-function (tree)
  "Return the comparision function for the avl tree TREE."
  (lyskom-elib-avl-cmpfun tree))


(defun lyskom-avltree-empty (tree)
  "Return t if TREE is emtpy, otherwise return nil."
  (null (lyskom-elib-avl-root tree)))


(defun lyskom-avltree-enter (tree data)
  "In the avl tree TREE insert DATA.
Return DATA."

  (lyskom-elib-avl-do-enter (lyskom-elib-avl-cmpfun tree)
		     (lyskom-elib-avl-dummyroot tree)
		     0
		     data)
  data)


(defun lyskom-avltree-delete (tree data)
  "From the avl tree TREE, delete DATA.
Return the element in TREE which matched DATA, nil if no element matched."

  (lyskom-elib-avl-do-delete (lyskom-elib-avl-cmpfun tree)
		      (lyskom-elib-avl-dummyroot tree)
		      0
		      data))


(defun lyskom-avltree-member (tree data)
  "Return the element in the avl tree TREE which matches DATA.
Matching uses the compare function previously specified in `lyskom-avltree-create'
when TREE was created.

If there is no such element in the tree, the value is nil."

  (let ((node (lyskom-elib-avl-root tree))
	(compare-function (lyskom-elib-avl-cmpfun tree))
	found)
    (while (and node 
		(not found))
      (cond
       ((funcall compare-function data (lyskom-elib-node-data node))
	(setq node (lyskom-elib-node-left node)))
       ((funcall compare-function (lyskom-elib-node-data node) data)
	(setq node (lyskom-elib-node-right node)))
       (t 
	(setq found t))))

    (if node
	(lyskom-elib-node-data node)
      nil)))



;; (defun lyskom-avltree-map (__map-function__ tree)
;;   "Apply MAP-FUNCTION to all elements in the avl tree TREE.
;; Alters the data of each AVL tree element to the value returned by 
;; the function."
;;   (lyskom-elib-avl-mapc
;;    (function (lambda (node)
;; 	       (lyskom-elib-node-set-data node
;; 				   (funcall __map-function__
;; 					    (lyskom-elib-node-data node)))))
;;    (lyskom-elib-avl-root tree)))

(defun lyskom-avltree-traverse (__map-function__ tree)
  "Apply MAP-FUNCTION to all elements in the avl tree TREE."
  (lyskom-elib-avl-mapc
   (function (lambda (node)
               (funcall __map-function__
                        (lyskom-elib-node-data node))))
   (lyskom-elib-avl-root tree)))



(defun lyskom-avltree-first (tree)
  "Return the first element in TREE, or nil if TREE is empty."

  (let ((node (lyskom-elib-avl-root tree)))
    (if node
	(progn
	  (while (lyskom-elib-node-left node)
	    (setq node (lyskom-elib-node-left node)))
	  (lyskom-elib-node-data node))
      nil)))


(defun lyskom-avltree-last (tree)
  "Return the last element in TREE, or nil if TREE is empty."
  (let ((node (lyskom-elib-avl-root tree)))
    (if node
	(progn
	  (while (lyskom-elib-node-right node)
	    (setq node (lyskom-elib-node-right node)))
	  (lyskom-elib-node-data node))
      nil)))


(defun lyskom-avltree-copy (tree)
  "Return a copy of the avl tree TREE."
  (let ((new-tree (lyskom-avltree-create 
		   (lyskom-elib-avl-cmpfun tree))))
    (lyskom-elib-node-set-left (lyskom-elib-avl-dummyroot new-tree)
			(lyskom-elib-avl-do-copy (lyskom-elib-avl-root tree)))
    new-tree))


(defun lyskom-avltree-flatten (tree)
  "Return a sorted list containing all elements of TREE."
  (nreverse
   (let ((treelist nil))
     (lyskom-elib-avl-mapc (function (lambda (node)
				(setq treelist (cons (lyskom-elib-node-data node)
						     treelist))))
		    (lyskom-elib-avl-root tree))
     treelist)))


(defun lyskom-avltree-size (tree)
  "Return the number of elements in TREE."
  (let ((treesize 0))
    (lyskom-elib-avl-mapc (function (lambda (data)
			       (setq treesize (1+ treesize))
			       data))
		   (lyskom-elib-avl-root tree))
    treesize))


(defun lyskom-avltree-clear (tree)
  "Clear the avl tree TREE."
  (lyskom-elib-node-set-left (lyskom-elib-avl-dummyroot tree) nil))

;;; avltree.el ends here
