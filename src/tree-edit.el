;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: tree-edit.el,v 44.3 2003-01-05 21:37:08 byers Exp $
;;;;; Copyright (C) 2001 David Byers
;;;;;
;;;;; This file is part of the LysKOM Emacs lisp client
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
;;;; File: tree-edit.el
;;;;
;;;; This file contains basic code for editing trees.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: tree-edit.el,v 44.3 2003-01-05 21:37:08 byers Exp $\n"))

;;;;
;;;; TODO
;;;;
;;;; Cache hanging-indent-strings 'cos it'll be expensive to do
;;;; make-string so many times.
;;;;
;;;; Compile tree styles?
;;;;
;;;; Have a link from each child to the root, and store tree
;;;; style in the root (or perhaps let tree-get-property 
;;;; traverse the parent links so we can do inherited 
;;;; properties).
;;;;
;;;; Make it possible to collapse all but the first line of a
;;;; multiline item (with minimal redrawing, please).
;;;;
;;;; Optimize drawing a bit. Right now, if we attach a node, we
;;;; redraw the parent (and therefore all the children). This is
;;;; easy, but some trees are really deep (we hit max-specpdl-size
;;;; in one example), so doing this is needlessly expensive.
;;;;
;;;; Allow us to set a prefix length for the entire tree and have
;;;; prefixes to the items. These are text snippets to display to
;;;; the far left of the tree.
;;;;
;;;; Make it possible to collapse the parent of a node. Doing this
;;;; means that we don't display the entire path to the node, just
;;;; a marker saying that this node is underneath something else.
;;;;
;;;; Implement incremental computation of children, so that we can
;;;; set the prune attribute of a node and register a callback to
;;;; compute children of the node.
;;;;
;;;; Implement incremental computation of parent nodes.
;;;;
;;;; Document tree node properties
;;;; - prune (don't draw the subtree)
;;;; - leaf (don't allow move-up and friends to attach a subtree)
;;;; - hanging-indent (hanging indent of multiline items, string or int)
;;;;


(defvar tree-edit-kill-buffer nil
  "Buffer containing the most recently killed tree.")


;;; ============================================================
;;; Drawing styles

(defvar tree-edit-current-style 'compact)
(defvar tree-edit-styles
  '((default . ((indent-a . "|  ")
                (indent-b . "   ")
                (before-children . "|\n")
                (after-children . "\n")
                (item-header . tree-edit-item-header)
                (last-item-header . tree-edit-last-item-header)))

    (compact . ((indent-a . "|  ")
                (indent-b . "   ")
                (before-children . nil)
                (after-children . nil)
                (item-header . tree-edit-item-header)
                (last-item-header . tree-edit-last-item-header)))

    (semi-compact . ((indent-a . "|  ")
                     (indent-b . "   ")
                     (before-children . nil)
                     (after-children . "\n")
                     (item-header . tree-edit-item-header)
                     (last-item-header . tree-edit-last-item-header)))

    (very-compact . ((indent-a . "|")
                     (indent-b . " ")
                     (before-children . nil)
                     (after-children . nil)
                     (item-header . "+")
                     (last-item-header . "`")))
    "Definitions of tree styles.
Each item in this list is a definition of a tree style. A 
definition is a cons whose car is the name of the style (a 
symbol) and the cdr is an alist specifying the style.

The following keys must be present in the alist defining the
style:

indent-a        The string to print to the right of items
                that is used to connect children of a node,
                when there are additional children at this
                level. Normally a vertical bar and some
                whitespace.

indent-b        The string to print to the right of items
                to indent them when there are no additional
                children at this level. Normally some 
                whitespace.

before-children The string to print before the first child of
                a node. This string will be prefixed as the 
                parent node.

after-children  The string to print after the last child of
                a node. This string will be prefixed as the 
                parent node.

item-header     The header of an item. Normally this connects
                the indent-a printed on the prevous line with
                the current item and the indent-a below.

last-item-header   The header used for the last child of a 
                node. This item usually connects the indent-a
                printed on the previous line with the indent-b
                printed on the next line.

IMPORTANT: The lenghts of indent-a, indent-b and last-item-header
           must be the same for all nodes at the same level in
           the tree.

The cdrs of each item in the defining alist is either a string;
the name of a function (a symbol) that returns a string or nil; 
or nil (in which case nothing is printed).
"
    ))

(defun tree-edit-get-style-data (item &optional style)
  (cdr (assq item (cdr (assq (or style tree-edit-current-style)
                             tree-edit-styles)))))


;;; ============================================================
;;; Error symbols

(put 'tree-edit-error 'error-conditions '(tree-edit-error error))
(put 'tree-edit-error 'error-message "Error editing tree")

(put 'tree-edit-nothing-to-kill 'error-conditions '(tree-edit-nothing-to-kill tree-edit-error error))
(put 'tree-edit-nothing-to-kill 'error-message "Nothing to kill here")

(put 'tree-edit-cant-yank-outside-tree 'error-conditions '(tree-edit-cant-yank-outside-tree tree-edit-error error))
(put 'tree-edit-cant-yank-outside-tree 'error-message "Can't yank here")

(put 'tree-edit-cant-yank-toplevel 'error-conditions '(tree-edit-cant-yank-toplevel tree-edit-error error))
(put 'tree-edit-cant-yank-toplevel 'error-message "Can't yank here")

(put 'tree-edit-cant-move-outside-tree 'error-conditions '(tree-edit-cant-move-outside-tree tree-edit-error error))
(put 'tree-edit-cant-move-outside-tree 'error-message "Cant't add tree nodes outside the tree")

(put 'tree-edit-no-tree 'error-conditions '(tree-edit-no-tree tree-edit-error error))
(put 'tree-edit-no-tree 'error-message "Not on a tree node")

(put 'tree-edit-cant-move-root 'error-conditions '(tree-edit-cant-move-root tree-edit-error error))
(put 'tree-edit-cant-move-root 'error-message "Can't move the root of a tree")

(put 'tree-edit-no-parent 'error-conditions '(tree-edit-no-parent tree-edit-error error))
(put 'tree-edit-no-parent 'error-message "Unable to determine parent of node")

(put 'tree-edit-leaf-node-adopt 'error-conditions '(tree-edit-leaf-node-adopt tree-edit-error error))
(put 'tree-edit-no-parent 'error-message "Can't add children to a leaf node")



;;; ============================================================
;;; Basic data operations
;;;

(def-komtype tree-node 
  (
   text                                 ; Text of node
   data                                 ; Data of node
   start                                ; Start marker for node
   text-end                             ; Marker for end of node text
   text-start                           ; Marker for start of node text
   end                                  ; End marker for entire subtree
   parent                               ; Parent of node
   properties                           ; General properties (plist)
   children                             ; Children of node
   ))

(defun tree-node-root (node)
  "Return the root of the tree that includes NODE."
  (while (tree-node->parent node)
    (setq node (tree-node->parent node)))
  node)

(defun tree-node-next-sibling (node)
  "Return the next sibling of NODE if there is one"
  (and (tree-node->parent node)
       (car (cdr (memq node (tree-node->children (tree-node->parent node)))))))

(defun tree-node-previous-sibling (node)
  "Return the previous sibling of NODE if there is one"
  (when (tree-node->parent node)
    (let ((sibs (memq node (tree-node->children (tree-node->parent node)))))
      (cond ((>= (length sibs) (length (tree-node->children (tree-node->parent node))))
             nil)
            (t (car (nthcdr (1- (- (length (tree-node->children (tree-node->parent node))) (length sibs)))
                            (tree-node->children (tree-node->parent node)))))))))

(defun tree-node-property (node property &optional default)
  "For NODE, return PROPERTY.
If there is no property PROPERTY for NODE, return DEFAULT."
  (if (plist-member (tree-node->properties node) property)
      (plist-get (tree-node->properties node) property)
    default))

(defun set-tree-node-property (node property value)
  "For NODE, set property PROPERTY to VALUE."
  (set-tree-node->properties 
   node
   (plist-put (tree-node->properties node) property value)))

(defun tree-node-copy-sequence (data)
  "Copy the sequence DATA."
  (cond ((vectorp data) (copy-sequence data))
        ((consp data)
         (let (result dotted)
           (while data
             (setq result (cons (car data) result))
             (if (consp (cdr data))
                 (setq data (cdr data))
               (setq dotted (cons data result) data nil)))
           (setq result (nreverse result))
           (when dotted (setcdr (cdr dotted) (cdr (car dotted))))
           result))))

(defun tree-node-copy-tree (data)
  "Recursively copy list or vector DATA."
  (if (or (listp data) (vectorp data))
      (let ((result (tree-node-copy-sequence data)))
        (cond ((listp result)
               (let ((tmp result))
               (while (and (listp tmp) tmp)
                 (setcar tmp (tree-node-copy-tree (car tmp)))
                 (setq tmp (cdr tmp)))))
              ((vectorp result)
               (let ((i 0))
                 (while (< i (length result))
                   (aset result i (tree-node-copy-tree (aref result i)))
                   (setq i (1+ i))))))
        result)
    data))

(defun tree-node-copy (node)
  "Create a copy of tree node NODE."
  (let ((tmp (lyskom-create-tree-node (copy-seq (tree-node->text node))
                                      (tree-node-copy-tree (tree-node->data node))
                                      (and (tree-node->start node) (copy-marker (tree-node->start node)))
                                      (and (tree-node->text-end node) (copy-marker (tree-node->text-end node)))
                                      (and (tree-node->text-start node) (copy-marker (tree-node->text-start node)))
                                      (and (tree-node->end node) (copy-marker (tree-node->end node)))
                                      (tree-node->parent node)
                                      (tree-node-copy-tree (tree-node->properties node))
                                      nil)))
    (set-tree-node->children tmp
                             (mapcar (lambda (child)
                                       (let ((newchild (tree-node-copy child)))
                                         (set-tree-node->parent newchild tmp)
                                         newchild))
                                     (tree-node->children node)))
    tmp))



;;; ============================================================
;;; Basic drawing


(defun tree-edit-insert-part (part tree prefix)
  (let ((fn (tree-edit-get-style-data part)))
    (cond ((functionp fn) (setq fn (funcall fn tree)))
          ((and (symbolp fn) (boundp fn)) 
           (setq fn (symbol-value fn))))
    (cond ((null fn))
          ((stringp fn) 
           (insert (tree-edit-prefix-string prefix))
           (insert fn)))))

(defun tree-edit-prefix-string (prefix)
  "Return a string of spaces and bars according to values in PREFIX.
Used to draw the spaces and bars before a tree entry. Any strings
in PREFIX are included verbatim."
  (apply 'concat (mapcar (lambda (x) 
                           (cond ((stringp x) x)
                                 (x (tree-edit-get-style-data 'indent-a))
                                 (t (tree-edit-get-style-data 'indent-b)))) 
                         prefix)))

(defun tree-edit-compute-prefix (tree)
  "Compute the prefix for subtree TREE.
Returns a list of nil and non-nil values specifying where to draw
vertical lines and where to not draw vertical lines."
  (let ((result nil)
        (tree (tree-node->parent tree)))
    (while tree
      (setq result (cons (tree-node-next-sibling tree) result))
      (setq tree (tree-node->parent tree)))
    result))

(defun tree-edit-adjust-markers (tree)
  (while (and tree
              (tree-node->end tree)
              (tree-node->children tree))
    (let ((kids (tree-node->children tree)))
      (while kids
        (when (and (tree-node->end (car kids))
                   (> (tree-node->end (car kids))
                      (tree-node->end tree)))
          (set-marker (tree-node->end tree)
                      (tree-node->end (car kids))))
        (setq kids (cdr kids))))
    (setq tree (tree-node->parent tree))))

(defun tree-edit-hanging-indent-prefix (tree indent prefix)
  (let* ((spc-len nil)
         (next-prefix (if (tree-node->children tree)
                          (tree-edit-get-style-data 'indent-a)
                        (tree-edit-get-style-data 'indent-b)))
         (pfx-len (length next-prefix)))
    (cond ((stringp indent)
           (setq spc-len
                 (if (string-match "^\\s-*" indent)
                     (match-end 0)
                   0)))
          ((numberp indent)
           (setq spc-len indent)))

    ;; If spc-len is less than next-prefix, get a bit of next-prefix
    
    (if (> pfx-len spc-len)
        (setq next-prefix (substring next-prefix 0 spc-len)
              pfx-len spc-len)
      (setq spc-len pfx-len))

    (concat next-prefix (cond ((stringp indent)
                               (substring indent pfx-len))
                              ((numberp indent)
                               (make-string (- indent pfx-len) ?\ ))
                              (t "")))))



(defun tree-edit-draw (tree &optional prefix dont-adjust)
  "Draw the tree TREE in the current buffer at point.
Also update the data structures for the tree and add text properties
to the buffer specifying which tree nodes are where.

Optional argument PREFIX is a prefix to add to every line.
If optional argument DONT-ADJUST is non-nil, don't adjust end markers
of parent nodes."
  (let ((worklist nil))
    (setq worklist (list (list tree prefix (point-marker))))
    
    (while worklist
      (let* ((el (car worklist))
             (tree (elt el 0))
             (prefix (elt el 1))
             (marker (elt el 2)))
        (setq worklist (cdr worklist))
        (goto-char marker)
        (set-marker marker nil)
        (unless prefix (setq prefix (tree-edit-compute-prefix tree)))
        (let ((start (point-marker)))

          ;; Insert the tree item without prefixes
          (set-tree-node->text-start tree (point-marker))
          (insert (tree-node->text tree))
          (insert "\n")
          (set-tree-node->text-end tree (point-marker))

          ;; We have inserted the item, so now insert the item header
          ;; and prefixes for all following lines

          (save-excursion
            (goto-char (tree-node->text-start tree))
            (if (tree-node-next-sibling tree)
                (tree-edit-insert-part 'item-header tree prefix)
              (tree-edit-insert-part 'last-item-header tree prefix))
            (forward-line 1)

            ;; From here on we need the prefix for children

            (setq prefix (append prefix (list (tree-node-next-sibling tree))))

            (when (tree-node-property tree 'hanging-indent)
              (let* ((indent (tree-node-property tree 'hanging-indent))
                     (extra-prefix (tree-edit-hanging-indent-prefix 
                                    tree indent prefix)))
                (while (< (point) (tree-node->text-end tree))
                  (insert (tree-edit-prefix-string prefix))
                  (insert extra-prefix)
                  (forward-line 1)))))

          (cond 
           ((tree-node-property tree 'prune)
            (save-excursion (forward-char -1) (insert-before-markers " [...]")))

           ((tree-node->children tree)
            (tree-edit-insert-part 'before-children tree prefix)
            (mapcar (lambda (child) 
                      (setq worklist (cons (list child prefix (point-marker))
                                           worklist)))
                    ;;                  (tree-edit-draw child prefix t))
                    (tree-node->children tree))
            (when (tree-node-next-sibling tree)
              (tree-edit-insert-part 'after-children tree prefix))))

          (set-tree-node->start tree start)
          (set-tree-node->end tree (point-marker))
          (set-marker-insertion-type (tree-node->text-start tree) t)
          (set-marker-insertion-type (tree-node->text-end tree) nil)
          (set-marker-insertion-type (tree-node->start tree) t)
          (set-marker-insertion-type (tree-node->end tree) nil)
          (add-text-properties (tree-node->start tree)
                               (tree-node->text-end tree)
                               (list 'tree-node tree))
          (add-text-properties (tree-node->start tree)
                               (tree-node->end tree)
                               (list 'tree tree))
          (unless dont-adjust (tree-edit-adjust-markers (tree-node->parent tree)))
          )))))


(defun tree-edit-delete-tree (tree)
  "Delete TREE from the current buffer and move point.
Does not change the logical structure of the data but does modify
the tree and its children. Moves point to the original start of the
tree."
  (when (and (tree-node->start tree)
             (tree-node->end tree))
    (goto-char (tree-node->start tree))
    (delete-region (tree-node->start tree)
                   (tree-node->end tree))
    (set-tree-node->start tree nil)
    (set-tree-node->end tree nil)
    (set-tree-node->text-start tree nil)
    (set-tree-node->text-end tree nil)
    (tree-edit-adjust-markers tree)
))



;;; ============================================================
;;; Utility functions for converting to and from trees
;;; Utility functions for accessing data in trees
;;;

(defun tree-edit-compile-tree (tree &optional parent)
  "Compile a sequence TREE to an editable tree structure.
The parent of the tree will be PARENT. TREE can be a list or vector and
has the form (TEXT PROPERTIES DATA . CHILDREN), where TEXT is the text
of the node, PROPERTIES is a property list for the tree node, DATA is
the data and CHILDREN is a list of trees that form the children."
  (when tree
    (let ((node (lyskom-create-tree-node nil nil nil nil nil nil nil nil nil)))
      (set-tree-node->parent node parent)
      (set-tree-node->text node (elt tree 0))
      (set-tree-node->properties node (elt tree 1))
      (set-tree-node->data node (elt tree 2))
      (set-tree-node->children 
       node
       (mapcar (lambda (x) (tree-edit-compile-tree x node))
               (elt tree 3)))
      node)))

(defun tree-edit-collect-data (tree)
  "Collect data from TREE in a nested lisp list.
The resulting list is isomorphic to the tree structure."
  (cond ((tree-node->children tree)
         (cons (tree-node->data tree)
               (mapcar 'tree-edit-collect-data
                       (tree-node->children tree))))
        (t (tree-node->data tree))))


(defmacro tree-edit-traverse-tree (atom tree &rest body)
  (let ((stack (make-symbol "stack")))
    `(let ((,stack (list ,tree))
           (,atom nil))
       (while ,stack
         (setq ,atom (car ,stack))
         (setq ,stack (cdr ,stack))
         (and (tree-node->children ,atom)
              (setq ,stack (append (tree-node->children ,atom) ,stack)))
         ,@body))))



;;; ============================================================
;;; Utility functions for interactive commands
;;;

;;; ----------------------------------------
;;; General utilities

(defun tree-edit-insert-in-list (el list before)
  "Destructively insert EL in LIST before element BEFORE.
If BEFORE is not in the list, then insert EL at the end of the list."
  (cond ((eq before (car list))
         (cons el list))
        (t (setcdr (nthcdr (- (length list)
                              (length (memq before list)) 1) list)
                   (cons el (memq before list)))
           list)))

;;; ----------------------------------------
;;; Expansion of subtrees

(defun tree-edit-toggle-expansion (tree)
  "If children of TREE are visible, hide them, otherwise show them."
  (if (tree-node-property tree 'prune)
      (tree-edit-expand-subtree tree)
    (tree-edit-collapse-subtree tree)))

(defun tree-edit-collapse-subtree (tree &optional recursive)
  "Hide the children of TREE.
If optional RECURSIVE is non-nil, collapse all subtrees as well."
  (when (or recursive (not (tree-node-property tree 'prune)))
    (save-excursion
      (tree-edit-delete-tree tree)
      (set-tree-node-property tree 'prune t)
      (tree-edit-draw tree)
      (when recursive
        (tree-edit-traverse-tree node tree
          (when (tree-node->children node)
            (set-tree-node-property node 'prune t))))
      )))

(defun tree-edit-expand-subtree (tree &optional recursive)
  "Show the children of TREE.
If optional RECURSIVE is non-nil, expand all subtrees as well."
  (when (or recursive (tree-node-property tree 'prune))
    (save-excursion
      (tree-edit-delete-tree tree)
      (set-tree-node-property tree 'prune nil)
      (when recursive
        (tree-edit-traverse-tree node tree
          (when (tree-node->children node)
            (set-tree-node-property node 'prune nil))))
      (tree-edit-draw tree))))



;;; ----------------------------------------
;;; Attachment and removal of subtrees

(defun tree-edit-remove-tree (tree)
  "Remove TREE from its tree."
  (save-excursion
    (tree-edit-delete-tree tree)
    (when (tree-node->parent tree)
      (set-tree-node->children
       (tree-node->parent tree)
       (delq tree (tree-node->children (tree-node->parent tree))))
      (set-tree-node->parent tree nil))))

(defun tree-edit-attach-tree (tree parent &optional before)
  "Attach TREE as a child of PARENT before child BEFORE.
If BEFORE is nil, add as the last child of PARENT.
If TREE already has a parent, remove it from its old parent."
  (save-excursion
    (tree-edit-remove-tree tree)
    (set-tree-node->parent tree parent)
    (set-tree-node->children 
     parent 
     (tree-edit-insert-in-list tree
                               (tree-node->children parent)
                               before))
    (tree-edit-delete-tree parent)
    (tree-edit-draw parent)
))


(defun tree-node-at (pos)
  "Return the tree node at POS."
  (get-text-property pos 'tree-node))

(defun tree-at (pos)
  "Return the top-level tree node of tree at POS."
  (get-text-property pos 'tree))


;;;; FIXME: tree-edit-next-node actually works and is probably better
;;;; FIXME- than the current implementation.
;;;;
;;;; (defun tree-edit-next-node (pos num &optional count-slots)
;;;;   "Traverse the tree from the node at POS, NUM steps upwards.
;;;; Movement is according to what should be displayed in the buffer.
;;;; If optional COUNT-SLOTS is non-nil, the positions at the end of
;;;; existing subtrees are also counted.
;;;; 
;;;; Returns the appropriate node unless count stops on the end of a
;;;; subtree, when it return a cons cell containing the parent and nil."
;;;; 
;;;;   (let ((current (or (tree-node-at pos) 
;;;;                      (tree-node-before pos)))
;;;;         (stack nil)
;;;;         (tmp nil)
;;;;         (result nil))
;;;; 
;;;;     ;; Simulate traversal from the top by filling in the stack
;;;; 
;;;;     (setq tmp (tree-node->parent current) 
;;;;           last current
;;;;           stack (tree-node->children current))
;;;;     (while tmp
;;;;       (setq stack (append stack
;;;;                           (cdr (memq last (tree-node->children tmp))))
;;;;             last tmp
;;;;             tmp (tree-node->parent tmp))
;;;;       (when count-slots (setq stack (append stack (list (cons last nil))))))
;;;; 
;;;; 
;;;;     ;; Now do a real traversal from current
;;;; 
;;;;     (while (and stack (> num 0))
;;;;       (setq num (1- num))
;;;;       (setq result (car stack))
;;;;       (setq stack (cdr stack))
;;;;       (when (and (lyskom-tree-node-p result)
;;;;                  (tree-node->children result)
;;;;                  (not (tree-node-property result 'prune)))
;;;;         (setq stack (append (tree-node->children result)
;;;;                             (and count-slots (list (cons result nil)))
;;;;                             stack))))
;;;;     result))
;;;; 
;;;; (defun tree-edit-previous-node (pos num &optional count-slots)
;;;;   "Traverse the tree from the node at POS, num steps upwards.
;;;; Movement is according to what should be displayed in the buffer.
;;;; If optional COUNT-SLOTS is non-nil, the positions at the end of
;;;; existing subtrees are also counted."
;;;;   (let ((current (or (tree-node-at pos) (tree-node-after pos)))
;;;;         (stack nil)
;;;;         (tmp nil)
;;;;         (result nil))
;;;; 
;;;; ;; Move backwards one step in the tree
;;;; 
;;;;     (setq previous
;;;;           (cond 
;;;; 
;;;;            ;; We have a previous sibling,
;;;; 
;;;;            ((tree-node-previous-sibling current)
;;;;             (let ((tmp (tree-node-previous-sibling current)))
;;;;               (while (tree-node->children tmp)
;;;;                 (setq tmp (last (tree-node->children tmp)))
;;;;             (tree-node-previous-sibling current))
;;;; 
;;;;            ;; No previous sibling. 
;;;; 
;;;;                 
;;;; 
;;;;     )
;;;;   )))))


(defun tree-edit-next-node (pos num &optional count-slots)
  "Traverse the tree from the node at POS, NUM steps downwards.
Movement is according to what should be displayed in the buffer.
If optional COUNT-SLOTS is non-nil, the positions at the end of
existing subtrees are also counted.

Returns the appropriate node unless count stops on the end of a
subtree, when it return a cons cell containing the parent and nil."
  (let* ((start (if (lyskom-tree-node-p pos)
                    pos
                  (or (tree-node-at pos) (tree-node-before pos))))
         (root (and start (tree-node-root start)))
         (tmp (and root (tree-edit-flatten-tree root count-slots))))
    (or (car (nthcdr num (memq start tmp)))
        (car (last tmp))))
)

(defun tree-edit-previous-node (pos num &optional count-slots)
  "Traverse the tree from the node at POS, NUM steps upwards.
Movement is according to what should be displayed in the buffer.
If optional COUNT-SLOTS is non-nil, the positions at the end of
existing subtrees are also counted.

Returns the appropriate node unless count stops on the end of a
subtree, when it return a cons cell containing the parent and nil."
  (let* ((start 
          (if (lyskom-tree-node-p pos)
              pos
            (or (tree-node-at pos) (tree-node-after pos))))
         (root (and start (tree-node-root start)))
         (tmp (and root (nreverse (tree-edit-flatten-tree root count-slots)))))
    (or (car (nthcdr num (memq start tmp)))
        (car (last tmp))))
)


(defun tree-edit-flatten-tree (tree &optional include-slots)
  "Flatten TREE to a list.
If optional INCLUDE-SLOTS is non-nil represent empty slots at the
end of existing subtrees as a list containing only the parent of
the subtree."
  (cond ((and (not (tree-node-property tree 'leaf))
              (not (tree-node-property tree 'prune)))
         (cons tree
               (nconc
                (apply 'nconc
                       (mapcar (lambda (x) (tree-edit-flatten-tree x include-slots))
                               (tree-node->children tree)))
                (and include-slots (list (list tree))))))
        (t (list tree))))



(defun tree-node-after (pos)
  "Return the tree node closest after POS according to the display."
  (save-excursion
    (let ((current (tree-node-at pos)))
      (while (and pos
                  (or (null (get-text-property pos 'tree-node))
                      (eq current (get-text-property pos 'tree-node))))
        (setq pos (next-single-property-change pos 'tree-node)))
      (when pos (get-text-property pos 'tree-node)))))

(defun tree-node-before (pos)
  "Return the tree node closest before POS according to the display."
    (save-excursion 
      (let ((current (tree-node-at pos)))
        (while (and pos
                    (or (null (get-text-property pos 'tree-node))
                        (eq current (get-text-property pos 'tree-node))))
          (setq pos (previous-single-property-change pos 'tree-node)))
        (when pos (get-text-property pos 'tree-node)))))



;;; ============================================================
;;; Interactive functions

(defun tree-edit-next-line (arg)
  "Move cursor vertically down ARG tree elements."
  (interactive "p")
  (if (< arg 0)
      (tree-edit-previous-line arg)
    (let ((node (tree-edit-next-node (point) arg)))
      (when node
        (goto-char (tree-node->text-start node))))))

(defun tree-edit-previous-line (arg)
  "Move cursor vertically up ARG tree elements."
  (interactive "p")
  (if (< arg 0)
      (tree-edit-next-line arg)
    (let ((node (tree-edit-previous-node (point) arg)))
      (when node
        (goto-char (tree-node->text-start node))))))


(defun tree-edit-kill ()
  "Kill the current subtree, making it available for yanking later."
  (interactive)
  (let* ((tree (tree-node-at (point)))
         (parent (tree-node->parent tree))
         (redraw-parent (and parent
                             (null (tree-node-next-sibling tree)))))
    (unless tree (signal 'tree-edit-nothing-to-kill))
    (tree-edit-remove-tree tree)
    (if (eq last-command 'tree-edit-kill)
        (setq tree-edit-kill-buffer (cons tree tree-edit-kill-buffer))
      (setq tree-edit-kill-buffer (list tree)))
    (when redraw-parent
      (tree-edit-delete-tree parent)
      (tree-edit-draw parent))
    (setq this-command 'tree-edit-kill)))


(defun tree-edit-yank (arg)
  "Yank subtrees that have been killed with tree-edit-kill.
With prefix argument, yank as child of the current node."
  (interactive "P")
  (let ((tree (tree-at (point)))
        (before (tree-node-at (point)))
        (node (or (tree-node-at (point))
                  (if arg
                      (or (tree-edit-next-node (point) 1)
                          (tree-edit-previous-node (point) 1))
                    (tree-edit-previous-node (point) 1))))
        (first nil))
    (unless tree (signal 'tree-edit-cant-move-outside-tree))
    (unless (tree-node->parent node) (signal 'tree-edit-cant-yank-toplevel))

    (cond (arg
           (setq before (tree-node-next-sibling node)
                 node (tree-node->parent node)))
          (t (if (and (not before) (tree-node->children node))
                 (setq before (car (tree-node->children node)))
               (setq node (tree-node->parent node)))))
    (mapcar (lambda (x)
              (setq x (tree-node-copy x))
              (unless first (setq first x))
              (tree-edit-attach-tree x
                                     node
                                     before))
            (reverse tree-edit-kill-buffer))
    (when first (goto-char (tree-node->text-start first)))))


(defun tree-edit-move-up (arg)
  "Move current tree node down ARG steps. 
If the node is before a sibling with a subtree, move into that
subtree. If the node is at the end of a subtree, move up one 
level and make it the next sibling of its currrent parent."
  (interactive "p")
  (let* ((tree (tree-node-at (point)))
         (old-prune (tree-node-property tree 'prune))
         (target nil))
    (unless tree (signal 'tree-edit-no-tree))

    (unwind-protect
        (progn
          (set-tree-node-property tree 'prune t)
          (setq target (tree-edit-previous-node tree arg t)))
      (set-tree-node-property tree 'prune old-prune))
    (cond ((lyskom-tree-node-p target)
           (when (tree-node->parent target)
             (tree-edit-remove-tree tree)
             (tree-edit-attach-tree tree (tree-node->parent target) target)))
          ((null target))
          (t 
           (tree-edit-remove-tree tree)
           (tree-edit-attach-tree tree (car target) nil)))
    (goto-char (tree-node->text-start tree))
  ))

(defun tree-edit-move-down (arg)
  "Move current tree node down ARG steps. 
If the node is before a sibling with a subtree, move into that
subtree. If the node is at the end of a subtree, move up one 
level and make it the next sibling of its currrent parent."
  (interactive "p")
  (let* ((tree (tree-node-at (point)))
         (old-prune (tree-node-property tree 'prune))
         (target nil))
    (unless tree (signal 'tree-edit-no-tree))

    (unwind-protect
        (progn
          (set-tree-node-property tree 'prune t)
          (setq target (tree-edit-next-node tree (1+ arg) t)))
      (set-tree-node-property tree 'prune old-prune))
    (cond ((lyskom-tree-node-p target)
           (when (tree-node->parent target)
             (tree-edit-remove-tree tree)
             (tree-edit-attach-tree tree (tree-node->parent target) target)))
          ((null target))
          (t 
           (tree-edit-remove-tree tree)
           (tree-edit-attach-tree tree (car target) nil)))
    (goto-char (tree-node->text-start tree))
  ))

(defun tree-edit-move-in ()
  "Make the current node a child of its previous sibling, if possible."
  (interactive)
  (let* ((node (tree-node-at (point)))
         (parent (and node (tree-node-previous-sibling node))))
    (unless node (signal 'tree-edit-no-tree))
    (unless (tree-node->parent node) (signal 'tree-edit-cant-move-root))
    (unless parent (signal 'tree-edit-cant-find-parent))
    (when (tree-node-property parent 'leaf) (signal 'tree-edit-leaf-node-adopt))

    (tree-edit-remove-tree node)
    (tree-edit-attach-tree node parent)
    (goto-char (tree-node->text-start node))))

(defun tree-edit-move-out ()
  "Make the current node a child of its previous sibling, if possible."
  (interactive)
  (let* ((node (tree-node-at (point)))
         (parent (tree-node->parent node))
         (sib (and parent (tree-node-next-sibling parent))))
    (unless node (signal 'tree-edit-no-tree))
    (unless (and parent (tree-node->parent parent))
      (signal 'tree-edit-cant-move-outside-tree))

    (tree-edit-remove-tree node)
    (tree-edit-attach-tree node (tree-node->parent parent) sib)
    (goto-char (tree-node->text-start node))))

(defun tree-edit-collapse (arg)
  (interactive "P")
  (when (tree-node-at (point))
    (tree-edit-collapse-subtree (tree-node-at (point)) arg)))

(defun tree-edit-expand (arg)
  (interactive "P")
  (when (tree-node-at (point))
    (tree-edit-expand-subtree (tree-node-at (point)) arg)))


;;; Local Variables: 
;;; mode: lisp-interaction
;;; eval: (put 'tree-edit-traverse-tree 'lisp-indent-hook 2)
;;; eval: (put 'tree-edit-traverse-tree 'edebug-form-spec '(sexp form body))
;;; end: 

;;; ============================================================
;;; Stuff for testing

;;; (local-set-key (kbd "C-c p") 'tree-edit-previous-line)
;;; (local-set-key (kbd "<C-M-up>") 'tree-edit-move-up)
;;; (local-set-key (kbd "<C-M-down>") 'tree-edit-move-down)
;;; (local-set-key (kbd "<C-M-right>") 'tree-edit-move-in)
;;; (local-set-key (kbd "<C-M-left>") 'tree-edit-move-out)
;;; (local-set-key (kbd "C-c n") 'tree-edit-next-line)
;;; (local-set-key (kbd "C-c +") 'tree-edit-expand)
;;; (local-set-key (kbd "C-c -") 'tree-edit-collapse)
;;; (local-set-key (kbd "C-c k") 'tree-edit-kill)
;;; (local-set-key (kbd "C-c y") 'tree-edit-yank)
(tree-edit-draw
 (setq foo (tree-edit-compile-tree '("and" nil and (("and\nthis" nil and (("a" (leaf t) a nil)
                                                            ("b" (leaf t) b nil)
                                                            ("or" nil or (("x" (leaf t) x nil)
                                                                          ("not" nil not (("z" (leaf t) z nil)))))
                                                            ("X" (leaf t) x nil)
                                                            ))
                                                ("v" (leaf nil) v nil))))))
;;; +- and
;;;    |
;;;    +- and
;;;    |  |
;;;    |  +- a
;;;    |  +- b
;;;    |  +- or
;;;    |  |  |
;;;    |  |  +- x
;;;    |  |  +- not
;;;    |  |     |
;;;    |  |     +- z
;;;    |  |  
;;;    |  +- X
;;;    |  
;;;    +- v
;;; nil
;;; 
;;; 
;;; (tree-edit-draw (tree-edit-compile-tree foo))
;;; 
;;; 
;;; +- All of
;;;    |
;;;    +- author-re does not match "[Dd]x"
;;;    +- At least one of
;;;       |
;;;       +- recipient contains "Test"
;;;       +- recipient contains "Tset"
;;; nil
