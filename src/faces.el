;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: faces.el,v 44.5 2003-03-16 14:03:01 byers Exp $
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
;;;; File: faces.el
;;;;
;;;; Code to handle faces in LysKOM
;;;;
;;;; ================================================================
;;;; Faces in LysKOM
;;;;
;;;; There are a bunch of face variables that define witch face to use
;;;; in various roles. By using variables as a secnod level of
;;;; indirection it is possible to have divveret faces in different
;;;; buffers.
;;;;
;;;; The concept of face schemes that was used in versions up to
;;;; and including 0.47.1 is completely out the door.
;;;; ================================================================

;;;; TODO:
;;;;
;;;; Setup faces from resources? Do we need that any more? Or will
;;;; defface take care of it for us?
;;;;
;;;; Remove old code.
;;;;
;;;; In the customization buffer we need a new widget type that
;;;; can open customization of a face.
;;;;
;;;; The customization buffer should provide a choice for the
;;;; user of nil, the "factory default" and a specific face.
;;;;
;;;; Other than that, it's more or less done.



(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: faces.el,v 44.5 2003-03-16 14:03:01 byers Exp $\n"))



;;; ================================================================
;;; Face variables
;;;
;;; Here's a list of all face variables. Seems to be useful on occasion.
;;;
;;; kom-active-face
;;; kom-url-face
;;; kom-me-face
;;; kom-highlight-face
;;; kom-text-face
;;; kom-subject-face
;;; kom-text-no-face
;;; kom-friends-face
;;; kom-morons-face
;;; kom-presence-face
;;; kom-first-line-face
;;; kom-warning-face
;;; kom-mark-face
;;; kom-dim-face
;;; kom-text-body-face
;;; kom-dashed-lines-face
;;; kom-async-text-body-face
;;; kom-async-dashed-lines-face

(def-kom-var kom-active-face 'kom-face--default--active-face
  "*Face used for most \"clickable\" areas.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--active-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-active-strikethrough-face 'kom-face--default--active-strikethrough-face
  "*Face used for some \"clickable\" areas.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--active-strikethrough-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-active-highlight-face 'kom-face--default--active-highlight-face
  "*Face used for some \"clickable\" areas.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--active-highlight-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-url-face 'kom-face--default--url-face
  "*Face used for URLs.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--url-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-me-face 'kom-face--default--me-face
  "*Face used to show your own name.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--me-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-highlight-face 'kom-face--default--highlight-face
  "*Face used for various highlights.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--highlight.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-subject-face 'kom-face--default--subject-face
  "*Face used for subject lines.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--subject-fac.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-text-no-face 'kom-face--default--text-no-face
  "*Face used for text numbers.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--text-no-fac.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-friends-face 'kom-face--default--friends-face
  "*Face used for people in kom-friends.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--friends-fac.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-morons-face 'kom-face--default--morons-face
  "*Face used for people in kom-morons.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--morons-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-presence-face 'kom-face--default--presence-face
  "*Face used for presence messages.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--presence-f.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-first-line-face 'kom-face--default--first-line-face
  "*Face used for the first line header of each text.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--first-li.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-warning-face 'kom-face--default--warning-face
  "*Face used to display important warnings.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--warning-fac.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-mark-face 'kom-face--default--mark-face
  "*Face used for temporary marks.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--mark-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-dim-face 'kom-face--default--dim-face
  "*Face used to display dimmed items, such as passive memberships.
The value should be either a symbol naming a face or nil.
The default value is kom-face--default--dim-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-text-body-face nil
  "*Face to use to modify text bodies.
The value should be either a symbol naming a face or nil.

If this is set to nil, a face with a background that differs ever
so slightly from the default background will be created.

The default value is nil.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-dashed-lines-face nil
  "*Face to use to modify the dashed lines before and after texts.
The value should be either a symbol naming a face or nil.

If this is set to nil, a face with a background that differs slightly
from the default background will be created.

The default value is nil.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-async-text-body-face nil
  "*Face to use to modify asynchronos message bodies.
The value should be either a symbol naming a face or nil.

If this is set to nil, a face with a background that differs ever
so slightly from the default background will be created.

The default value is nil.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-async-dashed-lines-face nil
  "*Face to use to modify dashed lines around asynchronous messsages.
The value should be either a symbol naming a face or nil.

If this is set to nil, a face with a background that differs slightly
from the default background will be created.

The default value is nil.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var lyskom-default-text-body-face nil
  "Default text body face."
  inherited)

(def-kom-var lyskom-default-async-text-body-face nil
  "Default text body face."
  inherited)

(def-kom-var lyskom-default-dashed-lines-face nil
  "Default text body face."
  inherited)

(def-kom-var lyskom-default-async-dashed-lines-face nil
  "Default text body face."
  inherited)


;;; ================================================================
;;; Main entry points

(defun lyskom-setup-faces-for-buffer ()
  "Set up faces for the current buffer."
    (setq lyskom-default-text-body-face 'default)
    (setq lyskom-default-dashed-lines-face 'default)
    (setq lyskom-default-async-text-body-face 'default)
    (setq lyskom-default-async-dashed-lines-face 'default)
    (condition-case nil
	(let ((faces (lyskom-generate-faces-for-background
		      (or (face-background 'default)
			  (frame-property (selected-frame) 'background-color)
			  "#ffffff"))))
	  (setq lyskom-default-text-body-face
		(cdr (assq 'text-body faces))
		lyskom-default-dashed-lines-face
		(cdr (assq 'dashed-lines faces))
		lyskom-default-async-text-body-face
		(cdr (assq 'async-text-body faces))
		lyskom-default-async-dashed-lines-face
		(cdr (assq 'async-dashed-lines faces))))
      (error nil)))


;;; ================================================================
;;; Background faces
;;;
;;; The faces for text backgrounds cannot be hard-coded. It's OK
;;; to hard-code some known alternatives, but in the general case
;;; we're better off calculating the colors.
;;;
;;; The user can specify faces for these properties, but can also
;;; specify "default", which means calculate the face.
;;;

(defvar lyskom-background-faces nil
  "Pool of background faces.
This is an alist of `(COLOR . FACES)' elements, where COLOR is a
canonical representation of a background color and FACES is an
alist of automatically generated faces to use in that background.
Each element of FACES is of the form `(KEY . FACE-NAME)', where
KEY is one of `text-body', `dashed-lines', `async-text-body' or
`async-dashed-lines', and FACE-NAME is the name of the face to
use for the feature specified by KEY.

When the faces for a buffer are set, the client will look in this 
pool for the appropriate faces. If none are found, new faces will
be generated and stored in the pool.

This list is not consulted at any other time.

See `lyskom-background-colors' for additional information.")


(defvar lyskom-background-colors
  '(("#ffffff" 
     (text-body . "#f8f8ff")
     (dashed-lines . "#e8e8ff")
     (async-text-body . "#f8fff8")
     (async-dashed-lines . "#e8ffe8"))
    ("#000000"
     (text-body . "#080808")
     (dashed-lines . "#101010")
     (async-text-body . "#000020")
     (async-dashed-lines . "#101030")))
  "Specification for special background faces.

This is an alist of `(COLOR . SPEC)' elements, where COLOR is a color
name and SPEC is an alist specifying background colors to use when the
main background is COLOR. Elements in SPEC are `(KEY . COLOR)' where
KEY is one of `text-body', `dashed-lines', `async-text-body' or
`async-dashed-lines' and COLOR is the name of the background color to
use for the feature indicated by KEY.

This list is consulted only when generating new faces for a particular
background color. Changes to it after starting the client may have no
effect.

See `lyskom-background-faces' for additional information."
)

(defun lyskom-canonicalize-color (color)
  "Create a canonical string name for color COLOR.
COLOR must be a string (a color name)"
  (apply 'format 
         "#%02x%02x%02x"
         (mapcar (lambda (x) (lsh x -8))
                 (lyskom-color-values color))))

(defun lyskom-generate-faces-for-background (background)
  "Generate highlight faces for background BACKGROUND.
BACKGROUND must be a color name. This function updates
`lyskom-background-colors' and returns a list that has the same
structure as the value of an element in that list. See the documentation
for `lyskom-background-colors' for more information."
  (let* ((color (lyskom-canonicalize-color background))
         (cached (cdr (assoc color lyskom-background-faces))))
    (unless cached
      (let* ((text-body-face-name
              (intern (format "lyskom-generated-text-body-face-%s" color)))
             (dashed-lines-face-name
              (intern (format "lyskom-generated-dashed-lines-face-%s" color)))
             (async-text-body-face-name
              (intern (format "lyskom-generated-async-text-body-face-%s" color)))
             (async-dashed-lines-face-name
              (intern (format "lyskom-generated-async-dashed-lines-face-%s" color)))
             (predefined (cdr (assoc color lyskom-background-colors)))
             (weak (lyskom-get-color-highlight (lyskom-color-values background) 0.025))
             (strong (lyskom-get-color-highlight (lyskom-color-values background) 0.05))
             (text-body-color (or (cdr (assq 'text-body predefined)) weak))
             (dashed-lines-color (or (cdr (assq 'dashed-lines predefined)) strong))
             (async-text-body-color (or (cdr (assq 'async-text-body predefined)) weak))
             (async-dashed-lines-color (or (cdr (assq 'async-dashed-lines predefined)) strong))
             )
        (make-face text-body-face-name)
        (set-face-background text-body-face-name text-body-color)
        (make-face dashed-lines-face-name)
        (set-face-background dashed-lines-face-name dashed-lines-color)
        (make-face async-text-body-face-name)
        (set-face-background async-text-body-face-name async-text-body-color)
        (make-face async-dashed-lines-face-name)
        (set-face-background async-dashed-lines-face-name async-dashed-lines-color)
        (setq cached
              `((text-body ,text-body-face-name)
                (dashed-lines ,dashed-lines-face-name)
                (async-text-body ,async-text-body-face-name)
                (async-dashed-lines ,async-dashed-lines-face-name)))
        (setq lyskom-background-faces
              (cons (cons color cached) lyskom-background-faces))))
    cached))




;;; ================================================================
;;; Predefined faces
;;;
;;; There are two sets of predefined faces: the defaults and the
;;; user-defined faces. Users should not muck with the defaults.

(defface kom-face--default--active-face
  '((((background light))
     (:foreground "blue4")
     )
    (((background dark))
     (:foreground "lightblue")
     )
    )
  "Face suitable for most \"clickable\" areas.
Do not alter this face unless you know what you're doing."
  :group 'lyskom-faces
  )

(defface kom-face--default--active-strikethrough-face
  '((((background light))
     (:foreground "blue4"
      :strike-through t)
     )
    (((background dark))
     (:foreground "lightblue"
      :strike-through t)
     )
    )
  "Face suitable for some \"clickable\" areas.
Do not alter this face unless you know what you're doing."
  :group 'lyskom-faces
  )

(defface kom-face--default--active-highlight-face
  '((((background light))
     (:foreground "blue4"
      :weight bold)
     )
    (((background dark))
     (:foreground "lightblue"
      :weight bold)
     )
    )
  "Face suitable for some \"clickable\" areas.
Do not alter this face unless you know what you're doing."
  :group 'lyskom-faces
  )


(defface kom-face--default--url-face
  '((((background light))
     (:foreground "blueviolet")
     )
    (((background dark))
     (:foreground "moccasin")
     )
    )
  "Face suitable for URLs.
Do not change this face unless you know what you're doing."
  :group 'lyskom-faces
  )


(defface kom-face--default--me-face
  '((((background light))
     (:foreground "blue3"
     :background "lavender"
     :weight bold)
     )
    (((background dark))
     (:foreground "gold"
     :background "black"
     :weight bold)
     )
    )
  "Face suitable for showing your own name.
Do not change this face unless you know what you're doing."
  :group 'lyskom-faces
  )


(defface kom-face--default--highlight-face
  '((((background light))
     (:background "darkseagreen2")
     )
    (((background dark))
     (:background "darkblue")
     )
    )
  "Face suitable for various highlights.
Do not change this face unless you know what you're doing."
  :group 'lyskom-faces
  )

(defface kom-face--default--subject-face
  '((t nil))
  "Face suitable for subject lines.
Do not change this face unless you know what you're doing."
  :group 'lyskom-faces
  )

(defface kom-face--default--text-no-face
  '((((background light))
     (:foreground "blue4")
     )
    (((background dark))
     (:foreground "lightblue")
     )
    )
  "Face suitable for text numbers.
Do not change this face unless you know what you're doing."
  :group 'lyskom-faces
  )

(defface kom-face--default--friends-face
  '((((background light))
     (:foreground "blue3"
     :background "lavender")
     )
    (((background dark))
     (:foreground "red")
     )
    )
  "Face suitable for people in kom-friends.
Do not change this face unless you know what you're doing."
  :group 'lyskom-faces
  )

(defface kom-face--default--morons-face
  '((((background light))
     (:foreground "blue3"
     :background "yellow")
     )
    (((background dark))
     (:foreground "yellow")
     )
    )
  "Face suitable for people in kom-morons.
Do not change this face unless you know what you're doing."
  :group 'lyskom-faces
  )

(defface kom-face--default--presence-face
  '((((background light))
     (:foreground "dim gray"
     :slant italic)
     )
    (((background dark))
     (:foreground "gray"
     :slant italic)
     )
    )
  "Face suitable for presence messages.
Do not change this face unless you know what you're doing."
  :group 'lyskom-faces
  )


(defface kom-face--default--mark-face
  '((((background light))
     (:foreground "blue3"
     :background "lavender")
     )
    (((background dark))
     (:foreground "gold"
     :background "black")
     )
    )
  "Face suitable for temporary marks.
Do not change this face unless you know what you're doing."
  :group 'lyskom-faces
  )


(defface kom-face--default--first-line-face
  '((t (:weight bold)))
  "Face suitable for the first line header of each text.
Do not change this face unless you know what you're doing."
  :group 'lyskom-faces
  )

(defface kom-face--default--warning-face
  '((t (:weight bold :foreground "red")))
  "Face suitable for displaying important warnings.
Do not change this face unless you know what you're doing."
  :group 'lyskom-faces
  )

(defface kom-face--default--dim-face
  '((t (:foreground "gray")))
  "Face suitable for dimmed items, such as passive memberships.
Do not change this face unless you know what you're doing."
  :group 'lyskom-faces
  )


;;; ----------------------------------------------------------------
;;; Dummy faces (copies of the default faces for uses to muck with.
;;;

(defface kom-face--user-defined--active-face
  '((((background light))
     (:foreground "blue4")
     )
    (((background dark))
     (:foreground "lightblue")
     )
    )
  "Face suitable for most \"clickable\" areas."
  :group 'lyskom-faces
  )


(defface kom-face--user-defined--active-strikethrough-face
  '((((background light))
     (:foreground "blue4"
      :strike-through t)
     )
    (((background dark))
     (:foreground "lightblue"
      :strike-through t)
     )
    )
  "Face suitable for some \"clickable\" areas."
  :group 'lyskom-faces
  )

(defface kom-face--user-defined--active-highlight-face
  '((((background light))
     (:foreground "blue4"
      :weight bold)
     )
    (((background dark))
     (:foreground "lightblue"
      :weight bold)
     )
    )
  "Face suitable for some \"clickable\" areas."
  :group 'lyskom-faces
  )


(defface kom-face--user-defined--url-face
  '((((background light))
     (:foreground "blueviolet")
     )
    (((background dark))
     (:foreground "moccasin")
     )
    )
  "Face suitable for URLs."
  :group 'lyskom-faces
  )


(defface kom-face--user-defined--me-face
  '((((background light))
     (:foreground "blue3"
     :background "lavender"
     :weight bold)
     )
    (((background dark))
     (:foreground "gold"
     :background "black"
     :weight bold)
     )
    )
  "Face suitable for showing your own name."
  :group 'lyskom-faces
  )


(defface kom-face--user-defined--highlight-face
  '((((background light))
     (:background "darkseagreen2")
     )
    (((background dark))
     (:background "darkblue")
     )
    )
  "Face suitable for various highlights."
  :group 'lyskom-faces
  )

(defface kom-face--user-defined--subject-face
  '((t nil))
  "Face suitable for subject lines."
  :group 'lyskom-faces
  )

(defface kom-face--user-defined--text-no-face
  '((((background light))
     (:foreground "blue4")
     )
    (((background dark))
     (:foreground "lightblue")
     )
    )
  "Face suitable for text numbers."
  :group 'lyskom-faces
  )

(defface kom-face--user-defined--friends-face
  '((((background light))
     (:foreground "blue3"
     :background "lavender")
     )
    (((background dark))
     (:foreground "red")
     )
    )
  "Face suitable for people in kom-friends."
  :group 'lyskom-faces
  )

(defface kom-face--user-defined--morons-face
  '((((background light))
     (:foreground "blue3"
     :background "yellow")
     )
    (((background dark))
     (:foreground "yellow")
     )
    )
  "Face suitable for people in kom-morons."
  :group 'lyskom-faces
  )

(defface kom-face--user-defined--presence-face
  '((((background light))
     (:foreground "dim gray"
     :slant italic)
     )
    (((background dark))
     (:foreground "gray"
     :slant italic)
     )
    )
  "Face suitable for presence messages."
  :group 'lyskom-faces
  )


(defface kom-face--user-defined--mark-face
  '((((background light))
     (:foreground "blue3"
     :background "lavender")
     )
    (((background dark))
     (:foreground "gold"
     :background "black")
     )
    )
  "Face suitable for temporary marks."
  :group 'lyskom-faces
  )


(defface kom-face--user-defined--first-line-face
  '((t (:weight bold)))
  "Face suitable for the first line header of each text."
  :group 'lyskom-faces
  )

(defface kom-face--user-defined--warning-face
  '((t (:weight bold :foreground "red")))
  "Face suitable for displaying important warnings."
  :group 'lyskom-faces
  )

(defface kom-face--user-defined--dim-face
  '((t (:foreground "gray")))
  "Face suitable for dimmed items, such as passive memberships."
  :group 'lyskom-faces
  )





(defvar lyskom-predefined-faces
  '((kom-active-face . (kom-face--default--active-face
                        kom-face--user-defined--active-face))
    (kom-url-face . (kom-face--default--url-face
                     kom-face--user-defined--url-face))
    (kom-me-face . (kom-face--default--me-face
                    kom-face--user-defined--me-face))
    (kom-highlight-face . (kom-face--default--highlight-face
                           kom-face--user-defined--highlight-face))
    (kom-text-face . (kom-face--default--text-face
                      kom-face--user-defined--text-face))
    (kom-subject-face . (kom-face--default--subject-face
                         kom-face--user-defined--subject-face))
    (kom-text-no-face . (kom-face--default--text-no-face
                         kom-face--user-defined--text-no-face))
    (kom-friends-face . (kom-face--default--friends-face
                         kom-face--user-defined--friends-face))
    (kom-morons-face . (kom-face--default--morons-face
                        kom-face--user-defined--morons-face))
    (kom-presence-face . (kom-face--default--presence-face
                          kom-face--user-defined--presence-face))
    (kom-first-line-face . (kom-face--default--first-line-face
                            kom-face--user-defined--first-line-face))
    (kom-warning-face . (kom-face--default--warning-face
                         kom-face--user-defined--warning-face))
    (kom-mark-face . (kom-face--default--mark-face
                      kom-face--user-defined--mark-face))
    (kom-dim-face . (kom-face--default--dim-face
                     kom-face--user-defined--dim-face))
    (kom-text-body-face . (kom-face--default--text-body-face
                           kom-face--user-defined--text-body-face))
    (kom-dashed-lines-face . (kom-face--default--dashed-lines-face
                              kom-face--user-defined--dashed-lines-face))
    (kom-async-text-body-face . (kom-face--default--async-text-body-face
                                 kom-face--user-defined--async-text-body-face))
    (kom-async-dashed-lines-face . (kom-face--default--async-dashed-lines-face
                                    kom-face--user-defined--async-dashed-lines-face))
    (kom-active-highlight-face . (kom-face--default--active-highlight-face
                                  kom-face--user-defined--active-highlight-face))
    (kom-active-strikethrough-face . (kom-face--default--active-strikethrough-face
                                      kom-face--user-defined--active-strikethrough-face))

    )
  "Alist of predefined faces suitable for each face variable.
Used for customization and nothing else."
)
