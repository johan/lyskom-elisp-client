;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: faces.el,v 44.2 2003-01-02 17:12:26 byers Exp $
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
	      "$Id: faces.el,v 44.2 2003-01-02 17:12:26 byers Exp $\n"))



;;; ================================================================
;;; Face variables
;;;
;;; Aye, there are a lot of them.
;;;

(def-kom-var kom-active-face 'kom-face--plain--active-face
  "*Face used for most \"clickable\" areas.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--active-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-url-face 'kom-face--plain--url-face
  "*Face used for URLs.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--url-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-me-face 'kom-face--plain--me-face
  "*Face used to show your own name.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--me-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-highlight-face 'kom-face--plain--highlight-face
  "*Face used for various highlights.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--highlight.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-text-face 'kom-face--plain--text-face
  "*Face used for text bodies.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--text-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-subject-face 'kom-face--plain--subject-face
  "*Face used for subject lines.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--subject-fac.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-text-no-face 'kom-face--plain--text-no-face
  "*Face used for text numbers.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--text-no-fac.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-friends-face 'kom-face--plain--friends-face
  "*Face used for people in kom-friends.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--friends-fac.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-morons-face 'kom-face--plain--morons-face
  "*Face used for people in kom-morons.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--morons-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-presence-face 'kom-face--plain--presence-face
  "*Face used for presence messages.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--presence-f.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-first-line-face 'kom-face--plain--first-line-face
  "*Face used for the first line header of each text.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--first-li.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-warning-face 'kom-face--plain--warning-face
  "*Face used to display important warnings.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--warning-fac.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-mark-face 'kom-face--plain--mark-face
  "*Face used for temporary marks.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--mark-face.

This is a LysKOM face variable."
  server
  inherited)

(def-kom-var kom-dim-face 'kom-face--plain--dim-face
  "*Face used to display dimmed items, such as passive memberships.
The value should be either a symbol naming a face or nil.
The default value is kom-face--plain--dim-face.

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


;;; ================================================================
;;; Main entry points

(defun lyskom-setup-faces-for-buffer ()
  "Set up faces for the current buffer."
  (let ((faces (lyskom-generate-faces-for-background
                (or (face-background 'default)
                    (frame-property (selected-frame) 'background-color)
                    "#ffffff"))))
    ;; We've generated default background faces. Now either use them
    ;; or lose them. If the user has selected specific faces, then
    ;; don't use the generated faces.
    (unless (facep kom-text-body-face)
      (setq kom-text-body-face (cdr (assq 'text-body faces))))
    (unless (facep kom-dashed-lines-face)
      (setq kom-dashed-lines-face (cdr (assq 'dashed-lines faces))))
    (unless (facep kom-async-text-body-face)
      (setq kom-async-text-body-face (cdr (assq 'async-text-body faces))))
    (unless (facep kom-async-dashed-lines-face)
      (setq kom-async-dashed-lines-face (cdr (assq 'async-dashed-lines faces))))
    ))


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

(defface kom-face--plain--active-face
  '((((background light))
     (:foreground "blue4")
     )
    (((background dark))
     (:foreground "lightblue")
     )
    )
  "Face used for most \"clickable\" areas."
  :group 'lyskom-faces
  )


(defface kom-face--plain--url-face
  '((((background light))
     (:foreground "blueviolet")
     )
    (((background dark))
     (:foreground "moccasin")
     )
    )
  "Face used for URLs."
  :group 'lyskom-faces
  )


(defface kom-face--plain--me-face
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
  "Face used to show your own name."
  :group 'lyskom-faces
  )


(defface kom-face--plain--highlight-face
  '((((background light))
     (:background "seagreen")
     )
    (((background dark))
     (:background "darkblue")
     )
    )
  "Face used for various highlights."
  :group 'lyskom-faces
  )

(defface kom-face--plain--text-face
  '((t nil))
  "Face used for text bodies."
  :group 'lyskom-faces
  )

(defface kom-face--plain--subject-face
  '((t nil))
  "Face used for subject lines."
  :group 'lyskom-faces
  )

(defface kom-face--plain--text-no-face
  '((((background light))
     (:foreground "blue4")
     )
    (((background dark))
     (:foreground "lightblue")
     )
    )
  "Face used for text numbers."
  :group 'lyskom-faces
  )

(defface kom-face--plain--friends-face
  '((((background light))
     (:foreground "blue3"
     :background "lavender")
     )
    (((background dark))
     (:foreground "red")
     )
    )
  "Face used for people in kom-friends."
  :group 'lyskom-faces
  )

(defface kom-face--plain--morons-face
  '((((background light))
     (:foreground "blue3"
     :background "yellow")
     )
    (((background dark))
     (:foreground "yellow")
     )
    )
  "Face used for people in kom-morons."
  :group 'lyskom-faces
  )

(defface kom-face--plain--presence-face
  '((((background light))
     (:foreground "dim gray"
     :slant italic)
     )
    (((background dark))
     (:foreground "gray"
     :slant italic)
     )
    )
  "Face used for presence messages."
  :group 'lyskom-faces
  )


(defface kom-face--plain--mark-face
  '((((background light))
     (:foreground "blue3"
     :background "lavender")
     )
    (((background dark))
     (:foreground "gold"
     :background "black")
     )
    )
  "Face used for temporary marks."
  :group 'lyskom-faces
  )


(defface kom-face--plain--first-line-face
  '((t (:weight bold)))
  "Face used for the first line header of each text."
  :group 'lyskom-faces
  )

(defface kom-face--plain--warning-face
  '((t (:weight bold :foreground "red")))
  "Face used to display important warnings."
  :group 'lyskom-faces
  )

(defface kom-face--plain--dim-face
  '((t (:foreground "gray")))
  "Face used to display dimmed items, such as passive memberships."
  :group 'lyskom-faces
  )
