;;Program Name: Etykiety Civil
;;Author: Dariusz Duda
;;dudawebsite.com
;;Description: Wtyczka do tworzenia bloków dynamicznych kot dla przekrojów poprzecznych drogi
(defun c:klp (/ FirstAltitude  NextAltitude NApositionY  FApositionY NApositionX FApositionX OffsetY OffsetX vals)
	(vl-load-com)

	(LM:insertwithstate
		"Kota-mm"
		"Underline YES | Wipeout YES"
	)

	(setq FirstAltitude (vlax-ename->vla-object (entlast)))

	;;Set default Value at First Altitude 
		
  (setq	*ans*
	 (cond
	   (
	    (getreal
	      (strcat "\nWpisz wysokosc odniesienia w metrach <"
		      (rtos
			(setq *ans*
			       (cond (*ans*)
				     (0.00)
			       )
			)
		      )
		      ">: "
	      )
	    )
	   )
	   (*ans*)
	 )
  )

    (LM:vl-setattributevalue
      FirstAltitude
      "TEXT"
      "Odniesienie"
    )
     (LM:vl-setattributevalue
       FirstAltitude
       "HEIGHT"
       (strcat (rtos *ans* 2 2) "m")
     )

    (while (and (setq vals (grread 't)) (not (equal vals '(2 13))))


      (LM:insertwithstate
		"Kota-mm"
		"Underline NO   | Wipeout NO"
      )
      (setq NextAltitude (vlax-ename->vla-object (entlast)))
      (setq NApositionY
	     (strcat
	       "%<\\AcObjProp Object(%<\\_ObjId "
	       (LM:ObjectID NextAltitude)
	       ">%).InsertionPoint \\f \""
	       "%lu2%pt2%pr3"
	       ">%"
	       " - "
	       ;;minus First Altitude at position Y 
	     )
      )
      (setq FApositionY
	     (strcat
	       "%<\\AcObjProp Object(%<\\_ObjId "
	       (LM:ObjectID FirstAltitude)
	       ">%).InsertionPoint \\f \""
	       "%lu2%pt2%pr3%"
	       ">%"
	       " + "
	       ;;plus constant
	       (rtos *ans*)
	       ;;value at First Altitude = constant
	     )
      )
      (setq NApositionX
	     (strcat
	       "%<\\AcObjProp Object(%<\\_ObjId "
	       (LM:ObjectID NextAltitude)
	       ">%).InsertionPoint \\f \""
	       "%lu2%pt1%pr3"
	       ">%"
	       " - "
	       ;;minus First Altitude at position X 
	     )
      )
      (setq FApositionX
	     (strcat
	       "%<\\AcObjProp Object(%<\\_ObjId "
	       (LM:ObjectID FirstAltitude)
	       ">%).InsertionPoint \\f \""
	       "%lu2%pt1%pr3"
	       ">%"
	     )
      )
      (setq OffsetY
	     (strcat
	       "%<\\AcExpr "
	       (strcat NApositionY FApositionY)
	       " \\f \""
	       "%lu2%pr2%ds44"
	       "\">%"
	     )
      )
      (setq OffsetX
	     (strcat
	       "%<\\AcExpr "
	       (strcat NApositionX FApositionX)
	       " \\f \""
	       "%lu2%pr2%ds44"
	       "\">%"
	     )
      )
      (LM:vl-setattributevalue
	     NextAltitude
	     "HEIGHT"
	     OffsetY
      )
      (LM:vl-setattributevalue
		NextAltitude
	    "TEXT"
	    OffsetX
      )
      (command "_.REGEN")
 )
)
  ;; ObjectID - Lee Mac
  ;; Returns a string containing the ObjectID of a supplied VLA-Object
  ;; Compatible with 32-bit & 64-bit systems

(defun LM:ObjectID (obj)
    (eval
      (list 'defun
	    'LM:ObjectID
	    '(obj)
	    (if
	      (and
		(vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
		(vlax-method-applicable-p
		  (vla-get-utility (LM:acdoc))
		  'getobjectidstring
		)
	      )
	       (list 'vla-getobjectidstring
		     (vla-get-utility (LM:acdoc))
		     'obj
		     ':vlax-false
	       )
	       '(itoa (vla-get-objectid obj))
	    )
      )
    )
    (LM:ObjectID obj)
 )
  ;; Set Attribute Value  -  Lee Mac
  ;; Sets the value of the first attribute with the given tag found within the block, if present.
  ;; blk - [vla] VLA Block Reference Object
  ;; tag - [str] Attribute TagString
  ;; val - [str] Attribute Value
  ;; Returns: [str] Attribute value if successful, else nil.

(defun LM:vl-setattributevalue (blk tag val)
  (setq tag (strcase tag))
  (vl-some
    '(lambda (att)
       (if (= tag (strcase (vla-get-tagstring att)))
	 (progn (vla-put-textstring att val) val)
       )
     )
    (vlax-invoke blk 'getattributes)
  )
)

  ;; Insert With Visibility State - Lee Mac
  ;; Provides an interface through which the user can insert a dynamic block
  ;; with a preselected visibility state.
  ;; blk - [str] Block name, filename or full filepath
  ;; vis - [str] Visibility State to set on insertion
  ;; Returns: [vla] Inserted VLA block reference object, else nil if unsuccessful
(defun LM:insertwithstate
	 (blk vis / bse cmd def ent ext new obj pth rtn tmp)
    (setq pth (vl-string-translate "/" "\\" (vl-filename-directory blk))
	  ext (cond ((vl-filename-extension blk))
		    (".dwg")
	      )
	  bse (vl-filename-base blk)
    )
    (if	(/= "" pth)
      (setq pth (strcat pth "\\"))
    )
    (cond
      ((not
	 (or
	   (and
	     (tblsearch "block" bse)
	     (setq blk bse)
	   )
	   (setq blk (findfile (strcat pth bse ext)))
	 )
       )
       (prompt (strcat "\nBlock \"" bse "\" not found."))
      )
      ((progn
	 (setq obj
		(vlax-invoke
		  (vlax-get-property
		    (LM:acdoc)
		    (if	(= 1 (getvar 'cvport))
		      'paperspace
		      'modelspace
		    )
		  )
		  'insertblock
		  '(0.0 0.0 0.0)
		  blk
		  0.05
		  0.05
		  0.05
		  0.0
		)
	 )
	 (vla-put-visible obj :vlax-false)
	 (= :vlax-false (vla-get-isdynamicblock obj))
       )
       (vla-delete obj)
       (prompt (strcat "\nBlock \"" bse "\" is not dynamic."))
      )
      ((null (LM:setvisibilitystate obj vis))
       (vla-delete obj)
       (prompt
	 (strcat
	   "\nUnable to set visibility state of block \""
	   bse
	   "\" to \""
	   vis
	   "\"."
	  )
       )
      )
      ((setq tmp 0)
       (while
	 (tblsearch "block"
		    (setq blk
			   (strcat "tmp" (itoa (setq tmp (1+ tmp))))
		    )
	 )
       )
       (vla-put-visible
	 (car
	   (vlax-invoke
	     (LM:acdoc)
	     'copyobjects
	     (list obj)
	     (setq def
		    (vlax-invoke
		      (vla-get-blocks (LM:acdoc))
		      'add
		      '(0.0 0.0 0.0)
		      blk
		    )
	     )
	   )
	 )
	 :vlax-true
       )
       (vla-delete obj)
       (setq ent (entlast)
	     cmd (getvar 'cmdecho)
       )
       (setvar 'cmdecho 0)
       (princ "\nWybierz punkt wstawienia: \n")
       (if
	 (and
	   (vl-cmdf "_.-insert" blk "_S" 1.0 "_R" 0.0 "\\")
	   (not (eq ent (setq ent (entlast))))
	   (setq new (vlax-ename->vla-object ent))
	   (= "AcDbBlockReference" (vla-get-objectname new))
	 )
	  (progn
	    (setq rtn (car (vlax-invoke new 'explode)))
	    (vla-delete new)
	  )
       )
       (setvar 'cmdecho cmd)
       (vl-catch-all-apply 'vla-delete (list def))
      )
    )
    rtn
)
;; Get Visibility Parameter Name - Lee Mac
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil
(defun LM:getvisibilityparametername (blk / vis)
  (if
    (and
      (vlax-property-available-p blk 'effectivename)
      (setq blk
	     (vla-item
	       (vla-get-blocks (vla-get-document blk))
	       (vla-get-effectivename blk)
	     )
      )
      (= :vlax-true (vla-get-isdynamicblock blk))
      (= :vlax-true (vla-get-hasextensiondictionary blk))
      (setq vis
	     (vl-some
	       '(lambda	(pair)
		  (if
		    (and
		      (= 360 (car pair))
		      (= "BLOCKVISIBILITYPARAMETER"
			 (cdr (assoc 0 (entget (cdr pair))))
		      )
		    )
		     (cdr pair)
		  )
		)
	       (dictsearch
		 (vlax-vla-object->ename (vla-getextensiondictionary blk))
		 "acad_enhancedblock"
	       )
	     )
      )
    )
     (cdr (assoc 301 (entget vis)))
  )
)
;; Set Dynamic Block Visibility State - Lee Mac
;; Sets the Visibility Parameter of a Dynamic Block to a specific value
;; blk - [vla] VLA Dynamic Block Reference object
;; val - [str] Visibility State Parameter value
;; Returns: [str] New value of Visibility Parameter, else nil
(defun LM:SetVisibilityState (blk val / vis)
  (if
    (and
      (setq vis (LM:getvisibilityparametername blk))
      (member
	(strcase val)
	(mapcar 'strcase (LM:getdynpropallowedvalues blk vis))
      )
    )
     (LM:setdynpropvalue blk vis val)
  )
)

;; Get Dynamic Block Property Allowed Values  -  Lee Mac
;; Returns the allowed values for a specific Dynamic Block property.
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; Returns: [lst] List of allowed values for property, else nil if no restrictions

(defun LM:getdynpropallowedvalues (blk prp)
  (setq prp (strcase prp))
  (vl-some '(lambda (x)
	      (if (= prp (strcase (vla-get-propertyname x)))
		(vlax-get x 'allowedvalues)
	      )
	    )
	   (vlax-invoke blk 'getdynamicblockproperties)
  )
)
;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; val - [any] New value for property
;; Returns: [any] New value if successful, else nil

(defun LM:setdynpropvalue (blk prp val)
  (setq prp (strcase prp))
  (vl-some
    '(lambda (x)
       (if (= prp (strcase (vla-get-propertyname x)))
	 (progn
	   (vla-put-value
	     x
	     (vlax-make-variant
	       val
	       (vlax-variant-type (vla-get-value x))
	     )
	   )
	   (cond (val)
		 (t)
	   )
	 )
       )
     )
    (vlax-invoke blk 'getdynamicblockproperties)
  )
)
;; Active Document - Lee Mac
;; Returns the VLA Active Document Object
(defun LM:acdoc	nil
  (eval
    (list 'defun
	  'LM:acdoc
	  'nil
	  (vla-get-activedocument (vlax-get-acad-object))
    )
  )
  (LM:acdoc)
)
(vl-load-com)
(princ)
