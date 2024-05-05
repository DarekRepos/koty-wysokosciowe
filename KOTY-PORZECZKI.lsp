;;Program Name: Etykiety Civil
;;Author: Dariusz Duda
;;darekduda.pl
;;Description: Wtyczka do tworzenia bloków dynamicznych kot dla przekrojów poprzecznych drogi
(defun c:klp (/ referenceElevation  currentElevation nextBlockYOffset  firstBlockYOffset nextBlockXOffset firstBlockXOffset elevationOffset textOffset)
	(vl-load-com)

	(LM:insertwithstate
		"Kota-mm"
		"Underline YES | Wipeout YES"
	)

	(setq referenceElevation (vlax-ename->vla-object (entlast)))

	;;Set default Value at First Altitude 
		
    (setq	*userInputElevation*
	  (cond
	    (
	      (getreal
	        (strcat "\nWpisz wysokosc odniesienia w metrach <"
		      (rtos
			    (setq *userInputElevation*
			       (cond (*userInputElevation*)
						 (0.00)
			       )
			    )
		      )
		      ">: "
	        )
	      )
	   )
	   (*userInputElevation*)
	 )
   )

    (LM:vl-setattributevalue
      referenceElevation
      "TEXT"
      "Odniesienie"
    )
     (LM:vl-setattributevalue
       referenceElevation
       "HEIGHT"
       (strcat (rtos *userInputElevation* 2 2) "m")
     )

    (while  

      (LM:insertwithstate
		"Kota-mm"
		"Underline NO   | Wipeout NO"
      )
      (setq currentElevation (vlax-ename->vla-object (entlast)))
      (setq nextBlockYOffset
	     (strcat
	       "%<\\AcObjProp Object(%<\\_ObjId "
	       (LM:ObjectID currentElevation)
	       ">%).InsertionPoint \\f \""
	       "%lu2%pt2%pr3"
	       ">%"
	       " - "
	       ;;minus First Altitude at position Y 
	     )
      )
      (setq firstBlockYOffset
	     (strcat
	       "%<\\AcObjProp Object(%<\\_ObjId "
	       (LM:ObjectID referenceElevation)
	       ">%).InsertionPoint \\f \""
	       "%lu2%pt2%pr3%"
	       ">%"
	       " + "
	       ;;plus constant
	       (rtos *userInputElevation*)
	       ;;value at First Altitude = constant
	     )
      )
      (setq nextBlockXOffset
	     (strcat
	       "%<\\AcObjProp Object(%<\\_ObjId "
	       (LM:ObjectID currentElevation)
	       ">%).InsertionPoint \\f \""
	       "%lu2%pt1%pr3"
	       ">%"
	       " - "
	       ;;minus First Altitude at position X 
	     )
      )
      (setq firstBlockXOffset
	     (strcat
	       "%<\\AcObjProp Object(%<\\_ObjId "
	       (LM:ObjectID referenceElevation)
	       ">%).InsertionPoint \\f \""
	       "%lu2%pt1%pr3"
	       ">%"
	     )
      )
      (setq elevationOffset
	     (strcat
	       "%<\\AcExpr "
	       (strcat nextBlockYOffset firstBlockYOffset)
	       " \\f \""
	       "%lu2%pr2%ds44"
	       "\">%"
	     )
      )
      (setq textOffset
	     (strcat
	       "%<\\AcExpr "
	       (strcat nextBlockXOffset firstBlockXOffset)
	       " \\f \""
	       "%lu2%pr2%ds44"
	       "\">%"
	     )
      )
      (LM:vl-setattributevalue
	     currentElevation
	     "HEIGHT"
	     elevationOffset
      )
      (LM:vl-setattributevalue
		currentElevation
	    "TEXT"
	    textOffset
      )
      (command "_.REGEN")
 )
)
  ;; ObjectID - Lee Mac
  ;; Returns a string containing the ObjectID of a supplied VLA-Object
  ;; Compatible with 32-bit & 64-bit systems

(defun LM:ObjectID (insertedBlockObject)
    (eval
      (list 'defun
	    'LM:ObjectID
	    '(insertedBlockObject)
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
		     'insertedBlockObject
		     ':vlax-false
	       )
	       '(itoa (vla-get-objectid insertedBlockObject))
	    )
      )
    )
    (LM:ObjectID insertedBlockObject)
 )
  ;; Set Attribute Value  -  Lee Mac
  ;; Sets the value of the first attribute with the given tag found within the block, if present.
  ;; dynamicBlockName - [vla] VLA Block Reference Object
  ;; tag - [str] Attribute TagString
  ;; propertyValue - [str] Attribute Value
  ;; Returns: [str] Attribute value if successful, else nil.

(defun LM:vl-setattributevalue (dynamicBlockName tag propertyValue)
  (setq tag (strcase tag))
  (vl-some
    '(lambda (att)
       (if (= tag (strcase (vla-get-tagstring att)))
	 (progn (vla-put-textstring att propertyValue) propertyValue)
       )
     )
    (vlax-invoke dynamicBlockName 'getattributes)
  )
)

  ;; Insert With Visibility State - Lee Mac
  ;; Provides an interface through which the user can insert a dynamic block
  ;; with a preselected visibility state.
  ;; dynamicBlockName - [str] Block name, filename or full filepath
  ;; visibilityState - [str] Visibility State to set on insertion
  ;; Returns: [vla] Inserted VLA block reference object, else nil if unsuccessful
(defun LM:insertwithstate
	 (dynamicBlockName visibilityState / blockBaseName commandEchoSetting blockDefinition lastEntity blockExtension new insertedBlockObject blockPath explodeResult temporaryBlockName)
    (setq blockPath (vl-string-truserInputElevationlate "/" "\\" (vl-filename-directory dynamicBlockName))
	  blockExtension (cond ((vl-filename-extension dynamicBlockName))
		    (".dwg")
	      )
	  blockBaseName (vl-filename-base dynamicBlockName)
    )
    (if	(/= "" blockPath)
      (setq blockPath (strcat blockPath "\\"))
    )
    (cond
      ((not
	 (or
	   (and
	     (tblsearch "block" blockBaseName)
	     (setq dynamicBlockName blockBaseName)
	   )
	   (setq dynamicBlockName (findfile (strcat blockPath blockBaseName blockExtension)))
	 )
       )
       (prompt (strcat "\nBlock \"" blockBaseName "\" not found."))
      )
      ((progn
	 (setq insertedBlockObject
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
		  dynamicBlockName
		  0.05
		  0.05
		  0.05
		  0.0
		)
	 )
	 (vla-put-visible insertedBlockObject :vlax-false)
	 (= :vlax-false (vla-get-isdynamicblock insertedBlockObject))
       )
       (vla-delete insertedBlockObject)
       (prompt (strcat "\nBlock \"" blockBaseName "\" is not dynamic."))
      )
      ((null (LM:setvisibilitystate insertedBlockObject visibilityState))
       (vla-delete insertedBlockObject)
       (prompt
	 (strcat
	   "\nUnable to set visibility state of block \""
	   blockBaseName
	   "\" to \""
	   visibilityState
	   "\"."
	  )
       )
      )
      ((setq temporaryBlockName 0)
       (while
	 (tblsearch "block"
		    (setq dynamicBlockName
			   (strcat "temporaryBlockName" (itoa (setq temporaryBlockName (1+ temporaryBlockName))))
		    )
	 )
       )
       (vla-put-visible
	 (car
	   (vlax-invoke
	     (LM:acdoc)
	     'copyobjects
	     (list insertedBlockObject)
	     (setq blockDefinition
		    (vlax-invoke
		      (vla-get-blocks (LM:acdoc))
		      'add
		      '(0.0 0.0 0.0)
		      dynamicBlockName
		    )
	     )
	   )
	 )
	 :vlax-true
       )
       (vla-delete insertedBlockObject)
       (setq lastEntity (entlast)
	     commandEchoSetting (getvar 'cmdecho)
       )
       (setvar 'cmdecho 0)
       (princ "\nWybierz punkt wstawienia: [or ESC]\n")
       (if
	 (and
	   (vl-cmdf "_.-insert" dynamicBlockName "_S" 1.0 "_R" 0.0 "\\")
	   (not (eq lastEntity (setq lastEntity (entlast))))
	   (setq new (vlax-ename->vla-object lastEntity))
	   (= "AcDbBlockReference" (vla-get-objectname new))
	 )
	  (progn
	    (setq explodeResult (car (vlax-invoke new 'explode)))
	    (vla-delete new)
	  )
       )
       (setvar 'cmdecho commandEchoSetting)
       (vl-catch-all-apply 'vla-delete (list blockDefinition))
      )
    )
    explodeResult
)
;; Get Visibility Parameter Name - Lee Mac
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; dynamicBlockName - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil
(defun LM:getvisibilityparametername (dynamicBlockName / visibilityState)
  (if
    (and
      (vlax-property-available-p dynamicBlockName 'effectivename)
      (setq dynamicBlockName
	     (vla-item
	       (vla-get-blocks (vla-get-document dynamicBlockName))
	       (vla-get-effectivename dynamicBlockName)
	     )
      )
      (= :vlax-true (vla-get-isdynamicblock dynamicBlockName))
      (= :vlax-true (vla-get-hasextensiondictionary dynamicBlockName))
      (setq visibilityState
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
		 (vlax-vla-object->ename (vla-getextensiondictionary dynamicBlockName))
		 "acad_enhancedblock"
	       )
	     )
      )
    )
     (cdr (assoc 301 (entget visibilityState)))
  )
)
;; Set Dynamic Block Visibility State - Lee Mac
;; Sets the Visibility Parameter of a Dynamic Block to a specific value
;; dynamicBlockName - [vla] VLA Dynamic Block Reference object
;; propertyValue - [str] Visibility State Parameter value
;; Returns: [str] New value of Visibility Parameter, else nil
(defun LM:SetVisibilityState (dynamicBlockName propertyValue / visibilityState)
  (if
    (and
      (setq visibilityState (LM:getvisibilityparametername dynamicBlockName))
      (member
	(strcase propertyValue)
	(mapcar 'strcase (LM:getdynpropallowedvalues dynamicBlockName visibilityState))
      )
    )
     (LM:setdynpropvalue dynamicBlockName visibilityState propertyValue)
  )
)

;; Get Dynamic Block Property Allowed Values  -  Lee Mac
;; Returns the allowed values for a specific Dynamic Block property.
;; dynamicBlockName - [vla] VLA Dynamic Block Reference object
;; propertyName - [str] Dynamic Block property name (case-insensitive)
;; Returns: [lst] List of allowed values for property, else nil if no restrictions

(defun LM:getdynpropallowedvalues (dynamicBlockName propertyName)
  (setq propertyName (strcase propertyName))
  (vl-some '(lambda (x)
	      (if (= propertyName (strcase (vla-get-propertyname x)))
		(vlax-get x 'allowedvalues)
	      )
	    )
	   (vlax-invoke dynamicBlockName 'getdynamicblockproperties)
  )
)
;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; dynamicBlockName - [vla] VLA Dynamic Block Reference object
;; propertyName - [str] Dynamic Block property name (case-insensitive)
;; propertyValue - [any] New value for property
;; Returns: [any] New value if successful, else nil

(defun LM:setdynpropvalue (dynamicBlockName propertyName propertyValue)
  (setq propertyName (strcase propertyName))
  (vl-some
    '(lambda (x)
       (if (= propertyName (strcase (vla-get-propertyname x)))
	 (progn
	   (vla-put-value
	     x
	     (vlax-make-variant
	       propertyValue
	       (vlax-variant-type (vla-get-value x))
	     )
	   )
	   (cond (propertyValue)
		 (t)
	   )
	 )
       )
     )
    (vlax-invoke dynamicBlockName 'getdynamicblockproperties)
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
