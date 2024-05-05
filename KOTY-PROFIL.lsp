;;Main program
;;Dariusz Duda darekduda.pl
(defun c:jd (/ referenceBlock NextAltitude nextBlockInsertionY attributeExpression)
  (vl-load-com)
 
  (LM:insertwithstate
    "Kota-mm"
    "Underline YES | Wipeout YES"
  )

  (setq referenceBlock (vlax-ename->vla-object (entlast)))

  (setq	*referenceAltitude*
	 (cond
	   (
	    (getreal
	      (strcat "\nWpisz wysokosc odniesienia w metrach <"
		      (rtos
			(setq *referenceAltitude*
			       (cond (*referenceAltitude*)
				     (0.00)
			       )
			)
		      )
		      ">: "
	      )
	    )
	   )
	   (*referenceAltitude*)
	 )
  )

  (LM:vl-setattributevalue
    referenceBlock
    "TEXT"
    "Odniesienie"
  )
  (LM:vl-setattributevalue
    referenceBlock
    "HEIGHT"
    (strcat (rtos *referenceAltitude*) "m")
  )

  (while

    (LM:insertwithstate
      "Kota-mm"
      "Underline NO   | Wipeout NO"
    )

     (setq NextAltitude (vlax-ename->vla-object (entlast)))

     (setq nextBlockInsertionY (strcat
		" ( "
		 "%<\\AcObjProp Object(%<\\_ObjId "
		 (LM:ObjectID NextAltitude)
		 ">%).InsertionPoint \\f \""
		 "%lu2%pt2%pr3"
		 ">%"
		 " - "
	       )
     )
     (setq firstBlockInsertionY	(strcat
		  "%<\\AcObjProp Object(%<\\_ObjId "
		  (LM:ObjectID referenceBlock)
		  ">%).InsertionPoint \\f \""
		  "%lu2%pt2%pr3%"
		  ">%"
		")*0.1"		  
		"+"		
		  (rtos *referenceAltitude*)
		)
     )

     (setq attributeExpression
	    (strcat
	      "%<\\AcExpr "
	      (strcat ppnextBlockInsertionYp firstBlockInsertionY)
	      " \\f \""
	      "%lu2%pr2%ds44"
	      "\">%"
	    )
     )

     (LM:vl-setattributevalue
       NextAltitude
       "HEIGHT"
       attributeExpression
     )
     (command "_.REGEN")
  )
)
;; ObjectID - Lee Mac
;; Returns a string containing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems

(defun LM:ObjectID (insertedObject)
  (eval
    (list 'defun
	  'LM:ObjectID
	  '(insertedObject)
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
		   'insertedObject
		   ':vlax-false
	     )
	     '(itoa (vla-get-objectid insertedObject))
	  )
    )
  )
  (LM:ObjectID insertedObject)
)
;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; dynamicBlockName - [vla] VLA Block Reference Object
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil.

(defun LM:vl-setattributevalue (dynamicBlockName tag val)
  (setq tag (strcase tag))
  (vl-some
    '(lambda (att)
       (if (= tag (strcase (vla-get-tagstring att)))
	 (progn (vla-put-textstring att val) val)
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
       (dynamicBlockName visibilityState / blockBaseName commandEchoSetting attributeDefinition selectedEntity blockExtension new insertedObject pathToBlock returnObject temporaryIndex)
  (setq	pathToBlock (vl-string-translate "/" "\\" (vl-filename-directory dynamicBlockName))
	blockExtension (cond ((vl-filename-extension dynamicBlockName))
		  (".dwg")
	    )
	blockBaseName (vl-filename-base dynamicBlockName)
  )
  (if (/= "" pathToBlock)
    (setq pathToBlock (strcat pathToBlock "\\"))
  )
  (cond
    ((not
       (or
	 (and
	   (tblsearch "block" blockBaseName)
	   (setq dynamicBlockName blockBaseName)
	 )
	 (setq dynamicBlockName (findfile (strcat pathToBlock blockBaseName blockExtension)))
       )
     )
     (prompt (strcat "\nBlock \"" blockBaseName "\" not found."))
    )
    ((progn
       (setq insertedObject
	      (vlax-invoke
		(vlax-get-property
		  (LM:acdoc)
		  (if (= 1 (getvar 'cvport))
		    'paperspace
		    'modelspace
		  )
		)
		'insertblock
		'(0.0 0.0 0.0)
		dynamicBlockName
		0.30
		0.30
		0.30
		0.0
	      )
       )
       (vla-put-visible insertedObject :vlax-false)
       (= :vlax-false (vla-get-isdynamicblock insertedObject))
     )
     (vla-delete insertedObject)
     (prompt (strcat "\nBlock \"" blockBaseName "\" is not dynamic."))
    )
    ((null (LM:setvisibilitystate insertedObject visibilityState))
     (vla-delete insertedObject)
     (prompt
       (strcat
	 "\nUnable to set visibility state of block \""
	 blockBaseName				"\" to \""
	 visibilityState				"\"."
	)
     )
    )
    ((setq temporaryIndex 0)
     (while
       (tblsearch "block"
		  (setq	dynamicBlockName
			 (strcat "temporaryIndex" (itoa (setq temporaryIndex (1+ temporaryIndex))))
		  )
       )
     )
     (vla-put-visible
       (car
	 (vlax-invoke
	   (LM:acdoc)
	   'copyobjects
	   (list insertedObject)
	   (setq attributeDefinition
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
     (vla-delete insertedObject)
     (setq selectedEntity (entlast)
	   commandEchoSetting (getvar 'cmdecho)
     )
     (setvar 'cmdecho 0)
     (princ "\nWybierz punkt wstawienia: [or ESC]\n")
     (if
       (and
	 (vl-cmdf "_.-insert" dynamicBlockName "_S" 1.0 "_R" 0.0 "\\")
	 (not (eq selectedEntity (setq selectedEntity (entlast))))
	 (setq new (vlax-ename->vla-object selectedEntity))
	 (= "AcDbBlockReference" (vla-get-objectname new))
       )
	(progn
	  (setq returnObject (car (vlax-invoke new 'explode)))
	  (vla-delete new)
	)
     )
     (setvar 'cmdecho commandEchoSetting)
     (vl-catch-all-apply 'vla-delete (list attributeDefinition))
    )
  )
  returnObject
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
;; val - [str] Visibility State Parameter value
;; Returns: [str] New value of Visibility Parameter, else nil
(defun LM:SetVisibilityState (dynamicBlockName val / visibilityState)
  (if
    (and
      (setq visibilityState (LM:getvisibilityparametername dynamicBlockName))
      (member
	(strcase val)
	(mapcar 'strcase (LM:getdynpropallowedvalues dynamicBlockName visibilityState))
      )
    )
     (LM:setdynpropvalue dynamicBlockName visibilityState val)
  )
)

;; Get Dynamic Block Property Allowed Values  -  Lee Mac
;; Returns the allowed values for a specific Dynamic Block property.
;; dynamicBlockName - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; Returns: [lst] List of allowed values for property, else nil if no restrictions

(defun LM:getdynpropallowedvalues (dynamicBlockName prp)
  (setq prp (strcase prp))
  (vl-some '(lambda (x)
	      (if (= prp (strcase (vla-get-propertyname x)))
		(vlax-get x 'allowedvalues)
	      )
	    )
	   (vlax-invoke dynamicBlockName 'getdynamicblockproperties)
  )
)
;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; dynamicBlockName - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; val - [any] New value for property
;; Returns: [any] New value if successful, else nil

(defun LM:setdynpropvalue (dynamicBlockName prp val)
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

