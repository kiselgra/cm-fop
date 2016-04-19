(in-package :cm-fop)

;;; The composition system, see our 2015 GPCE paper.
(defmacro define-composition-system (cn)
  (check-type cn (integer 1 25) "valid as combination factor (number of targets that guide selection of implementations). Should be in [1..25] (the upper limit is arbitrary, no harm pushing it).")
  (flet ((cn (i) (cg-user:cintern (format nil "c~a" i))))
    `(progn
       (defclass default () ())
       (defvar *generics* nil)
       (defvar *composition-system-combination-factor* ,cn)

       (deflmacro define-target (name &optional (base-target 'default))
	 `(defclass ,name (,base-target) ()))

       (deflmacro define-feature (name &optional args)
	 (let* ((gen (symbol-append 'feature- name))
		(arg-names (if args (extract-parameter-names-from-lambda-list args)  nil)))
	   `(progn
	      (push (list (defgeneric ,gen ,',(loop for c from 1 to cn collect (cn c)))
			  (cons ',name (list ',args)))
		    *generics*)
	      (defmethod ,gen ,',(loop for c from 1 to cn collect `(,(cn c) t))
		'(let (,@(loop for f in arg-names collect `(,f ,f)))
		  (declare (ignore ,@arg-names)))))))

       (deflmacro implement (feature (,(cn 1) &optional ,@(loop for c from 2 to cn collect `(,(cn c) 'default))) &body body)
	 `(defmethod ,(symbol-append 'feature- feature) ,(list ,@(loop for c from 1 to cn collect `(list ',(cn c) ,(cn c))))
	    `(let ((parent ,(call-next-method)))
	       (declare (ignorable parent))
	       ,',@body)))

       (deflmacro make-config (&rest rest)
	 (let ((config (gensym)))
	   `(progn
	      (defclass ,config ,rest ())
	      (make-instance ',config))))

       (defmacro with-config (config &body body)
	 (lisp (let ((c (eval config)))
		 `(macrolet ,(loop for (feature head) in *generics*
				collect `(,@head ,(funcall feature ,@(loop for c from 1 to cn collect 'c))))
		    ,@body)))))))
