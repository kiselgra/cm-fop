(in-package :cm-fop)

;;; The composition system, see our 2015 GPCE paper.
(defmacro define-composition-system (cn)
  (check-type cn (integer 1 25) "valid as combination factor (number of targets that guide selection of implementations). Should be in [1..25] (the upper limit is arbitrary, no harm pushing it).")
  (flet ((cn (i) (intern (format nil "C~a" i))))
    `(progn
       (defclass default () ())
       (defparameter *generics* nil)
       (defparameter *composition-system-combination-factor* ,cn)

       (defmacro define-target (name &optional (base-target 'default))
	 `(defclass ,name (,base-target) ()))

       (defmacro define-feature (name &optional args)
	 (let ((gen (symbol-append 'feature- name))
	       (arg-names (if args (extract-parameter-names-from-lambda-list args)  nil)))
	   `(progn
	      (push (list (defgeneric ,gen ,',(loop for c from 1 to cn collect (cn c)))
			  '(,name ,args (declare (ignorable ,@arg-names))))
		    *generics*)
	      (defmethod ,gen ,',(loop for c from 1 to cn collect `(,(cn c) t))))))

       (defmacro implement (feature (,(cn 1) &optional ,@(loop for c from 2 to cn collect `(,(cn c) 'default))) &body body)
	 `(defmethod ,(symbol-append 'feature- feature) ,(list ,@(loop for c from 1 to cn collect `(list ',(cn c) ,(cn c))))
	    `(let ((parent ,(call-next-method)))
	       (declare (ignorable parent))
	       ,',@body)))

       (defmacro make-config (&rest rest)
	 (let ((config (gensym)))
	   `(progn
	      (defclass ,config ,rest ())
	      (make-instance ',config))))

       (defmacro with-config (config &body body)
	 (let ((c (eval config)))
	   `(macrolet ,(loop for (feature head) in *generics*
			     collect `(,@head ,(funcall feature ,@(loop for c from 1 to cn collect 'c))))
	      ,@body))))))
