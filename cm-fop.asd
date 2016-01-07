(defpackage :cm-fop
  (:use :common-lisp)
  (:import-from :cg-user
		:deflmacro
		:lisp
		:extract-parameter-names-from-lambda-list
		:symbol-append)
  (:export :define-feature
  	   :symbol-append
	   :define-target
	   :implement
	   :make-config
	   :with-config
	   :parent
	   :define-composition-system))

(defsystem cm-fop
    :name "cm-fop"
    :version "1.1.0"
    :serial t
    :components ((:file "src/composition-system"))
    :depends-on ("cgen"))
