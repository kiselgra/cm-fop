(require :c-mera)

(defpackage :cm-fop
  (:use :common-lisp)
  (:import-from :cmu-c
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

(asdf:defsystem cm-fop
    :name "cm-fop"
    :version "1.2.0"
    :components ((:file "src/composition-system"))
    :depends-on ("cmu-c"))
