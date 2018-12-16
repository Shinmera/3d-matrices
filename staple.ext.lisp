(asdf:load-system :staple-markdown)

(defmethod staple:definition-wanted-p ((_ definitions:setf-expander) page) NIL)
#+sbcl
(defmethod staple:definition-wanted-p ((_ definitions:source-transform) page) NIL)
