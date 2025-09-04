(declaim (optimize (speed 3) (safety 0)))

(defstruct tape
  (arr (make-array 30000 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (30000)))
  (ptr 0 :type fixnum))

(declaim (ftype (function (tape) t)
                forward
                backward
                increment
                decrement)
         (ftype (function (tape stream) t)
                input
                output))
                
                
(defun forward (tape)
  (incf (tape-ptr tape)))
(defun backward (tape)
  (decf (tape-ptr tape)))
(defmacro tape-value (tape)
  `(the (unsigned-byte 8) (aref (tape-arr ,tape) (tape-ptr ,tape))))
(defmacro wrapping-incf (form)
  `(setf ,form (mod (+ ,form 1) 256)))
(defmacro wrapping-decf (form)
  `(setf ,form (mod (- ,form 1) 256)))
(defun increment (tape)
  (wrapping-incf (tape-value tape)))
(defun decrement (tape)
  (wrapping-decf (tape-value tape)))
(defun input (tape stream)
  (setf (tape-value tape)
        (if (listen stream)
            (char-code (read-char stream))
            0)))
(defun output (tape stream)
  (format stream "~c" (code-char (tape-value tape))))

(deftype op () '(member :incr :decr :forw :back :input :output :loop))
;;(defclass ast-node ()
;;  ((op :accessor op :initarg :op :type op)
;;   (args :accessor args :initarg :args)))
(defstruct node (op nil :type op) args)

(defun parse (program)
  "parses a bf program into an ast"
  (loop
    :while (listen program)
    :for ch = (read-char program)
    :until (char= ch #\])
    :collect
    (case ch
      (#\+ (make-node :op :incr))
      (#\- (make-node :op :decr))
      (#\< (make-node :op :back))
      (#\> (make-node :op :forw))
      (#\. (make-node :op :output))
      (#\, (make-node :op :input))
      (#\[ (make-node :op :loop :args (parse program))))
      :into result
    :finally (return (remove-if #'null result))))

;;(defclass bf (tape)
;;  ((program :accessor program
;;            :initarg :program
;;            :type string)
;;   (i :accessor i
;;      :initform 0
;;      :type fixnum)))

(declaim (ftype (function (tape list) t) interpret-ast))

(declaim (inline interpret-node))
(defun interpret-node (tape node)
  (ecase (node-op node)
    (:incr (increment tape))
    (:decr (decrement tape))
    (:forw (forward tape))
    (:back (backward tape))
    (:output (output tape *standard-output*))
    (:input (input tape *standard-input*))
    (:loop (loop
             :until (= 0 (tape-value tape))
             :do (interpret-ast tape (node-args node)))))
    )

(defun interpret-ast (tape ast)
  (loop
    :for node :in ast
    :do (interpret-node tape node)))
(defun interpret (program-stream)
  (interpret-ast (make-tape) (parse program-stream)))

(defparameter *hello* "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
(interpret (make-string-input-stream *hello*))
