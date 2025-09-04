(defclass tape ()
  ((arr
    :accessor arr
    :initform (make-array 30000
                           :element-type '(unsigned-byte 8)
                           :initial-element 0))
   (ptr
    :accessor ptr
    :initform 0
    :type fixnum)))

(defun forward (tape)
  (incf (ptr tape)))
(defun backward (tape)
  (decf (ptr tape)))
(defmacro tape-value (tape)
  `(aref (arr ,tape) (ptr ,tape)))
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

(defclass bf (tape)
  ((program :accessor program
            :initarg :program
            :type string)
   (i :accessor i
      :initform 0
      :type fixnum)))
            
(defun interpret (program &optional (tape (make-instance 'tape)) (start 0))
  (loop :for i :from start
        :for ch = (char program i)
        :while ch
        :do
           (case ch
             (#\> (forward tape))
             (#\< (backward tape))
             (#\+ (increment tape))
             (#\- (decrement tape))
             (#\. (output tape *standard-output*))
             (#\, (input tape (make-string-input-stream "")))
             (#\] (when (not (= 0 (tape-value tape)))
                    (setf i start)
                    ))
             (#\[ (if (= 0 (tape-value tape))
                      (loop :with blocks = 1
                            :for j :from (1+ i)
                            :for c = (char program j)
                            :while (> blocks 0)
                            :do
                               (case c
                                 (#\[ (incf blocks))
                                 (#\] (decf blocks)))
                               (when (= blocks 0)
                                 (setf i (1+ j))))
                      (interpret program tape (1+ i)))))))


(defparameter *hello*
  "
++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
(interpret *hello*)
