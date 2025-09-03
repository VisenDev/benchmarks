(declaim (optimize (speed 3) (safety 1)))

(declaim (ftype (function (stream character)) count-duplicates))
(defun count-duplicates (stream char)
  (let ((i 1))
    (declare (type fixnum i))
    (loop
      :while (char= char (peek-char nil stream))
      :do (incf i)
          (read-char stream)
      )
    i))

(defmacro char-incf (char-form amount)
  `(setf ,char-form (code-char (+ (char-code ,char-form) ,amount)))
  )
(defmacro char-decf (char-form amount)
  `(setf ,char-form (code-char (- (char-code ,char-form) ,amount)))
  )

(defmacro byte-incf (form &optional (amount 1))
  `(setf ,form (mod (+ ,form ,amount) 256)))
(defmacro byte-decf (form &optional (amount 1))
  `(setf ,form (mod (- ,form ,amount) 256)))


(defun opcodes (stream)
  "Returns codegen"
  (loop
    :while (listen stream)
    :for ch = (peek-char nil stream)
    :while (not (member ch (list #\> #\< #\+ #\- #\[ #\. #\,)))
    :do (read-char stream))
  (loop
    :with codes = '()
    :while (listen stream)
    :for ch = (read-char stream)
    :while (not (eq #\] ch))
    :while (not (eq #\Newline ch))
    :do
       (ecase ch
         (#\> (push `(incf ptr ,(count-duplicates stream #\>)) codes))
         (#\< (push `(decf ptr ,(count-duplicates stream #\<)) codes))
         (#\+ (push `(byte-incf (aref arr ptr) ,(count-duplicates stream #\+)) codes))
         (#\- (push `(byte-decf (aref arr ptr) ,(count-duplicates stream #\-)) codes))
         (#\[ (let* ((start (gensym))
                     (end (gensym))
                     )
                (push start codes)
                (push `(when (= 0 (aref arr ptr))
                         (go ,end))
                      codes)
                (dolist (val (opcodes stream))
                  (push val codes))
                (push `(go ,start) codes)
                (push end codes)))
         
         (#\. (push `(format stdout "~a" (code-char (aref arr ptr))) codes))
         (#\, (push `(setf (aref arr ptr) (char-code (read-char stdin)))
                                          
                                              codes))
         )
    :finally (return (nreverse codes))
    ))




(defun codegen (stream)
  (eval
   `(lambda (&optional (stdin t) (stdout t))
      (declare (ignorable stdin stdout))
      (let ((arr (make-array 30000 :element-type `(unsigned-byte 8)
                                   :initial-element 0))
            (ptr 0)
            )
        (declare (type fixnum ptr))
        (tagbody 
           ,@(opcodes stream)
           )
        ))))

(defparameter *hello* "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")


(defun main ()
  (let* ((filename (first uiop:*command-line-arguments*))
         (program (codegen (uiop:read-file-string filename)))
         )
    (program)
    ))
