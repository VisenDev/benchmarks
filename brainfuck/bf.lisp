(declaim (optimize (speed 3) (safety 0)))

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
        

(defun opcodes (stream)
  "Returns codegen"
  (loop
    :while (listen stream)
    :for ch = (peek-char nil stream)
    :while (not (member ch (list #\> #\< #\+ #\- #\[ #\. #\,)))
    :do (read-char stream))
  (loop
    :while (listen stream)
    :for ch = (read-char stream)
    :while (not (eq #\] ch))
    :while (not (eq #\Newline ch))
    :collect
    (ecase ch
      (#\> `(incf ptr ,(count-duplicates stream #\>)))
      (#\< `(decf ptr ,(count-duplicates stream #\<)))
      (#\+ `(incf (aref arr ptr) ,(count-duplicates stream #\+)))
      (#\- `(decf (aref arr ptr) ,(count-duplicates stream #\-)))
      (#\[ (let* ((start (gensym))
                  (end (gensym))
                  )
             `(tagbody
                 ,start 
                 (when (= 0 (aref arr ptr))
                   (go ,end))
                 ,@(opcodes stream)
                 (go ,start)
                 ,end)))
                 
      (#\. `(format t "~c" (code-char (aref arr ptr))))
      (#\, `(setf (aref arr ptr) (read-byte *standard-input*)))
      )
    ))




(defun codegen (stream)
   (eval `(defun program ()
      (let ((arr (make-array 30000 :element-type `(integer 0 255)
                                   :initial-element 0))
            (ptr 0)
                  )
        (declare (type fixnum ptr))
        ,@(opcodes stream)
      
              ))))


(defun main ()
  (let* ((filename (first uiop:*command-line-arguments*))
         (program (codegen (uiop:read-file-string filename)))
         )
    (program)
    ))
