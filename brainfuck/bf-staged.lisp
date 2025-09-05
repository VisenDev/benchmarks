(declaim (optimize (speed 1) (safety 0) (debug 0) (space 0)))

(defun count-duplicates (stream char)
  (let ((i 1))
    (declare (type fixnum i))
    (loop
      :while (char= char (peek-char nil stream))
      :do (incf i)
          (read-char stream)
      )
    i))

(defmacro wrapping-incf (form &optional (delta 1))
  `(setf ,form (mod (+ ,form ,delta) 256)))
(defmacro wrapping-decf (form &optional (delta 1))
  `(setf ,form (mod (- ,form ,delta) 256)))

(defstruct printer
  (sum1 0 :type fixnum)
  (sum2 0 :type fixnum)
  quiet)
(defun printer-print (p byte)
  (if (printer-quiet p)
      (progn
        (wrapping-incf (printer-sum1 p) byte)
        (wrapping-incf (printer-sum2 p) (printer-sum1 p)))
      (progn
        (format t "~c" (code-char byte))
        (force-output))
      ))
      

(defun parse (program arr ptr printer)
  "parses a bf program into another program"
  (loop
    :while (listen program)
    :for ch = (read-char program)
    :until (char= ch #\])
    :collect
    (case ch
      (#\+ `(wrapping-incf (aref ,arr ,ptr) ,(count-duplicates program #\+)))
      (#\- `(wrapping-decf (aref ,arr ,ptr) ,(count-duplicates program #\-)))
      (#\< `(decf ,ptr ,(count-duplicates program #\<)))
      (#\> `(incf ,ptr ,(count-duplicates program #\>)))
      (#\. `(printer-print ,printer (aref ,arr ,ptr)))
      (#\[ (let* ((start (gensym)) (end (gensym)))
             `(tagbody (when (= 0 (aref ,arr ,ptr)) (go ,end))
               ,start
               ,@(parse program arr ptr printer)
               (unless (= 0 (aref ,arr ,ptr)) (go ,start))
               ,end))))
      :into result
    :finally (return (remove-if #'null result))))

(defun codegen (program quiet)
  (eval `(defun run ()
           (let*
               ((arr (make-array 30000 :element-type '(unsigned-byte 8) :initial-element 0))
                (ptr 0)
                (printer (make-printer :quiet ,quiet))
                )
             (declare (optimize (speed 0) (safety 0)))
             (tagbody ,@(parse program 'arr 'ptr 'printer))))))

(defparameter *hello* "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

