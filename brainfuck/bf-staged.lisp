(declaim (optimize (speed 3) (safety 3) (debug 3)))
(ql:quickload :usocket)

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
(declaim (ftype (function (printer (unsigned-byte 8)) t) printer-print))
(defun printer-print (p byte)
  (if (printer-quiet p)
      (progn
        (wrapping-incf (printer-sum1 p) byte)
        (wrapping-incf (printer-sum2 p) (printer-sum1 p)))
      (progn
        (format t "~c" (code-char byte))
        (force-output))
      ))
(declaim (ftype (function (printer) fixnum) printer-get-checksum))
(defun printer-get-checksum (p)
  (logior (the fixnum (printer-sum1 p))
          (the fixnum (ash (printer-sum2 p) 8))))

      

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

(defun codegen (program)
  (eval `(lambda (printer)
           (declare (optimize (speed 0) (safety 0) (debug 0)))
           (let*
               ((arr (make-array 30000 :element-type '(unsigned-byte 8) :initial-element 0))
                (ptr 0)
                )
             (declare (fixnum ptr))
             (declare ((array (unsigned-byte 8) (30000)) arr))
             (tagbody ,@(parse program 'arr 'ptr 'printer))))))

(defparameter *hello* "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

(defun notify (stream msg)
    (write-string msg stream)
    (force-output stream)
    )

(defvar *port* 9001)
(defun verify ()
  (let* ((text "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
         (stream (make-string-input-stream text))
         (left-printer (make-printer :quiet t))
         (right-printer (make-printer :quiet t))
         (fn (codegen stream))
         )
    (funcall fn left-printer)
    (loop :for ch across (format nil "Hello World!~%")
          :do (printer-print right-printer (char-code ch)))
    (unless (= (printer-get-checksum left-printer)
               (printer-get-checksum right-printer))
      (error "Checksum failed")
      )))

(defun main ()
  (verify)
  (let* ((p (make-printer :quiet (uiop:getenv "QUIET")))
         (name (second sb-ext:*posix-argv*))
         (pid (nix:getpid))
         (pid-str (format nil "Common-Lisp    ~a" pid))
         (fp (open name))
         (usock (usocket:socket-connect "localhost" *port*))
         (stream (usocket:socket-stream usock))
         )

    (notify stream pid-str)
    (funcall (codegen fp) p)
    (notify stream "stop")
    (when (printer-quiet p)
      (format t "Output checksum: ~a~%" (printer-get-checksum p))
      )
    (usocket:socket-close usock)
    ()))
