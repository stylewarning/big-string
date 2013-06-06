;;;; big-string.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:big-string)

(deftype array-size ()
  "Possible size of an array."
  `(integer 0 ,array-total-size-limit))

(deftype big-string-size ()
  "Possible size of a big string."
  `(integer 0 ,(expt array-total-size-limit 2)))

(defstruct (big-string (:constructor make-big-string ())
                       (:print-function
                        (lambda (obj stream depth)
                          (declare (ignore depth)
                                   (type big-string obj))
                          (print-unreadable-object (obj
                                                    stream 
                                                    :type t
                                                    :identity t)
                            (format stream
                                    "of ~D character~:P"
                                    (big-string-tracked-length obj))))))
  ;; Holds the total length of the string.
  (tracked-length 0 :type big-string-size)

  ;; Holds the cumulative lengths of the strings for fast
  ;; searching. This sequence will be strictly increasing.
  (cumulative-lengths (make-array 10 :element-type 'big-string-size
                                     :initial-element 0
                                     :adjustable t
                                     :fill-pointer 0)
   :type (and (vector big-string-size)
              (not simple-array)))

  ;; Holds the actual string data.
  (strings (make-array 10 :element-type 'simple-string
                          :initial-element ""
                          :adjustable t
                          :fill-pointer 0)
   :type (and (vector simple-string)
              (not simple-array))))


(declaim (ftype (function (big-string) big-string-size)
                big-string-length))
(defun big-string-length (bs)
  "Return the total length of the big string BS.

Time complexity: O(1)"
  (big-string-tracked-length bs))

(declaim (ftype (function (big-string simple-string) big-string)
                big-string-append))
(defun big-string-append (bs string)
  "Destructively append the string STRING to the big string BS.

Time complexity: O(1)   [amortized]"
  (let ((len (length string)))
    (unless (zerop len)
      (incf (big-string-tracked-length bs) len)
      (vector-push-extend (big-string-tracked-length bs)
                          (big-string-cumulative-lengths bs))
      (vector-push-extend string (big-string-strings bs)))
    bs))

(declaim (ftype (function (unsigned-byte (vector big-string-size)) 
                          (or null big-string-size))
                binary-search)
         (inline binary-search))
(defun binary-search (n array)
  "Find the least value in the array ARRAY greater than N. Return NIL
if N is negative or no such number is found."
  (let ((len (length array)))
    (labels ((bisect (lower upper)
               (declare (type big-string-size lower upper))
               (if (= lower upper)
                   lower
                   (let* ((middle (floor (+ lower upper) 2))
                          (middle-value (aref array middle)))
                     (if (< n middle-value)
                         (bisect lower middle)
                         (bisect (1+ middle) upper))))))
      (if (or (zerop len)
              (minusp n)
              (>= n (aref array (1- len))))
          nil
          (bisect 0 (1- len))))))

;; TODO: Make a proper out-of-bounds error.
(declaim (ftype (function (big-string big-string-size) character)
                big-string-char))
(defun big-string-char (bs n)
  "Find the Nth char in the big string BS.

Time complexity: O(log n)"
  (let* ((cumulative-lengths (big-string-cumulative-lengths bs))
         (pos (binary-search n cumulative-lengths)))
    (if (null pos)
        (error "The index ~D is out of bounds for the big string ~A."
               n
               bs)
        (let ((str (aref (big-string-strings bs) pos)))
          (schar str
                 (- (length str)
                    (- (aref cumulative-lengths pos)
                       n)))))))

;; TODO: This can be improved to linear time.
(defun big-string-substring (bs start &optional (end (big-string-length bs)))
  "Compute the substring from the big string BS from the index START
 to before the index END.

Time complexity: with n = end - start, O(n log n)"
  (declare (type big-string-size start end)
           (type big-string bs))
  (assert (<= start end)
          (start end)
          "START must be less than or equal to END.")
  (loop :with str := (make-string (- end start))
        :for idx :from 0
        :for i :from start :below end
        :do (setf (char str idx) (big-string-char bs i))
        :finally (return str)))

(declaim (ftype (function (big-string) simple-string)
                string-of-big-string))
(defun string-of-big-string (bs)
  "Build a string from the big string BS.

Time complexity: O(size(bs))"
  (let ((result (make-string (big-string-length bs)))
        (start 0))
    (declare (type simple-string result)
             (type big-string-size start))
    (map nil (lambda (str)
               (declare (type simple-string str))
               (replace result str :start1 start)
               (incf start (the unsigned-byte (length str))))
         (big-string-strings bs))

    ;; Return the result
    result))

