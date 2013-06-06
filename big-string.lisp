;;;; big-string.lisp
;;;; Copyright (c) 2013 Robert Smith

(in-package #:big-string)

(deftype array-size ()
  "Possible size of an array."
  `(integer 0 ,array-total-size-limit))

(defstruct (big-string (:constructor make-big-string ())
                       (:print-function
                        (lambda (obj stream depth)
                          (declare (ignore depth))
                          (print-unreadable-object (obj
                                                    stream 
                                                    :type t
                                                    :identity t)
                            (format stream
                                    "of ~D character~:P"
                                    (big-string-tracked-length obj))))))
  ;; Holds the total length of the string.
  (tracked-length 0 :type unsigned-byte)

  ;; Holds the cumulative lengths of the strings for fast
  ;; searching. This sequence will be strictly increasing.
  (cumulative-lengths (make-array 10 :element-type 'unsigned-byte
                                     :initial-element 0
                                     :adjustable t
                                     :fill-pointer 0)
   :type (and (vector unsigned-byte)
              (not simple-array)))

  ;; Holds the actual string data.
  (strings (make-array 10 :element-type 'simple-string
                          :initial-element ""
                          :adjustable t
                          :fill-pointer 0)
   :type (and (vector simple-string)
              (not simple-array))))

(defun big-string-length (bs)
  "Return the total length of the big string BS.

Time complexity: O(1)"
  (big-string-tracked-length bs))

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

(defun binary-search (n array)
  "Find the least value in the array ARRAY greater than N. Return NIL
if N is negative or no such number is found."
  (let ((len (length array)))
    (labels ((bisect (lower upper)
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
(defun big-string-char (bs n)
  "Find the Nth char in the big string BS.

Time complexity: O(log n)"
  (let* ((cumulative-lengths (big-string-cumulative-lengths bs))
         (pos (binary-search n cumulative-lengths))
         (str (aref (big-string-strings bs) pos)))
    (if (null pos)
        (error "The index ~D is out of bounds for the big string ~A."
               n
               bs)
        (schar str
               (- (length str)
                  (- (aref cumulative-lengths pos)
                     n))))))

;; TODO: This can be improved to linear time.
(defun big-string-substring (bs start &optional (end (big-string-length bs)))
  "Compute the substring from the big string BS from the index START
 to before the index END.

Time complexity: with n = end - start, O(n log n)"
  (assert (<= start end)
          (start end)
          "START must be less than or equal to END.")
  (loop :with str := (make-string (- end start))
        :for idx :from 0
        :for i :from start :below end
        :do (setf (char str idx) (big-string-char bs i))
        :finally (return str)))

(defun string-of-big-string (bs)
  "Build a string from the big string BS.

Time complexity: O(size(bs))"
  (loop :with result := (make-string (big-string-length bs))
        :and  start  := 0
        :for str :across (big-string-strings bs)
        :do (progn
              (replace result str :start1 start)
              (incf start (length str)))
        :finally (return result)))

