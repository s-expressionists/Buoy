(cl:in-package #:buoy-remez)

(defun solve-linear-system (matrix)
  (destructuring-bind (rows columns)
      (array-dimensions matrix)
    (labels ((swap-rows (a b)
               (loop for i from 0 below columns
                     do (rotatef (aref matrix a i)
                                 (aref matrix b i))))
             (multiply-row (a multiplier)
               (loop for i from 0 below columns
                     do (setf (aref matrix a i)
                              (* (aref matrix a i) multiplier))))
             (add-row-to-row (a b)
               (loop for i from 0 below columns
                     do (incf (aref matrix b i)
                              (aref matrix a i))))
             (find-row (start-row column)
               (loop for row from start-row below rows
                     when (not (zerop (aref matrix row column)))
                       return row
                     finally (error "no row can be found")))
             (maybe-swap-rows (k)
               (let ((candidate (find-row k k)))
                 (unless (= candidate k)
                   (swap-rows k candidate))))
             (eliminate-one-row (a b)
               (let ((first (aref matrix b a)))
                 (unless (zerop first)
                   (let ((multiplier (- (/ (aref matrix a a)
                                           (aref matrix b a)))))
                     (multiply-row b multiplier)
                     (add-row-to-row a b)))))
             (eliminate-all-rows (a)
               (loop for row from (1+ a) below rows
                     do (eliminate-one-row a row)))
             (canonicalize-row (row)
               (let ((coefficient (aref matrix row row)))
                 (multiply-row row (/ coefficient)))))
      (loop for row from 0 below (1- rows)
            do (maybe-swap-rows (1+ row))
               (eliminate-all-rows row))
      (loop for row from (1- rows) downto 1
            do (canonicalize-row row)
               (loop for row2 from (1- row) downto 0
                     do (eliminate-one-row row row2)))
      (canonicalize-row 0))))
