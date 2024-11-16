(defun find-min (lst current-min key test)
  (if (null lst)
    current-min
    (let ((lst-key (funcall key (car lst)))
        (min-key (funcall key current-min)))
      (find-min 
        (cdr lst)
        (if (funcall test lst-key min-key)
          (car lst)
          current-min)
      key
      test))))

(defun remove-element (lst element)
  (cond 
    ((null lst) nil)
    ((= (car lst) element) (cdr lst))
    (t (cons (car lst) (remove-element (cdr lst) element)))))

(defun selection-sort-functional (lst &key (key #'identity) (test #'<))
  (if (or (null lst) (null (cdr lst)))
    lst
    (let ((min (find-min lst (car lst) key test)))
      (cons min 
        (selection-sort-functional 
          (remove-element lst min)
          :key key
          :test test)))))

(defun check-selection-sort-functional (name input expected &key (key #'identity) (test #'<))
  "Перевіряє функцію selection-sort-functional з вхідними даними INPUT і очікуваним результатом EXPECTED."
  (format t "~:[FAILED~;PASSED~]... ~a~%" 
          (equal (selection-sort-functional input :key key :test test) expected) name))

(defun test-selection-sort-functional ()
  (check-selection-sort-functional "test 1" '(5 3 8 1 4) '(1 3 4 5 8))
  (check-selection-sort-functional "test 2" '(9 7 5 3 1) '(1 3 5 7 9))
  (check-selection-sort-functional "test 3" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-selection-sort-functional "test 4" '(10) '(10))
  (check-selection-sort-functional "test 5" '(nil) '(nil))
  (check-selection-sort-functional "test 6" '(-3 1 -2 5 4) '(1 -2 -3 4 5) :key #'abs)
)

(test-selection-sort-functional)

(defun propagator-fn (&key (comparator #'>))
  (let ((best nil))
    (lambda (current)
      (if (or (null best) (funcall comparator current best))
        (setf best current)
        best))))

(defun check-propagator-fn (name input comparator expected)
  "Перевіряє функцію propagator-fn з вхідними даними input, comparator і очікуваним результатом EXPECTED."
  (let* ((comparator-fn (or comparator #'>))
         (result (mapcar (propagator-fn :comparator comparator-fn) input)))
    (format t "~:[FAILED~;PASSED~]... ~a~%" 
            (equal result expected) name)))

(defun test-propagator-fn ()
  (check-propagator-fn "test 1" '(1 2 3) #'> '(1 2 3))
  (check-propagator-fn "test 2" '(3 1 4 2) #'> '(3 3 4 4))
  (check-propagator-fn "test 3" '(1 2 3) #'< '(1 1 1))
  (check-propagator-fn "test 4" '() #'> '())
  (check-propagator-fn "test 5" '(10) #'> '(10))
  (check-propagator-fn "test 6" '(5 5 5 5) #'> '(5 5 5 5))
  (check-propagator-fn "test 7" '(10 8 9 7) #'< '(10 8 8 7))
)

(test-propagator-fn)