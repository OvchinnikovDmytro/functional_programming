(defun find-min (lst current-min)
  (if (null lst) 
    current-min
    (find-min (cdr lst) 
      (if (< (car lst) current-min) (car lst) current-min))))

(defun remove-element (lst element)
  (cond ((null lst) nil)
    ((equal (car lst) element) (cdr lst))
    (t (cons (car lst) (remove-element (cdr lst) element)))))

(defun selection-sort-functional (lst)
  (if (null lst)
    nil
    (let ((min (find-min (cdr lst) (car lst))))
      (cons min (selection-sort-functional (remove-element lst min))))))

(defun check-selection-sort-functional (name input expected)
  "Перевіряє функцію selection-sort-functional з вхідними даними input і очікуваним результатом expected."
  (format t "~:[FAILED~;PASSED~]... ~a~%" (equal (selection-sort-functional input) expected) name))

(defun test-selection-sort-functional ()
  (check-selection-sort-functional "test 1" '(5 3 8 1 4) '(1 3 4 5 8))
  (check-selection-sort-functional "test 2" '(9 7 5 3 1) '(1 3 5 7 9))
  (check-selection-sort-functional "test 3" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-selection-sort-functional "test 4" '(10) '(10))
  (check-selection-sort-functional "test 5" '(nil) '(nil)))

(test-selection-sort-functional)

(defun selection-sort-imperative (lst)
  (let ((sorted-list (copy-list lst))
    (n (length lst)))
    (loop for i from 0 below (1- n) do
      (let ((min-index i))
        (loop for j from (1+ i) below n do
          (when (< (nth j sorted-list) (nth min-index sorted-list))
            (setf min-index j)))
        (when (not (= min-index i))
          (let ((temp (nth i sorted-list)))
            (setf (nth i sorted-list) (nth min-index sorted-list))
            (setf (nth min-index sorted-list) temp)))))
  sorted-list))

(defun check-selection-sort-imperative (name input expected)
  "Перевіряє функцію selection-sort-imperative з вхідними даними input і очікуваним результатом expected."
  (format t "~:[FAILED~;PASSED~]... ~a~%" (equal (selection-sort-imperative input) expected) name))

(defun test-selection-sort-imperative ()
  (check-selection-sort-imperative "test 1" '(5 3 8 1 4) '(1 3 4 5 8))
  (check-selection-sort-imperative "test 2" '(9 7 5 3 1) '(1 3 5 7 9))
  (check-selection-sort-imperative "test 3" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-selection-sort-imperative "test 4" '(10) '(10))
  (check-selection-sort-imperative "test 5" '(nil) '(nil)))

(test-selection-sort-imperative)
