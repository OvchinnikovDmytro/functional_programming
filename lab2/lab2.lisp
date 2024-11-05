(defun remove-seconds-and-thirds (lst &optional (index 1))
  (when lst
    (if (or (= index 2) (= index 3))
        (remove-seconds-and-thirds (cdr lst) (if (= index 3) 1 (1+ index)))
        (cons (car lst) (remove-seconds-and-thirds (cdr lst) (1+ index))))))


(defun check-remove-seconds-and-thirds (name input expected)
  "Execute `remove-seconds-and-thirds' on `input', compare result with `expected' and print comparison status"
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (remove-seconds-and-thirds input) expected)
          name))

(defun test-remove-seconds-and-thirds ()
  (check-remove-seconds-and-thirds "test 1" '(a b c d e f g) '(a d g))
  (check-remove-seconds-and-thirds "test 2" '(1 2 3 4 5 6) '(1 4))
  (check-remove-seconds-and-thirds "test 3" '() '())
  (check-remove-seconds-and-thirds "test 4" '(a) '(a))
  (check-remove-seconds-and-thirds "test 5" '(a b) '(a))
  (check-remove-seconds-and-thirds "test 6" '(1 a g d 5) '(1 d))
)

  (test-remove-seconds-and-thirds)

(defun list-set-intersection (a b)
  (when a
    (let ((x (car a)))
      (if (find-in-list x b)
        (cons x (list-set-intersection (cdr a) b))
        (list-set-intersection (cdr a) b)))))

(defun find-in-list (x lst)
  (when lst
      (if (eql x (car lst))
        t
        (find-in-list x (cdr lst)))))

(defun check-list-set-intersection (name set1 set2 expected)
  "Execute `list-set-intersection` on `set1` and `set2`, compare result with `expected` and print comparison status."
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (list-set-intersection set1 set2) expected)
          name))

(defun test-list-set-intersection ()
  (check-list-set-intersection "Test 1" '(1 2 3 4) '(3 4 5 6) '(3 4))
  (check-list-set-intersection "Test 2" '(a b c) '(b c d) '(b c))
  (check-list-set-intersection "Test 3" '() '(1 2 3) '())
  (check-list-set-intersection "Test 4" '(1 2 3) '() '())
  (check-list-set-intersection "Test 5" '(a b 1 d e) '(1 e f g) '(1 e))
  (check-list-set-intersection "Test 6" '(1 2 3 4) '(5 6 7 8) '())
  (check-list-set-intersection "Test 7" '(1 2 3) '(3 1 2) '(1 2 3))
  (check-list-set-intersection "Test 8" '(1 2 3 3) '(3 3 4 5) '(3 3))
)

(test-list-set-intersection)