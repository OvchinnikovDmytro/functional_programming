(defun split-string (string delimiter)
  (let ((result '())
        (start 0))
    (loop for pos = (position delimiter string :start start)
          while pos
          do (progn
               (push (string-trim '(#\Space #\Tab #\Newline) (subseq string start pos)) result)
               (setf start (1+ pos)))
          finally (push (string-trim '(#\Space #\Tab #\Newline) (subseq string start)) result))
    (nreverse result)))

(defun read-csv-as-list (file-path &key (separator #\,))
  (let ((rows '()))
    (with-open-file (stream file-path :direction :input)
      (read-line stream)
      (loop for line = (read-line stream nil nil)
            while line
            do (push (split-string line separator) rows)))
    (reverse rows)))

(defun select (file-path &key (structure '()) (separator #\,))
  (let ((rows (read-csv-as-list file-path :separator separator)))
    (lambda (&key (filters '()))
      (let ((filtered-rows rows))
        (dolist (filter filters filtered-rows)
          (let ((column (car filter))
                (expected-value (cdr filter)))
            (setf filtered-rows
                  (remove-if-not (lambda (row)
                                   (string= (nth column row) expected-value))
                                 filtered-rows))))))))

(defun write-hashtable-to-projects-csv (filepath hashtable keys)
  (let ((last-number 0)
        (file-non-empty-p (probe-file filepath)))
    (when file-non-empty-p
      (with-open-file (stream filepath :direction :input)
        (loop for line = (read-line stream nil nil)
              while line
              do (let ((parts (split-string line #\,)))
                   (when (and (not (null parts)) (integerp (parse-integer (first parts) :junk-allowed t)))
                     (setf last-number (max last-number (parse-integer (first parts)))))))))
    (with-open-file (stream filepath :direction :output :if-exists :append :if-does-not-exist :create)
      (let ((values (mapcar (lambda (key)
                              (let ((value (gethash key hashtable)))
                                (cond
                                 ((null value) "")
                                 ((stringp value) value)
                                 (t (princ-to-string value)))))
                            keys)))
        (format stream "~a,~{~a~^,~}~%" (1+ last-number) values)))))

(defun hashtable-to-alist (hashtable)
  (let ((alist '()))
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hashtable)
    (reverse alist)))

(defun print-alist (alist)
  (format t "((~%")
  (dolist (pair alist)
    (format t " (~a . \"~a\")~%" (car pair) (cdr pair)))
  (format t "))~%"))

(defun pretty-print-csv-table (filepath)
  (when (probe-file filepath)
    (with-open-file (stream filepath :direction :input)
      (let* ((lines (loop for line = (read-line stream nil nil)
                          while line
                          collect line))
             (headers (split-string (first lines) #\,))
             (rows (mapcar (lambda (line) (split-string line #\,)) (rest lines)))
             (column-widths (mapcar (lambda (col-index)
                                       (max (length (nth col-index headers))
                                            (loop for row in rows maximize (length (nth col-index row)))))
                                     (loop for i from 0 below (length headers) collect i))))

        (format t "~%~a~%" (make-line-separator column-widths))
        (format t "~a~%" (make-row headers column-widths))
        (format t "~a~%" (make-line-separator column-widths))

        (dolist (row rows)
          (format t "~a~%" (make-row row column-widths)))
        (format t "~a~%" (make-line-separator column-widths))))))

(defun make-row (values widths)
  (reduce (lambda (acc pair)
            (destructuring-bind (value width) pair
              (concatenate 'string acc " | " (format nil "~v@<~a~>" width (or value "")))))
          (mapcar #'list values widths)
          :initial-value "|"))

(defun make-line-separator (widths)
  (reduce (lambda (acc width)
            (concatenate 'string acc "+" (make-string (+ width 2) :initial-element #\-)))
          widths
          :initial-value "+"))

(defun test-models ()
  (let ((selector (select "AiModels.csv")))
    (format t "Усі моделі: ~a~%" (funcall selector))

    (format t "Модель з ID '2': ~a~%"
            (funcall selector :filters '((0 . "2"))))

    (format t "Моделі для проекту 'SmartCityManager': ~a~%"
            (funcall selector :filters '((3 . "SmartCityManager"))))

    (format t "Модель для фермерства: ~a~%"
            (funcall selector :filters '((2 . "Agriculture AI"))))

    (let ((my-hashtable (make-hash-table)))
      (setf (gethash 'name my-hashtable) "EcoPlanner")
      (setf (gethash 'designation my-hashtable) "Environmental AI")
      (setf (gethash 'project my-hashtable) "GreenFuture")
      (setf (gethash 'description my-hashtable) "Helps in planning eco-friendly infrastructure.")

      (write-hashtable-to-projects-csv "AiModels.csv" my-hashtable '(name designation project description))
      (let ((alist (hashtable-to-alist my-hashtable)))
        (print-alist alist)))

    (pretty-print-csv-table "AiModels.csv")))

(defun test-projects ()
  (let ((selector (select "projects.csv")))
    (format t "Усі проекти: ~a~%" (funcall selector))

    (format t "Проект за ключем '2': ~a~%"
            (funcall selector :filters '((0 . "2"))))

    (format t "Проект з роком запуску '2023': ~a~%"
            (funcall selector :filters '((3 . "2023"))))
    
    (format t "Проект від 'BioTech Corp': ~a~%"
            (funcall selector :filters '((2 . "BioTech Corp"))))

(let ((my-hashtable (make-hash-table)))
  (setf (gethash 'name my-hashtable) "Amogus")
  (setf (gethash 'researcher my-hashtable) "Definately Human")
  (setf (gethash 'year my-hashtable) 2024)
  (setf (gethash 'description my-hashtable) "The're here among us")

  (write-hashtable-to-projects-csv "projects.csv" my-hashtable '(name researcher year description))
  (let ((alist (hashtable-to-alist my-hashtable)))
    (print-alist alist)))

  (pretty-print-csv-table "projects.csv")))

(test-models)

(test-projects)
