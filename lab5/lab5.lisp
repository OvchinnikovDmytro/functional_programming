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

(defun hashtable-to-alist (hashtable)
  (let ((alist '()))
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hashtable)
    (reverse alist)))

(defun read-csv (file-path &key (separator #\,))
  (let ((rows '()))
    (with-open-file (stream file-path :direction :input)
      (let ((headers (split-string (read-line stream) separator)))
        (loop for line = (read-line stream nil nil)
              while line
              do (let ((values (split-string line separator)))
                   (let ((row (make-hash-table :test 'equal)))
                     (loop for header in headers
                           for value in values
                           do (setf (gethash header row) (string-trim '(#\Space #\Tab #\Newline) value)))
                     (push row rows))))))
    (reverse rows)))

(defun select (file-path &key (separator #\,))
  (let ((rows (read-csv file-path :separator separator)))
    (lambda (&key (filters '()))
      (let ((filtered-rows rows))
        (dolist (filter filters filtered-rows)
          (let ((key (car filter))
                (expected-value (cdr filter)))
            (setf filtered-rows
                  (remove-if-not (lambda (row)
                                   (string= (gethash key row) expected-value))
                                 filtered-rows))))
        filtered-rows))))

(defun write-csv (file-path rows &key (separator #\,))
  (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (row rows)
      (let ((values (mapcar #'cdr (hashtable-to-alist row))))
        (format stream "~{~a~^~a~}~%" values separator)))))

(defun hashtable-to-alist (hashtable)
  (let ((alist '()))
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hashtable)
    (reverse alist)))

(defun pretty-print-csv-table (filepath &key (separator #\,))
  (when (probe-file filepath)
    (with-open-file (stream filepath :direction :input)
      (let* ((lines (loop for line = (read-line stream nil nil)
                          while line
                          collect line))
             (headers (split-string (first lines) separator))
             (rows (mapcar (lambda (line) (split-string line separator)) (rest lines)))
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
    (format t "Усі моделі: ~a~%"
            (mapcar #'hashtable-to-alist (funcall selector)))

    (format t "Модель з ID '2': ~a~%"
            (mapcar #'hashtable-to-alist (funcall selector :filters '(("id" . "2")))))

    (format t "Моделі для проекту 'SmartCityManager': ~a~%"
            (mapcar #'hashtable-to-alist (funcall selector :filters '(("project" . "SmartCityManager")))))

    (let ((filtered-models (funcall selector :filters '(("name" . "StudyBuddy")))))
    (write-csv "modelsOut.csv" filtered-models :separator #\,)))

  (pretty-print-csv-table "AiModels.csv"))


(defun test-projects ()
  (let ((selector (select "projects.csv")))
    (format t "Усі проекти: ~a~%"
      (mapcar #'hashtable-to-alist (funcall selector)))

    (format t "Проект за ключем '2': ~a~%"
      (mapcar #'hashtable-to-alist (funcall selector :filters '(("id" . "2")))))
    
    (let ((filtered-projects (funcall selector :filters '(("launch year" . "2023")))))
      (write-csv "projectsOut.csv" filtered-projects :separator #\,)))
      
    (pretty-print-csv-table "projects.csv"))

(test-models)
(test-projects)
