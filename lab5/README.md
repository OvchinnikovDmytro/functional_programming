<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Овчінніков Дмитро Станіслававович КВ-11</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у select . При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
* структури у геш-таблиці
* геш-таблиці у асоціативні списки
* асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.


## Варіант 5(17)
* База даних - Проєкти із застосуванням ШІ
* Тип записів - Геш таблиця
* Таблиці - Проекти, Моделі штучного інтелекту
* Опис - База даних моделей штучного інтелекту та проєктів, в яких вони використовуються.

## Лістинг реалізації завдання
```lisp
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
```
### Тестові набори та утиліти
```lisp
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
```
### Вміст тестового файлу projects.csv
```
id,name,researcher,launch year,description
1,BioAnalysis,BioTech Corp,2022,A project for analyzing biological data using MedAI.
2,SmartCrop,AgroAI Ltd,2021,A system for monitoring agricultural fields using AgroBrain.
3,SmartCityManager,UrbanTech Solutions,2023,A tool for urban planning powered by UrbanOptimizer.
4,LearningEnhancer,EdTech Innovations,2024,An educational platform based on the StudyBuddy AI model.
5,DNAExplorer,BioTech Corp,2023,A project for genomic analysis using GenoMapper.

```
### Вміст тестового файлу AiModels.csv
```
id,name,desgnation,project,description
1,MedAI,Healthcare AI,BioAnalysis,An AI model designed for analyzing biological data.
2,AgroBrain,Agriculture AI,SmartCrop,A model for predicting crop yield and monitoring plant health.
3,UrbanOptimizer,City Planning AI,SmartCityManager,Optimizes urban traffic and infrastructure management.
4,StudyBuddy,Educational AI,LearningEnhancer,A personalized educational assistant powered by NLP.
5,GenoMapper,Genomic AI,DNAExplorer,Automates the analysis of genomic data.

```
### Тестування
```lisp
Усі моделі: ((1 MedAI Healthcare AI BioAnalysis
              An AI model designed for analyzing biological data.)
             (2 AgroBrain Agriculture AI SmartCrop
              A model for predicting crop yield and monitoring plant health.)
             (3 UrbanOptimizer City Planning AI SmartCityManager
              Optimizes urban traffic and infrastructure management.)
             (4 StudyBuddy Educational AI LearningEnhancer
              A personalized educational assistant powered by NLP.)
             (5 GenoMapper Genomic AI DNAExplorer
              Automates the analysis of genomic data.))
Модель з ID '2': ((2 AgroBrain Agriculture AI SmartCrop
                   A model for predicting crop yield and monitoring plant health.))
Моделі для проекту 'SmartCityManager': ((3 UrbanOptimizer City Planning AI
                                         SmartCityManager
                                         Optimizes urban traffic and infrastructure management.))
Модель для фермерства: ((2 AgroBrain Agriculture AI SmartCrop
                         A model for predicting crop yield and monitoring plant health.))
((
 (NAME . "EcoPlanner")
 (DESIGNATION . "Environmental AI")
 (PROJECT . "GreenFuture")
 (DESCRIPTION . "Helps in planning eco-friendly infrastructure.")
))

++----+----------------+------------------+------------------+----------------------------------------------------------------
| | id | name           | desgnation       | project          | description
++----+----------------+------------------+------------------+----------------------------------------------------------------
| | 1  | MedAI          | Healthcare AI    | BioAnalysis      | An AI model designed for analyzing biological data.
| | 2  | AgroBrain      | Agriculture AI   | SmartCrop        | A model for predicting crop yield and monitoring plant health.
| | 3  | UrbanOptimizer | City Planning AI | SmartCityManager | Optimizes urban traffic and infrastructure management.
| | 4  | StudyBuddy     | Educational AI   | LearningEnhancer | A personalized educational assistant powered by NLP.
| | 5  | GenoMapper     | Genomic AI       | DNAExplorer      | Automates the analysis of genomic data.
| | 6  | EcoPlanner     | Environmental AI | GreenFuture      | Helps in planning eco-friendly infrastructure.
++----+----------------+------------------+------------------+----------------------------------------------------------------
Усі проекти: ((1 BioAnalysis BioTech Corp 2022
               A project for analyzing biological data using MedAI.)
              (2 SmartCrop AgroAI Ltd 2021
               A system for monitoring agricultural fields using AgroBrain.)
              (3 SmartCityManager UrbanTech Solutions 2023
               A tool for urban planning powered by UrbanOptimizer.)
              (4 LearningEnhancer EdTech Innovations 2024
               An educational platform based on the StudyBuddy AI model.)
              (5 DNAExplorer BioTech Corp 2023
               A project for genomic analysis using GenoMapper.))
Проект за ключем '2': ((2 SmartCrop AgroAI Ltd 2021
                        A system for monitoring agricultural fields using AgroBrain.))
Проект з роком запуску '2023': ((3 SmartCityManager UrbanTech Solutions 2023
                                 A tool for urban planning powered by UrbanOptimizer.)
                                (5 DNAExplorer BioTech Corp 2023
                                 A project for genomic analysis using GenoMapper.))
Проект від 'BioTech Corp': ((1 BioAnalysis BioTech Corp 2022
                             A project for analyzing biological data using MedAI.)
                            (5 DNAExplorer BioTech Corp 2023
                             A project for genomic analysis using GenoMapper.))
((
 (NAME . "Amogus")
 (RESEARCHER . "Definately Human")
 (YEAR . "2024")
 (DESCRIPTION . "The're here among us")
))

++----+------------------+---------------------+-------------+--------------------------------------------------------------
| | id | name             | researcher          | launch year | description
++----+------------------+---------------------+-------------+--------------------------------------------------------------
| | 1  | BioAnalysis      | BioTech Corp        | 2022        | A project for analyzing biological data using MedAI.
| | 2  | SmartCrop        | AgroAI Ltd          | 2021        | A system for monitoring agricultural fields using AgroBrain.
| | 3  | SmartCityManager | UrbanTech Solutions | 2023        | A tool for urban planning powered by UrbanOptimizer.
| | 4  | LearningEnhancer | EdTech Innovations  | 2024        | An educational platform based on the StudyBuddy AI model.
| | 5  | DNAExplorer      | BioTech Corp        | 2023        | A project for genomic analysis using GenoMapper.
| | 6  | Amogus           | Definately Human    | 2024        | The're here among us
++----+------------------+---------------------+-------------+--------------------------------------------------------------
```
