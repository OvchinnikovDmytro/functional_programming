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

(defun print-hashtable (hashtable)
  (maphash (lambda (key value)
             (format t "~a: ~a~%" key value))
           hashtable))

```
### Тестові набори та утиліти
```lisp

(defun test-models ()
  (let ((selector (select "AiModels.csv")))
    (format t "Усі моделі: ~%")
    (dolist (row (funcall selector))
      (print-hashtable row))

    (format t "Модель з ID '2': ~%")
    (dolist (row (funcall selector :filters '(("id" . "2"))))
      (print-hashtable row))

    (format t "Моделі для проекту 'SmartCityManager': ~a~%"
            (mapcar #'hashtable-to-alist (funcall selector :filters '(("project" . "SmartCityManager")))))

    (let ((filtered-models (funcall selector :filters '(("name" . "StudyBuddy")))))
    (write-csv "modelsOut.csv" filtered-models :separator #\,)))

  (pretty-print-csv-table "AiModels.csv"))


(defun test-projects ()
  (let ((selector (select "projects.csv")))
    (format t "Усі проекти: ~%")
    (dolist (row (funcall selector))
      (print-hashtable row))
    
    (format t "Проект компанії BioTech Corp : ~%")
    (dolist (row (funcall selector :filters '(("researcher" . "BioTech Corp"))))
      (print-hashtable row))

    (format t "Модель з ID '3': ~a~%"
            (mapcar #'hashtable-to-alist (funcall selector :filters '(("id" . "3")))))

    (let ((filtered-projects (funcall selector :filters '(("launch year" . "2023")))))
      (write-csv "projectsOut.csv" filtered-projects :separator #\,)))
      
    (pretty-print-csv-table "projects.csv"))
```
### Вміст тестового файлу projects.csv
```
id,name,researcher,launch year,description
1,BioAnalysis,BioTech Corp,2022,A project for analyzing biological data using MedAI.
2,SmartCrop,AgroAI Ltd,2021,A system for monitoring agricultural fields using AgroBrain.
3,SmartCityManager,UrbanTech Solutions,2023,A tool for urban planning powered by UrbanOptimizer.
4,LearningEnhancer,EdTech Innovations,2024,An educational platform based on the StudyBuddy AI model.
5,DNAExplorer,BioTech Corp,2023,A project for genomic analysis using GenoMapper.
6,Amogus,Definately Human,2024,The're here among us

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
Усі моделі: 
id: 1
name: MedAI
designation: Healthcare AI
project: BioAnalysis
: An AI model designed for analyzing biological data.
id: 2
name: AgroBrain
designation: Agriculture AI
project: SmartCrop
: A model for predicting crop yield and monitoring plant health.
id: 3
name: UrbanOptimizer
designation: City Planning AI
project: SmartCityManager
: Optimizes urban traffic and infrastructure management.
id: 4
name: StudyBuddy
designation: Educational AI
project: LearningEnhancer
: A personalized educational assistant powered by NLP.
id: 5
name: GenoMapper
designation: Genomic AI
project: DNAExplorer
: Automates the analysis of genomic data.
Модель з ID '2':
id: 2
name: AgroBrain
designation: Agriculture AI
project: SmartCrop
: A model for predicting crop yield and monitoring plant health.
Моделі для проекту 'SmartCityManager': (((id . 3) (name . UrbanOptimizer)
                                         (designation . City Planning AI)
                                         (project . SmartCityManager)
                                         (description
)))                                       . Optimizes urban traffic and infrastructure management.

++----+----------------+------------------+------------------+-----------------------------------------------------------------
                                                  ct          | description
++----+----------------+------------------+------------------+-----------------------------------------------------------------
           dAI          | Healthcare AI    | BioAnalysis      | An AI model designed for analyzing biological data.
| | 2  | AgroBrain      | Agriculture AI   | SmartCrop        | A model for predicting crop yield and monitoring plant health.
         UrbanOptimizer | City Planning AI | SmartCityManager | Optimizes urban traffic and infrastructure management.
          tudyBuddy     | Educational AI   | LearningEnhancer | A personalized educational assistant powered by NLP.
                        | Genomic AI       | DNAExplorer      | Automates the analysis of genomic data.
++----+----------------+------------------+------------------+-----------------------------------------------------------------
Усі проекти:
id: 1
name: BioAnalysis
researcher: BioTech Corp
launch year: 2022
description: A project for analyzing biological data using MedAI.
id: 2
name: SmartCrop
researcher: AgroAI Ltd
launch year: 2021
description: A system for monitoring agricultural fields using AgroBrain.
id: 3
name: SmartCityManager
researcher: UrbanTech Solutions
launch year: 2023
description: A tool for urban planning powered by UrbanOptimizer.
id: 4
name: LearningEnhancer
researcher: EdTech Innovations
launch year: 2024
description: An educational platform based on the StudyBuddy AI model.
id: 5
name: DNAExplorer
researcher: BioTech Corp
launch year: 2023
description: A project for genomic analysis using GenoMapper.
id: 6
name: Amogus
researcher: Definately Human
launch year: 2024
description: The're here among us
Проект компанії BioTech Corp :
id: 1
name: BioAnalysis
researcher: BioTech Corp
launch year: 2022
description: A project for analyzing biological data using MedAI.
id: 5
name: DNAExplorer
researcher: BioTech Corp
launch year: 2023
description: A project for genomic analysis using GenoMapper.
Модель з ID '3': (((id . 3) (name . SmartCityManager)
                   (researcher . UrbanTech Solutions) (launch year . 2023)
                   (description
                    . A tool for urban planning powered by UrbanOptimizer.)))

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
