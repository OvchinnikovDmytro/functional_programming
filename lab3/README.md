<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b>
<p align="center">
<br>"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
з дисципліни "Вступ до функціонального програмування"
</p>

<div style="display: flex; justify-content: flex-end;">
  <div style="border: 0px; padding: 10px;">
    <p>Студент: Овчінніков Дмитро Станіславович</p>
    <p>Група: КВ-11</p>
    <p>Рік: 2024</p>
  </div>
</div>


## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного
списку. Не допускається використання: деструктивних операцій, циклів, функцій

вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Також реалізована функція не має
бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список
цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
Також реалізована функція не має бути функціоналом (тобто приймати на вхід
функції в якості аргументів).
Алгоритм, який необхідно реалізувати, задається варіантом (п. 3.1.1). Зміст і шаблон
звіту наведені в п. 3.2.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (наприклад, як наведено у п. 2.3).

## Варіант 1(17)
Алгоритм сортування вибором за незменшенням

## Лістинг функції selection-sort-functional
```lisp
(defun find-min (lst current-min)
  (if (null lst) 
    current-min
    (find-min (cdr lst) 
      (if (< (car lst) current-min) 
        (car lst)
        current-min))))

(defun remove-element (lst element)
  (cond ((null lst) nil)
    ((equal (car lst) element) (cdr lst))
    (t (cons (car lst) (remove-element (cdr lst) element)))))

(defun selection-sort-functional (lst)
  (if (null lst)
    nil
    (let ((min (find-min (cdr lst) (car lst))))
      (cons min (selection-sort-functional (remove-element lst min))))))
```
### Тестові набори
```lisp
(defun check-selection-sort-functional (name input expected)
  "Перевіряє функцію selection-sort-functional з вхідними даними input і очікуваним результатом expected."
  (format t "~:[FAILED~;PASSED~]... ~a~%" (equal (selection-sort-functional input) expected) name))

(defun test-selection-sort-functional ()
  (check-selection-sort-functional "test 1" '(5 3 8 1 4) '(1 3 4 5 8))
  (check-selection-sort-functional "test 2" '(9 7 5 3 1) '(1 3 5 7 9))
  (check-selection-sort-functional "test 3" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-selection-sort-functional "test 4" '(10) '(10))
  (check-selection-sort-functional "test 5" '(nil) '(nil)))
```
### Тестування
```lisp
PASSED... test 1
PASSED... test 2
PASSED... test 3
PASSED... test 4
PASSED... test 5
PASSED... test 6
```
## Лістинг функції selection-sort-imperative
```lisp
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
```
### Тестові набори
```lisp
(defun check-selection-sort-imperative (name input expected)
  "Перевіряє функцію selection-sort-imperative з вхідними даними input і очікуваним результатом expected."
  (format t "~:[FAILED~;PASSED~]... ~a~%" (equal (selection-sort-imperative input) expected) name))

(defun test-selection-sort-imperative ()
  (check-selection-sort-imperative "test 1" '(5 3 8 1 4) '(1 3 4 5 8))
  (check-selection-sort-imperative "test 2" '(9 7 5 3 1) '(1 3 5 7 9))
  (check-selection-sort-imperative "test 3" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-selection-sort-imperative "test 4" '(10) '(10))
  (check-selection-sort-imperative "test 5" '(nil) '(nil)))
```
### Тестування
```lisp
PASSED... test 1
PASSED... test 2
PASSED... test 3
PASSED... test 4
PASSED... test 5
PASSED... test 6
```


