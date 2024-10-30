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
1. Написати функцію remove-seconds-and-thirds , яка видаляє зі списку кожен другий
і третій елементи:
```lisp
CL-USER> (remove-seconds-and-thirds '(a b c d e f g))
(A D G)
```
2. Написати функцію list-set-intersection , яка визначає перетин двох множин,
заданих списками атомів:
```lisp
CL-USER> (list-set-intersection '(1 2 3 4) '(3 4 5 6))
(3 4) ; порядок може відрізнятись
```
## Лістинг функції remove-seconds-and-thirds
```lisp
(defun remove-seconds-and-thirds (lst &optional (index 1))
  (when lst
    (if (or (= index 2) (= index 3))
        (remove-seconds-and-thirds (cdr lst) (if (= index 3) 1 (1+ index)))
        (cons (car lst) (remove-seconds-and-thirds (cdr lst) (1+ index))))))
```
### Тестові набори
```lisp
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
```
### Тестування
```lisp
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
passed... test 6
```
## Лістинг функції list-set-intersection
```lisp
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
```
### Тестові набори
```lisp
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
```
### Тестування
```lisp
passed... Test 1
passed... Test 2
passed... Test 3
passed... Test 4
passed... Test 5
passed... Test 6
passed... Test 7
passed... Test 8
```


