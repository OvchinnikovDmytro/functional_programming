<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b>
<p align="center">
<br>"Функції вищого порядку та замикання"<br/>
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
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
використати функції вищого порядку для роботи з послідовностями (де це
доречно);
додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями. При
цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
можливості, має бути мінімізоване.


Алгоритм сортування вибором за незменшенням

## Лістинг функції selection-sort-functional
```lisp
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
```
### Тестові набори
```lisp
(defun check-selection-sort-functional (name input expected &key (key #'identity) (test #'<))
  "Перевіряє функцію selection-sort-functional з вхідними даними input і очікуваним результатом EXPECTED."
  (format t "~:[FAILED~;PASSED~]... ~a~%" (equal (selection-sort-functional input :key key :test test) expected) name))

(defun test-selection-sort-functional ()
  (check-selection-sort-functional "test 1" '(5 3 8 1 4) '(1 3 4 5 8))
  (check-selection-sort-functional "test 2" '(9 7 5 3 1) '(1 3 5 7 9))
  (check-selection-sort-functional "test 3" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-selection-sort-functional "test 4" '(10) '(10))
  (check-selection-sort-functional "test 5" '(nil) '(nil))
  (check-selection-sort-functional "test 6" '(-3 1 -2 5 4) '(1 -2 -3 4 5) :key #'abs)
)
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

## Варіант 5(17)

Написати функцію propagator-fn , яка має один ключовий параметр — функцію
comparator . propagator-fn має повернути функцію, яка при застосуванні в якості
першого аргументу mapcar разом з одним списком-аргументом робить наступне: якщо
елемент не "кращий" за попередній згідно з comparator , тоді він заміняється на
значення попереднього, тобто "кращого", елемента. Якщо ж він "кращий" за попередній
елемент згідно comparator , тоді заміна не відбувається. Функція comparator за
замовчуванням має значення #'> .

```lisp
CL-USER> (mapcar (propagator-fn) '(1 2 3))
(1 2 3)
CL-USER> (mapcar (propagator-fn) '(3 1 4 2))
(3 3 4 4)
CL-USER> (mapcar (propagator-fn :comparator #'<) '(1 2 3))
(1 1 1)
```

## Лістинг функції propagator-fn
```lisp
(defun propagator-fn (&key (comparator #'>))
  (let ((best nil))
    (lambda (current)
      (if (or (null best) (funcall comparator current best))
        (setf best current)
        best))))
```
### Тестові набори
```lisp
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
```
### Тестування
```lisp
PASSED... test 1
PASSED... test 2
PASSED... test 3
PASSED... test 4
PASSED... test 5
PASSED... test 6
PASSED... test 7
```


