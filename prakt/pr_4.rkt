#lang racket
(display  "Задание 4. Описать функцию, которая находила бы сумму всех числовых элементов списка с учетом наличия подсписков. Пример: для списка
‘(1((2 3) 4) 5 6) результатом будет 21.
")
(define (append l1 l2)
(if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2))))
(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(display "Введите список с подсписками в формате (1((2 3) 4) 5 6): ")
(define in (read))
(enumerate-tree in)
(define (add in)
  (define (count in sum)
    (if(null? in)
       sum
       (count(cdr in) (+ sum (car in)))))
(count (enumerate-tree in) 0))
(and(display  "Сумма элементов: " )(add in))

(display  "\n\nЗадание 8. Реализовать функцию, которая выдавала бы элемент списка по
заданному номеру с конца.")
(define (reverse lst)
  (let loop ([lst lst] [lst-reversed '()])
    (if (empty? lst)
        lst-reversed
        (loop (rest lst) (cons (first lst) lst-reversed)))))
(display  "Введите список в формате (1 2 3 4): ")
(define lst (read))
(display  "Введите индекс элемента, начиная с конца: ")
(define ind (read))
(define tmp (reverse lst))
(if (> ind (length lst))
    (display  "Индекс больше размера списка")
    (display (list-ref tmp ind)))

(display  "\n\nЗадание 12. Реализовать функцию, меняющую местами первый и последний
элементы исходного списка.")
(define (swap-index idx1 idx2 lst)
  (define (build-list lst idx e1 e2)
    (cond ((null? lst)
           '())
          ((= idx idx1)
           (cons e2 (build-list (cdr lst) (add1 idx) e1 e2)))
          ((= idx idx2)
           (cons e1 (build-list (cdr lst) (add1 idx) e1 e2)))
          (else
           (cons (car lst) (build-list (cdr lst) (add1 idx) e1 e2)))))
  (build-list lst 0 (list-ref lst idx1) (list-ref lst idx2)))
(display  "Введите список в формате (1 2 3 4): ")
(define lst1 (read))
(define tmp1 (swap-index 0 (- (length lst1) 1) lst1))
(display "Изменённый список: ")
(display tmp1)

