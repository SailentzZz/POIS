#lang racket
(display "Задание 1. Сортировка пузырьком")
(display "\nВведите список в формате (5 4 3 2 1): ")
(define lst1 (read))
(define (buble-sort lst)
(define sorted-list lst)
(define temp 0)
(if (> 2 (length lst))
(void)
(begin
(for/list ([i (- (length lst) 1)])
(for/list ([j (-(- (length lst) 1) i)])
(if (> (list-ref sorted-list j) (list-ref sorted-list (+ j 1)))
(begin
(set! temp (list-ref sorted-list (+ j 1)))
(set! sorted-list (list-set sorted-list (+ j 1) (list-ref sorted-list j)))
(set! sorted-list (list-set sorted-list j temp)))
(void))))))
sorted-list)
(display "Отсортированный список: ")
(display (buble-sort lst1))

(display "\n\nЗадание 2. Быстрая сортировка")
(display "\nВведите список в формате (5 4 3 2 1): ")
(define lst2 (read))
(define (quick_sort lst)
  (cond
    [(< (length lst) 2) lst]
    [else (let ([pivot (car lst)] [rest (cdr lst)])
        (append
           (quick_sort (filter (lambda (x) (< x pivot)) rest))
           (list pivot)
           (quick_sort (filter (lambda (x) (>= x pivot)) rest))
        )
   )]))
(display "Отсортированный список: ")
(display (buble-sort lst2))

(display "\n\nЗадание 3. Написать программу-парсер строки, вводимой пользователем для
вычисления математического выражения, строка может содержать
скобки, числа, знаки арифметических операций")
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(define custom_lexer
  (lexer
   [(:: "define")
    (cons `(DEFINE ,(string->symbol lexeme))
          (custom_lexer input-port))]
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
    (cons `(ID ,(string->symbol lexeme))
          (custom_lexer input-port))]
   [(:: "#")
    (cons `(SHARP ,(string->symbol lexeme))
          (custom_lexer input-port))]
   [(:: (:? #\-) (:+ numeric))
    (cons `(INT ,(string->number lexeme))
          (custom_lexer input-port))]
   [whitespace
    (custom_lexer input-port)]
   [(eof)
    '()]))
(define (run_lexer value)
    (let ([input (open-input-string value)]) (custom_lexer input))
)
(display "\nВведите строку: ")
(define str (read))
(define (calc_string str)
  (eval (str)))
(display (calc_string str))