;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname polyominoes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;
;; *************************************************
;;
;; CS 135 Assignment 10
;; Mark Luo, 20509972
;; (Polynominoes)
;;
;; *************************************************
;;

(require "a10.rkt")

;; Uncomment the following line if you want to use
;; the examples in kanoodle.rkt
;; (require "kanoodle.rkt")

;; A Grid is a (ne-listof (ne-listof Character))

(define-struct pos (x y))
;; A Pos is a (make-pos Int Int)

(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))


;;1.
;;a)
;;build-2dlist: Nat Nat (Nat Nat -> Any) -> (listof (listof (Nat Nat -> Any)))
;;Purpose: Build a grid of with a give Height (height)
;;and Width (width) and apply a coordinate function (coord) to 
;;the two parameter.
;;Example:
;(check-expect (build-2dlist 2 2 list) (list (list (list 0 0) (list 1 0))
;                                            (list (list 0 1) (list 1 1))))
;(check-expect (build-2dlist 1 1 make-pos) (list (list (make-pos 0 0))))
;(check-expect (build-2dlist 3 2 list) 
;              (list (list (list 0 0) (list 1 0) (list 2 0))
;                    (list (list 0 1) (list 1 1) (list 2 1))))
;(check-expect (build-2dlist 0 0 list) empty)


(define (build-2dlist width height coord)
  (build-list height (lambda (h)
                       (build-list width (lambda (w) (coord w h))))))


;;build-2dlist tests:
;(check-expect (build-2dlist 2 2 list) (list (list (list 0 0) (list 1 0))
;                                            (list (list 0 1) (list 1 1))))
;(check-expect (build-2dlist 1 1 make-pos) (list (list (make-pos 0 0))))
;(check-expect (build-2dlist 3 2 list) 
;              (list (list (list 0 0) (list 1 0) (list 2 0))
;                    (list (list 0 1) (list 1 1) (list 2 1))))
;(check-expect (build-2dlist 0 0 list) empty)

;;b)
;;all-positions: Nat Nat -> (listof Pos)
;;Purpose: Proce duce a list of Pos with the helper function
;;build-2dlist. The function takes in two Postive Natural Number
;;with the variable name h and w. 
;;Example:
;(check-expect (all-positions 1 1) (list(make-pos 0 0)))
;(check-expect (all-positions 3 2) (list(make-pos 0 0)
;                                       (make-pos 1 0)
;                                       (make-pos 2 0)
;                                       (make-pos 0 1) 
;                                       (make-pos 1 1) 
;                                       (make-pos 2 1)))
;(check-expect (all-positions 0 0) empty)


(define (all-positions w h)
  (foldr append empty (build-2dlist w h make-pos)))


;;all-positions tests:
;(check-expect (all-positions 1 1) (list(make-pos 0 0)))
;(check-expect (all-positions 3 2) (list(make-pos 0 0)
;                                       (make-pos 1 0)
;                                       (make-pos 2 0)
;                                       (make-pos 0 1) 
;                                       (make-pos 1 1) 
;                                       (make-pos 2 1)))
;(check-expect (all-positions 0 0) empty)


;;2.
;;all-orientations: Grid -> (listof Grid)
;;Purpose: To obtain all 8 orientations of a grid (shape) but
;;there can't be any repeated.
;;*Using assignment 9's remove-duplicate
;;Examples:
;(check-expect (all-orientations b) (list (list (list #\b))))
;(check-expect (all-orientations B) (list (list (list #\B #\B)
;                                               (list #\B #\B))))
;
;(check-expect (all-orientations R) (list (list (list #\R #\R #\R)
;                                               (list #\R #\R #\.))
;                                         (list (list #\R #\R #\R)
;                                               (list #\. #\R #\R))
;                                         (list (list #\R #\.)
;                                               (list #\R #\R)
;                                               (list #\R #\R))
;                                         (list (list #\. #\R)
;                                               (list #\R #\R)
;                                               (list #\R #\R))
;                                         (list (list #\. #\R #\R)
;                                               (list #\R #\R #\R))
;                                         (list (list #\R #\R #\.)
;                                               (list #\R #\R #\R))
;                                         (list (list #\R #\R)
;                                               (list #\R #\R)
;                                               (list #\. #\R))
;                                         (list (list #\R #\R)
;                                               (list #\R #\R)
;                                               (list #\R #\.))))
;(check-expect (all-orientations L) (list (list (list #\L #\L))
;                                         (list (list #\L)
;                                               (list #\L))))


(define (all-orientations shape)
  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (equal? x z))) y)))
         ;;Using Assignment 9's remove-duplicate function above
         empty
         (local [
                 ;;rotation: Grid Nat[= 4] -> (listof Grid)
                 ;;Purpose: Rotate the given Grid(grid) 4 times in total
                 (define (rotation grid count)
                   (cond
                     [(empty? (rest grid)) (list grid (map list (first grid)))]
                     [(zero? count) empty]
                     [else
                      (local [
                              ;;flip: Grid -> (listof Grid)
                              ;;Purpose: To flip a grid on its x-axis with
                              ;;the reverse function
                              (define (flip shape)
                                (cond
                                  [(empty? (first (rest shape)))empty]
                                  [else (cons (map first shape) 
                                              (flip (map rest shape)))]))]
                        (append (cons (flip grid) (list (flip (reverse grid))))
                                (rotation (flip (reverse grid)) 
                                          (sub1 count))))]))]
           (rotation shape 4))))


;;all-orientations tests:
;(check-expect (all-orientations b) (list (list (list #\b))))
;(check-expect (all-orientations B) (list (list (list #\B #\B)
;                                               (list #\B #\B))))
;
;(check-expect (all-orientations R) (list (list (list #\R #\R #\R)
;                                               (list #\R #\R #\.))
;                                         (list (list #\R #\R #\R)
;                                               (list #\. #\R #\R))
;                                         (list (list #\R #\.)
;                                               (list #\R #\R)
;                                               (list #\R #\R))
;                                         (list (list #\. #\R)
;                                               (list #\R #\R)
;                                               (list #\R #\R))
;                                         (list (list #\. #\R #\R)
;                                               (list #\R #\R #\R))
;                                         (list (list #\R #\R #\.)
;                                               (list #\R #\R #\R))
;                                         (list (list #\R #\R)
;                                               (list #\R #\R)
;                                               (list #\. #\R))
;                                         (list (list #\R #\R)
;                                               (list #\R #\R)
;                                               (list #\R #\.))))
;(check-expect (all-orientations L) (list (list (list #\L #\L))
;                                         (list (list #\L)
;                                               (list #\L))))



;;3.
;;first-empty-pos: Grid -> Pos
;;Purpose: Produce the first #\. existed in the list
;;and give it's Pos value. If the Grid has no empty #\.,
;;produce false.
;;Examples:
;(check-expect (first-empty-pos B) false)
;(check-expect (first-empty-pos b) false)
;(check-expect (first-empty-pos R) (make-pos 1 2))
;(check-expect (first-empty-pos T) (make-pos 1 0))
;(check-expect (first-empty-pos L) false)


(define (first-empty-pos shape)
  (local [
          ;;find-y: Grid Nat -> (Union Boolean Pos)
          ;;Purpose: To identify the y coordinate of the first empty spot
          ;;indicated by #\. by consuming a grid and an accumulator (y)
          ;;which starts from 0.
          (define (find-y grid y)
            (cond
              [(empty? grid) false]
              [(not(member? #\. (first grid))) (find-y (rest grid) (add1 y))]
              [else
               (make-pos (local [
                                 ;;find-x: (listof (listof Char)) Nat[= 0]
                                 ;;         -> Pos
                                 ;;Purpose: Consume a list of a list of 
                                 ;;Chars and find the position of the first
                                 ;;empty spot, indicated by #\., by
                                 ;;accumulating a variable (x) each time.
                                 ;;When the position of x is allocated,
                                 ;;the coordinate will then be made into a Pos.
                                 (define (find-x lst x)
                                   (cond
                                     [(char=? #\. (first lst)) x]
                                     [else (find-x (rest lst) (add1 x))]))]
                           (find-x (first grid) 0)) y)]))]
    (find-y shape 0)))

;;first-empty-pos tests:
;(check-expect (first-empty-pos B) false)
;(check-expect (first-empty-pos b) false)
;(check-expect (first-empty-pos R) (make-pos 1 2))
;(check-expect (first-empty-pos T) (make-pos 1 0))
;(check-expect (first-empty-pos L) false)



;;4.
;;superimpose: Grid Grid Pos -> Grid
;;Purpose: Produce the over lapping image from both consumed Grid
;;called "top" and "base". The Pos variable will indicate the position
;;where the top will start to overlap with the base.
;;The empty spot from the top, indicated by #\., will not cover the base.
;;Examples:
;(check-expect (superimpose T R (make-pos 4 1))
;              (list (list #\T #\. #\. #\. #\.)
;                    (list #\T #\T #\T #\. #\R)
;                    (list #\. #\T #\. #\. #\R)))
;(check-expect (superimpose T R (make-pos 0 0))
;              (list (list #\R #\R #\. #\. #\.)
;                    (list #\R #\R #\T #\. #\.)
;                    (list #\R #\T #\. #\. #\.)))
;(check-expect (superimpose T R (make-pos 3 0))
;              (list (list #\T #\. #\. #\R #\R)
;                    (list #\T #\T #\T #\R #\R)
;                    (list #\. #\T #\. #\R #\.)))
;(check-expect (superimpose b R (make-pos 3 0))
;              (list (list #\b)))
;(check-expect (superimpose B b (make-pos 1 1))
;              (list (list #\B #\B)
;                    (list #\B #\b)))



(define (superimpose base top Pos)
  (local [
          ;;merge: Grid Grid Pos Nat Nat -> Grid
          ;;Purpose: To use two accumulator, x y, it tracks where 
          ;;the top(gridT) is suppose to lay onto of the base (grideB)
          ;;by the given position (gridPos)
          (define (merge gridB gridT gridPos x y)
            (cond
              [(empty? gridB) empty]
              [(empty? gridT) gridB]
              [(< y (pos-y Pos)) (cons (first gridB) 
                                       (merge (rest gridB) 
                                              gridT 
                                              gridPos
                                              x (add1 y)))]
              [(<= x (pos-x Pos)) 
               (local [
                       ;;create-list: (listof Char) 
                       ;;             (listof Char)
                       ;;             Pos
                       ;;             Nat
                       ;;             -> Grid
                       ;;Purpose: Find if the value of the position (xPos)
                       ;;of the x coordinate is actually on the base list
                       ;;(listB) and that the top list (listT) can be cons onto
                       ;;the new list.
                       (define (create-list listB listT xPos x)
                         (cond
                           [(empty? listB) empty]
                           [(empty? listT) listB]
                           [(< x xPos) (cons (first listB)
                                             (create-list (rest listB)
                                                          listT
                                                          xPos
                                                          (add1 x)))]
                           [else (cond
                                   [(char=? #\. (first listT))
                                    (cons (first listB)
                                          (create-list (rest listB)
                                                       (rest listT) 
                                                       xPos
                                                       (add1 x)))]
                                   [else (cons (first listT) 
                                               (create-list (rest listB)
                                                            (rest listT)
                                                            xPos
                                                            (add1 x)))])]))]
                 (cons (create-list (first gridB) 
                                    (first gridT)
                                    (pos-x Pos) 0)
                       (merge (rest gridB)
                              (rest gridT)
                              gridPos x y)))]))]
    (merge base top Pos 0 0)))


;;superimpose tests:
;(check-expect (superimpose T R (make-pos 4 1))
;              (list (list #\T #\. #\. #\. #\.)
;                    (list #\T #\T #\T #\. #\R)
;                    (list #\. #\T #\. #\. #\R)))
;(check-expect (superimpose T R (make-pos 0 0))
;              (list (list #\R #\R #\. #\. #\.)
;                    (list #\R #\R #\T #\. #\.)
;                    (list #\R #\T #\. #\. #\.)))
;(check-expect (superimpose T R (make-pos 3 0))
;              (list (list #\T #\. #\. #\R #\R)
;                    (list #\T #\T #\T #\R #\R)
;                    (list #\. #\T #\. #\R #\.)))
;(check-expect (superimpose b R (make-pos 3 0))
;              (list (list #\b)))
;(check-expect (superimpose B b (make-pos 1 1))
;              (list (list #\B #\B)
;                    (list #\B #\b)))



;;5.
;;neighbours: State -> (listof State)
;;Purpose: Consume a single State as input and produces a list
;;of States such that an additional Polynomino has been added to
;;the puzzle
;;Example:

;;Note: Due to unfinished and codes that do not work properly,
;;codes below are commented out to pass the public test.
;;Also, examples will not be provided due to this reason.

;(define Q (list (list #\Q #\Q #\.)
;                (list #\Q #\Q #\.)
;                (list #\Q #\. #\.)))
;(define P (list (list #\. #\P)
;                (list #\P #\P)))
;
;(define (superposition B T)
;  (local [(define (check-super base top)
;            (cond
;              [(empty? top) false]
;              [(superimpose? base (first top) (first-empty-pos base))
;               (superimpose base (first top) (first-empty-pos base))]
;              [else (check-super base (rest top))]))]
;    (check-super B (all-orientations T))))
;
;(define (superimpose? base top Pos)
;  (local [
;          ;;merge: Grid Grid Pos Nat Nat -> Grid
;          ;;Purpose: To use two accumulator, x y, it tracks where 
;          ;;the top(gridT) is suppose to lay onto of the base (grideB)
;          ;;by the given position (gridPos)
;          (define (merge gridB gridT gridPos x y)
;            (cond
;              [(empty? gridB) false]
;              [(empty? gridT) true]
;              [(< y (pos-y Pos)) (merge (rest gridB) 
;                                        gridT 
;                                        gridPos
;                                        x (add1 y))]
;              [(<= x (pos-x Pos)) 
;               (local [
;                       ;;create-list: (listof Char) 
;                       ;;             (listof Char)
;                       ;;             Pos
;                       ;;             Nat
;                       ;;             -> Grid
;                       ;;Purpose: Find if the value of the position (xPos)
;                       ;;of the x coordinate is actually on the base list
;                       ;;(listB) and that the top list (listT) can be cons onto
;                       ;;the new list.
;                       (define (create-list listB listT xPos x)
;                         (cond
;                           [(empty? listB) false]
;                           [(empty? listT) true]
;                           [(< x xPos) (create-list (rest listB)
;                                                    listT
;                                                    xPos
;                                                    (add1 x))]
;                           [else (cond
;                                   [(char=? #\. (first listT))
;                                    (create-list listB
;                                                 (rest listT) 
;                                                 (sub1 xPos)
;                                                 x)]
;                                   [else (create-list (rest listB)
;                                                      (rest listT)
;                                                      xPos
;                                                      (add1 x))])]))]
;                 (and (create-list (first gridB) 
;                                   (first gridT)
;                                   (pos-x Pos) 0)
;                      (merge (rest gridB)
;                             (rest gridT)
;                             gridPos x y)))]))]
;    (merge base top Pos 0 0)))
;
;
;(superposition Q P)

;;neighbours tests:

;;Note: Unfinished codes will not run or pass the public test materials,
;;so there will not be any tests as well.

(define (make-empty-grid width height)
  (make-list height (make-list width #\.)))

(define (grid-ref grid p)
  (local
    [(define width (length (first grid)))
     (define height (length grid))
     (define (list-lookup lst pos)
       (cond
         [(empty? lst) false]
         [(= pos 0) (first lst)]
         [else (list-lookup (rest lst) (sub1 pos))]))]
    (cond
      [(< (pos-x p) 0) 'undef]
      [(>= (pos-x p) width) 'undef]
      [(< (pos-y p) 0) 'undef]
      [(>= (pos-y p) height) 'undef]
      [else (list-lookup (list-lookup grid (pos-y p)) (pos-x p))])))

(define (first-pos-pred grid pred?)
  (local
    [(define (first-pred-in-list L)
       (cond
         [(empty? L) false]
         [(pred? (grid-ref grid (first L))) (first L)]
         [else (first-pred-in-list (rest L))]))]
    (first-pred-in-list (all-positions (length (first grid)) (length grid)))))


(define (neighbours s)
  (local
    [
     (define puzz (state-puzzle s))
     (define width (length (first puzz)))
     (define height (length puzz))
     (define pieces (state-pieces s))
     
     (define (can-superimpose? top offset)
       (local
         [(define (check-one c-base c-top)
            (cond
              [(symbol? c-base) (char=? c-top #\.)]
              [(not (char=? c-top #\.)) (char=? c-base #\.)]
              [else true]))]
         (andmap
          (lambda (p)
            (check-one
             (grid-ref puzz
                       (make-pos (+ (pos-x offset) (pos-x p))
                                 (+ (pos-y offset) (pos-y p))))
             (grid-ref top p)))
          (all-positions (length (first top)) (length top)))))
     
     (define (first-valid-offset empty-pos top)
       (local
         [(define pt (first-pos-pred top (lambda (ch) (not (char=? ch #\.)))))]
         (make-pos (- (pos-x empty-pos) (pos-x pt))
                   (- (pos-y empty-pos) (pos-y pt)))))
     
     (define (test-orientations oris all-but)
       (cond
         [(empty? oris) empty]
         [else
          (local
            [(define ori (first oris))
             (define fe (first-valid-offset first-empty ori))]
            (cond
              [(can-superimpose? ori fe)
               (cons (make-state (superimpose puzz ori fe) all-but)
                     (test-orientations (rest oris) all-but))]
              [else (test-orientations (rest oris) all-but)]))]))
     
     (define first-empty (first-empty-pos puzz))]
    
    (cond
      [(boolean? first-empty) empty]
      [else
       (foldr
        append empty
        (map (lambda (poly)
               (local
                 [(define all-but (remove poly pieces))]
                 (test-orientations (all-orientations poly) all-but)))
             pieces))])))

(define (solve-puzzle grid polys viz-style)
  (local
    [(define result
       (search
        (lambda (S) (empty? (state-pieces S)))
        neighbours
        (cond
          [(symbol=? viz-style 'interactive)
           (lambda (S) (draw-grid (state-puzzle S)))]
          [else false])
        (make-state grid polys)))
     
     (define maybe-last-draw
       (cond
         [(and (state? result)
               (symbol=? viz-style 'at-end))
          (draw-grid (state-puzzle result))]
         [else false]))]
    (cond
      [(boolean? result) result]
      [else (map list->string (state-puzzle result))])))

(solve-puzzle pent-grid-1 pent-pieces-1 'online)
