#lang racket

;;> (define c (device "USB0"))
;;> (c 'config)
;; TIM3_ch1_WRITE == teta1
;; TIM3_ch3_WRITE == teta2
;; TIM3_ch4_WRITE == teta3
;;> (define setDuty (demoTCC c 'teta1 'teta2 'teta3))
;;> (setDuty -10 10 10)

(require "main.rkt")
(require "../../robot-cinematica/cp-guile/robot/controler.rkt")

(define (regra3 ang settings)
  (match settings
    [(list angI andF dutyI dutyF)
     (let ( (a1 (- ang angI))
            (a2 (- andF ang))
            (d2 (- dutyF dutyI)) )
       (floor (+ (* (/ a1 a2) d2) dutyI))) ]) )

(define servo1 '(0 180 2260000 660000))
(define servo2 '(0 180 2195000 605000))
(define servo3 '(0 130 89000   2290000))

(define (demoTCC deviceCall teta1W teta2W teta3W)
  (lambda (x y z)
    (let-values ( ((teta1 teta2 teta3) (demo x y z)) )
      (deviceCall 'write teta1W teta1 (cadr servo1))
      (sleep 2)
      (deviceCall 'write teta2W teta2 (cadr servo2))
      (sleep 2)
      (deviceCall 'write teta3W teta3 (cadr servo3))
    )) )