(define (main)
  (GPIO_init GPIOA 'Input  'Pull-down SPEED_in Pin_0)

  (let ( (led-verde (led! Pin_9))
         (led-azul  (led! Pin_8))
         (ledd (led! Pin_4)) )

    (ledd #t)
    (led-azul #t)
    (let loop ( (pressionado? (IO_read  GPIOA Pin_0)) )
      (if pressionado?
          (led-verde #t)
          (led-verde #f))
      (sleep 10000)
      (led-verde #f)
      (sleep 10000)
      (loop (IO_read  GPIOA Pin_0)) )) )

(main)
