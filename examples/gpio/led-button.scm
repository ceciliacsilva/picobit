(define (main)
  (let ( (led-verde  (led! Pin_9))
         (led-azul   (led! Pin_8))
         (botao-read (BUTTON-user)) )
    (led-azul #t)
    (let loop ( (pressionado? (botao-read)) )
      (if pressionado?
          (led-verde #t)
          (led-verde #f))
      (sleep 10000)
      (led-verde #f)
      (sleep 10000)
      (loop (botao-read)) )) )

(main)


