(define (main)
  (setup)

  (let ( (led-duty (PWM_simple TIM_3 (- 24 1) (- 20000 1) TIM_Channel_3 #t))
         (readValue_ch14 (ADC_single 14 #f))
         (led-verde (led! Pin_9))
         (botao-read (BUTTON-user)) )
    (led-verde #t)
    (let loop ( (value-ad (readValue_ch14))
                (pressionado? (botao-read)) )
      (sleep 10000)
      (if pressionado?
          (begin (led-verde #t)
                 (led-duty value-ad 4096))
          (led-verde #f) )
      (loop (readValue_ch14) (botao-read) )        
      )
    )
  )

(define (setup)
  ;;(GPIO_init GPIOC 'Alternative-function 'Push-pull SPEED_50 Pin_8)
  (PWM_output GPIOC Pin_8) 
  
  (AFIO_clock)
  (GPIO_FullRemap_TIM3)  )

(main)
