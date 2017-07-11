(define (main)
  (setup)

  (let ( (teta1 (PWM_simple TIM_3 (- 24 1) (- 20000 1) TIM_Channel_1 #t))
	 (teta2 (PWM_simple TIM_3 (- 24 1) (- 20000 1) TIM_Channel_3 #t))
	 (teta3 (PWM_simple TIM_3 (- 24 1) (- 20000 1) TIM_Channel_4 #t))
	 (botao-read (BUTTON-user)) )
    (let loop ( (pressionado? (botao-read)) )
      (sleep 40000)
      (if pressionado?
	  (teta1 20 100)
	  )
      (loop (botao-read) )        
      )
    )
  )

(define (setup)
  ;;(GPIO_init GPIOC 'Alternative-function 'Push-pull SPEED_50 Pin_8)
  (PWM_output GPIOC Pin_6)
  (PWM_output GPIOC Pin_8)
  (PWM_output GPIOC Pin_9)
  
  (AFIO_clock)
  (GPIO_FullRemap_TIM3)  )

(main)
