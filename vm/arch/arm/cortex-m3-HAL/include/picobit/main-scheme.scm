;;File: main-scheme.scm - function in scheme
;;Cecilia Carneiro e Silva

;;SLEEP, enable-disable
;;sleep
(define (sleep time)
  (#%sleep time))
;;enable
(define disable 0)
(define enable  1)

;;GPIO - defines
;;GPIOs
(define GPIOA 0)
(define GPIOB 1)
(define GPIOC 2)
(define GPIOD 3)
(define GPIOE 4)
;;Direction
(define DIR_output 'Output)
(define DIR_input  'Input)
(define DIR_af     'Af)
;;Mode
(define MODE_pp        'Push-pull)
(define MODE_od        'Open-drain)
(define MODE_analog    'Analog)
(define MODE_floating  'Floation)
(define MODE_pull-down 'Pull-down)
(define MODE_pull-up   'Pull-up)
;;Speed
(define SPEED_2  1)
(define SPEED_10 1)
(define SPEED_50 1)
(define SPEED_in 0)
;;Pin
(define Pin_0           #x0001) 
(define Pin_1           #x0002) 
(define Pin_2           #x0004) 
(define Pin_3           #x0008) 
(define Pin_4           #x0010) 
(define Pin_5           #x0020) 
(define Pin_6           #x0040) 
(define Pin_7           #x0080) 
(define Pin_8           #x0100) 
(define Pin_9           #x0200) 
(define Pin_10          #x0400) 
(define Pin_11          #x0800) 
(define Pin_12          #x1000) 
(define Pin_13          #x2000) 
(define Pin_14          #x4000) 
(define Pin_15          #x8000) 
(define Pin_All         #xFFFF) 
;;configure mode_direction
(define GPIO_Mode_AIN         #x0)
(define GPIO_Mode_IN_FLOATING #x04)
(define GPIO_Mode_IPD         #x28)
(define GPIO_Mode_IPU         #x48)
(define GPIO_Mode_Out_OD      #x14)
(define GPIO_Mode_Out_PP      #x10)
(define GPIO_Mode_AF_OD       #x1C)
(define GPIO_Mode_AF_PP       #x18)

;;Configure GPIO
(define (GPIO_init GPIOx direction mode speed pin)
  ;;enable clock
  (cond [(equal? GPIOx GPIOA)
         (GPIOA_clock)]
        [(equal? GPIOx GPIOB)
         (GPIOB_clock)]
        [(equal? GPIOx GPIOC)
         (GPIOC_clock)]
        [(equal? GPIOx GPIOD)
         (GPIOD_clock)]
        [(equal? GPIOx GPIOE)
         (GPIOE_clock)])
  ;;configure 
  (cond [(equal? direction DIR_output)
         (if (equal? mode MODE_pp)
             (GPIO_config GPIOx GPIO_Mode_Out_PP pin speed)
             (GPIO_config GPIOx GPIO_Mode_Out_OD pin speed))]
        
        [(equal? direction DIR_af)
         (if (equal? mode MODE_pp)
             (GPIO_config GPIOx GPIO_Mode_AF_PP pin speed)
             (GPIO_config GPIOx GPIO_Mode_AF_OD pin speed))]

        [(equal? direction DIR_input)
         (cond [(equal? mode MODE_analog)
                (GPIO_config GPIOx GPIO_Mode_AIN pin speed)]
               [(equal? mode MODE_floating)
                (GPIO_config GPIOx GPIO_Mode_IN_FLOATING pin speed)]
               [(equal? mode MODE_pull-down)
                (GPIO_config GPIOx GPIO_Mode_IPD pin speed)]
               [(equal? mode MODE_pull-up)
                (GPIO_config GPIOx GPIO_Mode_IPU pin speed)] )]) )

;;IO_read and IO_write
(define (IO_read gpiox pin)
 (#%IO_read gpiox pin))
(define (IO_write gpiox pin value)
  (#%IO_write gpiox pin value))


;;ADC
;;ADC-defines
;;ADC-modes (uint32_t)
(define ADC_Mode_Independent                       #x00000000)
(define ADC_Mode_RegInjecSimult                    #x00010000)
(define ADC_Mode_RegSimult_AlterTrig               #x00020000)
(define ADC_Mode_InjecSimult_FastInterl            #x00030000)
(define ADC_Mode_InjecSimult_SlowInterl            #x00040000)
(define ADC_Mode_InjecSimult                       #x00050000)
(define ADC_Mode_RegSimult                         #x00060000)
(define ADC_Mode_FastInterl                        #x00070000)
(define ADC_Mode_SlowInterl                        #x00080000)
(define ADC_Mode_AlterTrig                         #x00090000)

;;ADC-trigger (only ADC1) (uint32_t)
(define ADC_ExternalTrigConv_T1_CC1                #x00000000) 
(define ADC_ExternalTrigConv_T1_CC2                #x00020000) 
(define ADC_ExternalTrigConv_T2_CC2                #x00060000) 
(define ADC_ExternalTrigConv_T3_TRGO               #x00080000) 
(define ADC_ExternalTrigConv_T4_CC4                #x000A0000) 
(define ADC_ExternalTrigConv_Ext_IT11_TIM8_TRGO    #x000C0000)
(define ADC_ExternalTrigConv_T1_CC3                #x00040000)
(define ADC_ExternalTrigConv_None                  (arithmetic-shift #x000E0000 -8))

;;ADC-dataAlign (uint32_t)
(define ADC_DataAlign_Right                        #x00000000)
(define ADC_DataAlign_Left                         #x00000800)

;;ADC-Channel (uint8_t)
(define ADC_Channel_0                               #x00)
(define ADC_Channel_1                               #x01)
(define ADC_Channel_2                               #x02)
(define ADC_Channel_3                               #x03)
(define ADC_Channel_4                               #x04)
(define ADC_Channel_5                               #x05)
(define ADC_Channel_6                               #x06)
(define ADC_Channel_7                               #x07)
(define ADC_Channel_8                               #x08)
(define ADC_Channel_9                               #x09)
(define ADC_Channel_10                              #x0A)
(define ADC_Channel_11                              #x0B)
(define ADC_Channel_12                              #x0C)
(define ADC_Channel_13                              #x0D)
(define ADC_Channel_14                              #x0E)
(define ADC_Channel_15                              #x0F)
(define ADC_Channel_16                              #x10)
(define ADC_Channel_17                              #x11)

;;ADC-tempSensor (uint8_t)
(define ADC_Channel_TempSensor                      ADC_Channel_16)
(define ADC_Channel_Vrefint                         ADC_Channel_17)

;;ADC-Channels config
;;ADC-sampleTime (uint8_t)
(define ADC_SampleTime_1Cycles5                    #x00)
(define ADC_SampleTime_7Cycles5                    #x01)
(define ADC_SampleTime_13Cycles5                   #x02)
(define ADC_SampleTime_28Cycles5                   #x03)
(define ADC_SampleTime_41Cycles5                   #x04)
(define ADC_SampleTime_55Cycles5                   #x05)
(define ADC_SampleTime_71Cycles5                   #x06)
(define ADC_SampleTime_239Cycles5                  #x07)

;;ADC-externalTrig (uint32_t)
(define ADC_ExternalTrigInjecConv_T2_TRGO           #x00002000) 
(define ADC_ExternalTrigInjecConv_T2_CC1            #x00003000) 
(define ADC_ExternalTrigInjecConv_T3_CC4            #x00004000) 
(define ADC_ExternalTrigInjecConv_T4_TRGO           #x00005000) 
(define ADC_ExternalTrigInjecConv_Ext_IT15_TIM8_CC4 #x00006000) 
(define ADC_ExternalTrigInjecConv_T1_TRGO           #x00000000) 
(define ADC_ExternalTrigInjecConv_T1_CC4            #x00001000) 
(define ADC_ExternalTrigInjecConv_None              #x00007000)

;;ADC-injectedSelection (uint8_t)
(define ADC_InjectedChannel_1                       #x14)
(define ADC_InjectedChannel_2                       #x18)
(define ADC_InjectedChannel_3                       #x1C)
(define ADC_InjectedChannel_4                       #x20)

;;ADC-analogWatchdog (uint32_t)
(define ADC_AnalogWatchdog_SingleRegEnable         #x00800200)
(define ADC_AnalogWatchdog_SingleInjecEnable       #x00400200)
(define ADC_AnalogWatchdog_SingleRegOrInjecEnable  #x00C00200)
(define ADC_AnalogWatchdog_AllRegEnable            #x00800000)
(define ADC_AnalogWatchdog_AllInjecEnable          #x00400000)
(define ADC_AnalogWatchdog_AllRegAllInjecEnable    #x00C00000)
(define ADC_AnalogWatchdog_None                    #x00000000)

;;ADC-interruptsDefinition
(define ADC_IT_EOC                                 #x0220)
(define ADC_IT_AWD                                 #x0140)
(define ADC_IT_JEOC                                #x0480)

;;ADC-flagsDefinition
(define ADC_FLAG_AWD                               #x01)
(define ADC_FLAG_EOC                               #x02)
(define ADC_FLAG_JEOC                              #x04)
(define ADC_FLAG_JSTRT                             #x08)
(define ADC_FLAG_STRT                              #x10)

(define (ADC_config scanConv contMode externalTrig nChannels)
  (#%ADC_config scanConv contMode externalTrig nChannels) )

(define (ADC_testando)
  (let loop ( (ab (ADC_read)) )
    (if (< ab 2000)
        (IO_write GPIOC Pin_8 #f)
        (IO_write GPIOC Pin_8 #t))
    (loop (ADC_read)) ))

