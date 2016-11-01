#include <picobit.h>
#include <dispatch.h>
#include <arch/stm32/rcc.h>
#include <arch/stm32/gpio.h>
#include <primitives.h>
#include <bignum.h>

void halt_with_error ()
{
  GPIOC->ODR |= BIT(8);

  while(1);
}

PRIMITIVE_UNSPEC(#%sleep, arch_sleep, 1)
{
  static int a, b;

  a1 = decode_int (arg1);

  for(a = 0; a < a1; a++) {
    for(b = 0; b < 100; b++) {
#if defined(CONFIG_ARM_COMPILER_GCC)
      __asm__ __volatile__("nop");
#endif

#if defined(CONFIG_ARM_COMPILER_KEIL)
      __nop();
#endif
    }
  }

  arg1 = OBJ_FALSE;
}

PRIMITIVE_UNSPEC(#%set-led!, arch_set_led, 1)
{
  if (arg1 == OBJ_FALSE) {
    GPIOC->ODR &= ~BIT(9);
  } else {
    GPIOC->ODR |= BIT(9);
  }

  arg1 = OBJ_FALSE;
}

PRIMITIVE_UNSPEC(#%GPIO-output, arch_GPIO_output, 2)
{
  int ch, port;

  port = decode_int(arg1);
  ch   = decode_int(arg2);

  switch(port){
  case 0:
    if(ch < 8){
      GPIOA->CRL |= 0x11111111 & (0x0000000B << (4*ch));
    }
    else if (ch < 16){
      GPIOA->CRH |= 0x11111111 & (0x0000000B << (4*(ch-8)));
    }
    break;

  case 1:
    if(ch < 8){
      GPIOB->CRL |= 0x11111111 & (0x0000000B << (4*ch));
    }
    else if (ch < 16){
      GPIOB->CRH |= 0x11111111 & (0x0000000B << (4*(ch-8)));
    }
    break;

  case 2:
    if(ch < 8){
      GPIOC->CRL |= 0x11111111 & (0x0000000B << (4*ch));
    }
    else if (ch < 16){
      GPIOC->CRH |= 0x11111111 & (0x0000000B << (4*(ch-8)));
    }
    break;
  }  
}

void main ()
{
  RCC->APB2ENR |= IOPCEN;

  GPIOC->CRL = 0x00000000;
  GPIOC->CRH = 0x00000000;
  GPIOC->ODR = 0x00000000;

  interpreter();
}
