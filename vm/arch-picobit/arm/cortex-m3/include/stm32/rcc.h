#ifndef _STM32_RCC_H_
#define _STM32_RCC_H_

#include <arch/stm32/bits.h>

struct regs_RCC {
	reg CR;
#define		PLLRDY		BIT(25)
#define		PLLON		BIT(24)
#define		CSSON		BIT(19)
#define		HSEBYP		BIT(18)
#define		HSERDY		BIT(17)
#define		HSEON		BIT(16)
#define		R_HSICAL(v)	R_BITS(v, 8, 15)
#define		W_HSICAL(v, x)	W_BITS(v, x, 8, 15)
#define		HSICAL(x)	BITS(x, 8, 15)
#define		R_HSITRIM(v)	R_BITS(x, 3, 7)
#define		W_HSITRIM(v, x)	W_BITS(v, x, 3, 7)
#define		HSITRIM(x)	BITS(x, 3, 7)
#define		HSIRDY		BIT(1)
#define		HSION		BIT(0)
	reg CFGR;
#define		R_MCO(v)	R_BITS(v, 24, 26)
#define		W_MCO(v, x)	W_BITS(v, 24, 26)
#define		MCO(x)		BITS(x, 24, 26)
#define			MCO_NONE	0
#define			MCO_SYSCLK	4
#define			MCO_HSI		5
#define			MCO_HSE		6
#define			MCO_PLL_DIV2	7
#define		R_PLLMUL(v)	R_BITS(v, 18, 21)
#define		W_PLLMUL(v, x)	W_BITS(v, x, 18, 21)
#define		PLLMUL(x)	BITS(x, 18, 21)
#define		PLLXTPRE	BIT(17)
#define		PLLSRC		BIT(16)
#define		R_ADCPRE(v)	R_BITS(v, 14, 15)
#define		W_ADCPRE(v, x)	W_BITS(v, x, 14, 15)
#define		ADCPRE(x)	BITS(x, 14, 15)
#define		R_PPRE2(v)	R_BITS(v, 11, 13)
#define		W_PPRE2(v, x)	W_BITS(v, x, 11, 13)
#define		PPRE2(x)	BITS(x, 11, 13)
#define		R_PPRE1(v)	R_BITS(v, 8, 10)
#define		W_PPRE1(v, x)	W_BITS(v, x, 8, 10)
#define		PPRE1(x)	BITS(x, 8, 10)
#define		R_HPRE(v)	R_BITS(v, 4, 7)
#define		W_HPRE(v, x)	W_BITS(v, x, 4, 7)
#define		HPRE(x)		BITS(x, 4, 7)
#define		R_SWS(v)	R_BITS(v, 2, 3)
#define			SWS_HSI		0
#define			SWS_HSE		1
#define			SWS_PLL		2
#define		R_SW(v)		R_BITS(v, 0, 1)
#define		W_SW(v, x)	W_BITS(v, x, 0, 1)
#define		SW(x)		BITS(x, 0, 1)
#define			SW_HSI		0
#define			SW_HSE		1
#define			SW_PLL		2
	reg CIR;
#define		CSSC		BIT(23)
#define		PLLRDYC		BIT(20)
#define		HSERDYC		BIT(19)
#define		HSIRDYC		BIT(18)
#define		LSERDYC		BIT(17)
#define		LSIRDYC		BIT(16)
#define		PLLRDYIE	BIT(12)
#define		HSERDYIE	BIT(11)
#define		HSIRDYIE	BIT(10)
#define		LSERDYIE	BIT(9)
#define		LSIRDYIE	BIT(8)
#define		CSSF		BIT(7)
#define		PLLRDYF		BIT(4)
#define		HSERDYF		BIT(3)
#define		HSIRDYF		BIT(2)
#define		LSERDYF		BIT(1)
#define		LSIRDYF		BIT(0)
	reg APB2RSTR;
#define		TIM17RST	BIT(18)
#define		TIM16RST	BIT(17)
#define		TIM15RST	BIT(16)
#define		USART1RST	BIT(14)
#define		SPI1RST		BIT(12)
#define		TIM1RST		BIT(11)
#define		ADC1RST		BIT(9)
#define		IOPGRST		BIT(8)
#define		IOPFRST		BIT(7)
#define		IOPERST		BIT(6)
#define		IOPDRST		BIT(5)
#define		IOPCRST		BIT(4)
#define		IOPBRST		BIT(3)
#define		IOPARST		BIT(2)
#define		AFIORST		BIT(0)
	reg APB1RSTR;
#define		CECRST		BIT(30)
#define		DACRST		BIT(29)
#define		PWRRST		BIT(28)
#define		BKPRST		BIT(27)
#define		I2C2RST		BIT(22)
#define		I2C1RST		BIT(21)
#define		UART5RST	BIT(20)
#define		UART4RST	BIT(19)
#define		USART3RST	BIT(18)
#define		USART2RST	BIT(17)
#define		SPI3RST		BIT(15)
#define		SPI2RST		BIT(14)
#define		WWDGRST		BIT(11)
#define		TIM14RST	BIT(8)
#define		TIM13RST	BIT(7)
#define		TIM12RST	BIT(6)
#define		TIM7RST		BIT(5)
#define		TIM6RST		BIT(4)
#define		TIM5RST		BIT(3)
#define		TIM4RST		BIT(2)
#define		TIM3RST		BIT(1)
#define		TIM2RST		BIT(0)
	reg AHBENR;
#define		FMSCEN		BIT(8)
#define		CRCEN		BIT(6)
#define		FLITFEN		BIT(4)
#define		SRAMEN		BIT(2)
#define		DMA2EN		BIT(1)
#define		DMA1EN		BIT(0)
	reg APB2ENR;
#define		TIM17EN		BIT(18)
#define		TIM16EN		BIT(17)
#define		TIM15EN		BIT(16)
#define		USART1EN	BIT(14)
#define		SPI1EN		BIT(12)
#define		TIM1EN		BIT(11)
#define		ADC1EN		BIT(9)
#define		IOPGEN		BIT(8)
#define		IOPFEN		BIT(7)
#define		IOPEEN		BIT(6)
#define		IOPDEN		BIT(5)
#define		IOPCEN		BIT(4)
#define		IOPBEN		BIT(3)
#define		IOPAEN		BIT(2)
#define		AFIOEN		BIT(0)
	reg APB1ENR;
#define		CECEN		BIT(30)
#define		DACEN		BIT(29)
#define		PWREN		BIT(28)
#define		BKPEN		BIT(27)
#define		I2C2EN		BIT(22)
#define		I2C1EN		BIT(21)
#define		UART5EN		BIT(20)
#define		UART4EN		BIT(19)
#define		USART3EN	BIT(18)
#define		USART2EN	BIT(17)
#define		SPI3EN		BIT(15)
#define		SPI2EN		BIT(14)
#define		WWDGEN		BIT(11)
#define		TIM14EN		BIT(8)
#define		TIM13EN		BIT(7)
#define		TIM12EN		BIT(6)
#define		TIM7EN		BIT(5)
#define		TIM6EN		BIT(4)
#define		TIM5EN		BIT(3)
#define		TIM4EN		BIT(2)
#define		TIM3EN		BIT(1)
#define		TIM2EN		BIT(0)
	reg BDCR;
#define		BDRST		BIT(16)
#define		RTCEN		BIT(15)
#define		R_RTCSEL(v)	R_BITS(v, 8, 9)
#define		W_RTCSEL(v, x)	W_BITS(v, x, 8, 9)
#define		RTCSEL(x)	BITS(x, 8, 9)
#define			RTCSEL_NONE	0
#define			RTCSEL_LSI	1
#define			RTCSEL_LSE	2
#define			RTCSEL_HSE	3
#define		LSEBYP		BIT(2)
#define		LSERDY		BIT(1)
#define		LSEON		BIT(0)
	reg CSR;
#define		LPWRRSTF	BIT(31)
#define		WWDGRSTF	BIT(30)
#define		IWDGRSTF	BIT(29)
#define		SFTRSTF		BIT(28)
#define		PORRSTF		BIT(27)
#define		PINRSTF		BIT(26)
#define		RWMF		BIT(24)
#define		LSIRDY		BIT(1)
#define		LSION		BIT(0)
	reg CFGR2;
#define		R_PREDIV1(v)	R_BITS(v, 0, 3)
#define		W_PREDIV1(v, x)	W_BITS(v, x, 0, 3)
#define		PREDIV1(x)	BITS(x, 0, 3)
};

CONFIG_AREA(regs_RCC, RCC, 0x40021000);

#endif
