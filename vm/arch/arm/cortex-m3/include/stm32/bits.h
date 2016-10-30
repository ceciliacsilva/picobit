#ifndef _BITS_H_
#define _BITS_H_

typedef volatile unsigned int   reg;
typedef volatile unsigned short reg16;

typedef unsigned int   u32;
typedef unsigned short u16;
typedef unsigned char  u8;

typedef signed int   s32;
typedef signed short s16;
typedef signed char  s8;

#define CONFIG_AREA(type, name, addr) \
			static struct type* const name = (struct type*) addr;

#define BIT(n)		(1 << (n))
#define MASK(nbits)	((1 << (nbits)) - 1)

#define BITS(x, s, e) (((x) & MASK(e - s + 1)) << s)
#define R_BITS(v, s, e) (((v) & MASK(e - s + 1)) >> s)
#define W_BITS(v, x, s, e) (v) = ((v) & ~(MASK(e - s + 1) << s)) | BITS(x, s, e)

// Acrescentado em 26/06/2012
#define  RCC_APB2ENR_IOPAEN                  ((u16)0x0004)            /*!< I/O port A clock enable */
#define  RCC_APB2ENR_IOPBEN                  ((u16)0x0008)            /*!< I/O port B clock enable */
#define  RCC_APB2ENR_IOPCEN                  ((u16)0x0010)            /*!< I/O port C clock enable */

#endif
