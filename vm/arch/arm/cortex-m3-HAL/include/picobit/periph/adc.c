#include <stdint.h>
#include <picobit.h>
#include <dispatch.h>
#include <primitives.h>
#include <bignum.h>
#include <stm32f10x.h>
#include <stm32f10x_adc.h>
#include <stm32f10x_dma.h>

#define ADC1_DR_Address    ((uint32_t)0x4001244C)

__IO uint16_t ADCConvertedValue;
//ADC - basics config
uint32_t u32_mode = ADC_Mode_Independent, u32_dataAlign = ADC_DataAlign_Right;
uint32_t u32_scanMode, u32_contMode, u32_externalTrig, u32_nChannel;

PRIMITIVE_UNSPEC(ADC1_clock, arch_ADC1_clock, 0)
{
  RCC_APB2PeriphClockCmd(RCC_APB2Periph_ADC1, ENABLE);
}

PRIMITIVE_UNSPEC(DMA1_clock, arch_DMA1_clock, 0)
{
  RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, ENABLE);
}

PRIMITIVE_UNSPEC(#%ADC_config, arch_ADC_config, 4)
{
  ADC_InitTypeDef ADC_InitStructure;
  DMA_InitTypeDef DMA_InitStructure;
  //uint32_t u32_scanMode, u32_contMode, u32_externalTrig, u32_nChannel;
    
  u32_scanMode     = decode_int(arg1);
  u32_contMode     = decode_int(arg2);
  u32_externalTrig = decode_int(arg3);
  u32_nChannel     = decode_int(arg4);

  u32_scanMode = u32_scanMode;
  u32_contMode = u32_contMode;
  u32_externalTrig = arithmetic_shift(u32_externalTrig, 3);
  u32_nChannel = u32_nChannel;
  
  DMA_DeInit(DMA1_Channel1);
  DMA_InitStructure.DMA_PeripheralBaseAddr = ADC1_DR_Address;
  DMA_InitStructure.DMA_MemoryBaseAddr = (uint32_t)&ADCConvertedValue;
  DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralSRC;
  DMA_InitStructure.DMA_BufferSize = 1;
  DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;
  DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Disable;
  DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;
  DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;
  DMA_InitStructure.DMA_Mode = DMA_Mode_Circular;
  DMA_InitStructure.DMA_Priority = DMA_Priority_High;
  DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;
  DMA_Init(DMA1_Channel1, &DMA_InitStructure);
  DMA_Cmd(DMA1_Channel1, ENABLE);

  ADC_InitStructure.ADC_Mode               = u32_mode;
  ADC_InitStructure.ADC_ScanConvMode       = u32_scanMode;
  ADC_InitStructure.ADC_ContinuousConvMode = u32_contMode;
  ADC_InitStructure.ADC_ExternalTrigConv   = ADC_ExternalTrigConv_None;//u32_externalTrig;
  ADC_InitStructure.ADC_DataAlign          = u32_dataAlign;
  ADC_InitStructure.ADC_NbrOfChannel       = u32_nChannel;
  ADC_Init(ADC1, &ADC_InitStructure);

  ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 1, ADC_SampleTime_55Cycles5);

  ADC_DMACmd(ADC1, ENABLE);
  ADC_Cmd(ADC1, ENABLE);

  ADC_ResetCalibration(ADC1);

  while(ADC_GetResetCalibrationStatus(ADC1));
  ADC_StartCalibration(ADC1);

  while(ADC_GetCalibrationStatus(ADC1));

  ADC_SoftwareStartConvCmd(ADC1, ENABLE);

}

void GPIO_Configuration(void)
{
  GPIO_InitTypeDef GPIO_InitStructure;
  
  RCC_APB2PeriphClockCmd(RCC_APB2Periph_GPIOC, ENABLE);
  /* Configure PC.04 (ADC Channel14) as analog input -------------------------*/
  GPIO_InitStructure.GPIO_Pin = GPIO_Pin_4;
  GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AIN;
  GPIO_Init(GPIOC, &GPIO_InitStructure);
  /*
  GPIO_InitStructure.GPIO_Pin = GPIO_Pin_9;
  GPIO_InitStructure.GPIO_Mode = GPIO_Mode_Out_PP;
  GPIO_InitStructure.GPIO_Speed = GPIO_Speed_50MHz;
  GPIO_Init(GPIOC, &GPIO_InitStructure);
  */
}

PRIMITIVE(ADC_example, arch_ADC_example, 0)
{
  ADC_InitTypeDef ADC_InitStructure;
  DMA_InitTypeDef DMA_InitStructure;

  RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, ENABLE);
  RCC_APB2PeriphClockCmd(RCC_APB2Periph_ADC1, ENABLE);

  DMA_DeInit(DMA1_Channel1);
  DMA_InitStructure.DMA_PeripheralBaseAddr = ADC1_DR_Address;
  DMA_InitStructure.DMA_MemoryBaseAddr = (uint32_t)&ADCConvertedValue;
  DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralSRC;
  DMA_InitStructure.DMA_BufferSize = 1;
  DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;
  DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Disable;
  DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;
  DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;
  DMA_InitStructure.DMA_Mode = DMA_Mode_Circular;
  DMA_InitStructure.DMA_Priority = DMA_Priority_High;
  DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;
  DMA_Init(DMA1_Channel1, &DMA_InitStructure);
  DMA_Cmd(DMA1_Channel1, ENABLE);
  
  ADC_InitStructure.ADC_Mode = ADC_Mode_Independent;
  ADC_InitStructure.ADC_ScanConvMode = ENABLE;
  ADC_InitStructure.ADC_ContinuousConvMode = ENABLE;
  ADC_InitStructure.ADC_ExternalTrigConv = ADC_ExternalTrigConv_None;
  ADC_InitStructure.ADC_DataAlign = ADC_DataAlign_Right;
  ADC_InitStructure.ADC_NbrOfChannel = 1;
  ADC_Init(ADC1, &ADC_InitStructure);
  
  ADC_RegularChannelConfig(ADC1, ADC_Channel_14, 1, ADC_SampleTime_55Cycles5);

  ADC_DMACmd(ADC1, ENABLE);
  ADC_Cmd(ADC1, ENABLE);

  ADC_ResetCalibration(ADC1);

  while(ADC_GetResetCalibrationStatus(ADC1));
  ADC_StartCalibration(ADC1);

  while(ADC_GetCalibrationStatus(ADC1));

  ADC_SoftwareStartConvCmd(ADC1, ENABLE);

  //GPIO_Configuration();

  //GPIOC->ODR = GPIO_Pin_9;
}


PRIMITIVE(ADC_read, arch_ADC_read, 0)
{
  arg1 = encode_int(ADCConvertedValue);
}

