#include <stdint.h>
#include <picobit.h>
#include <dispatch.h>
#include <primitives.h>
#include <bignum.h>
#include <stm32f10x.h>
#include <stm32f10x_adc.h>
#include <stm32f10x_dma.h>

#define ADC1_DR_Address    ((uint32_t)0x4001244C)

volatile uint16_t ADCConvertedValue[16];

PRIMITIVE_UNSPEC(#%DMA_config, arch_DMA_config, 1)
{
  DMA_InitTypeDef DMA_InitStructure;
  uint16_t u16_bufSize;

  u16_bufSize = decode_int(arg1);
  
  DMA_DeInit(DMA1_Channel1);

  DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)&ADC1->DR;
  DMA_InitStructure.DMA_MemoryBaseAddr     = (uint32_t)ADCConvertedValue;
  DMA_InitStructure.DMA_DIR                = DMA_DIR_PeripheralSRC;
  DMA_InitStructure.DMA_BufferSize         = u16_bufSize;
  DMA_InitStructure.DMA_PeripheralInc      = DMA_PeripheralInc_Disable;
  DMA_InitStructure.DMA_MemoryInc          = DMA_MemoryInc_Enable;
  DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;
  DMA_InitStructure.DMA_MemoryDataSize     = DMA_MemoryDataSize_HalfWord;
  DMA_InitStructure.DMA_Mode               = DMA_Mode_Circular;
  DMA_InitStructure.DMA_Priority           = DMA_Priority_High;
  DMA_InitStructure.DMA_M2M                = DMA_M2M_Disable;
  DMA_Init(DMA1_Channel1, &DMA_InitStructure);
  DMA_Cmd(DMA1_Channel1, ENABLE);
}

PRIMITIVE_UNSPEC(#%ADC_config, arch_ADC_config, 4)
{
  ADC_InitTypeDef ADC_InitStructure;
 
  uint16_t u16_scanMode, u16_contMode, u16_dma, u16_nChannel;
  uint16_t u16_mode = ADC_Mode_Independent, u16_dataAlign = ADC_DataAlign_Right;
  uint32_t u32_externalTrig = ADC_ExternalTrigConv_None;
  
  u16_scanMode     = decode_int(arg1);
  u16_contMode     = decode_int(arg2);
  u16_dma          = decode_int(arg3);
  u16_nChannel     = decode_int(arg4);

  ADC_InitStructure.ADC_Mode               = u16_mode;
  ADC_InitStructure.ADC_ScanConvMode       = u16_scanMode;
  ADC_InitStructure.ADC_ContinuousConvMode = u16_contMode;
  ADC_InitStructure.ADC_ExternalTrigConv   = u32_externalTrig;
  ADC_InitStructure.ADC_DataAlign          = u16_dataAlign;
  ADC_InitStructure.ADC_NbrOfChannel       = u16_nChannel;
  ADC_Init(ADC1, &ADC_InitStructure);

  if(u16_dma == ENABLE){
    ADC_DMACmd(ADC1, ENABLE);
  }
 
  ADC_Cmd(ADC1, ENABLE);

  ADC_ResetCalibration(ADC1);

  while(ADC_GetResetCalibrationStatus(ADC1));
  ADC_StartCalibration(ADC1);

  while(ADC_GetCalibrationStatus(ADC1));

  ADC_SoftwareStartConvCmd(ADC1, ENABLE);
}

PRIMITIVE(ADC_configChannel, arch_ADC_configChannel, 3)
{
  uint16_t u16_channel, u16_sampleTime, u16_pos;

  u16_channel    = decode_int(arg1);
  u16_sampleTime = decode_int(arg2);
  u16_pos        = decode_int(arg3);
  
  ADC_RegularChannelConfig(ADC1, u16_channel, u16_pos, u16_sampleTime);
}  

PRIMITIVE(ADC_startConversion, arch_ADC_startConversion, 0)
{
  ADC_SoftwareStartConvCmd(ADC1, ENABLE);
}

PRIMITIVE(ADC_readValue-DMA, arch_ADC_readValue_DMA, 2)
{
  uint16_t u16_cont, u16_pos;
  
  u16_cont = decode_int(arg1);
  u16_pos  = decode_int(arg2);

  if(u16_cont == DISABLE){
    ADC_SoftwareStartConvCmd(ADC1, ENABLE);
  }

  arg1 = encode_int(ADCConvertedValue[u16_pos]);
}

PRIMITIVE(ADC_readValue, arch_ADC_readValue, 1)
{  
  uint16_t u16_cont;
  
  u16_cont = decode_int(arg1);
 
  if(u16_cont == DISABLE){
    ADC_SoftwareStartConvCmd(ADC1, ENABLE);
  }
  
  arg1 = encode_int(ADC_GetConversionValue(ADC1));
}

uint16_t read_value (uint8_t pos)
{
  return ADCConvertedValue[pos];
}
