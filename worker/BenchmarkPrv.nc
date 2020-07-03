/*
 * whip6: Warsaw High-performance IPv6.
 *
 * Copyright (c) 2012-2017 Przemyslaw Horban
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE     
 * files.
 */

#include <inc/hw_vims.h>
#include <driverlib/cpu.h>
#include <driverlib/vims.h>

#include "debug.h"

extern uint32_t benchmarkCode();
extern void     dumpMem();

#define MAX_SIZE 100
uint32_t volatile safeRam[1000];
uint32_t volatile safeGpram[1000] CC26XX_PLACE_IN_GPRAM;
uint32_t volatile times[MAX_SIZE] = {0}, results[MAX_SIZE] = {0};

#ifndef BENCHMARK_ID
#define BENCHMARK_ID ""
#endif

#ifndef BENCHMARK_WRITE_BUFFER_DIS
#define BENCHMARK_WRITE_BUFFER_DIS 0
#endif

#ifndef BENCHMARK_VIMS_LB_DIS
#define BENCHMARK_VIMS_LB_DIS 0
#endif

module BenchmarkPrv {
    uses interface Boot;
}

implementation {

    void prepare() {
        int i = 0;

        if(BENCHMARK_VIMS_LB_DIS) {
            VIMSLineBufDisable(VIMS_BASE);
        }
        if(BENCHMARK_WRITE_BUFFER_DIS) {
            CPU_WriteBufferDisable();
        }
        for(i = 0; i < 4096; i++);

        ITM->LAR = 0xC5ACCE55;                           // Instruction Trace Macrocell, unlock register access
        CoreDebug->DEMCR |= CoreDebug_DEMCR_TRCENA_Msk;  // Debug Exception and Monitor Control, enable trace cell

        /* Configure CYCCNT */
        DWT->CYCCNT = 0;                                 // Reset counter
        DWT->CTRL |= DWT_CTRL_CYCCNTENA_Msk;             // Enable counter

        /* Configure CPICNT */
        DWT->CPICNT &= 0xfff0;                           // Reset counter (high 24 bits are reserved)
        DWT->CTRL |= DWT_CTRL_CPIEVTENA_Msk;             // Enable counter

        /* Configure LSUCNT */
        DWT->LSUCNT &= 0xfff0;                           // Reset counter (high 24 bits are reserved)
        DWT->CTRL |= DWT_CTRL_LSUEVTENA_Msk;             // Enable counter

        /* Configure FOLDCNT */
        DWT->FOLDCNT &= 0xfff0;                          // Reset counter (high 24 bits are reserved)
        DWT->CTRL |= DWT_CTRL_FOLDEVTENA_Msk;            // Enable counter

        /* Configure SLEEPCNT */
        DWT->SLEEPCNT &= 0xfff0;                         // Reset counter (high 24 bits are reserved)
        DWT->CTRL |= DWT_CTRL_SLEEPEVTENA_Msk;           // Enable counter

        /* Configure EXCCNT */
        DWT->EXCCNT &= 0xfff0;                           // Reset counter (high 24 bits are reserved)
        DWT->CTRL |= DWT_CTRL_EXCEVTENA_Msk;             // Enable counter
    }

    void printUint32Array(uint32_t volatile *arr, uint16_t len) {
        int i = 0;

        if (len == 0) {
            printf("[]\n");
            return;
        }

        printf("[");
        while (i < len - 1) {
            printf("%" PRIu32 ", ", arr[i]);
            i++;
        }
        printf("%" PRIu32 "]\n", arr[i]);
    }

    void benchmark() {
        uint32_t t = 0;
        uint16_t savedTimes = 0, savedResults = 0;

        // In order to prevent linker and compiler from removing this variables.
        // As suggested in: https://answers.launchpad.net/gcc-arm-embedded/+question/280104
        __asm__("" : : "" (safeRam));
        __asm__("" : : "" (safeGpram));
        __asm__("" : : "" (times));
        __asm__("" : : "" (results));

        t = benchmarkCode();

        savedTimes = (uint16_t)((t & 0xffff0000) >> 16);
        savedResults = (uint16_t)(t & 0xffff);
        
        printf("times: ");
        printUint32Array(times, savedTimes);
        printf("results: ");
        printUint32Array(results, savedResults);
    }

    event void Boot.booted() {
        printf("Benchmark ID: " BENCHMARK_ID "\n");
        prepare();
        benchmark();
        dumpMem();

        for (;;);
    }
}
