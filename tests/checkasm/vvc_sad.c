#include <string.h>

#include "libavutil/intreadwrite.h"
#include "libavutil/mem_internal.h"

#include "libavcodec/avcodec.h"
#include "checkasm.h"
#include "libavcodec/vvc/vvc_ctu.h"

static const uint32_t pixel_mask[] = { 0xffffffff, 0x03ff03ff, 0x0fff0fff };

#define SIZEOF_PIXEL ((bit_depth + 7) / 8)

#define randomize_buffers(buf0, buf1, size, mask)           \
    do {                                                    \
        int k;                                              \
        for (k = 0; k < size; k += 2) {                     \
            uint32_t r = rnd() & mask;                      \
            AV_WN32A(buf0 + k,     r);                          \
            AV_WN32A(buf1 + k, r);                          \
        }                                                   \
    } while (0)

#define randomize_pixels(buf0, buf1, size)                  \
    do {                                                    \
        uint32_t mask = pixel_mask[(bit_depth - 8) >> 1];   \
        randomize_buffers(buf0, buf1, size, mask);          \
    } while (0)



static void check_vvc_sad_8x8_16bpc(void)
{
    const int bit_depth = 10;
    VVCDSPContext c;
    ff_vvc_dsp_init(&c, bit_depth);

    LOCAL_ALIGNED_32(uint16_t, src0, [MAX_CTU_SIZE * MAX_CTU_SIZE * 2]);
    LOCAL_ALIGNED_32(uint16_t, src1, [MAX_CTU_SIZE * MAX_CTU_SIZE * 2]);

    memset(src0, 0, MAX_CTU_SIZE * MAX_CTU_SIZE * 2);
    memset(src1, 0, MAX_CTU_SIZE * MAX_CTU_SIZE * 2);

    randomize_pixels(src0, src1, MAX_CTU_SIZE * MAX_CTU_SIZE * 2);

    declare_func_emms(AV_CPU_FLAG_AVX2,int, const int16_t *src0, const int16_t *src1, int dx, int dy, int block_w, int block_h);

    // for(uint16_t i = 0; i < 16; ++i)
    // {
    //     *(src0) = i;
    // }
    //memset(src0, 0, MAX_CTU_SIZE * MAX_CTU_SIZE * 2);
    
    for(uint16_t i = 0; i < 128 * 128; ++i)
    {
        src0[i] = 1;
    }

    if(check_func(c.inter.sad[1], "vvc_sad_8x8_16bpc"))
    {
            //int result0 = c.inter.sad(src0 + 128 * 2, src1 + 128 * 2, 2, 2, 8, 8);
            //int result1 = c.inter.mysad(src0 + 128 * 2, src1 + 128 * 2, 2, 2, 8, 8);

            int result0 = call_ref(src0 + 2 * 128, src1 + 2 * 128, 2, 2, 8, 8);
            int result1 = call_new(src0 + 2 * 128, src1 + 2 * 128, 2, 2, 8, 8);

            if(result1 != result0)
            {
                fail();
            }
            bench_new(src0 + 128 * 2, src1 + 128 * 2, 2, 2, 8, 8);
    }
    report("check_vvc_sad_8x8_16bpc");
}

static void check_vvc_sad_16x16_16bpc(void)
{
    const int bit_depth = 10;
    VVCDSPContext c;
    ff_vvc_dsp_init(&c, bit_depth);

    LOCAL_ALIGNED_32(uint16_t, src0, [MAX_CTU_SIZE * MAX_CTU_SIZE * 2]);
    LOCAL_ALIGNED_32(uint16_t, src1, [MAX_CTU_SIZE * MAX_CTU_SIZE * 2]);

    memset(src0, 0, MAX_CTU_SIZE * MAX_CTU_SIZE * 2);
    memset(src1, 0, MAX_CTU_SIZE * MAX_CTU_SIZE * 2);

    randomize_pixels(src0, src1, MAX_CTU_SIZE * MAX_CTU_SIZE * 2);

    declare_func_emms(AV_CPU_FLAG_AVX2,int, const int16_t *src0, const int16_t *src1, int dx, int dy, int block_w, int block_h);

    // for(uint16_t i = 0; i < 16; ++i)
    // {
    //     *(src0) = i;
    // }
    //memset(src0, 0, MAX_CTU_SIZE * MAX_CTU_SIZE * 2);
    
    for(uint16_t i = 0; i < 128 * 128; ++i)
    {
        src0[i] = 1;
    }

    if(check_func(c.inter.sad[2], "ff_16bpc_test"))
    {
            //int result0 = c.inter.sad(src0 + 128 * 2, src1 + 128 * 2, 2, 2, 16, 16);
            //int result1 = c.inter.mysad16(src0 + 128 * 2, src1 + 128 * 2, 2, 2, 16, 16);

            int result0 = call_ref(src0 + 2 * 128, src1 + 2 * 128, 2, 2, 16, 16);
            int result1 = call_new(src0 + 2 * 128, src1 + 2 * 128, 2, 2, 16, 16);

            if(result1 != result0)
            {
                fail();
            }
            bench_new(src0 + 128 * 2, src1 + 128 * 2, 2, 2, 16, 16);
    }
    report("check_vvc_sad_16x16_16bpc");
}

void checkasm_check_vvc_sad(void)
{
    check_vvc_sad_16x16_16bpc();
    check_vvc_sad_8x8_16bpc();
}