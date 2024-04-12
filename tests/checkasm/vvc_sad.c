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
            AV_WN32A(buf0 + k, r);                          \
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

    //randomize_pixels(src0, src1, MAX_CTU_SIZE * MAX_CTU_SIZE * 2);

    //declare_func_emms(AV_CPU_FLAG_AVX2, void, uint16_t *_dst, ptrdiff_t dst_stride, const int width, const int height, const uint16_t *_lut);

    for(uint16_t i = 0; i < 16; ++i)
    {
        src0[i] = i;
    }

    //if(check_func(c.lmcs.filter[2], "ff_16bpc_test"))
    {
            int result0 = c.inter.sad(src0, src1, 2, 2, 16, 16);
            int result1 = c.inter.mysad(src0, src1, 2, 2, 16, 16);

            //call_new(src1 + i, MAX_CTU_SIZE*2, ctu_sizes[ctu_size_idx], random_height, lut0);
            //call_ref(src0 + i, MAX_CTU_SIZE*2, ctu_sizes[ctu_size_idx], random_height, lut0);
            if(result0 != result1)
            {
                fail();
            }
    }
    //report("check_vvc_sad_8x8_16bpc");
}

void checkasm_check_vvc_sad(void)
{
    check_vvc_sad_8x8_16bpc();
}