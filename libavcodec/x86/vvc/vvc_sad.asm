%include "libavutil/x86/x86util.asm"

%if ARCH_X86_64
%if HAVE_AVX2_EXTERNAL

INIT_YMM avx2
cglobal vvc_sad_8x8_16bpc, 4, 4, 1, src1, stride1, src2, stride2
    pxor                 m0, m0
    movu                 m0, [src1q]

    RET

%endif
%endif