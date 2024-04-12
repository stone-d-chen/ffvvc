%include "libavutil/x86/x86util.asm"


SECTION .text

INIT_YMM avx2
cglobal vvc_sad_8x8_16bpc, 6, 8, 13, src1, src2, dx, dy, block_w, block_h, off1, off2

    sub               dxd, 2
    sub               dyd, 2
    mov             off1d, 2
    mov             off2d, 2

    add             off1d, dyd   
    sub             off2d, dyd

    imul             off1d, 128
    imul             off2d, 128

    add             off1d, 2
    add             off2d, 2
    
    add             off1d, dxd
    sub             off2d, dxd

    lea             src1q, [src1q + off1q]
    lea             src2q, [src2q + off2q]
    
.load_8pixel:
    movu               xm0, [src1q]
    movu               xm1, [src2q]
    vpminuw            xm2, xm0, xm1
    vpmaxuw            xm1, xm0, xm1
    vpsubusw           xm1, xm1, xm2

    vpmovzxwd           m1, xm1
    vextracti128       xm0, m1 , q0001 ;        3        2      1          0
    vpaddd             xm0, xm1        ; xm0 (7 + 3) (6 + 2) (5 + 1)   (4 + 0)
    vpshufd            xm1, xm0, q0032 ; xm1    -      -     (7 + 3)   (6 + 2)
    vpaddd             xm0, xm0, xm1   ; xm0    _      _     (5 1 7 3) (4 0 6 2)
    vpshufd            xm1, xm0, q0001 ; xm1    _      _     (5 1 7 3) (5 1 7 3)
    vpaddd             xm0, xm0, xm1   ;                               (01234567)

    movd               eax, xm0

    RET
