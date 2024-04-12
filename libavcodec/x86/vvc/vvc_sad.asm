%include "libavutil/x86/x86util.asm"


SECTION .text

INIT_YMM avx2
cglobal vvc_sad_8x8_16bpc, 6, 9, 13, src1, src2, dx, dy, block_w, block_h, off1, off2, sad
    ; calculate initial offsets...gross
    sub               dxq, 2
    sub               dyq, 2

    mov             off1q, 2
    mov             off2q, 2

    add             off1q, dyq   
    sub             off2q, dyq

    imul            off1q, 128
    imul            off2q, 128

    add             off1q, 2
    add             off2q, 2
    
    add             off1q, dxq
    sub             off2q, dxq

    lea             src1q, [src1q + off1q * 2]
    lea             src2q, [src2q + off2q * 2]
    xor              eax, eax
    
.load_8pixels:
        movu              xm0, [src1q]
        movu              xm1, [src2q]
        vpminuw           xm2, xm0, xm1
        vpmaxuw           xm1, xm0, xm1
        vpsubusw          xm1, xm1, xm2

        vpmovzxwd          m1, xm1
        
        ; so this is the final step
        vextracti128      xm0, m1 , q0001 ;        3        2      1          0
        vpaddd            xm0, xm1        ; xm0 (7 + 3) (6 + 2) (5 + 1)   (4 + 0)
        vpshufd           xm1, xm0, q0032 ; xm1    -      -     (7 + 3)   (6 + 2)
        vpaddd            xm0, xm0, xm1   ; xm0    _      _     (5 1 7 3) (4 0 6 2)
        vpshufd           xm1, xm0, q0001 ; xm1    _      _     (5 1 7 3) (5 1 7 3)
        vpaddd            xm0, xm0, xm1   ;                               (01234567)

        movd             sadd, xm0
        add               eax, sadd

        add             src1q, 4 * 128
        add             src2q, 4 * 128

        sub          block_hd, 2
        jg  .load_8pixel

    

    RET
