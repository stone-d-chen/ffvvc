%include "libavutil/x86/x86util.asm"


SECTION .text

%macro MIN_MAX_SAD 3 ; bpc, op, count
    vpminuw           %1, %2, %3
    vpmaxuw           %3, %2, %3
    vpsubusw          %3, %3, %1
%endmacro


INIT_YMM avx2
cglobal vvc_sad_8x8_16bpc, 6, 8, 13, src1, src2, dx, dy, block_w, block_h, off1, off2
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
.load_8pixels:
        movu              xm0, [src1q]
        movu              xm1, [src2q]
        vpminuw           xm2, xm0, xm1
        vpmaxuw           xm1, xm0, xm1
        vpsubusw          xm1, xm1, xm2

        vpmovzxwd          m1, xm1

        ; MIN_MAX_SAD m2, m0, m1

        movu              xm5, [src1q + 128 * 2 * 2]
        movu              xm6, [src2q + 128 * 2 * 2]
        vpminuw           xm7, xm5, xm6
        vpmaxuw           xm6, xm5, xm6
        vpsubusw          xm6, xm6, xm7

        vpmovzxwd          m6, xm6
        vpaddd             m1, m6

        movu              xm8, [src1q + 128 * 4 * 2]
        movu              xm9, [src2q + 128 * 4 * 2]
        vpminuw           xm10, xm8, xm9
        vpmaxuw           xm9, xm8, xm9
        vpsubusw          xm9, xm9, xm10

        vpmovzxwd          m9, xm9
        vpaddd             m1, m9

        movu              xm11, [src1q + 128 * 6 * 2]
        movu              xm12, [src2q + 128 * 6 * 2]
        vpminuw           xm13, xm11, xm12
        vpmaxuw           xm12, xm11, xm12
        vpsubusw          xm12, xm12, xm13

        vpmovzxwd          m12, xm12

        vpaddd              m1, m12


        ; so this is the final step
        vextracti128      xm0, m1 , q0001 ;        3        2      1          0
        vpaddd            xm0, xm1        ; xm0 (7 + 3) (6 + 2) (5 + 1)   (4 + 0)
        vpshufd           xm1, xm0, q0032 ; xm1    -      -     (7 + 3)   (6 + 2)
        vpaddd            xm0, xm0, xm1   ; xm0    _      _     (5 1 7 3) (4 0 6 2)
        vpshufd           xm1, xm0, q0001 ; xm1    _      _     (5 1 7 3) (5 1 7 3)
        vpaddd            xm0, xm0, xm1   ;                               (01234567)

        movd              eax, xm0
        ;movd             sadd, xm0
        ;add               eax, sadd

        ;add             src1q, 2 * 128 * 2 
        ;add             src2q, 2 * 128 * 2

        ;sub          block_hd, 6
        ;jg  .load_8pixels

    RET


cglobal vvc_sad_16x16_16bpc, 6, 9, 13, src1, src2, dx, dy, block_w, block_h, off1, off2, sad
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
    pxor               m8, m8
.load_pixels:
        movu              xm0, [src1q]
        movu              xm1, [src2q]
        vpminuw           xm2, xm0, xm1
        vpmaxuw           xm1, xm0, xm1
        vpsubusw          xm1, xm1, xm2

        vpmovzxwd          m1, xm1
        vpaddd             m8, m1

        movu              xm5, [src1q + 16]
        movu              xm6, [src2q + 16]
        vpminuw           xm7, xm5, xm6
        vpmaxuw           xm6, xm5, xm6
        vpsubusw          xm6, xm6, xm7

        vpmovzxwd          m6, xm6
        vpaddd             m8, m6


        add             src1q, (2 * 128) * 2 
        add             src2q, (2 * 128) * 2

        sub  block_hd, 2
        jg  .load_pixels

        ; so this is the final step
        vextracti128      xm0, m8 , q0001 ;        3        2      1          0
        vpaddd            xm0, xm8        ; xm0 (7 + 3) (6 + 2) (5 + 1)   (4 + 0)
        vpshufd           xm8, xm0, q0032 ; xm1    -      -     (7 + 3)   (6 + 2)
        vpaddd            xm0, xm0, xm8   ; xm0    _      _     (5 1 7 3) (4 0 6 2)
        vpshufd           xm8, xm0, q0001 ; xm1    _      _     (5 1 7 3) (5 1 7 3)
        vpaddd            xm0, xm0, xm8   ;                               (01234567)

        movd              eax, xm0

    RET
