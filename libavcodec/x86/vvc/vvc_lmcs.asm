%include "libavutil/x86/x86util.asm"

SECTION_RODATA 32

pb_shufmask:   db 0, 1, 4, 5, 8, 9, 12, 13, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 4, 5, 8, 9, 12, 13, -1, -1, -1, -1, -1, -1, -1, -1

SECTION .text

; Only handles widths 16 to 128
INIT_YMM avx2

cglobal lmcs_16bpc, 5, 8, 12, src, src_stride, width, height, lut, src_pt, cnt, src2
    pxor             m0, m0 ; 0 register
    pxor             m1, m1 ; low
    pxor             m2, m2 ; hi
    pxor             m3, m3
    pxor             m4, m4
    pxor             m5, m5
    pxor             m6, m6
    pxor             m7, m7
    pxor             m8, m8
    pxor             m9, m9
    pxor           m10, m10
    pxor           m11, m11

    .loopheight:
        mov             src_ptq, srcq ; saveptr
        lea             src2q, [srcq + src_strideq]

        prefetcht0       [src2q + src_strideq]
        prefetcht0       [src2q + src_strideq * 2]

        mov             cntd, widthd
        shr             cntd, mmsize / 8 ; width loop counter

        .loopwidth:
            prefetcht0       [srcq  + 256]  ; arbitrary prefetch location
            prefetcht0       [src2q + 256]

            ; first row
            movu             m1, [srcq]
            punpcklwd        m2, m1, m0
            punpckhwd        m3, m1, m0

            vpcmpeqd         m6, m6, m6
            vpgatherdd       m4, [lutq + m2 * 2], m6
            vpcmpeqd         m6, m6, m6              
            vpgatherdd       m5, [lutq + m3 * 2], m6
            ; m4, m5 contain final data
            
            ; second row
            movu             m7, [src2q]
            punpcklwd        m8, m7, m0 
            punpckhwd        m9, m7, m0

            vpcmpeqd         m0, m0, m0
            vpgatherdd       m10, [lutq + m8 * 2], m0
            vpcmpeqd         m0, m0, m0
            vpgatherdd       m11, [lutq + m9 * 2], m0

            ; shuf off garbage data
            pshufb          m4,  [pb_shufmask]
            pshufb          m5,  [pb_shufmask]
            pshufb          m10, [pb_shufmask]
            pshufb          m11, [pb_shufmask]

            ; repack data
            punpcklqdq       m4, m5
            punpcklqdq      m10, m11

            ; write out
            movu             [srcq], m4
            movu             [src2q], m10

            ; load next and increment if necessary
            lea              srcq, [srcq + mmsize]
            lea              src2q, [src2q + mmsize]
            dec cntd
            jg .loopwidth

        lea             srcq, [src_ptq + src_strideq * 2]
        sub             heightq, 2
        jg .loopheight

    RET

SECTION .text
INIT_YMM avx2


; XMM register to fit 8 pixel * 16 bit = 128bit with no code change
INIT_XMM avx2
cglobal lmcs_16bpc_8pix, 5, 8, 12, src, src_stride, width, height, lut, src_pt, cnt, src2
    pxor             m0, m0 ; 0 register
    pxor             m1, m1 ; low
    pxor             m2, m2 ; hi
    pxor             m3, m3
    pxor             m4, m4
    pxor             m5, m5
    pxor             m6, m6
    pxor             m7, m7
    pxor             m8, m8
    pxor             m9, m9
    pxor           m10, m10
    pxor           m11, m11

    .loopheight:
        mov             src_ptq, srcq ; saveptr
        lea             src2q, [srcq + src_strideq]

        prefetcht0       [src2q + src_strideq]
        prefetcht0       [src2q + src_strideq * 2]

        mov             cntd, 1 ; width loop counter

        .loopwidth:
            prefetcht0       [srcq  + 256]  ; arbitrary prefetch location
            prefetcht0       [src2q + 256]

            ; first row
            movu             m1, [srcq]
            punpcklwd        m2, m1, m0
            punpckhwd        m3, m1, m0

            vpcmpeqd         m6, m6, m6
            vpgatherdd       m4, [lutq + m2 * 2], m6
            vpcmpeqd         m6, m6, m6              
            vpgatherdd       m5, [lutq + m3 * 2], m6
            ; m4, m5 contain final data
            
            ; second row
            movu             m7, [src2q]
            punpcklwd        m8, m7, m0 
            punpckhwd        m9, m7, m0

            vpcmpeqd         m0, m0, m0
            vpgatherdd       m10, [lutq + m8 * 2], m0
            vpcmpeqd         m0, m0, m0
            vpgatherdd       m11, [lutq + m9 * 2], m0

            ; shuf off garbage data
            pshufb          m4,  [pb_shufmask]
            pshufb          m5,  [pb_shufmask]
            pshufb          m10, [pb_shufmask]
            pshufb          m11, [pb_shufmask]

            ; repack data
            punpcklqdq       m4, m5
            punpcklqdq      m10, m11

            ; write out
            movu             [srcq], m4
            movu             [src2q], m10

            ; load next and increment if necessary
            lea              srcq, [srcq + mmsize]
            lea              src2q, [src2q + mmsize]
            dec cntd
            jg .loopwidth

        lea             srcq, [src_ptq + src_strideq * 2]
        sub             heightq, 2
        jg .loopheight

    RET
