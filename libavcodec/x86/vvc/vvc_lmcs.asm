%include "libavutil/x86/x86util.asm"

SECTION_RODATA 32

pb_shufmask_16bpc:   db 0, 1, 4, 5, 8, 9, 12, 13, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 4, 5, 8, 9, 12, 13, -1, -1, -1, -1, -1, -1, -1, -1
pb_shufmask_8bpc:  db 0, 4, 8, 12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 4, 8, 12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1

%if ARCH_X86_64
%if HAVE_AVX2_EXTERNAL

SECTION .text

%macro PACK_ZEROS_16 4
    punpcklwd %3, %2, %1
    punpckhwd %4, %2, %1
%endmacro

%macro GATHER_16 3
  vpcmpeqd         %1, %1, %1
  vpgatherdd       %3, [lutq + %2 * 2], %1
%endmacro

%macro PACK_GATHER_UNPACK_16 0
    PACK_ZEROS_16    m0, m1, m2, m3
    GATHER_16        m6, m2, m4
    GATHER_16        m6, m3, m5
    

    PACK_ZEROS_16    m0, m7, m8, m9
    GATHER_16        m0, m8, m10
    GATHER_16        m0, m9, m11

    ; shuf off garbage data
    pshufb           m4, m12
    pshufb           m5, m12
    pshufb          m10, m12
    pshufb          m11, m12
    
    ; repack data
    punpcklqdq       m4, m5
    punpcklqdq      m10, m11
%endmacro

; Only handles widths 16 to 128
INIT_YMM avx2

cglobal lmcs_16bpc, 5, 8, 13, src, src_stride, width, height, lut, src_pt, cnt, src2
    pxor             m0, m0
    movu            m12, [pb_shufmask_16bpc]

    .loopheight:
        mov          src_ptq, srcq
        lea            src2q, [srcq + src_strideq]

        prefetcht0     [src2q + src_strideq]
        prefetcht0     [src2q + src_strideq * 2]

        mov             cntd, widthd
        shr             cntd, mmsize / 8 ; width loop counter

        .loopwidth:
            prefetcht0      [srcq + 128]
            prefetcht0      [src2q + 128]

            ; first row
            movu             m1, [srcq]
            movu             m7, [src2q]

            PACK_GATHER_UNPACK

            ; write out
            movu         [srcq], m4
            movu        [src2q], m10

            ; load next and increment if necessary
            lea            srcq, [srcq + mmsize]
            lea           src2q, [src2q + mmsize]
            dec cntd
            jg .loopwidth

        lea             srcq, [src_ptq + src_strideq * 2]
        sub          heightq, 2
        jg .loopheight

    RET


INIT_YMM avx2
cglobal lmcs_16bpc_8pix, 5, 7, 13, src, src_stride, width, height, lut, src_pt, src2
    pxor             m0, m0
    movu            m12, [pb_shufmask_16bpc]

    .loopheight:
        mov           src_ptq, srcq ; saveptr
        lea             src2q, [srcq + src_strideq]

        .loopwidth:
            ; rows 1 & 2
            movu                 xm1, [srcq]
            movu                 xm2, [src2q]
            vperm2i128            m1, m1, m2, q0200 ; pack two row
            ; rows 3 & 4    
            movu                 xm7, [src2q + src_strideq]
            movu                 xm8, [src2q + src_strideq * 2]
            vperm2i128            m7, m7, m8, q0200

            PACK_GATHER_UNPACK

            ; write out
            movu               [srcq], xm4
            vperm2i128             m4, m4, m4, q1001
            movu              [src2q], xm4

            movu            [src2q + src_strideq], xm10
            vperm2i128                        m10, m10, m10, q1001
            movu        [src2q + src_strideq * 2], xm10

        lea                srcq, [src_ptq + src_strideq * 4]
        sub             heightq, 4
        jg .loopheight

    RET


INIT_YMM avx2
cglobal lmcs_8bpc, 5, 7, 14, src, src_stride, width, height, lut, src_pt, cnt
    pxor             m0,  m0
    movu             m12, [pb_shufmask_8bpc]

    .loopheight:
        mov          src_ptq, srcq    ; saveptr
        mov             cntd, widthd
        shr             cntd, 5
        .loopwidth:

            ; extra layer of unpacking
            movu            m13, [srcq]
            punpcklbw        m1, m13, m0
            punpckhbw        m7, m13, m0

            ; first row
            punpcklwd        m2, m1, m0
            punpckhwd        m3, m1, m0

            vpcmpeqd         m6, m6, m6
            vpgatherdd       m4, [lutq + m2], m6
            vpcmpeqd         m6, m6, m6
            vpgatherdd       m5, [lutq + m3], m6
            
            ; second row
            punpcklwd        m8, m7, m0 
            punpckhwd        m9, m7, m0

            vpcmpeqd         m0, m0, m0
            vpgatherdd       m10, [lutq + m8], m0
            vpcmpeqd         m0, m0, m0
            vpgatherdd       m11, [lutq + m9], m0

            ; shuf off garbage data
            pshufb          m4,  m12
            pshufb          m5,  m12
            pshufb          m10, m12
            pshufb          m11, m12

            ; repack data
            punpckldq        m4, m5
            punpckldq       m10, m11
 
            punpcklqdq       m5, m4, m10
 
            ; write out
            movu         [srcq], m5

            ; load next and increment if necessary
            lea            srcq, [srcq + mmsize]
            dec            cntd
            jg .loopwidth

        lea                srcq, [src_ptq + src_strideq]
        dec             heightq
        jg .loopheight
    RET

INIT_YMM avx2
cglobal lmcs_8bpc_16pix, 5, 5, 14, src, src_stride, width, height, lut
    pxor             m0, m0 ; 0 register
    movu             m12, [pb_shufmask_8bpc]

    .loopheight:
        .loopwidth:

            ; extra layer of unpacking
            movu             xm13, [srcq]
            movu             xm1, [srcq + src_strideq]
            vperm2i128       m13, m13, m1, q0200
            punpcklbw        m1, m13, m0
            punpckhbw        m7, m13, m0

            ; first row
            punpcklwd        m2, m1, m0
            punpckhwd        m3, m1, m0

            vpcmpeqd         m6, m6, m6
            vpgatherdd       m4, [lutq + m2], m6
            vpcmpeqd         m6, m6, m6
            vpgatherdd       m5, [lutq + m3], m6
            
            ; second row
            ; movu             m7, [src2q]
            punpcklwd        m8, m7, m0 
            punpckhwd        m9, m7, m0

            vpcmpeqd         m0, m0, m0
            vpgatherdd       m10, [lutq + m8], m0
            vpcmpeqd         m0, m0, m0
            vpgatherdd       m11, [lutq + m9], m0

            ; shuf off garbage data
            pshufb          m4,  m12
            pshufb          m5,  m12
            pshufb          m10, m12
            pshufb          m11, m12

            ; repack data
            punpckldq       m4, m5
            punpckldq      m10, m11

            punpcklqdq      m5, m4, m10

            ; write out
            movu                     [srcq], xm5
            vperm2i128                   m5, m5, m5, q1001
            movu       [srcq + src_strideq], xm5

        lea             srcq, [srcq + src_strideq * 2]
        sub          heightq, 2
        jg .loopheight

    RET

%endif
%endif
