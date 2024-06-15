; from hevc_deblock.asm, grap all the tranpose macros

%include "libavutil/x86/x86util.asm"

SECTION_RODATA

cextern pw_1023
%define pw_pixel_max_10 pw_1023
pw_pixel_max_12: times 8 dw ((1 << 12)-1)
pw_2 :           times 8 dw  2
pw_m2:           times 8 dw -2
pd_1 :           times 4 dd  1
pd_5 :           times 4 dd  5

cextern pw_4
cextern pw_8
cextern pw_m1

SECTION .text
INIT_XMM sse2
; INIT_YMM avx2


; in: 8 rows of 4 bytes in %4..%11
; out: 4 rows of 8 words in m0..m3
%macro TRANSPOSE4x8B_LOAD 8
    movd             m0, %1
    movd             m2, %2
    movd             m1, %3
    movd             m3, %4

    punpcklbw        m0, m2
    punpcklbw        m1, m3
    punpcklwd        m0, m1

    movd             m4, %5
    movd             m6, %6
    movd             m5, %7
    movd             m3, %8

    punpcklbw        m4, m6
    punpcklbw        m5, m3
    punpcklwd        m4, m5

    punpckhdq        m2, m0, m4
    punpckldq        m0, m4

    pxor             m5, m5
    punpckhbw        m1, m0, m5
    punpcklbw        m0, m5
    punpckhbw        m3, m2, m5
    punpcklbw        m2, m5
%endmacro

; in: 4 rows of 8 words in m0..m3
; out: 8 rows of 4 bytes in %1..%8
%macro TRANSPOSE8x4B_STORE 8
    packuswb         m0, m2
    packuswb         m1, m3
    SBUTTERFLY bw, 0, 1, 2
    SBUTTERFLY wd, 0, 1, 2

    movd             %1, m0
    pshufd           m0, m0, 0x39
    movd             %2, m0
    pshufd           m0, m0, 0x39
    movd             %3, m0
    pshufd           m0, m0, 0x39
    movd             %4, m0

    movd             %5, m1
    pshufd           m1, m1, 0x39
    movd             %6, m1
    pshufd           m1, m1, 0x39
    movd             %7, m1
    pshufd           m1, m1, 0x39
    movd             %8, m1
%endmacro

; in: 8 rows of 4 words in %4..%11
; out: 4 rows of 8 words in m0..m3
%macro TRANSPOSE4x8W_LOAD 8
    movq             m0, %1
    movq             m2, %2
    movq             m1, %3
    movq             m3, %4

    punpcklwd        m0, m2
    punpcklwd        m1, m3
    punpckhdq        m2, m0, m1
    punpckldq        m0, m1

    movq             m4, %5
    movq             m6, %6
    movq             m5, %7
    movq             m3, %8

    punpcklwd        m4, m6
    punpcklwd        m5, m3
    punpckhdq        m6, m4, m5
    punpckldq        m4, m5

    punpckhqdq       m1, m0, m4
    punpcklqdq       m0, m4
    punpckhqdq       m3, m2, m6
    punpcklqdq       m2, m6

%endmacro

; in: 4 rows of 8 words in m0..m3
; out: 8 rows of 4 words in %1..%8
%macro TRANSPOSE8x4W_STORE 9
    TRANSPOSE4x4W     0, 1, 2, 3, 4

    pxor             m5, m5; zeros reg
    CLIPW            m0, m5, %9
    CLIPW            m1, m5, %9
    CLIPW            m2, m5, %9
    CLIPW            m3, m5, %9

    movq             %1, m0
    movhps           %2, m0
    movq             %3, m1
    movhps           %4, m1
    movq             %5, m2
    movhps           %6, m2
    movq             %7, m3
    movhps           %8, m3
%endmacro

; in: 8 rows of 8 bytes in %1..%8
; out: 8 rows of 8 words in m0..m7
%macro TRANSPOSE8x8B_LOAD 8
    movq             m7, %1
    movq             m2, %2
    movq             m1, %3
    movq             m3, %4

    punpcklbw        m7, m2
    punpcklbw        m1, m3
    punpcklwd        m3, m7, m1
    punpckhwd        m7, m1

    movq             m4, %5
    movq             m6, %6
    movq             m5, %7
    movq            m15, %8

    punpcklbw        m4, m6
    punpcklbw        m5, m15
    punpcklwd        m9, m4, m5
    punpckhwd        m4, m5

    punpckldq        m1, m3, m9;  0, 1
    punpckhdq        m3, m9;  2, 3

    punpckldq        m5, m7, m4;  4, 5
    punpckhdq        m7, m4;  6, 7

    pxor            m13, m13

    punpcklbw        m0, m1, m13; 0 in 16 bit
    punpckhbw        m1, m13; 1 in 16 bit

    punpcklbw        m2, m3, m13; 2
    punpckhbw        m3, m13; 3

    punpcklbw        m4, m5, m13; 4
    punpckhbw        m5, m13; 5

    punpcklbw        m6, m7, m13; 6
    punpckhbw        m7, m13; 7
%endmacro


; in: 8 rows of 8 words in m0..m8
; out: 8 rows of 8 bytes in %1..%8
%macro TRANSPOSE8x8B_STORE 8
    packuswb         m0, m4
    packuswb         m1, m5
    packuswb         m2, m6
    packuswb         m3, m7
    TRANSPOSE2x4x4B   0, 1, 2, 3, 4

    movq             %1, m0
    movhps           %2, m0
    movq             %3, m1
    movhps           %4, m1
    movq             %5, m2
    movhps           %6, m2
    movq             %7, m3
    movhps           %8, m3
%endmacro

; in: 8 rows of 8 words in %1..%8
; out: 8 rows of 8 words in m0..m7
%macro TRANSPOSE8x8W_LOAD 8
    movdqu           m0, %1
    movdqu           m1, %2
    movdqu           m2, %3
    movdqu           m3, %4
    movdqu           m4, %5
    movdqu           m5, %6
    movdqu           m6, %7
    movdqu           m7, %8
    TRANSPOSE8x8W     0, 1, 2, 3, 4, 5, 6, 7, 8
%endmacro

; in: 8 rows of 8 words in m0..m8
; out: 8 rows of 8 words in %1..%8
%macro TRANSPOSE8x8W_STORE 9
    TRANSPOSE8x8W     0, 1, 2, 3, 4, 5, 6, 7, 8

    pxor             m8, m8
    CLIPW            m0, m8, %9
    CLIPW            m1, m8, %9
    CLIPW            m2, m8, %9
    CLIPW            m3, m8, %9
    CLIPW            m4, m8, %9
    CLIPW            m5, m8, %9
    CLIPW            m6, m8, %9
    CLIPW            m7, m8, %9

    movdqu           %1, m0
    movdqu           %2, m1
    movdqu           %3, m2
    movdqu           %4, m3
    movdqu           %5, m4
    movdqu           %6, m5
    movdqu           %7, m6
    movdqu           %8, m7
%endmacro


; in: %2 clobbered
; out: %1
; mask in m11
; clobbers m10
%macro MASKED_COPY 2
    pand             %2, m11 ; and mask
    pandn           m10, m11, %1; and -mask
    por              %2, m10
    mova             %1, %2
%endmacro

%macro MASKED_COPY_NOT 2
    pandn          %2, m11, %2 ; and -mask
    pand           m10, m11, %1; and mask
    por              %2, m10
    mova             %1, %2
%endmacro

; in: %2 clobbered
; out: %1
; mask in %3, will be clobbered
%macro MASKED_COPY2 3
    pand             %2, %3 ; and mask
    pandn            %3, %1; and -mask
    por              %2, %3
    mova             %1, %2
%endmacro


ALIGN 16
; input in m0 ... m3 and tcs in r2. Output in m1 and m2
%macro CHROMA_DEBLOCK_BODY 1

    psubw            m4, m2, m1; q0 - p0
    psubw            m5, m0, m3; p1 - q1
    psllw            m4, 2; << 2
    paddw            m5, m4;

    ;tc calculations
    movq             m6, [tcq]; tc0
    punpcklwd        m6, m6
    pshufd           m6, m6, 0xA0; tc0, tc1

%if   %1 == 8
    paddw            m6, [pw_2]
    psraw            m6, 2
%elif %1 == 12
    psllw            m6, 2
%endif

    psignw           m4, m6, [pw_m1]; -tc0, -tc1

    paddw            m5, [pw_4]; +4
    psraw            m5, 3; >> 3

    pmaxsw           m5, m4
    pminsw           m5, m6
    paddw            m1, m5; p0 + delta0
    psubw            m2, m5; q0 - delta0
%endmacro



;-----------------------------------------------------------------------------
; void ff_hevc_v_loop_filter_chroma(uint8_t *_pix, ptrdiff_t _stride, int32_t *tc,
;                                   uint8_t *_no_p, uint8_t *_no_q);
;-----------------------------------------------------------------------------
%macro LOOP_FILTER_CHROMA 0
cglobal vvc_v_loop_filter_chroma_8, 4, 6, 7, pix, stride, beta, tc, pix0, r3stride
    sub            pixq, 2
    lea       r3strideq, [3*strideq]
    mov           pix0q, pixq
    add            pixq, r3strideq
    TRANSPOSE4x8B_LOAD  PASS8ROWS(pix0q, pixq, strideq, r3strideq)
    CHROMA_DEBLOCK_BODY 8
    TRANSPOSE8x4B_STORE PASS8ROWS(pix0q, pixq, strideq, r3strideq)
    RET

cglobal vvc_v_loop_filter_chroma_10, 4, 6, 7, pix, stride, beta, tc, pix0, r3stride
    sub            pixq, 4
    lea       r3strideq, [3*strideq]
    mov           pix0q, pixq
    add            pixq, r3strideq
    TRANSPOSE4x8W_LOAD  PASS8ROWS(pix0q, pixq, strideq, r3strideq)
    CHROMA_DEBLOCK_BODY 10
    TRANSPOSE8x4W_STORE PASS8ROWS(pix0q, pixq, strideq, r3strideq), [pw_pixel_max_10]
    RET

cglobal vvc_v_loop_filter_chroma_12, 4, 6, 7, pix, stride, beta, tc, pix0, r3stride
    sub            pixq, 4
    lea       r3strideq, [3*strideq]
    mov           pix0q, pixq
    add            pixq, r3strideq
    TRANSPOSE4x8W_LOAD  PASS8ROWS(pix0q, pixq, strideq, r3strideq)
    CHROMA_DEBLOCK_BODY 12
    TRANSPOSE8x4W_STORE PASS8ROWS(pix0q, pixq, strideq, r3strideq), [pw_pixel_max_12]
    RET

;        (uint8_t *pix, ptrdiff_t stride, const int32_t *beta, const int32_t *tc,
;        const uint8_t *no_p, const uint8_t *no_q, const uint8_t *max_len_p, const uint8_t *max_len_q, int shift);

;-----------------------------------------------------------------------------
; void ff_hevc_h_loop_filter_chroma(uint8_t *_pix, ptrdiff_t _stride, int32_t *tc,
;                                   uint8_t *_no_p, uint8_t *_no_q);
;-----------------------------------------------------------------------------
cglobal vvc_h_loop_filter_chroma_8, 8, 9, 12, pix, stride, beta, tc, no_p, no_q, max_len_p, max_len_q, pix0, src3stride
    mov           pix0q, pixq
    sub           pix0q, strideq
    sub           pix0q, strideq
    movq             m0, [pix0q];    p1
    movq             m1, [pix0q+strideq]; p0
    movq             m2, [pixq];    q0
    movq             m3, [pixq+strideq]; q1
    pxor             m5, m5; zeros reg
    punpcklbw        m0, m5
    punpcklbw        m1, m5
    punpcklbw        m2, m5
    punpcklbw        m3, m5
    CHROMA_DEBLOCK_BODY  8
    packuswb         m1, m2
    movh[pix0q+strideq], m1
    movhps       [pixq], m1
    RET

cglobal vvc_h_loop_filter_chroma_10, 9, 11, 12, pix, stride, beta, tc, no_p, no_q, max_len_p, max_len_q, pix0, q_len, src3stride

    lea    src3strideq, [3 * strideq]
    mov           pix0q, pixq
    sub           pix0q, src3strideq
    sub           pix0q, strideq

    ; load all values at the same time
    movu             m0, [pix0q];               p3
    movu             m1, [pix0q +     strideq]; p2
    movu             m2, [pix0q + 2 * strideq]; p1
    movu             m3, [pix0q + src3strideq]; p0
    movu             m4, [pixq];                q0
    movu             m5, [pixq +     strideq];  q1
    movu             m6, [pixq + 2 * strideq];  q2
    movu             m7, [pixq + src3strideq];  q3

    ; for max_len = 3 we need to determine whether to decrease the length 
    ; spatial activity for all q
    psllw            m9, m5, 1; q1_ * 2
    psubw           m11, m6, m9; p2_ - 2*q1
    paddw           m11, m4; + q0
    ABS1            m11, m13
    ; spatial activity for all p
    psllw            m9, m2, 1; p1_ *2
    psubw           m10, m1, m9
    paddw           m10, m3 
    ABS1            m10, m11

    paddw           m9, m10, m11 ; m10, m11 just the full row calcs

    ; load beta
    movq             m8, [betaq]
    psllw            m8, 10 - 8; 
    punpcklqdq       m8, m8, m8
    ; replicate beta values across the appropriate group of 4 samples
    pshufhw          m13, m8, q2222
    pshuflw          m13, m13, q0000
    ; end beta calcs 

    ; create comparison lanes, can change the shuffle mask as necessary
    ; assume no shift for now, we need to construct a mask ...
    ; get line 0, 3 (or 0, 1 for shift)

    pshufhw         m14,  m9, q0033  ;0b00001111;  d3 d3  d0 d0 in high (block 1) - low of block 9 copied
    pshufhw          m9,  m9, q3300 ;0b11110000;    d0 d0 d3 d3 

    pshuflw         m14, m14, q0033 ;0b00001111;   d3 d3  d0 d0 in low (block 2)
    pshuflw           m9, m9, q3300 ;0b11110000;    d0 d0 d3 d3

    paddw           m14, m9;   d0 + d3, d0 + d3, d0 + d3, .....

    ; compare to beta
    ; max beta value is 84

    pcmpgtw          m15, m13, m14 ; beta > d0 + d3, d0 + d3 (next block)
    ; for non-shift this is only two values, 
    movmskps         r13, m15
    movu              m11, m15
    ; if all 0 then jump to all strong

    ;weak / strong decision compare to beta_2
    psraw           m15, m13, 2 ; beta >> 2
    psllw           m8, m9, 1
    pcmpgtw        m15, m8; d0 ..  < beta_2, d0... < beta_2, d3... <
    ; if all 0 jump to all strong
    pand          m11, m15
    pshuflw       m15, m15, q0033; d3 < ... d3 < ...
    pshufhw       m15, m15, q0033; 
    pand          m11, m15           
    ; all zero then jump strong strong

    ;----beta_3 comparison-----
    psubw           m12, m0, m3;      p3 - p0
    ABS1            m12, m14; abs(p3 - p0)

    psubw           m15, m7, m4;      q3 - q0
    ABS1            m15, m14; abs(q3 - q0)

    paddw           m12, m15; abs(p3 - p0) + abs(q3 - q0)

    pshufhw         m12, m12, q0033 ;0b11110000;
    pshuflw         m12, m12, q0033 ;0b11110000;

    psraw           m13, 3; beta >> 3
    pcmpgtw         m13, m12;
    pand            m11, m13

    pshufhw         m13, m13, q0033 ;0b11110000;
    pshuflw         m13, m13, q0033 ;0b11110000;
    pand            m11, m13

    movmskps        r11, m13; zero represents strong
    and             r6, r11; 
    ;----beta_3 comparison end-----

    ; tc25
    movq             m8, [tcq]

    punpcklqdq       m8, m8, m8
    ; replicate beta values across the appropriate group of 4 samples
    pshufhw          m8, m8, q2222
    pshuflw          m8, m8, q0000

    movu             m9, m8

    pmulld           m8, [pd_5]
    paddd            m8, [pd_1]
    psrld            m8, 1     ; ((tc * 5 + 1) >> 1);

    ; end beta calcs 

    ;----tc25 comparison---
    psubw           m12, m3, m4;      p0 - q0
    ABS1            m12, m14; abs(p0 - q0)

    pshufhw         m12, m12, q3300 ;0b11110000;
    pshuflw         m12, m12, q3300 ;0b11110000;

    pcmpgtw          m15, m8, m12; tc25 comparisons
    pand             m11, m15

    pshufhw         m15, m15, q0033 ;0b11110000;
    pshuflw         m15, m15, q0033 ;0b11110000;
    pand            m11, m15

    ;----tc25 comparison end---
    

    ; the mask is actually a weak mask (1 = weak, 0 = strong)
    ; m11 mask, m8/m9 tc
    psignw           m8, m9, [pw_m1]; -tc0, -tc1


    ; weak one-sided
    ; p0
    paddw          m0, m3, m4 ; p0 + q0
    paddw          m0, m5     ; p0 + q0 + q1
    paddw          m0, m6     ; p0 + q0 + q1 + q2
    paddw          m0, [pw_4]; p0 + q0 + q1 + q2 + 4
    movu           m10, m0 
    psllw          m0, m2, 1
    paddw          m0, m2
    paddw          m0, m10
    paddw          m0, m3
    psrlw           m0, 3

    paddw           m8, m3 ; p0 - tc
    paddd           m9, m3 ; p0 + tc
    CLIPW           m0, m8, m9
    psubw           m8, m3
    psubw           m9, m3

    ; q0
    psllw          m12, m2, 1
    paddw          m12, m10 
    paddw          m12, m4   ; q0
    paddw          m12, m7   ;q3

    psrlw          m12, 3

    paddw          m8, m4 ; q0 - tc
    paddw          m9, m4 ; q0 + tc
    CLIPW          m12, m8, m9
    psubw          m8, m4  ; 
    psubw          m9, m4  ; 

    ; q1
    psllw          m13, m7, 1
    paddw          m13, m10
    paddw          m13, m5
    paddw          m13, m2

    psrlw          m13, 3

    paddw          m8, m5 ; p0 - tc
    paddw          m9, m5 ; p0 + tc
    CLIPW          m13, m8, m9
    psubw          m8, m5  ; 
    psubw          m9, m5  ; 

    ;q2 
    psllw          m14, m7, 1
    paddw          m14, m7
    paddw          m14, m10
    paddw          m14, m6

    psrlw          m14, 3

    paddw          m8, m6 ; p0 - tc
    paddw          m9, m6 ; p0 + tc
    CLIPW         m14, m8, m9
    psubw          m8, m6  ; 
    psubw          m9, m6  ; 

    MASKED_COPY_NOT   m3, m0  ; m2
    MASKED_COPY_NOT   m4, m12 ; m3
    MASKED_COPY_NOT   m5, m13 ; m4
    MASKED_COPY_NOT   m6, m14 ; m5

    ; calculate weak

    movu             m0, m2 ;
    movu             m1, m3 ;
    movu             m2, m4 ;
    movu             m3, m5 ;
    
    CHROMA_DEBLOCK_BODY 10
    pxor            m5, m5; zeros reg
    CLIPW           m1, m5, [pw_pixel_max_10] ; p0
    CLIPW           m2, m5, [pw_pixel_max_10] ; q0

    MASKED_COPY    [pix0q + src3strideq], m1
    MASKED_COPY             [pixq], m2
    RET
    


    ; pshuflw          m8, m2, 1
    ; paddw            m8, m2

    ; pshuflw          m15, m3, 1
    ; paddw            m8, m15
    ; paddw            m8, m4
    ; paddw            m8, m5
    ; paddw            m8, m6
    ; paddw            m8, [pw_4]
    ; pshuflw          m8, 3
    ; CLIPW          m8

.chroma_weak ; unused for now
    mov          pix0q, pixq
    sub          pix0q, strideq
    sub          pix0q, strideq
    movu            m0, [pix0q];    p1
    movu            m1, [pix0q+strideq]; p0
    movu            m2, [pixq];    q0
    movu            m3, [pixq+strideq]; q1
    CHROMA_DEBLOCK_BODY 10
    pxor            m5, m5; zeros reg
    CLIPW           m1, m5, [pw_pixel_max_10]
    CLIPW           m2, m5, [pw_pixel_max_10]
    movu [pix0q+strideq], m1
    movu        [pixq], m2

    RET

cglobal vvc_h_loop_filter_chroma_12, 8, 9, 12, pix, stride, stride, beta, tc, no_p, no_q, max_len_p, max_len_q, pix0, q_len
    mov          pix0q, pixq
    sub          pix0q, strideq
    sub          pix0q, strideq
    movu            m0, [pix0q];    p1
    movu            m1, [pix0q+strideq]; p0
    movu            m2, [pixq];    q0
    movu            m3, [pixq+strideq]; q1
    CHROMA_DEBLOCK_BODY 12
    pxor            m5, m5; zeros reg
    CLIPW           m1, m5, [pw_pixel_max_12]
    CLIPW           m2, m5, [pw_pixel_max_12]
    movu [pix0q+strideq], m1
    movu        [pixq], m2
    RET
%endmacro

INIT_XMM avx
LOOP_FILTER_CHROMA

INIT_XMM avx2
LOOP_FILTER_CHROMA
