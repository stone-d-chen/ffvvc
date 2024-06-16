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

%macro CLIP_RESTORE 4  ; toclip, value, -tc, +tc
    paddw           %3, %2
    paddd           %4, %2
    CLIPW           %1, %3, %4
    psubw           %3, %2
    psubw           %4, %2

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

; (uint8_t *pix, ptrdiff_t stride,
;     const int32_t *beta, const int32_t *tc, const uint8_t *no_p, const uint8_t *no_q,
;     const uint8_t *max_len_p, const uint8_t *max_len_q, int shift)
cglobal vvc_h_loop_filter_chroma_10, 9, 12, 15, pix, stride, beta, tc, no_p, no_q, max_len_p, max_len_q, shift , pix0, q_len, src3stride

    lea    src3strideq, [3 * strideq]
    mov           pix0q, pixq
    sub           pix0q, src3strideq
    sub           pix0q, strideq

    ; load all values at the same time
    movu             m0, [pix0q]               ;  p3
    movu             m1, [pix0q +     strideq] ;  p2
    movu             m2, [pix0q + 2 * strideq] ;  p1
    movu             m3, [pix0q + src3strideq] ;  p0
    movu             m4, [pixq]                ;  q0
    movu             m5, [pixq +     strideq]  ;  q1
    movu             m6, [pixq + 2 * strideq]  ;  q2
    movu             m7, [pixq + src3strideq]  ;  q3

    ; for horizontal, max_len_p == 1 and p3 and p2 are p1
    movu             m0, m2
    movu             m1, m2

; above will be per function

    ; for max_len = 3 we need to determine whether to decrease the length 
    psllw            m9, m2, 1   
    psubw           m10, m1, m9
    paddw           m10, m3 
    ABS1            m10, m11

    psllw            m9, m5, 1   
    psubw           m11, m6, m9  
    paddw           m11, m4      
    ABS1            m11, m13

    paddw           m9, m10, m11  ; m9 spatial activity sum for all cols

    ; ----  load beta   --------
    
    movq             m8, [betaq]  ; quad load 8 values for shift
    psllw            m8, 10 - 8   ; replace with bit_depth

    cmp           shiftd, 1
    je           .beta_load_shift
    
    punpcklqdq       m8, m8, m8
    pshufhw          m13, m8, q2222
    pshuflw          m13, m13, q0000

    pshufhw         m14,  m9, q0033  ;  d3 d3  d0 d0 in high (block 1) - low of block 9 copied
    pshufhw          m9,  m9, q3300  ;    d0 d0 d3 d3 
    pshuflw         m14, m14, q0033  ;   d3 d3  d0 d0 in low (block 2)
    pshuflw          m9,  m9, q3300  ;    d0 d0 d3 d3

    jmp  .spatial_activity

.beta_load_shift:

    pshufhw          m13, m8,  q2200
    pshuflw          m13, m13, q2200

    pshufhw         m14,  m9, q3210
    pshufhw          m9,  m9, q2301                              
    pshuflw         m14, m14, q3210
    pshuflw          m9, m9,  q2301 

.spatial_activity:
            
    paddw            m14, m9         ; d0 + d3, d0 + d3, d0 + d3, .....
    pcmpgtw          m15, m13, m14    ; beta > d0 + d3, d0 + d3 (next block)
    movu             m11, m15         ; if all 0 then jump to all strong


    ; ---- beta_2 comparison -----
    psraw          m15, m13, 2   ; beta >> 2
    psllw           m8, m9, 1    ;  d0, d1, d2, d3, ...
    
    pcmpgtw        m15, m8        ; d0 ..  < beta_2, d0... < beta_2, d3... <
    pand          m11, m15

    cmp           shiftd, 1
    je    .beta2_mask_shuffle   
  
    pshuflw       m15, m15, q0033    ; d3 < ... d3 < ...
    pshufhw       m15, m15, q0033    ; 
    pand          m11, m15
    jmp     .beta3_comparison

.beta2_mask_shuffle:
    pshuflw       m15, m15, q2301    ; d3 < ... d3 < ...
    pshufhw       m15, m15, q2301    ; 
    pand          m11, m15                      

.beta3_comparison:
    ; all zero then jump strong strong
    ;----beta_3 comparison-----
    psubw           m12, m0, m3     ; p3 - p0
    ABS1            m12, m14        ; abs(p3 - p0)

    psubw           m15, m7, m4     ; q3 - q0
    ABS1            m15, m14        ; abs(q3 - q0)

    paddw           m12, m15        ; abs(p3 - p0) + abs(q3 - q0)

    psraw           m13, 3          ; beta >> 3

    cmp           shiftd, 1
    je    .beta3_no_first_shuffle

    pshufhw         m12, m12, q3300 
    pshuflw         m12, m12, q3300 
    ; if shift, don't need to shuffle
    ; endshift

.beta3_no_first_shuffle:
    pcmpgtw         m13, m12        ; clobber beta?
    pand            m11, m13

    cmp           shiftd, 1
    je    .beta3_mask_shift_shuffle

    pshufhw         m13, m13, q0033
    pshuflw         m13, m13, q0033
    pand            m11, m13

    jmp   .tc25_comparison
    ; if shift then shuffcle

.beta3_mask_shift_shuffle:
    pshufhw         m13, m13, q2301
    pshuflw         m13, m13, q2301
    pand            m11, m13
    ;----beta_3 comparison end-----

    ;----tc25 comparison---
.tc25_comparison:
    movq             m8, [tcq]   ; preprocess non shift so that I load more
    psllw            m8, 10 - 8; 
    cmp           shiftd, 1
    je   .tc25_load_shift

    punpcklqdq       m8, m8, m8
    pshufhw          m8, m8, q2222
    pshuflw          m8, m8, q0000
    jmp     .tc25_calculation

.tc25_load_shift:

    pshufhw          m8, m8,  q2200
    pshuflw          m8, m8,  q2200

.tc25_calculation:
    movu             m9, m8
    pmulld           m8, [pd_5]
    paddd            m8, [pd_1]
    psrld            m8, 1          ; ((tc * 5 + 1) >> 1);

    ; --- comparison
    psubw           m12, m3, m4     ;      p0 - q0
    ABS1            m12, m14        ; abs(p0 - q0)

    cmp           shiftd, 1
    je  .tc25_mask

    pshufhw         m12, m12, q3300  
    pshuflw         m12, m12, q3300

.tc25_mask:

    pcmpgtw          m15, m8, m12   ; tc25 comparisons
    pand             m11, m15

    cmp           shiftd, 1
    je  .tc25_shift_mask

    pshufhw         m15, m15, q0033 
    pshuflw         m15, m15, q0033 
    pand            m11, m15
    jmp   .prep_clipping_masks

.tc25_shift_mask:
    pshufhw         m15, m15, q2301 
    pshuflw         m15, m15, q2301
    pand            m11, m15

    ;----tc25 comparison end---
    
    ; get clipping mask ready

.prep_clipping_masks:
    psignw           m8, m9, [pw_m1]; -tc0, -tc1 ; m11 mask, m8/m9 tc

    ; --------  strong calcs -------    
    ; p0
    paddw         m12, m0, m1
    paddw         m12, m2
    paddw         m12, m3
    paddw         m12, m4
    paddw         m12, [pw_4]
    movu          m15, m12      ; p3 +  p2 + p1 +  p0 +   q0 + 4
    paddw         m12, m3
    paddw         m12, m5       ; q1
    paddw         m12, m6       ; q2
    psrlw         m12, 3
    CLIP_RESTORE  m12, m3, m8, m9
    
    ; p1
    paddw        m13, m15, m10
    paddw        m13, m2
    paddw        m13, m5
    psrlw         m0, 3
    CLIP_RESTORE  m13, m2, m8, m9

    ; p2
    psllw         m14, m15, 1
    paddw         m14, m10
    paddw         m14, m1
    CLIP_RESTORE  m14, m1, m8, m9

    ; q0
    ; clobber m0 / P3 - not used anymore
    paddw         m0, m3, m4
    paddw         m0, m5
    paddw         m0, m6
    paddw         m0, m7
    paddw         m0, [pw_4]
    movu          m15, m0  ; p0 + q0 + q1 + q2 + q3+ 4
    paddw         m0, m1   ; p2 free
    paddw         m0, m2
    paddw         m0, m3
    psrlw         m0, 3
    CLIP_RESTORE  m0, m4, m8, m9

    ; q1
    ; clobber m1 / P2 - last use was q0 calc
    paddw         m1, m2, m15; p1 + ...
    paddw         m1, m5
    paddw         m1, m7
    psrlw         m1, 3
    CLIP_RESTORE  m1, m5, m8, m9

    ; q2
    ; clobber m15 - sum is fully used
    paddw         m15, m7
    paddw         m15, m7
    paddw         m15, m6
    psrlw         m15, 3
    CLIP_RESTORE  m15, m6, m8, m9

    MASKED_COPY_NOT m3, m12 ; p0 
    MASKED_COPY_NOT m2, m13 ; p1
    MASKED_COPY_NOT m1, m14 ; p2
    MASKED_COPY_NOT m4, m0  ; q0
    MASKED_COPY_NOT m5, m1  ; q1
    MASKED_COPY_NOT m6, m15 ; q2

    ; strong one-sided
    ; p0   -  clobber p3 again
    paddw          m0, m3, m4 ;      p0 + q0
    paddw          m0, m5     ;      p0 + q0 + q1
    paddw          m0, m6     ;      p0 + q0 + q1 + q2
    paddw          m0, [pw_4] ;      p0 + q0 + q1 + q2 + 4
    paddw          m0, m2     ; p1 + p0 + q0 + q1 + q2 + 4
    movu           m15, m0 
    paddw          m0, m2
    paddw          m0, m2
    paddw          m0, m3
    psrlw          m0, 3

    CLIP_RESTORE   m0, m3, m8, m9

    ; q0
    paddw          m12, m2, m15
    paddw          m12, m4   ;q0
    paddw          m12, m7   ;q3

    psrlw          m12, 3

    CLIP_RESTORE   m12, m4, m8, m9

    ; q1
    psllw          m13, m7, 1 ; 2*q3
    paddw          m13, m15  
    paddw          m13, m5   ; q1

    psrlw          m13, 3

    CLIP_RESTORE   m13, m5, m8, m9

    ;q2 
    psllw          m14, m7, 1
    paddw          m14, m7
    paddw          m14, m15
    paddw          m14, m6
    psubw          m14, m3  ; sub p0

    psrlw          m14, 3

    CLIP_RESTORE   m14, m6, m8, m9

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
