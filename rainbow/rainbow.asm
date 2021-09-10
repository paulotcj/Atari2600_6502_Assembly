    processor 6502

    include "vcs.h"
    include "macro.h"

    seg code
    org $F000

Start:
    CLEAN_START         ; Macro to safely clear the memory AND TIA
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start a new frame by turning on VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NextFrame:
    LDA #2              ; Same as binary value %00000010
    STA VBLANK          ; turn on VBLANK
    STA VSYNC           ; turn on VSYNC


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate the three lines of VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    STA WSYNC           ; First scanline
    STA WSYNC           ; Second scanline
    STA WSYNC           ; Third scanline
    
    LDA #0
    STA VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let the TIA output the recommended 37 scanlines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDX #37             ; X = 37 (to count 37 scanlines)
LoopVBlank:
    STA WSYNC           ; hit WSYNC and wait for the next scanline
    DEX                 ; X--
    BNE LoopVBlank      ; Loop while X != 0
    
    LDA #0
    STA VBLANK          ; Turn off VBLANK
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Draw 192 visible scanlines (Kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    LDX #192
LoopVisible:
    STX COLUBK
    STA WSYNC
    DEX 
    BNE LoopVisible
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Output 30 more VBLANK lines (overscan) to complete out frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #2              ; hit and turn VBLANK again
    STA VBLANK

    LDX #30             ; X = 30 (to count 30 scanlines)
LoopOverscan:
    STA WSYNC           ; hit WSYNC and wait for the next scanline
    DEX                 ; X--
    BNE LoopOverscan      ; Loop while X != 0
    
    JMP NextFrame  
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ORG $FFFC
    .word Start
    .word Start