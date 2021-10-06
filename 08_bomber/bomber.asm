;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; instruction set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    PROCESSOR 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    INCLUDE "vcs.h"
    INCLUDE "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variables - uninitialized segment starting at $80
;   We have memory from $80 to $ff to use as variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    SEG.U Variables
    ORG $80

JetXPos     BYTE        ;Player 0 X position
JetYPos     BYTE        ;player 0 y position
BomberXPos  BYTE        ;player 1 x position
BomberYPos  BYTE        ;player 1 y position

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    SEG Code
    ORG $F000

Reset:
    CLEAN_START         ;Clean the memory and the TIA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #10
    STA JetYPos         ;JetYPos = 10
    LDA #60
    STA JetXPos         ;JetXPos = 60

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDA #2
    STA VBLANK          ;turn on VBLANK
    STA VSYNC           ;turn on VSYNC
    REPEAT 3
        STA WSYNC       ;display 3 recommended lines of VSYNC
    REPEND
    LDA #0
    STA VSYNC           ;turn off VSYNC
    REPEAT 37
        STA WSYNC       ;display the 37 recommended lines of VBLANK
    REPEND
    STA VBLANK          ;turn off VBLANK


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display the 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    LDA #$84            ;
    STA COLUBK         ;set background/river color to blue
    LDA #$C2            ;
    STA COLUPF          ;set playfield/grass color to green
    LDA #%00000001      ;
    STA CTRLPF          ;enable playfield reflection
    LDA #$F0            ;
    STA PF0             ;setting PF0 bit pattern
    LDA #$FC            ;
    STA PF1             ;setting PF1 bit pattern
    LDA #0              ;
    STA PF2             ;setting PF2 bit pattern
    
    LDX #192            ;X counts the number of remaining scanlines
    
.GameLineLoop:
    STA WSYNC           ;
    DEX                 ;X--
    BNE .GameLineLoop   ;repeat next main game scanline until finished

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #2              ;
    STA VBLANK          ;turn on VBLANK
    REPEAT 30           ;
        STA WSYNC       ;display 30 recommended lines of VBlank
    REPEND              ;
    LDA #0              ;
    STA VBLANK          ;turn off VBLANK


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Loop back to start a new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    JMP StartFrame      ; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ORG $FFFC           ; move to position $FFFC
    WORD Reset          ; write 2 bytes with the program reset address
    WORD Reset          ; write 2 bytes with the interruption vector


