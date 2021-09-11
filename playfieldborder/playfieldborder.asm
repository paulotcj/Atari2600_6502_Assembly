    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Includes required with definitions and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ROM code start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg code
    org $F000           ; Decimal: 61440

Start:
    CLEAN_START

    LDX #$80            ; Background colour - blue
    STX COLUBK
    
    LDA #$1C            ; Playfield border colour: yellow
    STA COLUPF
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by configuring VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
    LDA #2              ; Same as binary value %00000010
    STA VSYNC           ; turn on VSYNC
    STA VBLANK          ; turn on VBLANK  
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate three VSYNC lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 3
        STA WSYNC
    REPEND

    LDA #0
    STA VSYNC           ; Turn off VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TIA output - 37 VSYNC lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 37
        STA WSYNC
    REPEND

    LDA #0
    STA VBLANK          ; Turn off VBLANK    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set the CTRLPF register to allow playfield reflection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDX #%00000001      ; CTRLPF register (D0 in this case is set to reflect)
    STX CTRLPF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Draw 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; POSITION = 7 6 5 4 3 2 1 0
    ; Bytes    = 0 0 0 0 0 0 0 0 
    ; PF0      = X X X X         (PF0 = 4 5 6 7)
    ; PF1      = X X X X X X X X (PF1 = 7 6 5 4 3 2 1 0)
    ; PF2      = X X X X X X X X (PF2 = 0 1 2 3 4 5 6 7)

    ; ORIENTATION
    ; PF0 >>>>>
    ; PF1 <<<<<
    ; PF2 >>>>>




    ; 7 invisible scanlines
    LDX #0
    STX PF0
    STX PF1
    STX PF2
    
    REPEAT 7
        STA WSYNC
    REPEND
    
    ; 7 visible scanlines
    ;   PF0 -> 1110 (LSB  first meaning the first bit/row will be transparent) 
    ;   PF1 & PF2 ->  1111  1111
    LDX #%11100000 ;PF0 is only 4 bits the last 4 bits are lost
    STX PF0
    LDX #%11111111
    STX PF1
    STX PF2    
    
    REPEAT 7
        STA WSYNC
    REPEND

    ; Next 164 visible lines
    LDX #%00100000 ;PF0 is only 4 bits the last 4 bits are lost
    STX PF0
    LDX #%00000000
    STX PF1
    LDX #%10000000
    STX PF2
    
    REPEAT 164
        STA WSYNC
    REPEND


    ; 7 visible scanlines
    ;   PF0 -> 1110 (LSB  first meaning the first bit/row will be transparent) 
    ;   PF1 & PF2 ->  1111  1111
    LDX #%11100000 ;PF0 is only 4 bits the last 4 bits are lost
    STX PF0
    LDX #%11111111
    STX PF1
    STX PF2    
    
    REPEAT 7
        STA WSYNC
    REPEND    


    ; 7 invisible scanlines
    LDX #0
    STX PF0
    STX PF1
    STX PF2
    
    REPEAT 7
        STA WSYNC
    REPEND    



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Output 30 VBLANK scanlines to complete the frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #2
    STA VBLANK
    REPEAT 30
        STA WSYNC
    REPEND
    LDA #0
    STA WSYNC
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Loop to the next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    JMP StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ORG $FFFC
    .word Start
    .word Start