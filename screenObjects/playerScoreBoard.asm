    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Includes required with definitions and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start an uninitialized segment at $80 for variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    ORG $80
P0Height .byte      ; one byte variable
P1Height .byte      ; one byte variable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ROM code start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    SEG CODE
    ORG $F000

Start:
    CLEAN_START     ; Clean memory and TIA

    LDX #$80
    STX COLUBK      ; Set background colour to blue

    LDA #%1111
    STA COLUPF      ; Set Playfield to white

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set P0Height and P1Height
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #10         ; A = 10
    STA P0Height    ; P0Height = 10
    STA P1Height    ; P1Height = 10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set Player1 and Player2 colours
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #$48        ; red
    STA COLUP0      ; Player1 colour
    
    LDA #$C6        ; green
    STA COLUP1      ; Player2 colour
    
    LDY #%00000010  ; CTRLPF D1 set to 1 means score
    STY CTRLPF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start a new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
    LDA #2
    STA VBLANK      ; Turn on VBLANK
    STA VSYNC       ; Turn on VSYNC
    
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
; Draw 192 visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; Draw 10 empty lines
    REPEAT 10
        STA WSYNC
    REPEND

    ; Draw 10 lines for the score
    ; We need to use a special flag for the scoreboard, otherwise the system might
    ;  try to reflect or duplicate what we are trying to render
    LDY #0              ; Y = 0
ScoreboardLoop:
    LDA NumberBitmap,Y ;Check the NumberBitmap region, fetch line Y
    STA PF1             ;Draw on Playfield1 (this is duplicated/reflected on the other side)
    STA WSYNC           ;Wait for signal
    INY                 ;Y++
    CPY #10             ;Is Y equal to 10?
    BNE ScoreboardLoop  ;If Y is not equal to 10, loop
    
    LDA #0
    STA PF1             ; Disable playfield
    

    ; Draw 50 empty lines
    REPEAT 50
        STA WSYNC
    REPEND  

    ; Draw 10 lines for Player0
    LDY #0
Player0Loop:
    LDA PlayerBitmap,Y ;Load on A the contents of PlayerBitmap on line Y
    STA GRP0            ;Register Graphics Player0
    STA WSYNC           ;Wait for signal
    INY                 ;Y++
    CPY P0Height
    BNE Player0Loop
    
    LDA #0
    STA GRP0            ;Disable Player0 graphics
    

    ; Draw 10 lines for Player1
    LDY #0
Player1Loop:
    LDA PlayerBitmap,Y ;Load on A the contents of PlayerBitmap on line Y
    STA GRP1            ;Register Graphics Player1
    STA WSYNC           ;Wait for signal
    INY                 ;Y++
    CPY P1Height
    BNE Player1Loop
    
    LDA #0
    STA GRP1            ;Disable Player1 graphics

    ;Draw the remaining 102 lines (10+10+50+10+10=90 , total visible 192 -> 192 - 90 = 102)
    REPEAT 102
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
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defines an array of bytes to draw the scoreboard number.
;; We add these bytes in the last ROM addresses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFE8                           ;65,512 (FFE8)
PlayerBitmap:
    .byte #%01111110   ;  ######        + 1 BYTE 
    .byte #%11111111   ; ########       + 1 BYTE
    .byte #%10011001   ; #  ##  #       + 1 BYTE
    .byte #%10011001   ; #  ##  #       + 1 BYTE
    .byte #%11111111   ; ########       + 1 BYTE
    .byte #%11111111   ; ########       + 1 BYTE
    .byte #%10111101   ; # #### #       + 1 BYTE
    .byte #%11000011   ; ##    ##       + 1 BYTE
    .byte #%11111111   ; ########       + 1 BYTE
    .byte #%01111110   ;  ######        + 1 BYTE -> FFE8 + A = FFF2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defines an array of bytes to draw the scoreboard number.
;; We add these bytes in the final ROM addresses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFF2                           ;bits       65,522
NumberBitmap:
    .byte #%00001110   ; ########
    .byte #%00001110   ; ########
    .byte #%00000010   ;      ###
    .byte #%00000010   ;      ###
    .byte #%00001110   ; ########
    .byte #%00001110   ; ########
    .byte #%00001000   ; ###
    .byte #%00001000   ; ###
    .byte #%00001110   ; ########
    .byte #%00001110   ; ########    FFF2 + A = FFFC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ORG $FFFC
    .word Start
    .word Start