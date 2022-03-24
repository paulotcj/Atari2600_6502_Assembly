    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    seg.u Variables
    org $80

JetYPos             byte    ; player0 y position
JetXPos             byte    ; player0 x position
BomberXPos          byte    ; player1 y position
BomberYPos          byte    ; player1 x position
JetSpritePtr        WORD    ; POINTER TO PLAYER0 SPRITE LOOKUP TABLE
JetColourPtr        WORD    ; POINTER TO PLAYER0 COLOUR LOOKUP TABLE
BomberSpritePtr     WORD    ; POINTER TO PLAYER1 SPRITE LOOKUP TABLE
BomberColourPtr     WORD    ; POINTER TO PLAYER1 COLOUR LOOKUP TABLE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DEFINE CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
JET_HEIGHT = 9      ; PLAYER0 SPRITE HEIGHT (# ROWS IN THE LOOKUP TABLE)
BOMBER_HEIGHT = 9   ; PLAYER1 SPRITE HEIGHT (# ROWS IN THE LOOKUP TABLE)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    seg Code
    org $F000

Reset:
    CLEAN_START     ;call macro to reset memory and registers


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    LDA #10
    STA JetYPos         ;JetYPos = 10
    
    LDA #60
    STA JetXPos         ;JetXPos = 60
    
    ;---------
    LDA #83
    STA BomberYPos      ; BomberYPos = 83
    
    LDA #54
    STA BomberXPos      ; BomberXPos = 54
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialize pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    LDA #<JetSprite
    STA JetSpritePtr            ; lo-byte pointer for jet sprite lookup table
    LDA #>JetSprite
    STA JetSpritePtr+1          ; hi-byte pointer for jet sprite lookup table
    ;---------
    LDA #<JetColour
    STA JetColourPtr            ; lo-byte pointer for jet colour lookup table
    LDA #>JetColour
    STA JetColourPtr+1          ; hi-byte pointer for jet colour lookup table
    
    ;---------

    LDA #<BomberSprite          
    STA BomberSpritePtr         ; lo-byte pointer for enemy sprite lookup table
    LDA #>BomberSprite
    STA BomberSpritePtr+1       ; hi-byte pointer for enemy sprite lookup table
    ;---------
    LDA #<BomberColour
    STA BomberColourPtr         ; lo-byte pointer for enemy colour lookup table
    LDA #>BomberColour
    STA BomberColourPtr+1       ; hi-byte pointer for enemy colour lookup table
    
    ;---------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start the main display loop and frame rendering 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
StartFrame:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    LDA #2
    STA VBLANK      ; turn on VBLANK
    STA VSYNC       ; turn on VSYNC
    REPEAT 3
        STA WSYNC   ; display 3 recommended lines of vsync
    REPEND

    LDA #0          ; turn off vsync
    STA VSYNC
    
    REPEAT 37
        STA WSYNC   ; display 3 recommended lines of vblank
    REPEND
    
    STA VBLANK      ; TURN OFF VBLANK
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RENDER 192 SCANLINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

GameVisibleScanLines:
    LDA #$84            ; SET COLOUR BACKGROUND TO BLUE
    STA COLUBK

    LDA #$C2
    STA COLUPF          ; SET PLAYFIELD TO GREEN

    LDA #%00000001
    STA CTRLPF          ; PLAYFIELD REFLECTION

    LDA #$F0
    STA PF0
    
    LDA #$FC
    STA PF1
    
    LDA #0
    STA PF2
    
    LDX #96            ; X COUNTS THE NUMBER OF REMAINING SCANLINES
.GameLineLoop:
.AreWeInsideJetSprite:      ; check if should render sprite player0
    TXA                     ; transfer X to A
    SEC                     ; make sure carry flag is set
    SBC JetYPos             ; subtract sprite Y coordinate
    CMP JET_HEIGHT          ; are we inside the sprite height bounds?
    BCC .DrawSpriteP0       ; if result < SpriteHeight, call subroutine
    LDA #0                  ; else, set lookup index to 0

.DrawSpriteP0:
    ;CLC                    ; clears carry flag before addition
    ;ADC JetAnimOffset      ; jumps to correct sprite frame in memory
    TAY                     ; load Y so we can work with pointer
    LDA (JetSpritePtr),Y    ; load player bitmap slice of data
    STA WSYNC               ; wait for next scanline
    STA GRP0                ; set graphics for player 0
    LDA (JetColourPtr),Y    ; load player colour from lookup table
    STA COLUP0              ; set colour for player 0 slice
    

.AreWeInsideBomberSprite:   ; check if should render sprite player1
    TXA                     ; transfer X to A
    SEC                     ; make sure carry flag is set
    SBC BomberYPos          ; subtract sprite Y coordinate
    CMP BOMBER_HEIGHT       ; are we inside the sprite height bounds?
    BCC .DrawSpriteP1       ; if result < SpriteHeight, call subroutine
    LDA #0                  ; else, set index to 0

.DrawSpriteP1
    TAY                     
    LDA #%00000101
    STA NUSIZ1              ; stretch player1 sprite - based on data set above
    LDA (BomberSpritePtr),Y ; load player bitmap slice of data
    STA WSYNC               ; wait for next scanline
    STA GRP1                ; set graphics for player 0
    LDA (BomberColourPtr),Y  ; load player colour from lookup table
    STA COLUP1              ; set colour for player 0 slice
    



    DEX                 ; X--
    BNE .GameLineLoop   ; REPEAT UNTIL FINISHED


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DISPLAY OVERSCAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

    LDA #2
    STA VBLANK
    REPEAT 30
        STA WSYNC
    REPEND

    LDA #0
    STA VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LOOP BACK TO START A  NEW FRAME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    JMP StartFrame      ; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; COMPLETE ROM SIZE UP TO 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    ORG $FFFC       ;MOVE TO POSITION $FFFC
    WORD Reset      ;WRITE 2 BYTES WITH THE PROGRAM RESET ADDRESS
    WORD Reset      ;WRITE 2 BYTES WITH THE INTERRUPTION VECTOR


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
; Sprites
JetSprite:
    .byte #%00000000         ;
    .byte #%00010100         ;   # #
    .byte #%01111111         ; #######
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #


JetSpriteTurn:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

BomberSprite:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00101010         ;  # # #
    .byte #%00111110         ;  #####
    .byte #%01111111         ; #######
    .byte #%00101010         ;  # # #
    .byte #%00001000         ;    #
    .byte #%00011100         ;   ###

;------------------------------------------------------------------
; Colour
JetColour:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColourTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColour:
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   