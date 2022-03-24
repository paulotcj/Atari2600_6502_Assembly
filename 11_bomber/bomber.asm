    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Declare (UNINITIALIZED) variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    seg.u Variables
    org $80

JetXPos         byte         ; player0 x-position
JetYPos         byte         ; player0 y-position
BomberXPos      byte         ; player1 x-position
BomberYPos      byte         ; player1 y-position
JetSpritePtr    word         ; pointer to player0 sprite lookup table
JetColorPtr     word         ; pointer to player0 color lookup table
BomberSpritePtr word         ; pointer to player1 sprite lookup table
BomberColorPtr  word         ; pointer to player1 color lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DEFINE CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
JET_HEIGHT = 9               ; player0 sprite height (# rows in lookup table)
BOMBER_HEIGHT = 9            ; player1 sprite height (# rows in lookup table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    seg Code
    org $F000

Reset:
    CLEAN_START              ; call macro to reset memory and registers

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
    LDA #<JetColor
    STA JetColorPtr            ; lo-byte pointer for jet color lookup table
    LDA #>JetColor
    STA JetColorPtr+1          ; hi-byte pointer for jet color lookup table
    
    ;---------

    LDA #<BomberSprite          
    STA BomberSpritePtr         ; lo-byte pointer for enemy sprite lookup table
    LDA #>BomberSprite
    STA BomberSpritePtr+1       ; hi-byte pointer for enemy sprite lookup table
    ;---------
    LDA #<BomberColor
    STA BomberColorPtr       ; lo-byte pointer for enemy color lookup table
    LDA #>BomberColor
    STA BomberColorPtr+1     ; hi-byte pointer for enemy color lookup table
    
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
    LDA #0
    STA VSYNC                ; turn off VSYNC
    REPEAT 37
        STA WSYNC   ; display 37 recommended lines of vblank
    REPEND
    
    STA VBLANK      ; TURN OFF VBLANK
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RENDER 192 SCANLINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

GameVisibleLine:
    LDA #$84
    STA COLUBK               ; set background/river color to blue

    LDA #$C2
    STA COLUPF               ; set playfield/grass color to green

    LDA #%00000001
    STA CTRLPF               ; enable playfield reflection

    LDA #$F0
    STA PF0                  ; setting PF0 bit pattern

    LDA #$FC
    STA PF1                  ; setting PF1 bit pattern

    LDA #0
    STA PF2                  ; setting PF2 bit pattern
    
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
    LDA (JetColorPtr),Y    ; load player color from lookup table
    STA COLUP0              ; set color for player 0 slice
    

.AreWeInsideBomberSprite:   ; check if should render sprite player1
    TXA                     ; transfer X to A
    SEC                     ; make sure carry flag is set
    SBC BomberYPos          ; subtract sprite Y coordinate
    CMP BOMBER_HEIGHT       ; are we inside the sprite height bounds?
    BCC .DrawSpriteP1       ; if result < SpriteHeight, call subroutine
    LDA #0                  ; else, set index to 0

.DrawSpriteP1:
    TAY                     
    LDA #%0000101
    STA NUSIZ1              ; stretch player1 sprite - based on data set above
    LDA (BomberSpritePtr),Y ; load player bitmap slice of data
    STA WSYNC               ; wait for next scanline
    STA GRP1                ; set graphics for player 0
    LDA (BomberColorPtr),Y  ; load player color from lookup table
    STA COLUP1              ; set color for player 0 slice
    



    DEX                 ; X--
    BNE .GameLineLoop        ; repeat next main game scanline while X != 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DISPLAY OVERSCAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

    LDA #2
    STA VBLANK               ; turn on VBLANK again
    REPEAT 30
        STA WSYNC            ; display 30 recommended lines of VBlank Overscan
    REPEND
    LDA #0
    STA VBLANK               ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LOOP BACK TO START A  NEW FRAME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    JMP StartFrame      ; continue to display the next frame

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
; Color
JetColor:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColor:
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
; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    org $FFFC                ; move to position $FFFC
    word Reset               ; write 2 bytes with the program reset address
    word Reset               ; write 2 bytes with the interruption vector

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