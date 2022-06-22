    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
JetAnimOffset   byte         ; player0 frame offset for sprite animation
Random          byte         ; used to generate random bomber x-position

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9               ; player0 sprite height (# rows in lookup table)
BOMBER_HEIGHT = 9            ; player1 sprite height (# rows in lookup table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

Reset:
    CLEAN_START              ; call macro to reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDA #68
    STA JetXPos         ;JetXPos = 68

    LDA #10
    STA JetYPos         ;JetYPos = 10
    
    ;---------
    LDA #62
    STA BomberXPos      ; BomberXPos = 62

    LDA #83
    STA BomberYPos      ; BomberYPos = 83
    ;---------

    LDA #%11010100
    STA Random          ; Random = $D4
    

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to the correct lookup table adresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    STA BomberSpritePtr         ; lo-byte pointer for bomber sprite lookup table
    LDA #>BomberSprite
    STA BomberSpritePtr+1       ; hi-byte pointer for bomber sprite lookup table
    ;---------
    LDA #<BomberColor
    STA BomberColorPtr       ; lo-byte pointer for bomber color lookup table
    LDA #>BomberColor
    STA BomberColorPtr+1     ; hi-byte pointer for bomber color lookup table
    
    ;---------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the pre-VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDA JetXPos
    LDY #0
    JSR SetObjectXPos       ; set player0 horizontal position - jump subroutine
    
    LDA BomberXPos
    LDY #1
    JSR SetObjectXPos       ; set player1 horizontal position
    
    STA WSYNC
    STA HMOVE               ; apply the horizontal offsets previously set


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

    LDA #0          ; clear TIA registers before each new frame
    STA PF0
    STA PF1
    STA PF2
    STA GRP0
    STA GRP1
    STA COLUPF
    REPEAT 20
        STA WSYNC   ; display 20 scanlines where the scoreboard goes
    REPEND



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the remaining visible scanlines of our main game (2-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    
    ;----------------------------------------
    ; this is the beginning of the loop
    LDX #84                 ; X COUNTS THE NUMBER OF REMAINING SCANLINES
.GameLineLoop:
.AreWeInsideJetSprite:      ; check if should render sprite player0
    TXA                     ; transfer X to A
    SEC                     ; make sure carry flag is set before subtraction
    SBC JetYPos             ; subtract sprite Y-coordinate
    CMP JET_HEIGHT          ; are we inside the sprite height bounds?
    BCC .DrawSpriteP0       ; if result < SpriteHeight, call the draw routine
    LDA #0                  ; else, set lookup index to zero

.DrawSpriteP0:
    CLC                     ; clears carry flag before addition
    ADC JetAnimOffset       ; jumps to correct sprite frame address in memory
    TAY                     ; load Y so we can work with the pointer
    LDA (JetSpritePtr),Y    ; load player0 bitmap data from lookup table
    STA WSYNC               ; wait for scanline
    STA GRP0                ; set graphics for player 0
    LDA (JetColorPtr),Y     ; load player color from lookup table
    STA COLUP0              ; set color of player 0
    
.AreWeInsideBomberSprite:   ; check if should render sprite player1
    TXA                     ; transfer X to A
    SEC                     ; make sure carry flag is set before subtraction
    SBC BomberYPos          ; subtract sprite Y-coordinate
    CMP BOMBER_HEIGHT       ; are we inside the sprite height bounds?
    BCC .DrawSpriteP1       ; if result < SpriteHeight, call the draw routine
    LDA #0                  ; else, set lookup index to zero

.DrawSpriteP1:
    TAY                     ; load Y so we can work with the pointer
    LDA #%00000101
    STA NUSIZ1              ; stretch player1 sprite - based on data set above
    LDA (BomberSpritePtr),Y ; load player1 bitmap data from lookup table
    STA WSYNC               ; wait for next scanline
    STA GRP1                ; set graphics for player1
    LDA (BomberColorPtr),Y  ; load player color from lookup table
    STA COLUP1              ; set color of player 1
    



    DEX                     ; X--
    BNE .GameLineLoop       ; repeat next main game scanline until finished
    
    LDA #0
    STA JetAnimOffset       ; reset jet animation frame to zero each frame
    STA WSYNC               ; wait for a scanline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDA #2
    STA VBLANK               ; turn on VBLANK again
    REPEAT 30
        STA WSYNC            ; display 30 recommended lines of VBlank Overscan
    REPEND
    LDA #0
    STA VBLANK               ; turn off VBLANK
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckP0Up:
    LDA #%00010000          ; player0 joystick up
    BIT SWCHA               ; bit test - this register is feed from the controller and should present the same bit battern as describe above (if the butto is pressed)
    BNE CheckP0Down         ; if bit pattern doesnt match, bypass Up block
    INC JetYPos
    LDA #0
    STA JetAnimOffset       ; reset sprite animation to first frame

CheckP0Down:
    LDA #%00100000          ; player0 joystick down
    BIT SWCHA
    BNE CheckP0Left         ; if bit pattern doesnt match, bypass Down block
    DEC JetYPos
    LDA #0
    STA JetAnimOffset       ; reset sprite animation to first frame

CheckP0Left:
    LDA #%01000000          ; player0 joystick left
    BIT SWCHA
    BNE CheckP0Right        ; if bit pattern doesnt match, bypass Left block
    DEC JetXPos
    LDA JET_HEIGHT          ; 9
    STA JetAnimOffset       ; set animation offset to the second frame

CheckP0Right:
    LDA #%10000000          ; player0 joystick right
    BIT SWCHA
    BNE EndInputCheck       ; if bit pattern doesnt match, bypass Right block
    INC JetXPos
    LDA JET_HEIGHT          ; 9
    STA JetAnimOffset       ; set animation offset to the second frame

EndInputCheck:              ; fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

UpdateBomberPosition:
    LDA BomberYPos
    CLC                         ; clears carry flag
    CMP #0                      ; compare bomber y-position with 0
    BMI .ResetBomberPosition    ; if it is < 0, then reset y-position to the top
    DEC BomberYPos              ; else, decrement enemy y-position for next frame
    JMP EndPositionUpdate

.ResetBomberPosition
    ;LDA #96
    ;STA BomberYPos             ; TODO: set bomber X position to random number
    jsr GetRandomBomberPos      ; call subroutine for random bomber position                              


EndPositionUpdate:              ; fallback for the position update code


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHECK FOR OBJECT COLLISION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    LDA #%10000000              ;CXPPMM bit 7 detects P0 and P1 collision
    BIT CXPPMM                  ;check CXPPMM bit 7 against the above pattern
    BNE .CollisionP0P1          ;if there is a collision between P0 and P1, branch to CollisionP0P1
    JMP CheckCollisionP0PF      ;else skip to next check
.CollisionP0P1:
    JSR GameOver                ;jump to subroutine GameOver

CheckCollisionP0PF:
    LDA #%10000000              
    BIT CXP0FB                  ;CXP0FB bit 7 detects P0 and PF collision
    BNE .CollisionP0PF          ;if collision P0 and PF happened, branch
    JMP EndCollisionCheck       ;else, skip to next check

.CollisionP0PF:
    JSR GameOver                ;jump to subroutine GameOver

EndCollisionCheck:
    STA CXCLR                   ;clear all collision flags before the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LOOP BACK TO START A  NEW FRAME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    JMP StartFrame      ; continue to display the next frame
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of our object
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
SetObjectXPos subroutine
    STA WSYNC               ; start a fresh new scanline
    SEC                     ; make sure carry-flag is set before subtracion
.Div15Loop
    SBC #15                 ; subtract 15 from accumulator
    BCS .Div15Loop          ; loop until carry-flag is clear
    EOR #7                  ; handle offset range from -8 to 7 ------- check this
    ASL                     ;                                  ------- and this
    ASL
    ASL
    ASL                     ; four shift lefts to get only the top 4 bits
    STA HMP0,Y              ; store the fine offset to the correct HMxx
    STA RESP0,Y             ; fix object position in 15-step increment
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    LDA #$30                ;COLOR RED
    STA COLUBK              ;SET THE BACKGROUND TO RED
    RTS

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate a Linear-Feedback Shift Register random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a LFSR random number for the X-position of the bomber.
;; Divide the random value by 4 to limit the size of the result to match river.
;; Add 30 to compensate for the left green playfield
;; The routine also sets the Y-position of the bomber to the top of the screen.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetRandomBomberPos subroutine
    LDA Random
    ASL                 ;Arithmetic Shift Left
    EOR Random
    ASL
    EOR Random
    ASL
    ASL
    EOR Random
    ASL
    ROL Random          ; performs a series of shifts and bit operations
    
    LSR                 ; Logical Shift Right
    LSR                 ; divide the value by 4 with 2 right shifts
    STA BomberXPos      ; save it to the variable BomberXPos
    LDA #30             
    ADC BomberXPos      ; adds 30 + BomberXPos to compensate for left PF
    STA BomberXPos      ; and sets the new value to the bomber x-position
    
    LDA #96
    STA BomberYPos      ; set the y-position to the top of the screen
    
    RTS




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                ; move to position $FFFC
    word Reset               ; write 2 bytes with the program reset address
    word Reset               ; write 2 bytes with the interruption vector
