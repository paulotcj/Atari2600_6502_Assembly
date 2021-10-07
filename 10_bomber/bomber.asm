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

JetXPos         byte        ; player0 x-position                - 8bits
JetYPos         byte        ; player0 y-position
BomberXPos      byte        ; player1 x-position
BomberYPos      byte        ; player1 y-position
JetSpritePtr    word        ; pointer to player0 sprite lookup table - 16 bits
JetColorPtr     word        ; pointer to player0 color lookup table
BomberSpritePtr word        ; pointer to player1 sprite lookup table
BomberColorPtr  word        ; pointer to player1 color lookup table
JetAnimOffset   byte        ; player0 frame offset for sprite animation



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9              ; player0 sprite height (# rows in lookup table)
BOMBER_HEIGHT = 9           ; player1 sprite height (# rows in lookup table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    SEG Code
    ORG $F000

Reset:
    CLEAN_START         ;Clean the memory AND the TIA
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #10
    STA JetYPos         ;JetYPos = 10
    LDA #60
    STA JetXPos         ;JetXPos = 60
    LDA #83
    STA BomberYPos      ; BomberYPos = 83
    LDA #54
    STA BomberXPos      ; BomberXPos = 54

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialize pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDA #<JetSprite
    STA JetSpritePtr        ; lo-byte pointer for jet sprite lookup table
    LDA #>JetSprite
    STA JetSpritePtr+1      ; hi-byte pointer for jet sprite lookup table
    
    LDA #<JetColor
    STA JetColorPtr         ; lo-byte pointer for jet color lookup table
    LDA #>JetColor
    STA JetColorPtr+1       ; hi-byte pointer for jet color lookup table
    
    LDA #<BomberSprite
    STA BomberSpritePtr     ; lo-byte pointer for bomber sprite lookup table
    LDA #>BomberSprite
    STA BomberSpritePtr+1   ; hi-byte pointer for bomber sprite lookup table
    
    LDA #<BomberColor
    STA BomberColorPtr      ; lo-byte pointer for bomber color lookup table
    LDA #>BomberColor
    STA BomberColorPtr+1    ; hi-byte pointer for bomber color lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Calculations and tasks performed in the pre-VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDA JetXPos
    LDY #0
    JSR SetObjectXPos       ; set player0 horizontal position - Jump to SubRoutine
    
    LDA BomberXPos
    LDY #1
    JSR SetObjectXPos       ; set player1 horizontal position - Jump to SubRoutine
    
    STA WSYNC
    STA HMOVE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #2
    STA VBLANK      ; turn on VBLANK
    STA VSYNC       ; turn on VSYNC
    
    REPEAT 3
        STA WSYNC   ; display 3 recommended lines of VSYNC
    REPEND
    LDA #0          ; turn off VSYNC
    STA VSYNC
    
    REPEAT 37
        STA WSYNC   ; display the 37 recommended lines of VBLANK
    REPEND
    LDA #0
    STA VBLANK      ; turn off VBLANK


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display the 96 visible scanlines of our main game (because 2-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    LDA #$84
    STA COLUBK          ; set background/river color to blue
    LDA #$C2
    STA COLUPF          ; set playfield/grass color to green
    LDA #%00000001
    STA CTRLPF          ; enable playfield reflection
    LDA #$F0
    STA PF0             ; setting PF0 bit pattern
    LDA #$FC
    STA PF1             ; setting PF1 bit pattern
    LDA #0
    STA PF2             ; setting PF2 bit pattern
    
    LDX #96            ; X counts the number of remaining scanlines
    
.GameLineLoop:
.AreWeInsideJetSprite:      ; check if should render sprite player0
    TXA                     ; transfer X to A
    SEC                     ; make sure carry flag is set before subtraction
    SBC JetYPos             ; subtract sprite Y coordinate
    CMP JET_HEIGHT          ; are we inside the sprite height bounds?
    BCC .DrawSpriteP0       ; if result < SpriteHeight, call the draw routine - branch on carry clear
    LDA #0                  ; else, set lookup index to zero

.DrawSpriteP0
    CLC                     ; clear carry flag before addition
    ADC JetAnimOffset       ; jump to correct sprite frame address in memory
    TAY                     ; load Y so we can work with pointer
    LDA (JetSpritePtr),Y    ; load player bitmap slice of data
    STA WSYNC               ; wait for next scanline
    STA GRP0                ; set graphics for player 0
    LDA (JetColorPtr),Y     ; load player color from lookup table
    STA COLUP0              ; set color for player 0 slice


.AreWeInsideBomberSprite:   ; check if should render sprite player1
    TXA                     ; transfer X to A
    SEC                     ; make sure carry flag is set before subtraction
    SBC BomberYPos          ; subtract sprite Y coordinate
    CMP BOMBER_HEIGHT       ; are we inside the sprite height bounds?
    BCC .DrawSpriteP1       ; if result < SpriteHeight, call subroutine
    LDA #0                  ; else, set index to 0

.DrawSpriteP1:
    TAY                         ; load Y so we can work with the pointer
    LDA #%0000101
    STA NUSIZ1                  ; stretch player1 sprite
    LDA (BomberSpritePtr),Y     ; load player bitmap slice of data
    STA WSYNC                   ; wait for next scanline
    STA GRP1                    ; set graphics for player 0
    LDA (BomberColorPtr),Y      ; load player color from lookup table
    STA COLUP1                  ; set color for player 0 slice
    
    DEX                         ; X--
    BNE .GameLineLoop           ; repeat next main game scanline while X != 0
    
    LDA #0
    STA JetAnimOffset           ; reset jet animation frame to zero each frame
    
    STA WSYNC                   ; wait for final scanline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDA #2              ; turn on VBLANK
    STA VBLANK
    REPEAT 30
        STA WSYNC       ; display 30 recommended lines of VBlank Overscan
    REPEND
    LDA #0
    STA VBLANK          ; turn off VBLANK
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Process joystick input for player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
CheckP0Up:
    LDA #%00010000          ; player0 joystick up
    BIT SWCHA
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
    LDA #%01000000          ; player0 joystick Left
    BIT SWCHA
    BNE CheckP0Right        ; if bit pattern doesnt match, bypass Left block
    DEC JetXPos
    LDA JET_HEIGHT          ; 9
    STA JetAnimOffset       ; set animation offset to the second frame

CheckP0Right:
    LDA #%10000000          ; player0 joystick RIGHT
    BIT SWCHA
    BNE EndInputCheck       ; if bit pattern doesnt match, bypass Right block
    INC JetXPos
    LDA JET_HEIGHT          ; 9
    STA JetAnimOffset       ; set animation offset to the second frame

EndInputCheck:              ; fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    LDA BomberYPos
    CLC
    CMP #0                      ; compare bomber y-position with 0
    BMI .ResetBomberPosition    ; if it is < 0, then reset y-position to the top
    DEC BomberYPos              ; else, decrement enemy y-position for next frame
    JMP EndPositionUpdate
    
.ResetBomberPosition    
    LDA #96
    STA BomberYPos
                                ; TODO: set bomber X position to random number
EndPositionUpdate:              ; fallback for the position update code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    JMP StartFrame      ; continue to display the next frame
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A is the target x-coordinate position in pixels of our object
; Y is the object type (0:player0, 1:player1, 2:missile0, 3:missile1, 4:ball)

SetObjectXPos SUBROUTINE
    STA WSYNC               ; start a fresh new scanline
    SEC                     ; make sure carry-flag is set before subtracion
.Div15Loop
    SBC #15                 ; subtract 15 from accumulator
    BCS .Div15Loop          ; loop until carry-flag is clear
    EOR #7                  ; handle offset range from -8 to 7
    ASL
    ASL
    ASL
    ASL
    STA HMP0,Y              ; four shift lefts to get only the top 4 bits
    STA RESP0,Y             ; store the fine offset to the correct HMxx
    RTS                     ; fix object position in 15-step increment



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ORG $FFFC       ; move to position $FFFC
    WORD Reset      ; write 2 bytes with the program reset address
    WORD Reset      ; write 2 bytes with the interruption vector
