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
    ComberColorPtr  word        ; pointer to player1 color lookup table




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
    STA BomberSpritePtr     ; lo-byte pointer for enemy sprite lookup table
    LDA #>BomberSprite
    STA BomberSpritePtr+1   ; hi-byte pointer for enemy sprite lookup table
    
    LDA #<BomberColor
    STA BomberColorPtr      ; lo-byte pointer for enemy color lookup table
    LDA #>bomberColor
    STA BomberColorPtr+1    ; hi-byte pointer for enemy color lookup table

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
; Display the 192 visible scanlines of our main game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    LDA #84
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
    
    LDX #196            ; X counts the number of remaining scanlines
    
.GameLineLoop:
.AreWeInsideJetSprite:      ; check if should render sprite player0
    TXA                     ; transfer X to A
    SEC                     ; make sure carry flag is set
    SBC JetYPos             ; subtract sprite Y coordinate
    CMP JET_HEIGHT          ; are we inside the sprite height bounds?
    BCC .DrawSpriteP0       ; if result < SpriteHeight, call subroutine
    LDA #0                  ; else, set lookup index to 0

.DrawSpriteP0
    TAY                     ; load Y so we can work with pointer
    LDA (JetSpritePtr),Y    ; load player bitmap slice of data
    STA WSYNC               ; wait for next scanline
    STA GRP0                ; set graphics for player 0
    LDA (JetColorPtr),Y     ; load player color from lookup table
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
    STA NUSIZ1                  ; stretch player1 sprite
    LDA (BomberSpritePtr),Y     ; load player bitmap slice of data
    STA WSYNC                   ; wait for next scanline
    STA GRP1                    ; set graphics for player 0
    LDA (BomberColorPtr),Y      ; load player color from lookup table
    STA COLUP1                  ; set color for player 0 slice
    
    DEX                         ; X--
    BNE .GameLineLoop           ; repeat next main game scanline while X != 0

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;