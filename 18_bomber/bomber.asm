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

JetXPos         byte        ; player 0 x-position
JetYPos         byte        ; player 0 y-position
BomberXPos      byte        ; player 1 x-position
BomberYPos      byte        ; player 1 y-position
MissileXPos     byte         ; missile x-position
MissileYPos     byte         ; missile y-position
Score           byte        ; 2-digit score stored as BCD
Timer           byte        ; 2-digit timer stored as BCD
Temp            byte        ; auxiliary variable to store temp values
OnesDigitOffset word        ; lookup table offset for the score Ones digit
TensDigitOffset word        ; lookup table offset for the score Tens digit
JetSpritePtr    word        ; pointer to player0 sprite lookup table
JetColorPtr     word        ; pointer to player0 color lookup table
BomberSpritePtr word        ; pointer to player1 sprite lookup table
BomberColorPtr  word        ; pointer to player1 color lookup table
JetAnimOffset   byte        ; player0 frame offset for sprite animation
Random          byte        ; used to generate random bomber x-position
ScoreSprite     byte        ; store the sprite bit pattern for the score
TimerSprite     byte        ; store the sprite bit pattern for the timer
TerrainColor    byte        ; store the color of the terrain playfield
RiverColor      byte        ; store the color of the river playfield

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9               ; player0 sprite height (# rows in lookup table)
BOMBER_HEIGHT = 9            ; player1 sprite height (# rows in lookup table)
DIGITS_HEIGHT = 5            ; scoreboard digit height (#rows in lookup table)

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

    LDA #0
    STA Score           ;Score = 0

    STA Timer           ;Timer = 0
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare a MACRO to check if we should display the missile 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        LDA #%00000000
        CPX MissileYPos      ; compare X (current scanline) with missile Y pos
        BNE .SkipMissileDraw ; if (X != missile Y position), then skip draw
.DrawMissile:                ; else:
        LDA #%00000010       ;     enable missile 0 display
        INC MissileYPos      ;     MissileYPos++
.SkipMissileDraw:
        STA ENAM0            ; store correct value in the TIA missile register
    ENDM    

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
    REPEAT 31
        STA WSYNC   ; display the recommended lines of vblank
    REPEND


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the pre-VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDA JetXPos
    LDY #0
    JSR SetObjectXPos           ; set player0 horizontal position - jump subroutine
    
    LDA BomberXPos
    LDY #1
    JSR SetObjectXPos           ; set player1 horizontal position

    LDA MissileXPos
    LDY #2
    JSR SetObjectXPos           ; set missile horizontal position    
    
    JSR CalculateDigitOffset    ; calculate scoreboard digits lookup table offset

    JSR GenerateJetSound        ; configure and enable our jet engine audio

    STA WSYNC
    STA HMOVE                   ; apply the horizontal offsets previously set

    LDA #0
    
    STA VBLANK                  ; TURN OFF VBLANK
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

    LDA #0                  ; clear TIA registers before each new frame
    STA COLUBK
    STA PF0
    STA PF1
    STA PF2
    STA GRP0
    STA GRP1

    STA CTRLPF
    LDA #$1E
    STA COLUPF              ; set the scoreboard playfield color with yellow


    LDX #DIGITS_HEIGHT      ; start X counter with 5 (height of digits)

.ScoreDigitLoop
    LDY TensDigitOffset     ; get the tens digit offset for the Score
    LDA Digits,Y           ;Digits is a area/lookup table, Y is a register
    AND #%11110000          ; mask/remove the graphics for the ones digit
    STA ScoreSprite         ; save the score tens digit pattern in a variable

    LDY OnesDigitOffset     ; get the ones digit offset for the Score
    LDA Digits,Y           ; load the digit bit pattern from lookup table
    AND #%00001111          ; mask/remove the graphics for the tens digit
    ORA ScoreSprite         ; merge it with the saved tens digit sprite
    STA ScoreSprite         ; and save it
    STA WSYNC               ; wait for the end of scanline
    STA PF1                 ; update the playfield to display the Score sprite

    LDY TensDigitOffset+1   ; get the left digit offset for the Timer
    LDA Digits,Y           ; load the digit pattern from lookup table
    AND #%11110000          ; mask/remove the graphics for the ones digit
    STA TimerSprite         ; save the timer tens digit pattern in a variable

    LDY OnesDigitOffset+1   ; get the ones digit offset for the Timer
    LDA Digits,Y           ; load digit pattern from the lookup table
    AND #%00001111          ; mask/remove the graphics for the tens digit
    ORA TimerSprite         ; merge with the saved tens digit graphics
    STA TimerSprite         ; and save it

    JSR Sleep12Cycles       ; wait for 12 cycles

    STA PF1                 ; update the playfield for Timer display

    LDY ScoreSprite         ; preload for the next scanline
    STA WSYNC               ; wait for next scanline

    STY PF1                 ; update playfield for the score display

    INC TensDigitOffset
    INC TensDigitOffset+1
    INC OnesDigitOffset
    INC OnesDigitOffset+1   ; increment all digits for the next line of data

    JSR Sleep12Cycles       ; wait for 12 cycles

    DEX                     ; X--
    STA PF1                 ; update the playfield for the Timer display
    BNE .ScoreDigitLoop     ; if dex != 0, then branch to ScoreDigitLoop

    STA WSYNC

    LDA #0
    STA PF0
    STA PF1
    STA PF2
    STA WSYNC
    STA WSYNC
    STA WSYNC




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the remaining visible scanlines of our main game (2-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GameVisibleLine:
    LDA TerrainColor
    STA COLUPF               ; set playfield/grass color to green

    LDA RiverColor
    STA COLUBK               ; set background/river color to blue

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
    LDX #89                 ; X COUNTS THE NUMBER OF REMAINING SCANLINES
.GameLineLoop:
    DRAW_MISSILE            ; macro to check if we should draw the missile
.AreWeInsideJetSprite:      ; check if should render sprite player0
    TXA                     ; transfer X to A
    SEC                     ; make sure carry flag is set before subtraction
    SBC JetYPos             ; subtract sprite Y-coordinate
    CMP #JET_HEIGHT          ; are we inside the sprite height bounds?
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
    CMP #BOMBER_HEIGHT       ; are we inside the sprite height bounds?
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

    LDA JetYPos
    CMP #70                  ; if (player0 Y position > 70)
    BPL CheckP0Down          ;    then: skip increment (BRANCH ON PLUS)
.P0UpPressed:    
    INC JetYPos              ;    else: increment Y position
    LDA #0
    STA JetAnimOffset        ; set jet animation frame to zero


CheckP0Down:
    LDA #%00100000           ; if player 0 joystick down
    BIT SWCHA
    BNE CheckP0Left
    LDA JetYPos
    CMP #5                   ; if (player0 Y position < 5)
    BMI CheckP0Left          ; BRANCH MINUS   then: skip decrement
.P0DownPressed:    
    DEC JetYPos              ;    else: decrement Y position
    LDA #0
    STA JetAnimOffset        ; set jet animation frame to zero

CheckP0Left:
    LDA #%01000000           ; if player 0 joystick left
    BIT SWCHA
    BNE CheckP0Right
    LDA JetXPos
    CMP #35                  ; if (player0 X position < 35)
    BMI CheckP0Right         ;    then: skip decrement
.P0LeftPressed:    
    DEC JetXPos              ;    else: decrement X position
    LDA #JET_HEIGHT
    STA JetAnimOffset        ; set new offset to display second sprite frame

CheckP0Right:
    LDA #%10000000           ; if player 0 joystick right
    BIT SWCHA
    BNE CheckButtonPressed
    LDA JetXPos
    CMP #100                 ; if (player0 X position > 100)
    BPL CheckButtonPressed        ;    then: skip increment
.P0RightPressed:    
    INC JetXPos              ;    else: increment X position

    LDA #JET_HEIGHT
    STA JetAnimOffset        ; set new offset to display second sprite frame

CheckButtonPressed:
    LDA #%10000000           ; if button is pressed
    BIT INPT4
    BNE EndInputCheck
.ButtonPressed:
    LDA JetXPos
    CLC
    ADC #5
    STA MissileXPos          ; set the missile X position equal to the player 0
    LDA JetYPos
    CLC
    ADC #8
    STA MissileYPos          ; set the missile Y position equal to the player 0    

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

.ResetBomberPosition:
    JSR GetRandomBomberPos      ; call subroutine for random bomber position

.SetScoreValues:
    SED                         ; set BCD mode for score and timer values
    ; LDA Score
    ; CLC                         ; CLEAR CARRY
    ; ADC #1                      ; ADD WITH CARRY
    ; STA Score                   ; add 1 to the Score (BCD does not like INC)
    LDA Timer
    CLC
    ADC #1
    STA Timer                   ; add 1 to the Timer (BCD does not like INC)
    CLD                         ; disable BCD after updating Score and Timer   (CLEAR DECIMAL)                         


EndPositionUpdate:              ; fallback for the position update code


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHECK FOR OBJECT COLLISION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    LDA #%10000000              ;CXPPMM bit 7 detects P0 and P1 collision
    BIT CXPPMM                  ;check CXPPMM bit 7 against the above pattern
    BNE .P0P1Collided           ;if there is a collision between P0 and P1, branch to CollisionP0P1
    JSR SetGreenBlueTerrain    ; else, set playfield color to green/blue
    JMP CheckCollisionM0P1      ; check next possible collision

.P0P1Collided:
    JSR GameOver                ;jump to subroutine GameOver


CheckCollisionM0P1:
    LDA #%10000000           ; CXM0P bit 7 detects M0 and P1 collision
    BIT CXM0P                ; check CXM0P bit 7 with the above pattern
    BNE .M0P1Collided        ; collision missile 0 and player 1 happened
    JMP EndCollisionCheck
.M0P1Collided:
    SED
    LDA Score
    CLC
    ADC #1
    STA Score                ; adds 1 to the Score using decimal mode
    CLD                      ; CLEAR DECIMAL
    LDA #0
    STA MissileYPos          ; reset the missile position    

EndCollisionCheck:              ; fallback
    STA CXCLR                   ; clear all collision flags before the next frame    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LOOP BACK TO START A  NEW FRAME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    JMP StartFrame      ; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate audio for the jet engine sound based on the jet y-position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The frequency/pitch will be modified based on the jet current y-position.
;; Normally, the TIA audio frequency goes from 0 (highest) to 31 (lowest).
;; We subtract 31 - (JetYPos/8) to achieve the desired final pitch value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GenerateJetSound subroutine
    LDA #3
    STA AUDV0                ; set the audio volume register

    LDA #8
    STA AUDC0                ; set the audio control register to white noise

    LDA JetYPos              ; loads the accumulator with the jet y-position
    LSR
    LSR
    LSR                      ; divide the accumulator by 8 (using right-shifts)
    STA Temp                 ; save the Y/8 value in a temp variable
    LDA #31
    SEC
    SBC Temp                 ; subtract 31-(Y/8)
    STA AUDF0                ; set the audio frequency/pitch register

    RTS    
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the colors for the terrain and river to green & blue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetGreenBlueTerrain subroutine
    LDA #$C2
    STA TerrainColor        ; set terrain color to green
    LDA #$84
    STA RiverColor          ; set river color to blue
    RTS                     ; return subroutine     
    

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
    EOR #7                  ; handle offset range from -8 to 7
    ASL
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
    STA TerrainColor        ; set terrain color to red
    STA RiverColor          ; set river color to red
    LDA #0
    STA Score               ; Score = 0
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
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The scoreboard is stored using BCD, so the display shows hex numbers.
;; This converts the high and low nibbles of the variable Score and Timer
;; into the offsets of digits lookup table so the values can be displayed.
;; Each digit has a height of 5 bytes in the lookup table.
;;
;; For the low nibble we need to multiply by 5
;;   - we can use left shifts to perform multiplication by 2
;;   - for any number N, the value of N*5 = (N*2*2)+N
;;
;; For the upper nibble, since its already times 16, we need to divide it
;; and then multiply by 5:
;;   - we can use right shifts to perform division by 2
;;   - for any number N, the value of (N/16)*5 is equal to (N/4)+(N/16)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    LDX #1                      ; X register is the loop counter

.PrepareScoreLoop               ; this will loop twice, first X=1, and then X=0
    LDA Score,X                 ; load A with Timer (X=1) or Score (X=0)
    AND #%00001111              ; remove the tens digit by masking 4 bits 00001111
    STA Temp                    ; save the value of A into Temp
    ASL                         ; shift left (it is now N*2) -------> effectively we are multiplying by 5
    ASL                         ; shift left (it is now N*4)
    ADC Temp                    ; add the value saved in Temp (+N)
    STA OnesDigitOffset,X       ; save A in OnesDigitOffset+1 or OnesDigitOffset

    LDA Score,X                 ; load A with Timer (X=1) or Score (X=0)
    AND #%11110000              ; remove the ones digit by masking 4 bits 11110000
    LSR                         ; shift right (it is now N/2)
    LSR                         ; shift right (it is now N/4)
    STA Temp                    ; save the value of A into Temp
    LSR                         ; shift right (it is now N/8)
    LSR                         ; shift right (it is now N/16)
    ADC Temp                    ; add the value saved in Temp (N/16+N/4)
    STA TensDigitOffset,X       ; store A in TensDigitOffset+1 or TensDigitOffset

    DEX                         ; X--
    BPL .PrepareScoreLoop       ;branch on plus - while X >= 0, loop to pass a second time

    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to wait for 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

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
