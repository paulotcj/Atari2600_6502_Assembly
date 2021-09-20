	PROCESSOR 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include external files containing useful definitions and macros 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	INCLUDE "vcs.h"
	INCLUDE "macro.h"

	SEG CODE
	ORG $F000      ; Define the origin of the ROM at $F000
	
    CLEAN_START    ; Call macro to safely clear the memory
START:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set background luminance color to yellow (NTSC color code $1E)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    LDA #$1E       ; Load color code into A register
    STA COLUBK     ; Store A to memory address $09 (TIA COLUBK)

    JMP START      ; Repeat from START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill ROM size to exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ORG $FFFC      ; Defines origin to $FFFC
    .WORD START    ; Reset vector at $FFFC (where program starts)
    .WORD START    ; Interrupt vector at $FFFE (unused by the VCS)
