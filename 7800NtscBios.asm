; The NTSC 7800 BIOS source code
;
; Comments added by Keith Henrickson <flipper@phin.com>
; Further commented by Daniel Boris <dboris@home.com>
; They may be wrong. Use at your own risk.  Modification suggested for 
; bypassing authentication scheme is at this date untested.
; Fixed to compile correctly with dasm, and additional comments by Mike Saarna.
;

 processor 6502

; some misc equates
LFD80 = $FD80
CartKey           = $FF80
CartRegion        = $FFF8
CartKeyStartPage  = $FFF9
CartResetVectorHi = $FFFD
LF112 = $F112
LF118 = $F118
LF460   =   $F460
LF700   =   $F700
LF800   =   $F800


; 7800 system

INPTCTRL = $01

; TIA

VSYNC   =  $00
VBLANK  =  $01
WSYNC   =  $02
RSYNC   =  $03
COLUP0  =  $06
COLUPF  =  $08
COLUBK  =  $09
PF2     =  $0F
RESP1   =  $11
GRP0    =  $1B
GRP1    =  $1C

;Maria

BACKGRND =  $20
POC1     =  $21
P0C2     =  $22
POC3     =  $23
MWSYNC   =  $24
P1C1     =  $25
P1C2     =  $26
P1C3     =  $27
MSTAT    =  $28
DPPH     =  $2C
DPPL     =  $30
CTRL     =  $3C

DLIAddr       =  $F4
ConsoleRegion = $FE

       ORG $F000

LF000: pha                ; This is the NMI routine.
       jmp (DLIAddr)      ; RAM NMI vector


       ORG $F400

; this code is designed to be executed while stored in ram at 2300.
LF400
SwitchTo2600Mode1
       jmp    $26C2                   ; Enter 2600 mode.
SwitchTo2600Mode2
       jmp    $26C2                   ; Enter 2600 mode.

CartTest
       lda    #$16                    ; ($2306)
       sta    INPTCTRL                ; Switch in cart + enable Maria

       ldy    #$FF                    ;
       ldx    #$7F                    ;
CartTestLoop
       lda    LFE00,X                 ; Compare same FE7F+ ROM with differing opcodes, 
       cmp    LFD80,Y                 ;    to avoid false positives from a floating bus.
       bne    SwitchTo2600Mode1       ; Switch to 2600 mode
       dey                            ;
       dex                            ;
       bpl    CartTestLoop

       lda    CartResetVectorLo       ;
       and    CartResetVectorHi       ;
       cmp    #$FF                    ;
       beq    SwitchTo2600Mode1       ; If RESET vector is FFFF, then go 2600.
       lda    CartResetVectorLo       ;
       ora    CartResetVectorHi       ;
       beq    SwitchTo2600Mode1       ; If RESET vector is 0000, then go 2600.
       lda    CartRegion              ; "Region verification"
       ora    #ConsoleRegion
       cmp    #$FF                    ; 
       bne    SwitchTo2600Mode2       ; If low-bit of CartRegion=0, then go 2600.
       lda    CartRegion              ;
       eor    #$F0                    ; Invert high nibble
       and    #$F0                    ; Extract high nibble
       bne    SwitchTo2600Mode2       ; If high nibble was not F, then go 2600.
       lda    CartKeyStartPage        ;
       and    #$0B                    ; 
       cmp    #$03                    ; 
       bne    SwitchTo2600Mode2       ; If low nibble FFF9 was not 3 or 7, go 2600.
       lda    CartKeyStartPage        ;
       and    #$F0                    ; Extract ROM start from high nibble
       sta    $EE                     ; Store it 
       sta    $2406                   ; Store it a couple of places for auth. (whocares)
       cmp    #$40                    ;
       bcc    SwitchTo2600Mode2       ; If ROM start < than 4, then go 2600
       sbc    #$01                    ; Subtract 1.
       cmp    CartResetVectorHi       ; High byte of reset vector
       bcs    SwitchTo2600Mode2       ; If high-byte of start is >=, go 2600
       jsr    $2536                   ; Start the authentication process.

       lda    #$00
       sta    $F0     
       jsr    $241B   
       lda    #$16    
       sta    INPTCTRL  
       ldx    #$00    
       txa            
LF46D: sta    $1800,X 
       dex            
       bne    LF46D   
       pha            
       ldy    #$7F    
LF476: lda    LFF00,Y 
       sta    $1800,Y 
       dey            
       cpy    #$F8    
       bne    LF476   
       lda    #$2E    
       sta    $2409   
       lda    #$24    
       sta    $240A   

LF48B
       jsr    $241B   
       pla            
       jsr    $23FF   
       pha            
       inc    $2406   
       lda    $2406   
       cmp    #$FF    
       bne    LF48B   

       jsr    $241B   
       jsr    $2412   
       jsr    $2412   
       lda    #$36    
       sta    $2409   
       lda    #$24    
       sta    $240A   
       dec    $2406   
LF4B3: jsr    $241B   
       pla            
       jsr    $23FF   
       pha            
       dec    $2406   
       lda    $2406   
       cmp    $EE     
       bcs    LF4B3   
       lda    #$60    
       sta    CTRL   
       ldx    #$77    
LF4CB: lda    $1800,X 
       eor    $1850,X 
       eor    $1888,X 
       sta    $1A00,X 
       dex            
       bpl    LF4CB   
       lda    $1A00   
       and    #$07    
       sta    $1A00   
       lda    #$00    
       ldx    #$04    
       sta    $1A00,X 
       sta    $2000,X 
       ldx    #$77         ; That was a lot, but we end up here, comparing
LF4EE: lda    $2000,X      ; two parts of memory.  If they match, then
       cmp    $1A00,X      ; go to 7800 mode....
       bne    LF4FC        ; otherwise go to 2600 mode.
       dex                 ; nop out above branch to allow user-written
       bpl    LF4EE        ; carts to run -- I think.
       jmp    $26B9        ; 7800 mode init
LF4FC: jmp    $26C2        ; 2600 mode init.


LF500 =  $F500             ; F500 is partway through the next opcode
       ldx    #$00         ; more authentication stuff all the way through
LF501: adc    $1800,X
       adc    LFF00,X 
       tay            
       lda    $2DD5,Y 
       sta    $1800,X 
       inx            
       bne    LF501   
       rts            

       ldx    #$00    
LF514: rol    $1800,X 
       inx            
       bne    LF514   
       rts            

;$241B
       php            
       dec    $F0     
       bpl    LF52C   
       lda    #$02    
       sta    INPTCTRL  
LF524: lda    $F0     
       bmi    LF524   
       lda    #$16    
       sta    INPTCTRL  
LF52C: plp            
       rts            

LF52E: .byte $C7,$65,$AB,$CA,$EE,$F7,$83,$09
LF536: .byte $E1,$D0,$92,$67,$62,$B6,$72,$55,$8E,$91,$DC,$C5,$81,$BE,$78,$20
       .byte $59,$B7,$E6,$3D,$06,$45,$AF,$C8,$08,$31,$38,$D1,$FB,$73,$84,$A9
       .byte $17,$FC,$34,$87,$A3,$94,$FA,$90,$B8,$ED,$CE,$3B,$5B,$0A,$43,$D9
       .byte $F3,$53,$82,$B3,$0D,$6D,$5A,$60,$9D,$51,$A7,$B9
LF572: .byte $11,$10,$BC,$E4,$7F,$80,$41,$E7,$E3
LF57B: .byte $F6,$56,$26,$35,$EC,$D6,$DF,$0C,$7F,$F4,$9E,$AC,$52,$46,$EF,$CF
       .byte $BF,$A2,$3F,$A4,$13,$15,$97,$4A,$1C,$B0,$42,$8C,$B1,$05,$58,$80
       .byte $18,$77,$2B,$02,$3E,$A8,$49,$1A,$6A
LF5A4: .byte $CB,$6E,$0B,$8A,$EB,$F1,$4F,$14,$79,$8B,$D8,$9F,$9B,$57,$19,$F8
       .byte $2A,$2D,$76,$0E,$E8,$2E,$4B,$F9,$07,$03,$DE,$93,$16,$7E,$D4,$E5
       .byte $B2,$F0,$7D,$7A,$DA,$D2,$A1,$CC,$1D,$E0,$5E,$23,$A0,$95,$22,$1E
       .byte $36,$85,$FE,$1F,$39
LF5D9: .byte $AA,$89,$96,$AD,$0F,$2F,$C0,$47
LF5E1: .byte $27,$5D,$24,$EA,$C3,$A5,$F5,$21,$5F,$1B,$40,$8F,$AE,$74,$25,$DD
       .byte $C1,$7C,$CD,$A6,$70,$D7,$33,$7B,$2C,$75,$BB,$86,$99,$BD,$54
LF600: .byte $9A,$6C,$63,$32,$48,$4C,$8D,$BA
LF608: .byte $5C,$61,$C4,$4E,$29,$37,$12,$C6,$98,$9C,$D5,$69,$6B,$E2,$04,$4D
       .byte $E9,$C2,$88,$3A,$DB,$64,$01,$44,$6F,$B5,$F2,$30,$28,$FD,$50,$71
       .byte $3C,$B4,$66,$68,$C9,$D3,$CA,$83,$C7,$AB,$F7,$65,$09,$EE

       ldx    #$77      ;($2536)
       stx    $E4     
       stx    $E5     
LF63C: lda    CartKey,X   ;Read Key
       sta    $1901,X           ;Store it
       sta    $2000,X           ;Store it again
       dex                      ;next byte of key
       bpl    LF63C             ;Continue

       lda    #$02      ;
       sta    INPTCTRL  ;Disable cart 
       jsr    LFB84     ;Init display
       jsr    $257B   
       dec    $F2     
       ldx    #$77    
       stx    $E4     
LF658: lda    LFED5,X 
       sta    $1901,X 
       dex            
       bpl    LF658   
       lda    $E1     
       sta    $E3     
       jsr    $25E1   
       dec    $F2     
LF66A: lda    $E0     
       sta    $2572   
       ldx    #$77    
LF671: lda    $1800,X 
LF674: sta    $2000,X 
       dex            
       bpl    LF671   
       rts            
    
       jsr    $2639   ;($257B)
       ldy    $E5     
       iny            
LF681: STY    $E1     
       TYA            
       clc            
       adc    $E2     
       pha            
LF688: tax            
       lda    #$00    
       sta    $2671   
LF68E: sta    $1800,X 
       dex            
LF692: bne    LF68E   
       sta    $1800   
LF697: iny            
       STY    $266E   
       STY    $2674   
       STY    $267C   
       STY    $2681   
       ldx    #$00    
LF6A6: dec    $266E   
LF6A9: dec    $2674   
LF6AC: dec    $267C   
       dec    $2681   
       dec    $E1     
       bmi    LF6D1   
LF6B6: ldy    $E1     
       lda    $2000,Y 
       and    $25D9,X 
       beq    LF6C9   
       lda    $2662,X 
       sta    $2672   
       jsr    $266A   
LF6C9: inx            
       cpx    #$08    
       bmi    LF6B6   
       jmp    $25A4   
LF6D1: pla            
       sta    $E1     
       lda    #$01    
       sta    $E0     
       rts            

LF6D9: .byte $01,$02,$04,$08,$10,$20,$40,$80

       jsr    $2639
       lda    $E3     
       sec            
       sbc    $E4     
       sta    $E0     
       sta    $E1     
       ldx    #$00    
       stx    $1800   
       stx    $268F   
       stx    $26AC   
       dex            
       stx    $26A9   
       stx    $268C   
       stx    $2692   
       stx    $269A   
       stx    $269F   
       ldx    #$07    
       inc    $26A9   
       inc    $268C   
       inc    $2692   
       inc    $269A   
       inc    $269F   
       dec    $E1     
       bmi    LF734   
LF71D: lda    $2662,X 
       sta    $2690   
       sta    $26AD   
       jsr    $26A6   
       bcc    LF72E   
       jsr    $2688   
LF72E: dex            
       bpl    LF71D   
       jmp    $2608   
LF734: lda    $E3     
       sta    $E1     
       rts            

       ldx    $E4   ;($2639)
       inx            
       stx    $E2     
       ldy    #$00    
       STY    $1900   
LF743: lda    $2662,Y 
       sta    $2655   
       iny            
       lda    $2662,Y 
       sta    $2659   
       ldx    $E2     
       clc            
LF753: lda    $1900,X 
       rol
       sta    $1900,X 
       dex            
       bpl    LF753   
       cpy    #$07    
       bmi    LF743   
       rts            

LF762: .byte $19,$1A,$1B,$1C,$1D,$1E,$1F,$21

       ldy    $E2
       clc            
LF76D: lda    $1800,Y 
       adc    $1900,Y 
       sta    $1800,Y 
       dey            
       bpl    LF76D   
       bcc    LF787   
       lda    $1700,Y 
       adc    #$00    
       sta    $1700,Y 
       dey            
       jmp    $2679   
LF787: rts            

       ldy    $E2     
       sec            
LF78B: lda    $1800,Y 
       sbc    $1900,Y 
       sta    $1800,Y 
       dey            
       bpl    LF78B   
       bcs    LF7A5   
       lda    $1700,Y 
       sbc    #$00    
       sta    $1700,Y 
       dey            
       jmp    $2697   
LF7A5: rts            

       ldy    #$00    
       lda    $1800,Y 
       cmp    $1900,Y 
       beq    LF7B1   
LF7B0: rts            

LF7B1: cpy    $E2     
       beq    LF7B0   
       iny            
       jmp    $26A8   

LF7B9:                    ; ($26B9) 7800 mode init
       ldx    #$16        ; finally, back to something important
       stx    INPTCTRL    ; switch out bios and use reset vector
       txs                ; on cart to start the game.
       SED            
       jmp (CartResetVectorLo)     

; Enable 2600 mode
LF7C2:                    ; ($26C2) 2600 mode init
       lda    #$02        ; Enable memory/bios
       sta    INPTCTRL    ; 
       ldx    #$7F
LF7C8: lda    LF7D4,X     ; again, copy code to ram....
       sta    $0480,X 
       dex            
       bpl    LF7C8   
       jmp    $0480       ; execute it

; code executed at $0480 (RIOT RAM)

LF7D4: lda    #$00        ; 
       tax                ; 
       sta    INPTCTRL    ; Disable 7800 RAM
LF7D9: sta    RSYNC,X     ; Clear the TIA
       inx                ;
       cpx    #$2A        ;
       bne    LF7D9       ;
       sta    WSYNC       ; wait for sync
       lda    #$04        ;
       nop                ;
       bmi    LF80A   
       ldx    #$04    
LF7E9: dex            
       bpl    LF7E9   
       txs            
       sta    $0110   
       jsr    $04CB   
       jsr    $04CB   
       sta    RESP1   
       sta    GRP0    
       sta    GRP1    
       sta    PF2     
       nop            
       sta    WSYNC   
       lda    #$00    
       nop            
       bmi    LF80A   
       bit    RSYNC   
       bmi    LF813   
LF80A: lda    #$02    
       sta    COLUBK  
       sta    LF112   
       bne    LF831   
LF813: bit    WSYNC   
       bmi    LF823   
       lda    #$02    
       sta    COLUP0  
       sta    LF118   
       sta    LF460   
       bne    LF831   
LF823: sta    DPPH   
       lda    #$08    
       sta    GRP0    
       jsr    $04CB   
       nop            
       bit    WSYNC   
       bmi    LF80A   
LF831: lda    #$FD    
       sta    COLUPF  
       jmp (CartResetVectorLo)        ; And use the reset vector on the cart to start
       nop                            ; the 2600 game.

       ORG $F880

LF880: lda    #$1D        ; this is called any time the self-test fails.
       sta    INPTCTRL    ; Eanble TIA/Cart/lock out INPTCTRL

START:                    ; $F884 
       SEI                ; BIOS entry point.  Disable interrupts
       CLD                ; Clear decimal flag
       lda    #$02        ;
LF888: sta    INPTCTRL    ; Enable 7800 RAM
       lda    #$FB        ;
       sta    $F5         ;
       lda    #$12        ;
       sta    $F4         ; set nmi vector to FB12. (shares cleanup of irq)
       lda    #$7F        ;
       sta    CTRL        ; Turn off DMA
       lda    #$00        ;
       sta    BACKGRND    ; Background color
       ldx    #$05        ;
LF89C: lda    LF91D,X     ; looks icky, but it's just a ram test
       ldy    #$00        ;
LF8A1: sta    $2000,Y     ;
       cmp    $2000,Y     ;
       bne    LF8D0       ; Test failed
       sta    $2100,Y     ;
       cmp    $2100,Y     ;
       bne    LF8D0       ; Test failed
       dey                ;
       bne    LF8A1       ;
       dex                ;
       bpl    LF89C       ; and right down here, it passes
       lda    #$43        ; Check RAM 0 mirror
       sta    $2080       ; 
       cmp    $80         ; 
       bne    LF8CB       ; make sure they match. If not, fail selftest.
       sta    $2180       ; Check RAM 1 mirror
       cmp    $0180       ; 
       bne    LF8CB       ; make sure they match. If not, fail selftest.
       jmp    LF938       ; continue selftest
LF8CB: ldy    #$04        ;
       jmp    LF880       ; selftest fail.

LF8D0: sta    $1800       ; test store and compare
       cmp    $1800   
       bne    LF8E2       ; some kind of error code is being determined
LF8D8: ldy    #$01    
       jmp    LF880   

LF8DD: ldy    #$02
       jmp    $F880

LF8E2: ldy    #$03
       jmp    LF880   

LF8E7: lda    #$00        ; this is a more comprehensive ram test than
       sta    $F0         ; the first routine, and it is called after
       sta    $F2         ; more of the selftest has completed.
       ldy    #$07    
       STY    $F4     
LF8F1: lda    LF923,Y 
       sta    $F1     
       lda    LF92B,Y 
       sta    $F3     
       ldx    #$05    
LF8FD: lda    LF91D,X 
LF900: ldy    #$00    
LF902: sta    ($F0),Y 
       cmp    ($F0),Y 
       bne    LF8D8   
       sta    ($F2),Y 
       cmp    ($F2),Y 
       bne    LF8DD   
       dey            
       bne    LF902   
       dex            
       bpl    LF8FD   
       dec    $F4     
       ldy    $F4     
       bpl    LF8F1   
       jmp    LFB17       ; ram test passed, so jump in here.

; ram test data
LF91D: .byte $00,$FF,$55,$AA,$69,$0F
LF923: .byte $22,$23,$24,$25,$26,$27,$22,$23
LF92B: .byte $18,$19,$1A,$1B,$1C,$1D,$1E,$1F

LF933: ldy    #$00       ; local place for selftest fail branch target
       jmp    LF880   

; 6502 CPU test

LF938: lda    #$AA       ; test some flags and branches
       beq    LF933      ; test failed
       bpl    LF933      ; test failed
       bmi    LF943      ; test passed
       jmp    LF933      ; test failed
LF943: bne    LF948      ; test passed
       jmp    LF933      ; test failed
LF948: sta    $AA        ; store AA to 00AA
       cmp    $AA        ; compare it back
       bne    LF933      ; if it doesn't match, selftest fail
       lda    #$00       ; do some more flag tests
       bne    LF933      ;
       bmi    LF933      ;
       bpl    LF959      ; test passed
       jmp    LF933      ;
LF959: beq    LF95E      ; test passed
       jmp    LF933      ;
LF95E: cmp    #$00       ; test the compare instruction
       bne    LF933      ;
       bcc    LF933      ;
       bcs    LF969      ; test passed, since they're equal
       jmp    LF933      ;
LF969: cmp    #$01       ; compare it to 01
       bcs    LF933      ;
       bcc    LF972      ; A < 01, so carry is clear
       jmp    LF933      ;
LF972: ldx    #$55       ; test comparisons with the X register
       cpx    #$56       ;
       beq    LF933      ;
       stx    $01AA      ;
       cpx    $01AA      ;
       bne    LF933      ;
       ldy    $AA        ; and with the Y register.
       cpy    #$AB       ;
LF984: beq    LF933      ;
       STY    $0155      ; put some stuff in the stack area to test stack
       cpy    $0155      ; and then access this data in many diffeent ways
       bne    LF933      ;
       dex               ;
       txs               ;
       inx               ;
       pla               ;
       cmp    #$AA       ;
       bne    LF9EB      ;
       txa               ;
       pha               ;
       cpx    $0155      ;
       bne    LF9EB      ;
       TYA               ;
       cmp    #$AA       ;
       bne    LF9EB      ;
       tax               ;
       lda    $0100,X    ;
       tay               ;
       cpy    #$55       ;
       bne    LF9EB      ;
       lda    VSYNC,X    ;
       cmp    $AA        ;
       bne    LF9EB      ;
       cmp    #$AA       ;
       bne    LF9EB      ;
       eor    #$FF       ;
       sta    $0000,Y    ;
       cmp    $55        ;
       bne    LF9EB      ;
       cmp    $0100,Y    ;
       bne    LF9EB      ;
       cmp    $20AB,X    ;
       bne    LF9EB      ;
       lda    #$20       ;
       sta    $F1        ;
       lda    #$CC       ;
       sta    $F0        ;
       sta    ($46,X)    ;
       cmp    $CC        ;
       bne    LF9EB      ;
       sta    ($F0),Y    ;
       cmp    $2121      ;
       bne    LF9EB      ;
       lda    #$EE       ; test the indirect jump by setting up a jump
       sta    $F0        ; to F9EE
       lda    #$F9       ;
       sta    $F1        ;
       jmp ($00F0)        ; and do it.

LF9E8: jmp    $F9EB

LF9EB: jmp    LF933


LF9EE: lda    #$55       ; now test out the math functions.
       clc            
       adc    #$55    
       nop            
       bcs    LF9EB      ; test addition.
       bpl    LF9EB   
       beq    LF9EB   
       cmp    #$AA       ; make sure it worked
       bne    LF9EB   
       adc    #$55       ; test addition again.
LFA00: nop            
       bcc    LF9EB   
       bmi    LF9EB   
       bne    LF9EB   
       sbc    #$55       ; test subtraction
       bcs    LF9EB   
       bpl    LF9EB   
       beq    LF9EB   
       cmp    #$AB       ; make sure it worked
       bne    LF9EB   
       clc            
       sbc    #$AA       ; test subtraction again
       bcc    LF9EB   
       bmi    LF9EB   
       bne    LF9EB   
       lda    #$FF       ; set up a stack
       tax               ; and do all kinds of stuff in it for tests
       inx            
       bne    LFA58   
       dex            
       beq    LFA58   
       bpl    LFA58   
       cpx    #$FF    
       bne    LFA58   
       tay            
       iny            
       bne    LFA58   
       dey            
       beq    LFA58      
       iny            
       bne    LFA58   
       sta    $F0     
       inc    $F0     
       bne    LFA58   
       cpy    $F0     
       bne    LFA58   
       dec    $F0     
       beq    LFA58   
       cmp    $F0     
       bne    LFA58   
       lda    #$AA    
       clc            
       rol               ; now we get onto the more complex math instrs
       rol
       rol
       cmp    #$52    
       bne    LFA58      ; make sure rotate left works.
       ror
       ror
       ror
       cmp    #$AA    
       beq    LFA5B      ; test rotate right
LFA58: jmp    LF933      ; fail!

LFA5B: asl               ; test arithmetic shift left
       bcc    LFA58   
       asl            
       bcs    LFA58   
       asl            
       cmp    #$50    
       bne    LFA58   
       eor    #$05    
       lsr               ; and logical shift right
       bcc    LFA58   
       lsr            
       bcs    LFA58   
       lsr            
       cmp    #$0A    
       bne    LFA58   
       lda    #$55       ; now test the ands and ors.
       ora    #$1B    
       cmp    #$5F    
       bne    LFA58   
       and    #$55    
       and    #$1B    
       cmp    #$11    
       bne    LFA58   
       ora    #$55    
       eor    #$1B       ; and the eors
       cmp    #$4E    
       bne    LFA58   
       jsr    LFA91      ; test jump subroutine instruction
       jmp    LFA58      ; if we return, fail

LFA91: tsx               
       cpx    #$52       ; check stack pointer
       bne    LFA58      ; fail if not right
       pla            
       cmp    #$8D    
       bne    LFA58   
       pla            
       cmp    #$FA    
       bne    LFA58      ; get the old return address off the stack
       lda    #$F8    
       pha            
       lda    #$E6    
       pha               ; and make our own
       rts               ; and 'return' to F8E7

       jmp    LFA58      ; another jump to catch a failure

; Interrupt routine  NMI. 

LFAAA: txa          ;Save A
       pha          ;  
       lda    #$43  ;Setup control register  
       sta    CTRL  ; 
       ldx    #$0F  ; Handle the color scrolling in the FUJI    
       lda    $EF     
       sta    P0C2
       bit    $F3     
       bvc    LFAC0   
       bpl    LFABE   
LFABC: sta    MWSYNC ;Wait for 3 scanlines 
LFABE: sta    MWSYNC ;    
LFAC0: sta    MWSYNC ;   
       sec            
       sbc    #$10  
       cmp    #$10    
       bcs    LFACB   
       sbc    #$0F    
LFACB: sta    P0C2
       dex            
       bpl    LFABC   ;Branch to do next section of fuji
       ldx    #$40    ;set 160x2/160x4 mode
       stx    CTRL    ;  
       and    #$F0    ;
       ora    #$0E    ;set Palette 1 color 3
       sta    P1C3    ;
       lda    $EF     ;
       and    #$F0    ;
       ora    #$06    ;set Palette 1 color 1
       sta    P1C1    ;
       and    #$F0    
       clc            
       adc    #$40    
       bcc    LFAEB   
       adc    #$0F    
LFAEB: ora    #$03   ;set Palette 1 color 2  
       sta    P1C2  
       dec    $F1     
       bpl    LFB0C   
       lda    $F3     
       adc    #$60    
       bcc    LFB0A   
       lda    $EF     
       clc            
       adc    #$10    
       bcc    LFB02   
       adc    #$0F    
LFB02: sta    $EF     
       lda    $F2     
       sta    $F1     
       lda    #$00    
LFB0A: sta    $F3     
LFB0C: lda    #$02    
       sta    $F0     
       pla            
       tax            
       pla            
       rti            

LFB14: jmp    LFB14   

LFB17: ldx    #$FF       ; selftest has passed, start system init
       txs               ; set up a stack
       lda    #$00       ; Clear TIA/Maria registers
       tax               ;
LFB1D: sta    $01,X ;    ;
       inx               ;
       cpx    #$2C       ;
       bne    LFB1D      ; 
       lda    #$02       ;
       sta    INPTCTRL   ; Enable 7800 RAM 
       ldx    #$00       ;
       stx    BACKGRND   ; Set background color 
LFB2C: lda    LF400,X    ; copy the authentication and title screen to 
       sta    $2300,X    ; ram
       lda    LF500,X    ;
       sta    $2400,X    ;
       lda    LF600,X    ;
       sta    $2500,X    ;
       lda    LF700,X    ;
       sta    $2600,X    ;
       lda    LF800,X    ;
       sta    $2700,X    ;
       lda    LFBBE,X    ;
       sta    $2200,X    ;
       cpx    #$00       ;
       bmi    LFB7E      ;
       lda    LFC4B,X     ;
       sta    $1F84,X     ;
       lda    LFCC6,X     ;
       sta    $1984,X     ;
       lda    LFD3D,X     ;
       sta    $1A84,X     ;
       lda    LFDB4,X     ;
       sta    $1B84,X     ;
       lda    LFE18,X     ;
       sta    $1C84,X     ;
       lda    LFE57,X     ;
       sta    $1D84,X     ;
       lda    LFE96,X     ;
       sta    $1E84,X     ;
LFB7E: dex                ;
       bne    LFB2C       ;
       jmp    $2306       ; and execute it

;Start display of Atari logo

LFB84: lda    CartKeyStartPage      ; Read ROM start byte from cart
       and    #$04       ; Is rom start greater then $4000?
       beq    LFBBD      ; Branch if not
       lda    #$03      
       sta    $F1     
       sta    $F2     
       lda    #$49    
       sta    $EF     
       lda    #$66       ;Palette 1 Color 1
       sta    P1C1       ;
       lda    #$56       ;Palette 1 Color 2
       sta    P1C2       ;
       lda    #$2E       ;Palette 1 Color 3
       sta    P1C3       ;
       lda    #$AA       ;Set NMI vector to FAAA
       sta    $F4        ;
       lda    #$FA       ;
       sta    $F5        ;
LFBA9: bit    MSTAT      ;Check VBLANK status
       bmi    LFBA9      ;Wait till end of VBLANK
LFBAD: bit    MSTAT      ;Check BLANK status
       bpl    LFBAD      ;Wait for start of next VBLANK
       lda    #$84       ;Set Display list pointer to $1f84
       sta    DPPL       ;
       lda    #$1F       ;
       sta    DPPH       ;
       lda    #$43       ;Maria mode = DMA on/320A or 320C
       sta    CTRL       ;
LFBBD: rts               ;

; Graphics and Display List Data

LFBBE:                                  ;$2200   Display Lists

       .byte $84,$1F,$19,$BB            ;$2200   Blank space
       .byte $00,$00

       .byte $84,$40,$19,$1F,$BB        ;$2206   First DL on screen
       .byte $00,$00

       .byte $85,$1C,$19,$4A            ;$220D   Blank space before Fuji
       .byte $00,$00

       .byte $89,$1C,$19,$4A            ;$2213   Fuji line 1
       .byte $00,$00

       .byte $8D,$1C,$19,$48            ;$2219   Fuji line 2
       .byte $00,$00

       .byte $91,$1B,$19,$46            ;$221F   Fuji line 3
       .byte $00,$00

       .byte $96,$19,$19,$42            ;$2225   Fuji line 4
       .byte $00,$00

       .byte $9D,$17,$19,$3E            ;$222B   Fuji line 5
       .byte $00,$00

       .byte $A6,$17,$19,$3E            ;$2231   Fuji line 6
       .byte $00,$00

       .byte $AF,$2C,$1C,$00            ;$2237   Start of Atari
       .byte $AF,$2C,$1C,$50            ;        and between lines
       .byte $00,$00

       .byte $AF,$2C,$1D,$00            ;$2241   End of Atari
       .byte $AF,$2C,$1D,$50 
       .byte $00,$00

       .byte $AF,$2D,$19,$28            ;$224B    Atari line 1
       .byte $00,$00

       .byte $C2,$2D,$19,$28            ;$2251    Atari line 2
       .byte $00,$00

       .byte $D5,$2D,$19,$28            ;$2257     Atari line 3
       .byte $00,$00
                                                   
       .byte $E8,$2D,$19,$28            ;$225D     Atari line 4
       .byte $00,$00

       .byte $AF,$2D,$1A,$28            ;$2263     Atari line 5
       .byte $00,$00

       .byte $C2,$2D,$1A,$28            ;$2269     Atari line 6
       .byte $00,$00

       .byte $D5,$2D,$1A,$28            ;$226F     Atari line 7
       .byte $00,$00

       .byte $E8,$2D,$1A,$28            ;$2275     Atari line 8
       .byte $00,$00

       .byte $AF,$2D,$1B,$28            ;$227B     Atari line 9
       .byte $00,$00

       .byte $C2,$2D,$1B,$28            ;$2281     Atari line 10
       .byte $00,$00

       .byte $D5,$2D,$1B,$28            ;$2287     Atari line 11
       .byte $00,$00

; $1F84  Display List List

LFC4B: .byte $0F,$22,$06      ;Blank space
       .byte $0F,$22,$00      ;
       .byte $0F,$22,$00      ;
       .byte $0F,$22,$00      ;
       .byte $03,$22,$00      ;
       .byte $85,$22,$0D      ;DLI
       .byte $05,$22,$13      ;Draw Fuji
       .byte $05,$22,$19      ;
       .byte $05,$22,$1F      ;
       .byte $05,$22,$25      ;
       .byte $05,$22,$2B      ;
       .byte $05,$22,$31      ;
       .byte $0F,$22,$00      ;Blank area
       .byte $01,$22,$37      ;Draw "ATARI"
       .byte $00,$22,$4B      ;Line 1 
       .byte $02,$22,$37
       .byte $00,$22,$51      ;Line 2
       .byte $02,$22,$37
       .byte $00,$22,$57      ;Line 3
       .byte $02,$22,$37
       .byte $00,$22,$5D      ;Line 4
       .byte $02,$22,$37
       .byte $00,$22,$63      ;Line 5
       .byte $02,$22,$37
       .byte $00,$22,$69      ;Line 6
       .byte $02,$22,$37
       .byte $00,$22,$6F      ;Line 7
       .byte $02,$22,$37
       .byte $00,$22,$75      ;Line 8
       .byte $02,$22,$37
       .byte $00,$22,$7B      ;Line 9
       .byte $02,$22,$37
       .byte $00,$22,$81      ;Line 10
       .byte $02,$22,$37
       .byte $00,$22,$87      ;Line 11
       .byte $01,$22,$41
       .byte $0F,$22,$00      ;Blank Space
       .byte $0F,$22,$00      ;
       .byte $0F,$22,$00      ;
       .byte $0F,$22,$00      ;
       .byte $0F,$22,$00      ;

LFCC6
    .byte $00,$7c,$7f,$8f,$80,$fc,$7f,$8f,$c0,$1f,$87,$f8,$7e,$0f,$e0,$7f
LFCD6
    .byte $81,$fc,$07,$ff,$80,$7f,$80,$7f,$f8,$1f,$ff,$f0,$00,$7f,$80,$03
LFCE6
    .byte $ff,$fe,$1f,$00,$00,$00,$7f,$80,$00,$00,$3e,$00,$00,$0c,$00,$3f
LFCF6
    .byte $ff,$ff,$ff,$f0,$00,$c0,$00,$00,$3f,$ff,$ff,$00,$03,$fc,$00,$00
LFD06
    .byte $3f,$00,$3f,$ff,$ff,$ff,$f0,$03,$f0,$00,$00,$3f,$ff,$ff,$fc,$03
LFD16
    .byte $fc,$00,$00,$ff,$c0,$00,$03,$ff,$00,$00,$0f,$fc,$00,$00,$3f,$f0
LFD26
    .byte $03,$ff,$c3,$fc,$00,$03,$ff,$f0,$00,$03,$ff,$00,$00,$3f,$ff,$00
LFD36
    .byte $00,$3f,$f0,$00,$3f,$c3,$fc,$00,$7c,$7f,$8f,$80,$7c,$7f,$8f,$80
LFD46
    .byte $1f,$87,$f8,$7e,$0f,$f0,$7f,$83,$fc,$01,$ff,$80,$7f,$80,$7f,$e0
LFD56
    .byte $1f,$ff,$f8,$00,$7f,$80,$07,$ff,$fe,$1f,$f0,$00,$00,$7f,$80,$00
LFD66
    .byte $03,$fe,$00,$0f,$f3,$fc,$00,$03,$ff,$00,$00,$ff,$3f,$c0,$00,$3f
LFD76
    .byte $f0,$00,$ff,$c3,$fc,$00,$3f,$c0,$ff,$00,$03,$ff,$00,$03,$fc,$0f
LFD86
    .byte $f0,$00,$3f,$f0,$3f,$fc,$03,$fc,$00,$ff,$00,$3f,$c0,$03,$ff,$00
LFD96
    .byte $0f,$f0,$03,$fc,$00,$3f,$f0,$ff,$c0,$03,$fc,$03,$ff,$ff,$ff,$f0
LFDA6
    .byte $03,$ff,$00,$3f,$ff,$ff,$ff,$00,$3f,$f0,$3f,$f0,$03,$fc,$00,$7c
LFDB6
    .byte $7f,$8f,$80,$7c,$7f,$8f,$80,$1f,$87,$f8,$7e,$07,$f0,$7f,$83,$f8
LFDC6
    .byte $00,$ff,$c0,$7f,$80,$ff,$c0,$1f,$ff,$fc,$00,$7f,$80,$0f,$ff,$fe
LFDD6
    .byte $1f,$fc,$00,$00,$7f,$80,$00,$0f,$fe,$0f,$ff,$ff,$ff,$fc,$03,$ff
LFDE6
    .byte $00,$ff,$ff,$ff,$ff,$c0,$3f,$f0,$0f,$fc,$03,$fc,$3f,$f0,$00,$03
LFDF6
    .byte $ff,$03,$ff,$03,$ff,$00,$00,$3f,$f0,$3f
LFE00
    .byte $f0,$03,$ff,$03,$fc,$ff
LFE06
    .byte $c0,$00,$00,$ff,$c3,$ff,$0f,$fc,$00,$00,$0f,$fc,$3f,$f0,$00,$ff
LFE16
    .byte $c3,$fc,$00,$7c,$7f,$8f,$80,$7c,$7f,$8f,$80,$0f,$87,$f8,$7c,$07
LFE26
    .byte $f0,$7f,$83,$f8,$00,$7f,$c0,$7f,$80,$ff,$80,$1f,$ff,$fe,$00,$7f
LFE36
    .byte $80,$1f,$ff,$fe,$1f,$ff,$00,$00,$7f,$80,$00,$3f,$fe,$55,$55,$55
LFE46
    .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
LFE56
    .byte $55,$00,$7c,$7f,$8f,$80,$7c,$7f,$8f,$80,$0f,$c7,$f8,$fc,$03,$f0
LFE66
    .byte $7f,$83,$f0,$00,$3f,$e0,$7f,$81,$ff,$00,$01,$ff,$fe,$00,$7f,$80
LFE76
    .byte $1f,$ff,$e0,$1f,$ff,$c0,$00,$7f,$80,$00,$ff,$fe,$aa,$aa,$aa,$aa
LFE86
    .byte $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa
LFE96
    .byte $00,$7c,$7f,$8f,$80,$7c,$7f,$8f,$80,$0f,$c7,$f8,$fc,$03,$f8,$7f
LFEA6
    .byte $87,$f0,$00,$1f,$e0,$7f,$81,$fe,$00,$00,$1f,$ff,$00,$7f,$80,$3f
LFEB6
    .byte $fe,$00,$1f,$ff,$e0,$00,$7f,$80,$01,$ff,$fe,$55,$55,$55,$55,$55
LFEC6
    .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$09
LFED6
    .byte $ca,$c9,$c6,$b4,$12,$08,$1b,$60,$58,$81,$4b,$86,$01,$d8,$bf,$d9
LFEE6
    .byte $25,$a0,$7b,$dc,$32,$79,$84,$3b,$7c,$bc,$2f,$e2,$e2,$fa,$8d,$0a
LFEF6
    .byte $00,$3b,$c5,$ec,$af,$2d,$8a,$cd,$06,$93
LFF00
    .byte $6a,$a5,$14,$46,$77,$c4
LFF06
    .byte $6a,$b2,$53,$36,$ef,$8c,$ce,$0c,$a2,$68,$71,$d3,$73,$e8,$f7,$6d
LFF16
    .byte $06,$b5,$20,$ef,$23,$47,$0c,$51,$55,$c8,$fe,$f4,$58,$c4,$3f,$20
LFF26
    .byte $a7,$67,$38,$b0,$76,$e2,$c4,$d8,$05,$63,$f8,$3c,$58,$3b,$2d,$22
LFF36
    .byte $cc,$88,$b3,$71,$8f,$1d,$80,$0a,$87,$bd,$a1,$59,$23,$e9,$70,$e2
LFF46
    .byte $d3,$ec,$46,$68,$80,$42,$39,$ea,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
LFF56
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
LFF66
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
LFF76
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
LFF86
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
LFF96
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
LFFA6
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
LFFB6
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
LFFC6
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
LFFD6
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
LFFE6
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff



LFD3D = $FD3D
LFDB4 = $FDB4
LFDD5 = $FDD5
LFE18 = $FE18
LFE57 = $FE57  
LFE84 = $FE84
LFE96 = $FE96
LFED5 = $FED5
LFF00 = $FF00

GccCopyright
       .byte $47,$43,$43,$28,$43,$29 ; 'GCC(C)'
       .byte $31,$39,$38,$34,$2D,$F7 ; '1984-'

; Classic 6502 vectors. Called Cart* because they often
; are referenced when the game cart is switched in.

CartNMIVectorLo
       .byte $00,$F0 ; system vector - nmi
CartResetVectorLo
       .word START   ; F884
CartIRQVectorLo
       .byte $33,$F9 ; system vector - irq

