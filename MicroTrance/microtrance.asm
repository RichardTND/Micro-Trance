;MicroTrance Music Player Source
;Written by
;Richard Bayliss
;2019 The New Dimension

        !to "microtrance.prg",cbm 
        
        ;IRQ RASTER MUSIC PLAYER ... MAIN MUSIC 
        ;ROUTINES ARE AT $1000+
        
        *=$0801 
        !basic 2064
        *=$0810 
        sei 
        jsr $ff81
        LDA #8
        JSR $FFD2
        lda #0
        sta $d020
        sta $d021
        lda #$14
        sta $d018 
        ldx #$00
copymsg lda message,x 
        sta $0400,x 
        lda #7
        sta $d800,x
        inx 
        cpx #messageend-message 
        bne copymsg 
        ldx #<irq
        ldy #>irq
        stx $0314
        sty $0315
        lda #$7f
        sta $dc0d
        sta $dd0d
        lda #$36
        sta $d012
        lda #$1b
        sta $d011
        lda #$01
        sta $d01a
        
        lda #0
        jsr musicinit
        cli 
        jmp *
irq     inc $d019 
        lda $dc0d
        sta $dd0d
        lda #$fa
        sta $d012
        dec $D020
        jsr musicplay
        inc $d020
        jmp $ea7e
        
        !ct scr
message 
        !text "microtrance, tune+player by richard/tnd!"
        !text "                                        "
        !text "please feel free to use this source if  "
        !text "you want to :)                          "
messageend        
;-------------------------------------------------------        
        ;MAIN MUSIC ROUTINES. 
        ;
        ;MUSIC INIT = $1000
        ;MUSIC PLAY = $1003

         *= $1000


musicinit
         jmp initsid
musicplay
         jmp playsid

;---------------------------------------
initsid

         ldx #$00
zerosidregs
         lda #$00
         sta $d400,x
         inx
         cpx #$19
         bne zerosidregs

         lda #$00
         sta patterndelay
         lda #$00
         sta voice1timer
         sta voice2timer
         sta voice3timer
         sta songpos1
         sta arppos1
         sta arptimer

         lda #$1f
         sta $d418
         ;Reset pointers
         rts

;---------------------------------------

playsid



playnextnote
         lda #$00
         sta patterndelay
         lda #$0e
         sta 54277
         sta 54277+7
         lda #$0a
         sta 54277+14
         lda #$ee
         sta 54278
         sta 54278+7
         lda #$ee
         sta 54278+14
         lda #1
         sta 54272
         lda #88
         sta 54272+7
         lda #5
         sta 54272+14
         jsr playvoice1
         jsr playvoice2
         jsr playvoice3
         rts

;---------------------------------------

playvoice1
         ldy voice1timer
         lda wavtab1,y
         sta 54276
         lda frqtab1,y
         sta 54273
         lda pulstb1a,y
         sta 54274
         lda pulstb1b,y
         sta 54275
         iny
         cpy #tabend-tabstart
         beq resetvoice1
         inc voice1timer
         rts
resetvoice1
         ldy #$00
         sty voice1timer


switcharp
         ldx arptimer
         lda arptbllo,x
         sta arpread+1
         lda arptblhi,x
         sta arpread+2
         inx
         cpx #arptbend-arptb
         beq resetarpmode
         inc arptimer
         rts
resetarpmode
         ldx #$00
         stx arptimer
         rts

;---------------------------------------

playvoice2

         ldx voice2timer
         lda leadtab1,x
         sta 54273+7
         lda #$41
         sta 54275+7
         sta 54274+7
         lda #$21
         sta 54276+7
         lda #$0e
         sta 54277+7
         sta 54277+14
         lda #$ee
         sta 54278+7
         sta 54278+14

         inx
         cpx #tab2end-tab2start
         beq resetvoice2
         inc voice2timer

         rts
resetvoice2
         ldx #$00
         stx voice2timer
         ldy songpos1
         lda notetb1,y
         sta leadtab1
         sta leadtab1+1
         sta leadtab1+2
         sta leadtab1+3
         sta leadtab1+4
         sta leadtab1+5
         lda basstab,y
         sta bass+1
         sta bass+2
         sta bass+3
         sta bass+4
         sta bass+5
         sta bass+6
         sta bass+7
         sta bass+8
         sta bass+9
         sta bass+10
         sta bass+11
         sta bass2+1
         sta bass2+2
         sta bass2+3
         sta bass2+4
         sta bass2+5
         sta bass2+6
         sta bass2+7
         sta bass2+8
         sta bass2+9
         sta bass2+10
         sta bass2+11


         iny
         cpy #noteend-notestart
         beq resetnote
         inc songpos1


         rts
resetnote ldy #$00
         sty songpos1

         rts

;---------------------------------------

;Voice 3 arps


playvoice3

         ldy voice3timer
arpread  lda arptbl,y
         sta 54273+14
         lda #$21
         sta 54276+14
         iny
         cpy #arpend-arpstart
         beq resetarp
         inc voice3timer
         rts

resetarp ldy #$00
         sty voice3timer
         rts



patterndelay

         !byte $00
voice1timer !byte 0
voice2timer !byte 0
voice3timer !byte 0
songpos1 !byte 0
arppos1  !byte 0
arptimer !byte 0

;---------------------------------------
tabstart
wavtab1

;Drum/Bass Wave Table

;Drum Kick

         !byte $81,$41,$41,$41,$41
         !byte $11,$11,$11,$00,$00
;BassLead1
         !byte $81,$41,$41,$41,$00
         !byte $41,$41,$41,$41,$00


;Drum Snare

         !byte $81,$41,$41,$81,$41
         !byte $81,$00,$00,$00,$00

;BassLead2
         !byte $81,$41,$41,$41,$00
         !byte $41,$41,$41,$41,$00
tabend
;---------------------------------------

;Drum/Bass Frequency Table

frqtab1

;Drum Kick

         !byte $98,$0c,$0a,$08,$06
         !byte $03,$02,$00,$00,$00
;BassLead1

bass
         !byte $99,$05,$05,$05,$05
         !byte $05,$05,$05,$05,$00

;Drum Snare
         !byte $98,$0e,$0c,$99,$0a
         !byte $98,$98,$00,$00,$00
bassstore2

;BassLead2
bass2
         !byte $99,$05,$05,$05,$05
         !byte $05,$05,$05,$05,$05

;-------------------------------------

;Drum/Bass Pulse table


;Drum Kick

pulstb1a !byte $08,$08,$08,$08,$08
         !byte $00,$00,$00,$00,$00

;BassLead


         !byte $55,$11,$22,$44,$55
         !byte $55,$11,$22,$44,$55

;Drum Snare

         !byte $08,$08,$08,$08,$08
         !byte $08,$08,$08,$08,$08

;Bass Lead2



         !byte $55,$11,$22,$44,$55
         !byte $55,$11,$22,$44,$55


;Drum Kick

pulstb1b !byte $88,$88,$88,$88,$88
         !byte $88,$88,$88,$88,$88

;bass lead1

         !byte $22,$22,$22,$22,$22
         !byte $22,$22,$22,$22,$22

;Drum snare

         !byte $88,$88,$88,$88,$00
         !byte $88,$88,$88,$88,$00

;bass lead 2

         !byte $22,$22,$22,$22,$22
         !byte $22,$22,$22,$22,$22

;---------------------------------------

;Lead tab 1

tab2start
leadtab1
         !byte $00,$00,$00,$00,$00
         !byte $00,$00,$00,$00,$00
tab2end
;---------------------------------------


notestart

notetb1

         !byte $30,$1e,$28,$1e
         !byte $00,$1e,$28,$00

         !byte $30,$1e,$28,$1e
         !byte $00,$1e,$28,$00

         !byte $2d,$24,$2d,$1e
         !byte $00,$2d,$24,$00

         !byte $2d,$24,$2d,$1e
         !byte $00,$2d,$24,$00

         !byte $20,$1e,$20,$24
         !byte $00,$1e,$20,$00

         !byte $20,$1e,$20,$24
         !byte $00,$28,$1e,$00

         !byte $30,$1e,$28,$1e
         !byte $00,$1e,$28,$00

         !byte $30,$1e,$28,$1e
         !byte $00,$18,$14,$00


noteend


basstab
         !byte $05,$05,$05,$05
         !byte $05,$05,$05,$05

         !byte $05,$05,$05,$05
         !byte $05,$05,$05,$05

         !byte $03,$03,$03,$03
         !byte $03,$03,$03,$03

         !byte $03,$03,$03,$03
         !byte $03,$03,$03,$03

         !byte $04,$04,$04,$04
         !byte $04,$04,$04,$04

         !byte $04,$04,$04,$04
         !byte $04,$04,$04,$04

         !byte $05,$05,$05,$05
         !byte $05,$05,$05,$05

         !byte $05,$05,$05,$05
         !byte $05,$05,$05,$05

         !byte $05,$05,$05,$05
         !byte $05,$05,$05,$05

         !byte $05,$05,$05,$05
         !byte $05,$05,$05,$05


;---------------------------------------

arptb
arptbllo

         !byte <minor,<minor,<minor

         !byte <majmin,<majmin,<majmin
         !byte <majmin
         !byte <major,<major,<major
         !byte <major
         !byte <minor,<minor,<minor
         !byte <minor,<minor
arptbend
arptblhi
         !byte >minor,>minor,>minor

         !byte >majmin,>majmin,>majmin
         !byte >majmin
         !byte >major,>major,>major
         !byte >major
         !byte >minor,>minor,>minor
         !byte >minor,>minor

;---------------------------------------



arpstart
arptbl
         !byte $14,$14,$18,$18
         !byte $1e,$1e
arpend

minor    !byte $14,$14,$18,$18,$1e,$1e

majmin   !byte $12,$12,$18,$18,$1e,$1e

major    !byte $10,$10,$14,$14,$18,$18

