;********************************************************
;
;    software serial Output Routines for PIC16Fx
;    and infrared 36khz modulated serial output routines.

;    Clock is 4.0 MHz.
;    ie. 1.0 us per cycle = 4/Fosc.
;
;    9600 Baud  = 104.17 us
;               = 104.17   CPU cycles
;
;
;       4800 baud = 208 us
;       1200 baud = 832 us
;
;       300 baud = 3333.333 us
;               - 3333.333 CPU cycles

;
;********************************************************
;
;       Output the character in W. Assumes Mac is ready.
;
;       Uses W
;
;       currently code below is result of some brainstorming and experiments.
;       infrared tsop4136 reciever cannot handle data rates below 1200 and above 2400,
;       so 300 baud output which was initial idea had to be abandoned.
;       1200 baud works perfectly - according to experiments, so this code is left so far.
;       in future, ifdef'ing the code is needed - this way user can select proper baud rate,
;       ir modulation or not, and ir modulation period and duty cycle.
;       right now one have to either to adjust stuff by hand, or use as-is.



putchr  movwf   S_Wtemp         ; Character being output

	bank1	
	bcf	LED_IR_TRIS	; we will use IR LED pin as output
	bank0


        movlw   0x09            ; Bit count, 9 - incude start bit. 
        movwf   S_count

;       bcf     S_out           ; Send a 0 - Start bit
        bcf     S_mask,0                ; send 0 . this is our first bit, start bit.

;       nop
;       nop
;       nop
;       nop

put_clp

                                ; baud delay
        btfsc   S_mask,0        	; if S_mask is 1
        call    delay_832uS      ; just idle delay...

        btfss   S_mask,0                ; if S_mask is 0
        call    IR_out_832uS     ; modulate light with 36khz for 832uS


        rrf     S_Wtemp,f       ; Transmit a bit
        bc      t_0

;       bcf     S_out           ; Send a 0
        bcf     S_mask,0

        goto     tx_1

;t_0    bsf     S_out           ; Send a 1
t_0     bsf     S_mask,0

tx_1    decfsz  S_count,f       ; Done all bits?
        goto    put_clp


;        btfsc   S_mask,0                ; if S_mask is 1
;        call    delay_832uS      ; just idle delay...
;
;        btfss   S_mask,0                ; if S_mask is 0
;        call    IR_out_832uS     ; modulate light with 36khz for 3333uS



;       bsf     S_out           ; Transmit two stop bit
        ; 1 is just idle delay with infrared TX. so just idle 2 bits.
        call    delay_832uS
        call    delay_832uS

        return

;---------------------------IR serial related delay routines

delay_832uS              ; ~832us, 1200 baud
                        ;828 cycles
        movlw   0xa5
        movwf   d1
        movlw   0x01
        movwf   d2
Delay_0
        decfsz  d1, f
        goto    $+2
        decfsz  d2, f
        goto    Delay_0
                        ;4 cycles (including call)
        return

;---------------------

IR_out_832uS

        movlw   d'31'           ; 30*27us = 810us  - plus 1
        movwf   d1              ; plus 2 = 812
IR_out_1
        bsf     LED_IR           ; 1us
	goto 	$+1		; 2+3
	goto	$+1		; 4+5
	goto	$+1		; 6+7
	goto	$+1		; 8+9
	goto	$+1		; 10+11
        nop                     ; 12
        bcf     LED_IR           ; 13
	goto 	$+1		; 14+15
	goto	$+1		; 16+17
	goto	$+1		; 18+19
	goto	$+1		; 20+21
	goto	$+1		; 22+23
        nop                     ; 24
        decfsz  d1,f            ; 25
        goto    IR_out_1        ; 26 and 27
                        ; 810
        bsf     LED_IR   ; 811, 1

	goto	$+1	; 812,813
	goto 	$+1	; 814,815
	goto	$+1	; 816,817
	goto	$+1	; 818,819
	goto	$+1	; 820,821
        nop     ; 822, 12
        bcf     LED_IR ; 823, 13
	goto	$+1	;824,825
	goto	$+1	;826,827
        nop     ; 828, 18
                        ; +4 (incl. call = 832)
        return



