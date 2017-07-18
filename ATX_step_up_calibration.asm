; this is dc-dc step up module cooperating with cheap ATX power supply.
 
; this code is cut down code allowing calibration of hardware. 
; it simply displays voltage readout from ADC, and threshold levels.
; one simply sets upper float voltage threshold level using voltage divider potentiometer
; and then one sets ADC scale potentiometer to make it display proper ADC value. 

;
;       CPU configuration
;

        MESSG           "Processor = 12F675"
        #define         RAMStart        0x20
        processor       12f675
        include         <p12f675.inc>

        __config        _INTRC_OSC_NOCLKOUT & _PWRTE_ON & _WDT_ON & _CP_OFF & _BODEN_ON  & _MCLRE_OFF
;       __config        _INTRC_OSC_NOCLKOUT & _PWRTE_ON & _WDT_OFF & _CP_OFF & _BODEN_ON  & _MCLRE_OFF

;*******************************************************************
;includes

	include 	"infrared_serial.asm"

; -----



bank0   macro
        errorlevel      +302            ; Re-enable bank warning
        bcf             STATUS,RP0      ; Select Bank 0
        endm

;-------------------------------------------------------------------
;       Select Register Bank 1

bank1   macro
        bsf             STATUS,RP0      ; Select Bank 1
        errorlevel      -302            ; disable warning
        endm


WDT_set_to_2s	macro			; sets WDT to 2 seconds. switches bank to bank0 
	bank1
        bsf     OPTION_REG,PS1
        nop
        bsf     OPTION_REG,PS2
        nop
        bsf     OPTION_REG,PS0  ; prescaler set to ~2s of time
        nop
        bsf     OPTION_REG,PSA  ; prescaler assigned to WDT.
	bank0
		endm


compare_gp1_gp0		macro	; turn on comparator, compare gp1 (inverting) and gp0 (noninverting)
        movlw   b'00000010'     ;  Comparator on, without output, comparing AN0 and AN1
        movwf   CMCON           ;
		endm

compare_gp1_CVref	macro
        movlw   b'00000100'     ;  Comparator on, without output, comparing AN1 and vref
        movwf   CMCON           ;  (we want to check battery state)
		endm

comparator_off	macro
        movlw   b'00000111'     ;  Comparator off, without output, inputs shorted to ground
        movwf   CMCON           ;  (conserve power)
		endm



if_comparator_set	macro
        btfsc   CMCON,COUT      ; if rectifier voltage is lower than battery, skip
	endm


#define		COMPARATOR_INVERTING_INPUT_TRIS		TRISIO,GP1
#define		COMPARATOR_NONINVERTING_INPUT_TRIS	TRISIO,GP0
#define		COMPARATOR_INVERTING_INPUT		GPIO,GP1
#define		COMPARATOR_NONINVERTING_INPUT		GPIO,GP0

#define		CHARGING_KEY_TRIS			TRISIO,GP5
#define		CHARGING_KEY				GPIO,GP5

#define		BATTERY_LOAD_KEY_TRIS			TRISIO,GP2
#define		BATTERY_LOAD_KEY			GPIO,GP2

#define		LED_IR_TRIS				TRISIO,GP4
#define		LED_IR					GPIO,GP4

#define		KEYBOARD_TRIS				TRISIO,GP3
#define		KEYBOARD				GPIO,GP3




;*******************************************************************

        cblock  RAMStart

	DELAY			; used by "CONVERT" subroutine
	bcd:8                   ; BCD, MSD first
        COUNT                   ; Bin to BCD convert (bit count)
        cnt                     ;                    (BCD BYTES)


        CHR
        TEMP                    ; DATS/Putchr temporary
        AccA:4                  ; Binary, MSByte first

        D_hex                   ; used by hex output routine
                                ; used by serial out routines
        S_Wtemp
        S_count
    	d1
    	d2
        S_mask                  ; used by IR routine
	COMPARATOR_LOW_RESULT	; internal comparator based ADC result low range
	COMPARATOR_HIGH_RESULT 	; internal comparator based ADC result high range
        endc



		org 0            ; Reset Vector
		goto INIT

		org 4            ; Interrupt Vector
		goto COMP
		org 5

INIT:		
		WDT_set_to_2s		; set WDT to time out after 2s and switch to bank0 

		clrwdt			; immediatelly clean it. 
		

;		movlw H'14'      ; 
		movlw b'00000100'      ; Timer 1:
					; 0 - TMR1ON - 0(off)
					; 1 - TMR1CS - 0(off) 
					; 2 - /T1SYNC - 1(off)
					; 3 - T1OSCEN - 0(off)

					; 4 - T1CKPS0 - 0 
					; 5 - T1CKPS1 - 0 ; 1:1 prescaler
					; 6 - - 0 
					; 7 - - 0 

		movwf T1CON      ; A 1 MHz clock is used


		bsf INTCON,GIE   ; Enable Global Int
		bsf INTCON,PEIE  ; Unmask Peripheral Int

it_is_hot:
	        bank1
	        call    calibration_hot		; calibration value for hot environment (above 0C)
        	movwf   OSCCAL			; calibrate internal oscillator - required as we do serial out which is timing critical

quit_thermal_calibration:
        	bank0   

MAIN:		
		clrwdt
;		call 	short_sleep		; sleep for 2 seconds. 
						; this ensures supply voltage settles, as chip consumes minimum power.

		call 	CONVERT			; measure voltage of battery
		clrwdt
		call	DISPLAY_VOLTAGE		; display voltage
		clrwdt
		call 	CONVERT_COMPARATOR_LOW	; measure voltage of battery using internal reference voltage and comparator
		clrwdt
		call 	DISPLAY_COMPARISON_RESULT_LOW ; display voltage of battery measured using comparator. 

		clrwdt
		call 	CONVERT_COMPARATOR_HIGH	; measure voltage of battery using internal reference voltage and comparator
		clrwdt
		call 	DISPLAY_COMPARISON_RESULT_HIGH	 ; display voltage of battery measured using comparator. 

		call	crlf			; new line. 
		clrwdt
;	call 	long_sleep		; sleep for 2 seconds.
		goto MAIN





DISPLAY_COMPARISON_RESULT_LOW:
	clrf 	AccA
	clrf	AccA+1
	clrf	AccA+2
	movfw	COMPARATOR_LOW_RESULT
	movwf	AccA+3
	goto	display_5_digit_number

DISPLAY_COMPARISON_RESULT_HIGH:
	clrf 	AccA
	clrf	AccA+1
	clrf	AccA+2
	movfw	COMPARATOR_HIGH_RESULT
	movwf	AccA+3
	goto	display_5_digit_number



;-------------------- simply dumps TMR1H and TMR1L value to console
DISPLAY_VOLTAGE:
        clrf    AccA
        clrf    AccA+1
        movfw 	TMR1H
        movwf   AccA+2
        movfw   TMR1L
        movwf   AccA+3

display_5_digit_number:		; this is common subroutine to display 5 digit (16bit decimal) number. 

;       Format as BCD string
;        iorwf   FPE,f           ; W may hold Error (0xff)

        call    B2_BCD          ; format as BCD
                                ; extract and send to display


        MOVF    bcd+5,W         ; 5  
        CALL    PutNyb

        SWAPF   bcd+6,W         ; 4  
        CALL    PutNyb

	movlw	"."
	call	putchr		; . 


        MOVF    bcd+6,W         ; 3  
        CALL    PutNyb

        SWAPF   bcd+7,W         ; 2  
        CALL    PutNyb

        MOVF    bcd+7,W         ; 1  
        CALL    PutNyb

	movlw	" "
	call	putchr		; " " space after each number 

	return

		


;-------------------------16bit ADC subroutine

CONVERT:	
		bank0
		compare_gp1_gp0		; turn on comparator, compare gp1 (inverting) and gp0 (noninverting)
		clrf 	TMR1L       ; Clear Timer 1 L
		clrf 	TMR1H       ; Clear Timer 1 H

		bank1
		bsf	COMPARATOR_INVERTING_INPUT_TRIS  ; set GP0 as input
		bcf 	PIR1,TMR1IF

DISCHARGE_COMPARATOR_CAP:
		bank1            ; 
		bcf 	COMPARATOR_NONINVERTING_INPUT_TRIS		; Set comparator inverting input as output
		bank0
		bcf 	COMPARATOR_NONINVERTING_INPUT		; Discharge capacitor
		clrf	DELAY
DISCHARGE:	
		decfsz  DELAY
		goto 	DISCHARGE
		clrf	DELAY
DISCHARGE2:	
		decfsz  DELAY
		goto 	DISCHARGE2
		clrf	DELAY
DISCHARGE3:	
		decfsz  DELAY
		goto 	DISCHARGE3
		clrf	DELAY
DISCHARGE4:	
		decfsz  DELAY
		goto 	DISCHARGE4

		bank1		;  switch bank to 1
		bsf	COMPARATOR_NONINVERTING_INPUT_TRIS		; make comparator inverting input input, 
		bank0		; switch back to bank0 . 
		btfsc	COMPARATOR_NONINVERTING_INPUT		;  make sure capacitor got discharged
		goto 	DISCHARGE_COMPARATOR_CAP		; if not, go back discharging it. 
				; else 
		bcf 	PIR1,CMIF				; Clear comp Int flag

				 ; cap starts charging 
				
		bank0
		bsf 	T1CON,TMR1ON				; Start Timer 1
		movf	CMCON,f					; Read to sync output
		bank1
		bsf	PIE1,TMR1IE				; unmask TMR1 interrupt 
		nop
		bsf	PIE1,CMIE				; Unmask Comp Int

WAIT:		nop
		btfsc 	PIE1,CMIE				; Wait for comp Int
								; ie wait until Int
		goto 	WAIT        				; Enable bit has been
				; cleared
GETDATA:			; Timer 1 contains
				; DATA (TMR1L TMR1H)

		bank1            ; 
		bcf	COMPARATOR_NONINVERTING_INPUT_TRIS		; Set comparator inverting input as output. not sure if this will make results better or worse...
		bank0
		bcf 	COMPARATOR_NONINVERTING_INPUT		; Discharge capacitor
		comparator_off					; turn off comparator to conserve power

		return

;           Comparator Interrupt Service routine

COMP:		bank0
		bcf 	T1CON,TMR1ON				; Stop Timer 1
		bcf 	PIR1,CMIF					; Clear Interrupt flag
		nop
		bcf 	PIR1,TMR1IF
		bank1
		bcf 	PIE1,CMIE					; Mask Comparator Int
		nop
		bcf 	PIE1,TMR1IE
		retfie 

;--------------------------measure voltage using built in reference voltage 

CONVERT_COMPARATOR_LOW:
	clrf	COMPARATOR_LOW_RESULT
	compare_gp1_CVref
convert_low_loop:
	movfw	COMPARATOR_LOW_RESULT
	call	set_vrcon_low
	btfsc	CMCON,COUT
	goto	comp_conv_exit
	incfsz	COMPARATOR_LOW_RESULT	
	goto	convert_low_loop

comp_conv_exit:
	comparator_off
	return


CONVERT_COMPARATOR_HIGH:
	clrf	COMPARATOR_HIGH_RESULT
	compare_gp1_CVref
convert_high_loop:
	movfw	COMPARATOR_HIGH_RESULT
	call	set_vrcon_high
	btfsc	CMCON,COUT
	goto	comp_conv_exit	
	incfsz	COMPARATOR_HIGH_RESULT
	goto	convert_high_loop

	comparator_off
	return




;------------watchdog triggered sleep delay routines

short_sleep:            ; less than second
        clrwdt                  ; wdt must start from 0 , otherwise will overflow..
        bank1
        bcf     OPTION_REG,PS2
        nop
        bcf     OPTION_REG,PS1
        sleep
        bsf     OPTION_REG,PS2
        nop
        bsf     OPTION_REG,PS1
        bank0
        clrwdt

        return

long_sleep:             ; about 2 seconds
        clrwdt
	comparator_off
	call	set_vrcon_off
        sleep
        clrwdt
        return

;--------------------vrcon subroutines

set_vrcon_off:
        bank1
        movlw   b'00000000'     ; set VRCON off
	goto	common_set_vrcon

set_vrcon_high:
	bank1
	andlw	b'00001111'
	iorlw	b'10100000'	; enable voltage reference (VREN bit) , VRR - 1 - high
	goto	common_set_vrcon

set_vrcon_low:
	bank1
	andlw	b'00001111'	; only values 0-15 matter
	iorlw	b'10000000'	; enable voltage reference (VREN bit) , VRR - 0 - low

common_set_vrcon:
        movwf   VRCON           ;       ~6.13V, 1.5325V per cell!
        bank0
	return

;******************************************************************
;
;       Convert 32-bit binary number at <AccA:4> into a bcd number
;       at <bcd:5>. Uses Mike Keitz's procedure for handling bcd
;       adjust. Microchip AN526
;

B2_BCD

b2bcd   movlw   .32             ; 32-bits
        movwf   COUNT           ; make cycle counter

        clrf    bcd+0           ; clear result area
        clrf    bcd+1
        clrf    bcd+2
        clrf    bcd+3
        clrf    bcd+4
        clrf    bcd+5
        clrf    bcd+6
        clrf    bcd+7


b2bcd2  movlw   bcd             ; make pointer
        movwf   FSR
        movlw   .8              ; Number of BCD bytes?
        movwf   cnt             ; 2 BCD digits per byte

; Mike's routine:

b2bcd3  movlw   0x33
        addwf   INDF,f          ; add to both nybbles
        btfsc   INDF,3          ; test if low result > 7
        andlw   0xf0            ; low result >7 so take the 3 out
        btfsc   INDF,7          ; test if high result > 7
        andlw   0x0f            ; high result > 7 so ok
        subwf   INDF,f          ; any results <= 7, subtract back
        incf    FSR,f           ; point to next
        decfsz  cnt,f
        goto    b2bcd3

        rlf     AccA+3,f        ; get another bit
        rlf     AccA+2,f
        rlf     AccA+1,f
        rlf     AccA+0,f

        rlf     bcd+7,f
        rlf     bcd+6,f
        rlf     bcd+5,f
        rlf     bcd+4,f         ; put it into bcd
        rlf     bcd+3,f
        rlf     bcd+2,f
        rlf     bcd+1,f
        rlf     bcd+0,f

        decfsz  COUNT,f         ; all done?
        goto    b2bcd2          ; no, loop
        return                  ; yes




;***********************************************************************
;
;    Print CRLF to serial
;

crlf
        movlw   0x0d            ; CRLF
        call    putchr
       movlw   0x0a            ; for gnuplot - only LF.
       goto    putchr          ;

;***********************************************************************
; 
; 	print  one nybble (4 bit BCD) to serial 
; 

PutNyb  ANDLW   0x0F            ; MASK OFF OTHER PACKED BCD DIGIT
        ADDLW   0x30            ; Convert BIN to ASCII

;**********************************************************
;
;       Put a data byte to display
;

DATS    movwf   TEMP            ; Save character for LCD
        call    putchr

        RETLW   0



;====================== calibration area - type your result
        org     0x3ff
calibration_hot:
;        retlw   d'55'   ; here
        retlw   d'56'   ; here

END
