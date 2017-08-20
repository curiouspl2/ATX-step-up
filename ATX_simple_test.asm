; this is dc-dc step up module cooperating with cheap ATX power supply.

; this is the simplest version which works in open loop allowing testing of various duty cycles. 
;
; AN1 pin regulates the duty cycle. 

 
;
;       CPU configuration
;

        MESSG           "Processor = 12F675"
        #define         RAMStart        0x20
        processor       12f675
        include         <p12f675.inc>

        __config        _INTRC_OSC_NOCLKOUT & _PWRTE_ON & _WDT_OFF & _CP_OFF & _BODEN_ON  & _MCLRE_OFF
;        __config        _INTRC_OSC_NOCLKOUT & _PWRTE_ON & _WDT_ON & _CP_OFF & _BODEN_ON  & _MCLRE_OFF
;       __config        _INTRC_OSC_NOCLKOUT & _PWRTE_ON & _WDT_OFF & _CP_OFF & _BODEN_ON  & _MCLRE_OFF

;*******************************************************************

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

analog_off	macro
	bcf	ADCON0,ADON
	endm

analog_GP0	macro
	bcf	ADCON0,ADFM
	nop
	bcf	ADCON0,CHS0
	nop
	bcf	ADCON0,CHS1
	nop
	bsf	ADCON0,ADON
	endm

analog_GP1	macro
	bcf	ADCON0,ADFM
	nop
	bcf	ADCON0,CHS0
	nop
	bsf	ADCON0,CHS1
	nop
	bsf	ADCON0,ADON
	endm

analog_GP2	macro
	bcf	ADCON0,ADFM
	nop
	bsf	ADCON0,CHS0
	nop
	bcf	ADCON0,CHS1
	nop
	bsf	ADCON0,ADON
	endm

analog_GP4	macro
	bcf	ADCON0,ADFM
	nop
	bsf	ADCON0,CHS0
	nop
	bsf	ADCON0,CHS1
	nop
	bsf	ADCON0,ADON
	endm

check_voltage 	macro
		clrwdt
		compare_gp1_CVref
		movlw	b'00001111'
		call	set_vrcon_high
		btfss	CMCON,COUT
		goto	voltage_too_high
		call 	ADC_GP1
		endm


	


#define		COMPARATOR_INVERTING_INPUT_TRIS		TRISIO,GP1
;#define		COMPARATOR_NONINVERTING_INPUT_TRIS	TRISIO,GP0
#define		COMPARATOR_INVERTING_INPUT		GPIO,GP1
;#define		COMPARATOR_NONINVERTING_INPUT		GPIO,GP0
#define 	COMPARATOR_INVERTING_INPUT_ANSEL	ANSEL,GP1
;#define		COMPARATOR_NONINVERTING_INPUT_ANSEL	ANSEL,GP0


;#define		ATX_12V_ANSEL				ANSEL,GP5
#define		IN_1_TRIS				TRISIO,GP5
#define		IN_1					GPIO,GP5

#define		IN_2_ANSEL				ANSEL,GP2
#define		IN_2_TRIS				TRISIO,GP2
#define		IN_2					GPIO,GP2

#define		IN_3_ANSEL				ANSEL,GP4
#define		IN_3_TRIS				TRISIO,GP4
#define		IN_3					GPIO,GP4

#define		IN_4_ANSEL				ANSEL,GP0
#define		IN_4_TRIS				TRISIO,GP0
#define		IN_4					GPIO,GP0

;#define		ENABLE_ANSEL				ANSEL,GP3
#define		ENABLE_TRIS				TRISIO,GP3
#define		ENABLE					GPIO,GP3

#define		LED_IR_ANSEL				ANSEL,GP0
#define		LED_IR_TRIS				TRISIO,GP0
#define		LED_IR					GPIO,GP0


; ----------input weights
#define		IN_1_weight	1
#define		IN_2_weight	2
#define		IN_3_weight	1
#define		IN_4_weight	1


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
	COMPARATOR_LOW_1_RESULT	; internal comparator based ADC result low range
	COMPARATOR_HIGH_1_RESULT 	; internal comparator based ADC result high range
	COMPARATOR_LOW_2_RESULT	; internal comparator based ADC result low range
	COMPARATOR_HIGH_2_RESULT 	; internal comparator based ADC result high range
	COMPARATOR_LOW_3_RESULT	; internal comparator based ADC result low range
	COMPARATOR_HIGH_3_RESULT 	; internal comparator based ADC result high range
	COMPARATOR_LOW_4_RESULT	; internal comparator based ADC result low range
	COMPARATOR_HIGH_4_RESULT 	; internal comparator based ADC result high range
	
	error_1_H
	error_1_L
	error_2_H
	error_2_L
	error_3_H
	error_3_L
	error_4_H
	error_4_L

	safety_init
        endc



		org 0            ; Reset Vector
		goto INIT

		org 4            ; Interrupt Vector
		retfie		;return from interrupt
;		goto COMP
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

		bcf	INTCON,GIE	; disable interrupts globally as they change bank randomly
;		bsf INTCON,GIE   ; Enable Global Int
;		bsf INTCON,PEIE  ; Unmask Peripheral Int

it_is_hot:
	        bank1
	        call    calibration_hot		; calibration value for hot environment (above 0C)
        	movwf   OSCCAL			; calibrate internal oscillator - required as we do serial out which is timing critical

quit_thermal_calibration:
        	bank0   

		bank1
		bsf	COMPARATOR_INVERTING_INPUT_TRIS 
		nop
;		bsf	COMPARATOR_NONINVERTING_INPUT_TRIS
		nop
		bsf	COMPARATOR_INVERTING_INPUT_ANSEL
		nop
;		bsf	COMPARATOR_NONINVERTING_INPUT_ANSEL

		; GP1 is set as input capable of ADC and analog comparator 


		bsf	ANSEL,ADCS0
		nop
		bcf	ANSEL,ADCS1	
		nop
		bsf	ANSEL,ADCS2	
		nop
		; 8Tosc is 001 - base for ADC system

		bcf	IN_1_TRIS
		nop
;		bcf	IN_1_ANSEL	; not applicable
		nop
		bcf	IN_2_TRIS
		nop
		bcf	IN_2_ANSEL
		nop
		bcf	IN_3_TRIS
		nop
		bcf	IN_3_ANSEL
		nop
		bcf	IN_4_TRIS
		nop
		bcf	IN_4_ANSEL
		nop
		; ATX DC DC converter pins set to outputs
		
		bsf	ENABLE_TRIS
		nop
;		bsf	ENABLE_ANSEL 
		nop
		; ENABLE pin as input and capable of analog input. it is not capable of analog input. 

		bank0

		bcf	IN_1
		nop
		bcf	IN_2
		nop
		bcf	IN_3
		nop
		bcf	IN_4
		nop

		clrf	safety_init	; clear safety init countdown

MAIN:		
		decfsz 	safety_init
		goto	MAIN_MAIN
		goto	INIT		; initalise all inputs once in a while to prevent EMP bugs
MAIN_MAIN:
		clrwdt
		compare_gp1_CVref
		movlw	b'00001111'
		call	set_vrcon_high
voltage_too_high:
		btfss	CMCON,COUT
		goto	voltage_too_high
		call	ADC_GP1
		call	CONVERT_COMPARATOR_HIGH_1
		check_voltage			;check
		call 	store_error_1
		call	CONVERT_COMPARATOR_HIGH_2
		check_voltage			;check
		call	store_error_2
		call	CONVERT_COMPARATOR_HIGH_3
		check_voltage			;check
		call	store_error_3
		call	CONVERT_COMPARATOR_HIGH_4
		check_voltage			;check
		call	store_error_4

		check_voltage

		call	rough_compensate_1
		call	rough_compensate_2
		call	rough_compensate_3
		call	rough_compensate_4

		goto	MAIN

;-----------------------------------

rough_compensate_1:
		movfw	COMPARATOR_HIGH_1_RESULT	; 1- highest error, 15 - lowest error
		andlw	b'00001111'
		sublw 	b'00001111'			; 15 highest error, 1 = lowest error
		movwf	DELAY
		incf	DELAY	; make sure it is not 0

rough_compensate_1_loop:
		call	high_duty_cycle_1
		decfsz	DELAY
		goto	rough_compensate_1_loop
		return

rough_compensate_2:
		movfw	COMPARATOR_HIGH_2_RESULT	; 1- highest error, 15 - lowest error
		andlw	b'00001111'
		sublw 	b'00001111'			; 15 highest error, 1 = lowest error
		movwf	DELAY
		incf	DELAY	; make sure it is not 0

rough_compensate_2_loop:
		call	high_duty_cycle_2
		decfsz	DELAY
		goto	rough_compensate_2_loop
		return

rough_compensate_3:
		movfw	COMPARATOR_HIGH_3_RESULT	; 1- highest error, 15 - lowest error
		andlw	b'00001111'
		sublw 	b'00001111'			; 15 highest error, 1 = lowest error
		movwf	DELAY
		incf	DELAY	; make sure it is not 0

rough_compensate_3_loop:
		call	high_duty_cycle_3
		decfsz	DELAY
		goto	rough_compensate_3_loop
		return

rough_compensate_4:
		movfw	COMPARATOR_HIGH_4_RESULT	; 1- highest error, 15 - lowest error
 		andlw	b'00001111'
		sublw 	b'00001111'			; 15 highest error, 1 = lowest error
		movwf	DELAY
		incf	DELAY	; make sure it is not 0

rough_compensate_4_loop:
		call	high_duty_cycle_4
		decfsz	DELAY
		goto	rough_compensate_4_loop
		return



; IN1 - GP5 
; IN2 - GP2 
; IN3 - GP4
; IN4 - GP0


high_duty_cycle_1:
		movlw	b'00100000'
		goto duty_cycle_common
high_duty_cycle_2:
		movlw	b'00000100'
		goto duty_cycle_common
high_duty_cycle_3:
		movlw	b'00010000'
		goto duty_cycle_common
high_duty_cycle_4:
		movlw	b'00000001'
		goto duty_cycle_common

duty_cycle_common:
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
		movwf	GPIO	;on
		clrf	GPIO	 ; off
;16
		return


;		call 	CONVERT			; measure voltage of battery
;		clrwdt
;		call	DISPLAY_VOLTAGE		; display voltage
;		clrwdt
;		call 	CONVERT_COMPARATOR_LOW	; measure voltage of battery using internal reference voltage and comparator
;		clrwdt
;		call 	DISPLAY_COMPARISON_RESULT_LOW ; display voltage of battery measured using comparator. 
;
;		clrwdt
;		call 	CONVERT_COMPARATOR_HIGH	; measure voltage of battery using internal reference voltage and comparator
;		clrwdt
;		call 	DISPLAY_COMPARISON_RESULT_HIGH	 ; display voltage of battery measured using comparator. 
;
;		call	crlf			; new line. 
;		clrwdt
;	call 	long_sleep		; sleep for 2 seconds.
;		goto MAIN

ADC_GP0:
	btfsc 	ADCON0,GO
	return
	analog_GP0
	nop
	bsf	ADCON0,GO
	return

ADC_GP1:
	btfsc 	ADCON0,GO
	return
	analog_GP1
	nop
	bsf	ADCON0,GO
	return

store_error_1:
	bank1
	movfw	ADRESL
	movwf	error_1_L
	movfw	ADRESH
	movwf	error_1_H
	bank0
	return

store_error_2:
	bank1
	movfw	ADRESL
	movwf	error_2_L
	movfw	ADRESH
	movwf	error_2_H
	bank0
	return

store_error_3:
	bank1
	movfw	ADRESL
	movwf	error_2_L
	movfw	ADRESH
	movwf	error_2_H
	bank0
	return

store_error_4:
	bank1
	movfw	ADRESL
	movwf	error_2_L
	movfw	ADRESH
	movwf	error_2_H
	bank0
	return





DISPLAY_COMPARISON_RESULT_LOW:
	clrf 	AccA
	clrf	AccA+1
	clrf	AccA+2
	movfw	COMPARATOR_LOW_1_RESULT
	movwf	AccA+3
	goto	display_5_digit_number

DISPLAY_COMPARISON_RESULT_HIGH:
	clrf 	AccA
	clrf	AccA+1
	clrf	AccA+2
	movfw	COMPARATOR_HIGH_1_RESULT
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

;CONVERT:	
;		bank0
;		compare_gp1_gp0		; turn on comparator, compare gp1 (inverting) and gp0 (noninverting)
;		clrf 	TMR1L       ; Clear Timer 1 L
;		clrf 	TMR1H       ; Clear Timer 1 H
;
;		bank1
;		bsf	COMPARATOR_INVERTING_INPUT_TRIS  ; set GP0 as input
;		bcf 	PIR1,TMR1IF
;
;DISCHARGE_COMPARATOR_CAP:
;		bank1            ; 
;		bcf 	COMPARATOR_NONINVERTING_INPUT_TRIS		; Set comparator inverting input as output
;		bank0
;		bcf 	COMPARATOR_NONINVERTING_INPUT		; Discharge capacitor
;		clrf	DELAY
;DISCHARGE:	
;		decfsz  DELAY
;		goto 	DISCHARGE
;		clrf	DELAY
;DISCHARGE2:	
;		decfsz  DELAY
;		goto 	DISCHARGE2
;		clrf	DELAY
;DISCHARGE3:	
;		decfsz  DELAY
;		goto 	DISCHARGE3
;		clrf	DELAY
;DISCHARGE4:	
;		decfsz  DELAY
;		goto 	DISCHARGE4
;
;		bank1		;  switch bank to 1
;		bsf	COMPARATOR_NONINVERTING_INPUT_TRIS		; make comparator inverting input input, 
;		bank0		; switch back to bank0 . 
;		btfsc	COMPARATOR_NONINVERTING_INPUT		;  make sure capacitor got discharged
;		goto 	DISCHARGE_COMPARATOR_CAP		; if not, go back discharging it. 
;				; else 
;		bcf 	PIR1,CMIF				; Clear comp Int flag
;
;				 ; cap starts charging 
;				
;		bank0
;		bsf 	T1CON,TMR1ON				; Start Timer 1
;		movf	CMCON,f					; Read to sync output
;		bank1
;		bsf	PIE1,TMR1IE				; unmask TMR1 interrupt 
;		nop
;		bsf	PIE1,CMIE				; Unmask Comp Int
;
;WAIT:		nop
;		btfsc 	PIE1,CMIE				; Wait for comp Int
;								; ie wait until Int
;		goto 	WAIT        				; Enable bit has been
;				; cleared
;GETDATA:			; Timer 1 contains
;				; DATA (TMR1L TMR1H)
;
;		bank1            ; 
;		bcf	COMPARATOR_NONINVERTING_INPUT_TRIS		; Set comparator inverting input as output. not sure if this will make results better or worse...
;		bank0
;		bcf 	COMPARATOR_NONINVERTING_INPUT		; Discharge capacitor
;		comparator_off					; turn off comparator to conserve power
;
;		return
;
;           Comparator Interrupt Service routine
;
;COMP:		bank0
;		bcf 	T1CON,TMR1ON				; Stop Timer 1
;		bcf 	PIR1,CMIF					; Clear Interrupt flag
;		nop
;		bcf 	PIR1,TMR1IF
;		bank1
;		bcf 	PIE1,CMIE					; Mask Comparator Int
;		nop
;		bcf 	PIE1,TMR1IE
;		retfie 

;--------------------------measure voltage using built in reference voltage 

;CONVERT_COMPARATOR_LOW:
;	clrf	COMPARATOR_LOW_RESULT
;	compare_gp1_CVref
;convert_low_loop:
;	movfw	COMPARATOR_LOW_RESULT
;	call	set_vrcon_low
;	btfsc	CMCON,COUT
;	goto	comp_conv_exit
;	incfsz	COMPARATOR_LOW_RESULT	
;	goto	convert_low_loop

;CONVERT_COMPARATOR_HIGH:
;	clrf	COMPARATOR_HIGH_RESULT
;	compare_gp1_CVref
;convert_high_loop:
;	movfw	COMPARATOR_HIGH_RESULT
;	call	set_vrcon_high
;	btfsc	CMCON,COUT
;	goto	comp_conv_exit	
;	incfsz	COMPARATOR_HIGH_RESULT
;	goto	convert_high_loop
;	goto	comp_conv_exit



comp_conv_exit:
	comparator_off
	call 	set_vrcon_off
	return

CONVERT_COMPARATOR_LOW_1:
	bsf	IN_1		; on
	clrf	COMPARATOR_LOW_1_RESULT
	compare_gp1_CVref
	bcf	IN_1		;off
	nop
convert_low_1_loop:
	bsf	IN_1		;on
	movfw	COMPARATOR_LOW_1_RESULT
	bcf	IN_1		;off
	call	set_vrcon_low
	btfsc	CMCON,COUT
	goto	comp_conv_exit
	incfsz	COMPARATOR_LOW_1_RESULT	
	goto	convert_low_1_loop
	movlw	b'00010000'
	movwf	COMPARATOR_LOW_1_RESULT
	goto	comp_conv_exit

CONVERT_COMPARATOR_HIGH_1:
	bsf	IN_1		; on
	clrf	COMPARATOR_HIGH_1_RESULT
	compare_gp1_CVref
	bcf	IN_1		;off
	nop
convert_high_1_loop:
	bsf	IN_1		;on
	movfw	COMPARATOR_HIGH_1_RESULT
	bcf	IN_1		;off
	call	set_vrcon_high
	btfsc	CMCON,COUT
	goto	comp_conv_exit
	incfsz	COMPARATOR_HIGH_1_RESULT	
	goto	convert_high_1_loop
	movlw	b'00010000'
	movwf	COMPARATOR_HIGH_1_RESULT
	goto	comp_conv_exit

CONVERT_COMPARATOR_HIGH_2:
	bsf	IN_2		; on
	clrf	COMPARATOR_HIGH_2_RESULT
	compare_gp1_CVref
	bcf	IN_2		;off
	nop
convert_high_2_loop:
	bsf	IN_2		;on
	movfw	COMPARATOR_HIGH_2_RESULT
	bcf	IN_2		;off
	call	set_vrcon_high
	btfsc	CMCON,COUT
	goto	comp_conv_exit
	incfsz	COMPARATOR_HIGH_2_RESULT	
	goto	convert_high_2_loop
	movlw	b'00010000'
	movwf	COMPARATOR_HIGH_2_RESULT
	goto	comp_conv_exit

CONVERT_COMPARATOR_HIGH_3:
	bsf	IN_3		; on
	clrf	COMPARATOR_HIGH_3_RESULT
	compare_gp1_CVref
	bcf	IN_3		;off
	nop
convert_high_3_loop:
	bsf	IN_3		;on
	movfw	COMPARATOR_HIGH_3_RESULT
	bcf	IN_3		;off
	call	set_vrcon_high
	btfsc	CMCON,COUT
	goto	comp_conv_exit
	incfsz	COMPARATOR_HIGH_3_RESULT	
	goto	convert_high_3_loop
	movlw	b'00010000'
	movwf	COMPARATOR_HIGH_3_RESULT
	goto	comp_conv_exit

CONVERT_COMPARATOR_HIGH_4:
	bsf	IN_4		; on
	clrf	COMPARATOR_HIGH_4_RESULT
	compare_gp1_CVref
	bcf	IN_4		;off
	nop
convert_high_4_loop:
	bsf	IN_4		;on
	movfw	COMPARATOR_HIGH_4_RESULT
	bcf	IN_4		;off
	call	set_vrcon_high
	btfsc	CMCON,COUT
	goto	comp_conv_exit
	incfsz	COMPARATOR_HIGH_4_RESULT	
	goto	convert_high_4_loop
	movlw	b'00010000'
	movwf	COMPARATOR_HIGH_4_RESULT
	goto	comp_conv_exit






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
	iorlw	b'10000000'	; enable voltage reference (VREN bit) , VRR - 0 - high
	goto	common_set_vrcon

set_vrcon_low:
	bank1
	andlw	b'00001111'	; only values 0-15 matter
	iorlw	b'10100000'	; enable voltage reference (VREN bit) , VRR - 1 - low

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







;====================== calibration area - type your result
        org     0x3ff
calibration_hot:
;        retlw   d'55'   ; here
        retlw   d'56'   ; here

END
