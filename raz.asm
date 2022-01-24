[org 0x0100]
jmp start

oldsir: dd 0
sc: db 'Score: ',0

tickcount: dw 0
sec: dw 00
minu: dw 00
time: db 'Time: 0 :  ',0

checkifmove: dw 0

lives: 	dw 10
livess:	db 'Lives:   ',0


opn1: db 'PLEASE PRESS ENTER TO START THE GAME',0
opn2: db 'GAME OVER',0
opn3: db 'YOUR SCORE IS   ',0


lastpg:
	pusha

	mov ax, 0xb800
	mov es, ax
	mov cx, 2000
	mov ax, 0x3020
	mov di, 0
	cld
	rep stosw

	push word 36
	push word 10
	push word 0x1E
	push opn2
	call printstr

	push word 30
	push word 12
	push word 0x79
	push opn3
	call printstr

	mov di,2008  ; push di position
	push di
	mov ax,[points]
	push ax
	call printnum

	popa
	ret
disptime: 	pusha

	cmp word [sec],30
	jne continu

	cmp word [cs:tickcount],17
	jne continu

minlife:	cmp word [checkifmove],0
		jne continu

		dec word [lives]
	

continu:	mov ax,0xb800
		mov es,ax

		push word 20
		push word 0
		push word 0x71
		push livess
		call printstr

		mov di,54 ; push di position
		push di
		mov ax,[lives]
		push ax
		call printnum

	push word 60
	push word 0
	push word 0x71
	push time
	call printstr

	inc word [cs:tickcount]; increment tick count
   	cmp word [cs:tickcount],18
	je incremen
	jne endtim

incremen:	
	inc word [sec]
	mov word [cs:tickcount],0

	cmp word [sec],60
	je min
	jne endtim

min:	inc word [minu]
	mov word [sec],0


endtim:	push word 134
	push word [minu]
	call printnum ; print tick count	

	push word 138
	push word [sec]
	call printnum ; print tick count

	mov al, 0x20
	out 0x20, al ; end of interrupt
	popa
	iret ; return from interrupt


starttime: pusha

	xor ax, ax
	mov es, ax ; point es to IVT base

	mov ax,[es:8*4]
	mov [oldsir],ax

	mov ax,[es:8*4+2]
	mov [oldsir+2],ax

	cli ; disable interrupts
	mov word [es:8*4], disptime ; store offset at n*4
	mov [es:8*4+2], cs ; store segment at n*4+2
	sti ; enable interrupts
	mov dx, start ; end of resident portion
	add dx, 15 ; round up to next para
	mov cl, 4
	shr dx, cl ; number of paras

	popa
	ret

endt:		pusha

		xor ax,ax
		mov es,ax
		cli
		mov ax,[oldsir]
		mov [es:8*4],ax

		mov ax,[oldsir+2]
		mov [es:8*4+2],ax
		sti

		popa
		ret

open2:
	pusha

	mov ax, 0xb800
	mov es, ax
	mov cx, 2000
	mov ax, 0x4020
	mov di, 0
	cld
	rep stosw

	push word 22
	push word 9
	push word 0x9E
	push opn1
	call printstr

	call ground

	push 15 ; y position for line (starting position)
	push 8 ; x position for line (starting position)
	call tree
	
	push 15 ; y position for line (starting position)
	push 23 ; x position for line (starting position)
	call tree

	push 15 ; y position for line (starting position)
	push 38  ; x position for line (starting position)
	call tree

	push 15 ; y position for line (starting position)
	push 53  ; x position for line (starting position)
	call tree
		
	push 15 ; y position for line (starting position)
	push 67 ; x position for line (starting position)
	call tree


	popa
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clrscr: 
		push es
		push ax
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base
		mov di, 0 ; point di to top left column
		nextloc:
		mov word [es:di], 0x7020 ; clear next char on screen
		add di, 2 ; move to next screen location
		cmp di, 4000 ; has the whole screen cleared
		jne nextloc ; if no clear next position
		pop di
		pop ax
		pop es
		ret
	
	sky:
		push es
		push ax
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base
		mov di, 0 ; point di to top left column
		nextloc1:
		mov word [es:di], 0x3020 ; clear next char on screen
		add di, 2 ; move to next screen location
		cmp di, 4000 ; has the whole screen cleared
		jne nextloc1 ; if no clear next position
		pop di
		pop ax
		pop es
		ret

	
	ground:
		push es
		push ax
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base
		mov di, 3520 ;  hard coded  (22 * 80 + 0) * 2 last 3 lines
		nextloc2:
		mov word [es:di], 0x2ADB ; clear next char on screen
		add di, 2 ; move to next screen location
		cmp di, 4000 ; has the whole screen cleared
		jne nextloc2 ; if no clear next position
		pop di
		pop ax
		pop es
		ret
		
	pig:
		push bp
		mov bp,sp
		push es
		push ax
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base	
		mov al, 80 ; load al with columns per row
		
		mul byte [bp+6] ; multiply with y position
		add ax, [bp+4] ; add x position
		shl ax, 1 	
		mov di, ax  
		mov word [es:di], 0x2020 ; put pig on this location
		
		
		
		pop di
		pop ax
		pop es
		pop bp
		ret 4
	
	pig2:
		push bp
		mov bp,sp
		push es
		push ax
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base	
		mov al, 80 ; load al with columns per row
		
		mul byte [bp+6] ; multiply with y position
		add ax, [bp+4] ; add x position
		shl ax, 1 	
		mov di, ax  
		mov word [es:di], 0x5AF8 ; put pig on this location with eye
		
		add di,2
		mov word [es:di], 0x5AF8 ; make pig fat  with eye
		
		
		
		pop di
		pop ax
		pop es
		pop bp
		ret 4
	
	pig3:
		push bp
		mov bp,sp
		push es
		push ax
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base	
		mov al, 80 ; load al with columns per row
		
		mul byte [bp+6] ; multiply with y position
		add ax, [bp+4] ; add x position
		shl ax, 1 	
		mov di, ax  
		mov word [es:di], 0x5FE9 ; put pig on this location
		
    		add di,2
		mov word [es:di], 0x5FE9 ; make pig fat
		
		
		pop di
		pop ax
		pop es
		pop bp
		ret 4


        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        cloud:
		push bp
		mov bp,sp
		push ax
		push bx
		push cx
		push dx   ; to store ascii  
		push cx   ; for length
		
		push word [bp + 10]   ;character and color to be printed
		push word[bp + 8] ; y position for line (starting position)
		push word [bp + 6]  ; x position for line (starting position)
		push word [bp + 4]; length of line
		call hline
		
		
		add word [bp + 8],1
		sub word [bp + 6],2
		add word [bp + 4],4
		
		
		push word [bp + 10]   ;character and color to be printed
		push word[bp + 8] ; y position for line (starting position)
		push word [bp + 6]  ; x position for line (starting position)
		push word [bp + 4]; length of line
		call hline
		
		add word [bp + 8],1
		add word [bp + 6],2
		sub word [bp + 4],4
		
		
		push word [bp + 10]   ;character and color to be printed
		push word[bp + 8] ; y position for line (starting position)
		push word [bp + 6]  ; x position for line (starting position)
		push word [bp + 4]; length of line
		call hline
		
	
		
		
		
		
		
		pop cx
		pop bx
		pop di
		pop ax
		pop es
		pop bp
		ret 8

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	sleep:		push cx
		push bx

		mov cx, 0xFFFF
	delay:	loop delay

		pop bx
		pop cx

		ret 
	
	
	
	
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		move:
			
                        pusha
						
						mov word [checkifmove],1
						mov cx,15
						
						birdloop:
						call clrscr;
						call background
						call obstacles
						
						
						push word [points]   
						call score
						
						add word [xpos],1
						sub word [ypos],1
						
						push word [ypos] ; y position for bird
						push word [xpos]  ; x position for bird
						
						
						call bird
						call sleep
						call sleep
						
						loop birdloop
						
						mov cx,35
						
						birdloop3:
						call clrscr;
						call background
						call obstacles
						
						
						add word [xpos],1
						
						
						push word [ypos] ; y position for bird
						push word [xpos]  ; x position for bird
						
						
						call bird
						call sleep
						call sleep
						
						loop birdloop3
						
						
						
						
						mov cx,15
						
						birdloop2:
						
						cmp word [stop],1
						je end
						
						call clrscr;
						call background
						call obstacles
						
						
						add word [xpos],1
						add word [ypos],1
						
						push word [ypos] ; y position for bird
						push word [xpos]  ; x position for bird
						
						
						call bird
						call sleep
						call sleep
						
						loop birdloop2
						
						end:
						
						
                        
                        popa
                        ret

			
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
	
	bird:
		push bp
		mov bp,sp
		push es
		push ax
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base	
		mov al, 80 ; load al with columns per row
		
		mul byte [bp+6] ; multiply with y position
		add ax, [bp+4] ; add x position
		shl ax, 1 	
		mov di, ax  
		
		
		
		
                
		mov word [es:di], 0x4020 ; put bird on this location
		mov word [es:di-158], 0xC76F ; put bird on this location
                add di, 2
                mov word [es:di], 0x4020
                add di, 2
                mov word [es:di], 0x4020
				
			add di,2
			
			cmp word [es:di],0x6631
			jne skip1
			;1		
			push 0x3020   ;character and color to be printed
			push 15 ; y position for line (starting position)
			push 67  ; x position for line (starting position)
			push 21 ; y position for line (ending position)
			push 67 ; x position for line (ending position)
			call vline   ;left line
			mov word [flag + 1],1
			mov word [stop],1
			
			add word [points],10
			
			skip1:
			
			cmp word [es:di],0x6632
			jne skip2
			;2
			push 0x3020  ;character and color to be printed
			push 18 ; y position for line (starting position)
			push 65 ; x position for line (starting position)
			push 13 ; length of line
			call hline
			mov word [flag + 2],1
			mov word [stop],1
			add word [points],10
			skip2:
			
			cmp word [es:di],0x6633 
			jne skip3
			;3
			push 0x3020   ;character and color to be printed
			push 21 ; y position for line (starting position)
			push 65  ; x position for line (starting position)
			push 13  ; length of line
			call hline   ;ground floor
			mov word [flag + 3],1
			mov word [stop],1
			add word [points],10
			skip3:
			
			
			cmp word [es:di],0x6630 
			jne skip0
			;0
			push 0x3020  ;character and color to be printed
			push 15 ; y position for line (starting position)
			push 65  ; x position for line (starting position)
			push 13 ; length of line
			call hline
			mov word [flag + 0],1
			mov word [stop],1
			add word [points],10
			skip0:
			
			
			cmp word [es:di],0x6634 
			jne skip4
			;4
			push 0x3020   ;character and color to be printed
			push 15 ; y position for line (starting position)
			push 75  ; x position for line (starting position)
			push 22 ; y position for line (ending position)
			push 75 ; x position for line (ending position)
			call vline   ;right line
			mov word [flag + 4],1
			mov word [stop],1
			add word [points],10
			skip4:	

			cmp word [es:di],0x6635
			jne skip5
			;5
			push 0x3020   ;character and color to be printed
			push 12 ; y position for line (starting position)
			push 71  ; x position for line (starting position)
			push 15 ; y position for line (ending position)
			push 71 ; x position for line (ending position)
			call vline   ;right line
			
			mov word [flag + 5],1
			mov word [stop],1
			add word [points],10
			skip5:	
			
			
			cmp word [es:di],0x6636
			jne skip6
			;6
			push 0x3020   ;character and color to be printed
			push 12 ; y position for line (starting position)
			push 72  ; x position for line (starting position)
			push 15 ; y position for line (ending position)
			push 72 ; x position for line (ending position)
			call vline   ;right line
			mov word [flag + 6],1
			mov word [stop],1
			add word [points],10
			skip6:
			
			
			
			cmp word [es:di],0x6637 
			jne skip7
			;7
			push 0x3020  ;character and color to be printed
			push 12 ; y position for line (starting position)
			push 63  ; x position for line (starting position)
			push 17 ; length of line
			call hline
			mov word [flag + 7],1
			mov word [stop],1
			add word [points],10
			
			
			skip7:
			
			
			cmp word [es:di],0x6638 
			jne skip8
			;8
			push 0x3020  ;character and color to be printed
			push 9 ; y position for line (starting position)
			push 75  ; x position for line (starting position)
			push 12 ; y position for line (ending position)
			push 75 ; x position for line (ending position)
			call vline   ;right line
			mov word [flag + 8],1
			mov word [stop],1
			add word [points],10
			skip8:
			
			cmp word [es:di],0x6639 
			jne skip9
			;9
			push 0x3020   ;character and color to be printed
			push 9 ; y position for line (starting position)
			push 68  ; x position for line (starting position)
			push 12 ; y position for line (ending position)
			push 68 ; x position for line (ending position)
			call vline   ;right line
			mov word [flag + 9],1
			mov word [stop],1
			add word [points],10
			skip9:
			
			cmp word [es:di],0x6698 
			jne skipb
		
			;b
			push 0x3020    ;character and color to be printed
			push 9 ; y position for line (starting position)
			push 67  ; x position for line (starting position)
			push 10; length of line
			call hline
			mov word [flag + 10],1
			mov word [stop],1
			add word [points],10
			skipb:
			
			
			
				
		pop di
		pop ax
		pop es
		pop bp
		ret 4

	
	hline:
		push bp
		mov bp,sp
		push es
		push ax
		push di
		push bx   ; to store ascii  
		push cx   ; for length
		
		mov bx,[bp + 10]
		
		mov ax, 0xb800
		mov es, ax ; point es to video base	
		
		mov al, 80 ; load al with columns per row
		mul byte [bp+8] ; multiply with y position (starting position)
		add ax, [bp+6] ; add x position              (starting position)
		shl ax, 1 	
		
		mov di, ax  
		
		
		mov cx,[bp + 4]
		
		
		
		nextloc4:
		mov word [es:di], bx ; clear next char on screen
		add di, 2 ; move to next screen location
		cmp di, cx ; has the whole screen cleared
		loop nextloc4 ; if no clear next position
		
		pop cx
		pop bx
		pop di
		pop ax
		pop es
		pop bp
		ret 8
		
vline:
		push bp
		mov bp,sp
		push es
		push ax
		push di
		push bx   ; to store ascii  
		
		mov bx,[bp + 12]
		
		mov ax, 0xb800
		mov es, ax ; point es to video base	
		
		mov al, 80 ; load al with columns per row
		mul byte [bp+10] ; multiply with y position (starting position)
		add ax, [bp+8] ; add x position              (starting position)
		shl ax, 1 	
		
		mov di, ax  
		
		
		mov al, 80 ; load al with columns per row
		mul byte [bp+6] ; multiply with y position    (ending position)
		add ax, [bp+4] ; add x position                 (ending position)
		shl ax, 1 	
		
		
		nextloc5:
		mov word [es:di], bx ; clear next char on screen
		add di, 160 ; move to next line location
		cmp di, ax ; has the whole screen cleared
		jne nextloc5 ; if no clear next position
		
		
		pop bx
		pop di
		pop ax
		pop es
		pop bp
		ret 10

ldiagonal:
		push bp
		mov bp,sp
		push es
		push ax
		push di
		push bx   ; to store ascii  
		push cx    ; length of diagonal
		
		mov bx,[bp + 10]
		
		mov ax, 0xb800
		mov es, ax ; point es to video base	
		
		mov al, 80 ; load al with columns per row
		mul byte [bp+8] ; multiply with y position (starting position)
		add ax, [bp+6] ; add x position              (starting position)
		shl ax, 1 	
		
		mov di, ax  
		
		mov cx,[bp + 4]
		
		
		l2:
		mov word [es:di], bx ; clear next char on screen
		add di, 160 ; move to next line location
		sub di,2    ;move right once
		loop l2
		
		pop cx
		pop bx
		pop di
		pop ax
		pop es
		pop bp
		ret 8





rdiagonal:
		push bp
		mov bp,sp
		push es
		push ax
		push di
		push bx   ; to store ascii  
		push cx    ; length of diagonal
		
		mov bx,[bp + 10]
		
		mov ax, 0xb800
		mov es, ax ; point es to video base	
		
		mov al, 80 ; load al with columns per row
		mul byte [bp+8] ; multiply with y position (starting position)
		add ax, [bp+6] ; add x position              (starting position)
		shl ax, 1 	
		
		mov di, ax  
		
		mov cx,[bp + 4]
		
		
		l1:
		mov word [es:di], bx ; clear next char on screen
		add di, 160 ; move to next line location
		add di,2    ;move right once
		loop l1
		
		pop cx
		pop bx
		pop di
		pop ax
		pop es
		pop bp
		ret 8




bow:
		push bp
		mov bp,sp
		
		push es
		push ax
		push di
		push cx
		
		
		push word [bp + 12] ; color and ascii of tree
		push word [bp + 10]; y position for line (starting position)
		push word [bp + 8] ; x position for line (starting position)
		push word [bp + 6] ; y position for line (ending position)
		push word [bp + 4] ; x position for line (ending position)
		call vline	;for base of bow
		
		mov ax, 0xb800
		mov es, ax ; point es to video base
		
		
		mov al, 80 ; load al with columns per row
		mul byte [bp+10] ; multiply with y position    (ending position)
		add ax, [bp+8] ; add x position                 (ending position)
		shl ax, 1 	
		
		
		
		mov di, ax ; 		;right diagonal
		
		push di
		
		add di,2
		sub di,160
		mov word [es:di], 0x0720 
		
		add di,2
		sub di,160
		mov word [es:di], 0x0720 
		  
		  
		  
		pop di		;left diagonal
		
		sub di,2
		sub di,160
		mov word [es:di], 0x0720 
		
		sub di,2
		sub di,160
		mov word [es:di], 0x0720 
		
		
		
		pop cx
		pop di
		pop ax
		pop es
		pop bp
		
		ret 10
	

etriangle:
	        push bp
		mov bp,sp
		push ax
		push bx
		push cx
		push dx   ; to store ascii  
		push di   ; for length
		push si
		
		mov si,1     ;counter
		mov cx, 1    ; current size of triangle
		
		mov ax,[bp + 10] ; ascii
		mov bx,[bp + 8]  ; y
		mov dx, [bp + 6] ;x
		mov di,[bp + 4]   ; total size of triangle
		
		add di,1   ; for loop adjustment 
		
		
		
	again:	
		push ax
		push bx 
		push dx
		push cx
		call hline
		add si,1  ; increase current length
		add cx,2
		add bx,1  ; move down
		sub dx,1  ; start from left
		cmp si,di
		jne again
		
		
		pop si
		pop di
		pop dx
		pop cx
		pop bx
		pop ax
		pop bp
		ret 8
	

tree:
                push bp
		mov bp,sp
		pusha
		
	
		push 0xA6A7  ;leafs
		push word [bp + 6] ; y position for line (starting position)
		push word [bp + 4 ]  ; x position for line (starting position)
		push 5 ; size of Leafs triangle
		call etriangle
		
		add word [bp + 6],5
		mov cx,[bp + 6]
		add cx,3
		
		push 0x6020   ;character and color to be printed
		push word [bp + 6] ; y position for line (starting position)
		push word [bp + 4] ; x position for line (starting position)
		push cx ; y position for line (ending position)
		push word  [bp + 4]; x position for line (ending position)
		call vline   ;right line
	
	
		popa
		pop bp
		ret 4

printnum: 	push bp	
		mov bp, sp
		push es
		push ax
		push bx
		push cx
		push dx
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base
		mov ax, [bp+4] ; load number in ax
		mov bx, 10 ; use base 10 for division
		mov cx, 0 ; initialize count of digits

   nextdigit: 	mov dx, 0 ; zero upper half of dividend
		div bx ; divide by 10
		add dl, 0x30 ; convert digit into ascii value
		push dx ; save ascii value on stack
		inc cx ; increment count of values
		cmp ax, 0 ; is the quotient zero
		jnz nextdigit ; if no divide it again
		mov di, [bp+6]

  nextpos: 	pop dx ; remove a digit from the stack
		mov dh, 0x71 ; use normal attribute
		mov [es:di], dx ; print char on screen
		add di, 2 ; move to next screen location
		loop nextpos ; repeat for all digits on stack
		pop di
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop bp
		ret 4	

printstr: 

	push bp 
 	mov bp, sp 
 	push es 
 	push ax 
 	push cx 
 	push si 
 	push di 
 	push ds 
 	pop es 			; load ds in es 
 	mov di, [bp+4] 		; point di to string 
 	mov cx, 0xffff 		; load maximum number in cx 
 	xor al, al 		; load a zero in al 
 	repne scasb 		; find zero in the string 
 	mov ax, 0xffff 		; load maximum number in ax 
 	sub ax, cx 		; find change in cx 
 	dec ax 			; exclude null from length 
 	jz ext 		; no printing if string is empty
 	mov cx, ax 		; load string length in cx 
 	mov ax, 0xb800 
 	mov es, ax 		; point es to video base 
 	mov al, 80 		; load al with columns per row 
 	mul byte [bp+8] 	; multiply with y position 
 	add ax, [bp+10] 	; add x position 
 	shl ax, 1 		; turn into byte offset 
 	mov di,ax 		; point di to required location 
 	mov si, [bp+4] 		; point si to string 
 	mov ah, [bp+6] 		; load attribute in ah 
 	cld 			; auto increment mode 
	nextchr: 
		lodsb 		; load next char in al 
 		stosw 		; print char/attribute pair 
 		loop nextchr 	; repeat for the whole string 
	ext: 
		pop di 
 		pop si 
 		pop cx 
 		pop ax 
 		pop es 
 		pop bp 
 		ret 8

score: 
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di


	cmp word [bp+4],10
	jne leave2
	
	mov word [bp+4],9
	mov word [points],9
	
	leave2:

		push word 5
		push word 0
		push word 0x71
		push sc
		call printstr

		mov di,24 ; push di position
		push di
		mov ax,[points]
		push ax
		call printnum

	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2


obstacles:
	pusha
	
	
	
	
	push 20 ; y position for pig
	push 70  ; x position for pig
	call pig3 ;pig on ground floor
	
	push 17 ; y position for pig
	push 70  ; x position for pig
	call pig2 ;pig on 1st floor
	
	
	mov bx,0
	cmp word [flag + bx], 1
	je one 
	
	;0
	push 0x6630   ;character and color to be printed
	push 15 ; y position for line (starting position)
	push 65  ; x position for line (starting position)
	push 13 ; length of line
	call hline
	
	
	one:
	add bx,1
	cmp word [flag + bx], 1
	je two 
	
	
	;1
	push 0x6631   ;character and color to be printed
	push 15 ; y position for line (starting position)
	push 67  ; x position for line (starting position)
	push 21 ; y position for line (ending position)
	push 67 ; x position for line (ending position)
	call vline   ;left line
	
	two:
	
	add bx,1
	cmp word [flag + bx], 1
	je three 
	
	
	;2
	push 0x6632   ;character and color to be printed
	push 18 ; y position for line (starting position)
	push 65 ; x position for line (starting position)
	push 13 ; length of line
	call hline
	
	three:
	add bx,1
	cmp word [flag + bx], 1
	je four
	
	;3
	push 0x6633   ;character and color to be printed
	push 21 ; y position for line (starting position)
	push 65  ; x position for line (starting position)
	push 13  ; length of line
	call hline   ;ground floor
	
	four:
	add bx,1
	cmp word [flag + bx], 1
	je five
	
	;4
	push 0x6634   ;character and color to be printed
	push 15 ; y position for line (starting position)
	push 75  ; x position for line (starting position)
	push 22 ; y position for line (ending position)
	push 75 ; x position for line (ending position)
	call vline   ;right line
	
	five:
	add bx,1
	cmp word [flag + bx], 1
	je six
	
	;5
	push 0x6635   ;character and color to be printed
	push 12 ; y position for line (starting position)
	push 71  ; x position for line (starting position)
	push 15 ; y position for line (ending position)
	push 71 ; x position for line (ending position)
	call vline   ;right line
	
	six:
	add bx,1
	cmp word [flag + bx], 1
	je seven
	
	;6
	push 0x6636   ;character and color to be printed
	push 12 ; y position for line (starting position)
	push 72  ; x position for line (starting position)
	push 15 ; y position for line (ending position)
	push 72 ; x position for line (ending position)
	call vline   ;right line
	
	seven:
	add bx,1
	cmp word [flag + bx], 1
	je eight
	
	;7
	push 0x6637   ;character and color to be printed
	push 12 ; y position for line (starting position)
	push 63  ; x position for line (starting position)
	push 17 ; length of line
	call hline
	
	eight:
	add bx,1
	cmp word [flag + bx], 1
	je nine
	
	
	;8
	push 0x6638   ;character and color to be printed
	push 9 ; y position for line (starting position)
	push 75  ; x position for line (starting position)
	push 12 ; y position for line (ending position)
	push 75 ; x position for line (ending position)
	call vline   ;right line
	
	nine:
	add bx,1
	cmp word [flag + bx], 1
	je ten
	
	;9
	push 0x6639   ;character and color to be printed
	push 9 ; y position for line (starting position)
	push 68  ; x position for line (starting position)
	push 12 ; y position for line (ending position)
	push 68 ; x position for line (ending position)
	call vline   ;right line
	
	
	ten:
	add bx,1
	cmp word [flag + bx], 1
	je eleven
	
	
	;b
	push 0x6698    ;character and color to be printed
	push 9 ; y position for line (starting position)
	push 67  ; x position for line (starting position)
	push 10; length of line
	call hline
	
	eleven:
	
	popa
	ret

background:
		pusha
		
		call sky    ; makes background blue
	call ground ; makes ground green

        push 0x7020  ;leafs
	push 5; y position for line (starting position)
	push 20 ; x position for line (starting position)
	push 17; size of Leafs triangle
	call etriangle

        push 0x7020  ;leafs
	push 5; y position for line (starting position)
	push 30 ; x position for line (starting position)
	push 17; size of Leafs triangle
	call etriangle

        push 0x7020  ;leafs
	push 5; y position for line (starting position)
	push 40 ; x position for line (starting position)
	push 17; size of Leafs triangle
	call etriangle

        push 0x7020  ;leafs
	push 5; y position for line (starting position)
	push 50 ; x position for line (starting position)
	push 17; size of Leafs triangle
	call etriangle


    push 0x1020   ;character and color to be printed
	push 1 ; y position for line (starting position)
	push 3  ; x position for line (starting position)
	push 10; length of line
	call cloud
	
	

	push 0x1020   ;character and color to be printed
	push 1 ; y position for line (starting position)
	push 21  ; x position for line (starting position)
	push 10; length of line
	call cloud
	

	push 0x1020   ;character and color to be printed
	push 1 ; y position for line (starting position)
	push 41  ; x position for line (starting position)
	push 10 ; length of line
	call cloud
	
	
	push 0x1020   ;character and color to be printed
	push 1 ; y position for line (starting position)
	push 61  ; x position for line (starting position)
	push 10 ; length of line
	call cloud

      
	
	push 0x6020   ;character and color to be printed
	push 19; y position for line (starting position)
	push 10  ; x position for line (starting position)
	push 22 ; y position for line (ending position)
	push 10 ; x position for line (ending position)
	call bow   
	
	push 15 ; y position for line (starting position)
	push 20  ; x position for line (starting position)
	call tree
	
	
	push 15 ; y position for line (starting position)
	push 30  ; x position for line (starting position)
	call tree


	push 15 ; y position for line (starting position)
	push 40  ; x position for line (starting position)
	call tree

        push 15
        push 50
        call tree
		
		popa
		ret


	left:
		
        pusha
						
		mov cx,3
						
						again1:
						call clrscr;
						call background
						call obstacles
						
						
						;push word [points]   
						;call score
						
						
						
						sub word [xpos],1
						
						
						push word [ypos] ; y position for bird
						push word [xpos]  ; x position for bird
						
						
						call bird
						call sleep
						call sleep			
						loop again1
						
		popa			
		ret	
	
	up:
		
        pusha
						
		mov cx,3
						
						again2:
						call clrscr;
						call background
						call obstacles
						
						
						push word [points]   
						call score
						
						
						
						sub word [ypos],1
						
						
						push word [ypos] ; y position for bird
						push word [xpos]  ; x position for bird
						
						
						call bird
						call sleep
						call sleep			
						loop again2
						
		popa			
		ret	
		
		
		down:
		
        pusha
						
		mov cx,3
						
						again3:
						call clrscr;
						call background
						call obstacles
						
						
						push word [points]   
						call score
						
						
						
						add word [ypos],1
						
						
						push word [ypos] ; y position for bird
						push word [xpos]  ; x position for bird
						
						
						call bird
						call sleep
						call sleep			
						loop again3
						
		popa			
		ret	
		
		

start:  
	call open2

	mov ah, 0
	int 16h

	call starttime
	call background
	call obstacles

	game:

	mov word [xpos], 9
	mov word [ypos], 16
	mov word [stop], 0 
	
	push word [points]   
	call score
	
	push word [ypos] ; y position for bird
	push word [xpos]  ; x position for bird
	call bird

	
        	mov ah, 0
		int 16h
        	cmp ah, 4bh
        
		jne skip
		call left
		
		
		mov ah, 0
		int 16h
        	cmp ah, 48h
		
		jne forget
		
		call up
		
		forget:
		cmp ah, 50h
		jne forget2
		call down
		
		
		forget2:
	
		mov ah, 0
		int 16h
        	cmp ah, 4dh
		

		jne skip
		call move
		
		skip:   

		dec word [lives]
		cmp word [lives],0
		jne game

	call endt

	call lastpg

	mov ax, 0x4c00 ; terminate program
	int 0x21

      
xpos: dw 0
ypos: dw 0

flag : db 0,0,0,0,0,0,0,0,0,0
stop : dw 0


check : dw 0
points: dw 0