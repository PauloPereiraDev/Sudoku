.8086
.MODEL SMALL
.STACK 2048

DADOS	SEGMENT PARA 'DATA'
	menu	 DB	'      	SUDOKU',13,10,13,10,' 1 - Iniciar jogo',13,10,' 2 - Intrucoes',13,10,13,10,' 3 - SAIR',13,10,'$'
	msgpress DB	'<Pressione uma tecla para continuar>$'
	adeus 	 DB	' Obrigado por utilizar o nosso SUDOKU :)$'
	intro	 DB	'      	SUDOKU',13,10,13,10,' O Sudoku e um jogo de raciocinio e logica. Apesar de ser bastante simples,',13,10,'e divertido e viciante. Basta completar cada linha, coluna e quadrado 3x3',13,10,'com numeros de 1 a 9 sem repeticao nas linhas, colunas ou quadrados.',13,10,' Use as SETAS para se movimentar e pressione no numero que deseja adicionar.',13,10,'$'
	certeza	 DB	'Tem a certeza?<s> para sim$'
	limpar	 DB	'                                        $'
	help1	 DB	'setas: mover$'
	help2	 DB	'1-9: introduz numero$'
	help3	 DB	'r: restart$'
	help4	 DB	'esc: sair  $'
	done	 DB	'PARABENS!!!!',13,10,13,10,'    completou o sudoku em(h:m)->  $'
	intronick DB	'Introduza o seu nome:  $'
	nome1	 DB	'mapa1.txt',0
	nome2	 DB	'mapa2.txt',0
	ident	 DB	15 dup (?)
	fd_m	 Dw	2 dup (?)
	conteudo DB	1 dup (?)
	mapa	 DW	81 dup (?)
	loaea	 DB	1 dup (?)
	lastc	 DB	1 dup (?)
	vector	 DB	'0','0','0','0','0','0','0','0','0','0'
	first	 DB	012h
	erro	 DB	0h
	win	 DB	0h
	volta	 DB	0h
	hora	 DB	0h
	min	 DB	0h
	num	 DB	0h
	errofecha DB	' erro ao fechar ficheiro$'
	erroabre DB	' erro ao abrir ficheiro$'
DADOS	ENDS

cseg	segment para public 'code'
	assume  cs:cseg, ds:dados ;ds:dseg
;---------------------------------------------------------------;
;					Procedimentos		;
;---------------------------------------------------------------;

goto_xy	macro lin,col
	mov ah,02h
	mov dh,lin
	mov dl,col
	mov bh,0
	int 10h
endm

temacerteza proc
	goto_xy 17,1
	MOV	AH,09H		;MOSTRA UMA STRING TERMINADA EM '$'
	LEA	DX,certeza
	INT	21H
	mov	ah,08h
	int 	21h
	goto_xy 0,0
	ret
temacerteza endp

limpacert proc
	goto_xy 17,1
	MOV	AH,09H		;MOSTRA UMA STRING TERMINADA EM '$'
	LEA	DX,limpar
	INT	21H
	goto_xy 0,0
	ret
limpacert endp	



LEFICHEIRO PROC			;PROCEDIMENTO PARA LER UM BYTE DO FICHEIRO
			MOV	AH,3FH
			MOV	BX,fd_m
			MOV	CX,1
			LEA	DX,conteudo
			INT	21H
			RET
LEFICHEIRO ENDP

PARAGRAFO PROC		;PROCEDIMENTO PARA FAZER UM PARAGRAFO
	MOV	AH,02H
	MOV	DL,0AH	; NEW LINE.
	INT	21H
	MOV	DL,0DH	; CARRIGE RETURN.
	INT	21H
	RET
PARAGRAFO ENDP

PRESSKEY PROC	;PROCEDIMENTO PARA PARAR O PROGRAMA ATÉ SER PRECIONADA UMA TECLA
	CALL PARAGRAFO
	CALL PARAGRAFO
	MOV	AH,09H		;IMPRIME UMA STRING
	LEA	DX,msgpress
	INT	21H

	MOV	AH,08H		;LE UMA TECLA
	INT	21H
	RET
PRESSKEY ENDP

MOSTRACHAR PROC			;PROCEDIMENTO PARA MOSTRAR UM CARACTER CONTIDO NO REGISTO BL
	MOV	AH,02H
	MOV 	DL,bl
	INT	21H
	RET
MOSTRACHAR ENDP

mostramapa proc
	xor cx,cx
	mov bx,0 
	mov cx,81   ;81 numeros para apresentar
	xor si,si
	xor dx,dx
	mov dh,3
	mov dl,3
apresenta:
	cmp dl,3
	jne colun
	push cx
	mov cx,13
	mov al,'-'
	mov ah,00001111b
traco:
	mov es:[bx],ax
	add bx,2
	loop traco
	pop cx
	mov dl,0
	add bx,134
colun:
	cmp dh,3
	jne nada
	mov al,'|'
	mov ah,00001111b
	mov es:[bx],ax
	add bx,2
	mov dh,0
nada:	
	mov ax, mapa[si]
	mov es:[bx],ah
	mov es:[bx+1],al
	add bx,2
	inc si
	inc si
	inc dh
	mov al,first    ;flag para saber kd mudar de linha no mapa
	mov ah,0h
	cmp ax,si		
	loopne apresenta
	inc cx
	inc dl
	mov al,'|'
	mov ah,00001111b
	mov es:[bx],ax

	add bx,136
	mov al,first	;incrementa flag para mudar de linha
	add al,12h
	mov first,al
	loop apresenta

	mov cx,13
	mov al,'-'
	mov ah,00001111b
traul:
	mov es:[bx],ax
	add bx,2
	loop traul

	mov al,12h		;para no proximo ciclo começar de novo
	mov first,al
	ret
mostramapa endp

carregamapa proc
	mov ax,0
	int 1Ah
	mov ax,dx
	xor dx,dx
	mov cx,2
	div cx
	cmp dx,0
	jne segund
	lea dx,nome1
	jmp abre
segund:
	lea dx,nome2

abre:				;;abrir ficheriro
	MOV	AH,3DH			;ABRE O FICHEIRO
	XOR 	al,al

	INT	21H
	JC salta				;JUMP IF CARRY
	MOV	fd_m,ax			;PASSA O HANDLE DO FICHEIRO PRA A MEMÓRIA "FD"
	xor si,si
leident:
	call LEFICHEIRO
	mov bl,conteudo	
	cmp bl,0Dh
	je antmapa
	mov ident[si],bl
	inc si
	jmp leident
antmapa:
	call LEFICHEIRO
	mov ident[si+1],'$'
	xor cx,cx
	mov cx,81 
	xor si,si

lemapa:
	push cx
	call LEFICHEIRO
	pop cx

	mov bh,conteudo	

	cmp bh,30H
	ja gray	
	mov bl,00001111b         ; zero fundo preto
	mov bh,' '
	mov mapa[si],bx
	inc si
	inc si
	push cx
	call LEFICHEIRO
	call LEFICHEIRO
	pop cx
	loop lemapa
	jmp fecha
gray:
	cmp bh,'9'
	ja salta
	mov bl, 01111111b     ;numero de origem fundo cinzento
	mov mapa[si],bx
	
	inc si
	inc si
	push cx
	call LEFICHEIRO
	call LEFICHEIRO
	pop cx
	loop lemapa
		
fecha:
	mov ah,3eh     ; fechar ficheiro
	mov bx,fd_m
	int 21h
	jnc saiii
	mov ah,09h	  ; apresenta string de erro
	lea dx, errofecha
	int 21h
	call presskey
	jmp saiii
salta:
	mov erro,1
	mov ah,09h	  ; apresenta string de erro
	lea dx, erroabre
	int 21h
	call presskey

saiii:	
	ret
carregamapa endp

buscatecla proc
	mov ah,08h
	int 21h
	mov ah,0
	cmp al,0
	jne normal
	mov ah,08h
	int 21h
	mov ah,1
normal:
	ret
buscatecla endp

mostrahelp proc
	goto_xy 1,19
	MOV	AH,09H		;MOSTRA UMA STRING TERMINADA EM '$'
	LEA	DX,ident
	INT	21H
	
	goto_xy 3,19
	MOV	AH,09H		;MOSTRA UMA STRING TERMINADA EM '$'
	LEA	DX,help1
	INT	21H

	goto_xy 5,19
	MOV	AH,09H		;MOSTRA UMA STRING TERMINADA EM '$'
	LEA	DX,help2
	INT	21H

	goto_xy 7,19
	MOV	AH,09H		;MOSTRA UMA STRING TERMINADA EM '$'
	LEA	DX,help3
	INT	21H

	goto_xy 9,19
	MOV	AH,09H		;MOSTRA UMA STRING TERMINADA EM '$'
	LEA	DX,help4
	INT	21H
	ret
mostrahelp endp


mostravic proc
	mov ax,03    ;limpa ecra
	int 10h	
	goto_xy 3,8
	MOV	AH,09H		;MOSTRA UMA STRING TERMINADA EM '$'
	LEA	DX,done
	INT	21H
	
	mov ah,2Ch
	int 21h
	sub ch,hora
	cmp  min,cl
	ja maior
	sub cl,min
	jmp tempo
maior:
	mov ah,60
	sub ah,min
	add cl,ah
tempo:
	mov bl,ch
	add bl,30h
	call mostrachar
	mov bl,':'
	call mostrachar
	mov bl,cl
	add bl,30h
	call mostrachar
	call paragrafo

	call presskey
	
	ret
mostravic endp


inijogo proc
	call carregamapa
	cmp erro,1
	je fimm
	mov ah,2Ch
	int 21h
	mov hora,ch
	mov min,cl


	xor di,di
	mov bx,mapa[di]
	mov lastc,bl
	mov bl,00101111b
	mov mapa[di],bx

	mov ax, 03    ;limpa ecra
	int 10h

	;;;mostra info mapa e assim
	call mostrahelp
	
teclas:	
	call limpacert
	call mostramapa
	mov erro,' '
	call testes
	call mostraerro
	cmp erro,'e'
	je conti
	cmp win,51h
	jne conti
	
	call mostravic

	ret

conti:
	call buscatecla
	cmp al,1Bh
	jne rest
	call temacerteza
	cmp al,'s'
	jne teclas
	jmp fimm
rest:
	cmp al,'r'
	jne nums
	call temacerteza
	cmp al,'s'
	jne teclas
	call restart
	jmp teclas
nums:
	cmp al,'0'
	jb teclas
	cmp al,'9'
	ja arrows
	mov bx,mapa[di]
	cmp lastc,01111111b
	je teclas
	mov bh,al
	cmp al,'0'
	jne norm
	mov bh,' '
norm:
	mov mapa[di],bx
	jmp teclas
arrows:
	cmp ah,1
	jne teclas
	cmp al,'K'
	jne dire
	cmp di,2h
	jb teclas
	mov bx,mapa[di]
	mov bl,lastc
	mov mapa[di],bx
	sub di,2h
	mov bx,mapa[di]
	mov lastc,bl
	mov bl,00101111b
	mov mapa[di],bx
	jmp teclas
dire:	
	cmp al,'M'
	jne cima
	cmp di,09eh
	ja teclas
	mov bx,mapa[di]
	mov bl,lastc
	mov mapa[di],bx
	add di,2h
	mov bx,mapa[di]
	mov lastc,bl
	mov bl,00101111b
	mov mapa[di],bx
	jmp teclas
cima:
	cmp al,'H'
	jne baixo
	mov bx,mapa[di]
	mov bl,lastc
	mov mapa[di],bx
	cmp di,12h
	jb cimabaixo
	sub di,12h
	jmp cimbai
cimabaixo:
	add di,90h
cimbai:
	mov bx,mapa[di]
	mov lastc,bl
	mov bl,00101111b
	mov mapa[di],bx
	jmp teclas

baixo:
	cmp al,'P'
	jne teclas
	mov bx,mapa[di]
	mov bl,lastc
	mov mapa[di],bx
	cmp di,8eh
	ja baixocima
	add di,12h
	jmp baici
baixocima:
	sub di,90h
baici:
	mov bx,mapa[di]
	mov lastc,bl
	mov bl,00101111b
	mov mapa[di],bx
	jmp teclas
fimm:
	ret
inijogo endp


incvic proc
	;push si
	xor si,si
	mov cx,9
	inc si
victory:
	cmp si,10
	je break
	mov al,vector[si]
	mov vector[si],'0'
	inc si
	cmp al,'1'
	jne victory
	add win,1
	jmp victory
break:	
	;pop si
	ret
incvic endp


testlin proc
	mov num,'0'
	mov cx,9

newvolta:
	add num,1
	mov bl,volta
	sub bl,12h
	xor bh,bh
	mov si,bx
testnum:
	mov bx,si
	cmp bl,volta
	je fazvolta	
	mov ax,mapa[si]
	add si,2
	cmp ah,num
	jne testnum
	mov bl,num
	sub bl,30h
	xor bh,bh
	cmp vector[bx],'1'
	je errrorr
	mov vector[bx],'1'
	jmp testnum
fazvolta:
	cmp num,'9'
	loopne newvolta
	ret
errrorr:
	mov erro,'e'
	ret
testlin endp


testcol proc
	mov num,'0'
	mov cx,9
nvolta:
	add num,1
	mov bl,volta
	sub bl,90h
	xor bh,bh
	mov si,bx
testenum:
	mov bx,si
	sub bl,12h
	cmp bl,volta
	je fazvo	
	mov ax,mapa[si]
	add si,12h
	cmp ah,num
	jne testenum
	mov bl,num
	sub bl,30h
	xor bh,bh
	cmp vector[bx],'1'
	je erorr
	mov vector[bx],'1'
	jmp testenum
fazvo:
	cmp num,'9'
	loopne nvolta
	ret
erorr:
	mov erro,'e'
	ret
testcol endp


testquad proc
	mov num,'0'
	mov cx,9
	xor bp,bp
newvolt:
	add num,1
	mov bl,volta
	sub bl,28h
	xor bh,bh
	mov si,bx
tesnum:
	cmp bp,3
	jne semuda
	xor bp,bp
	add si,0ch
semuda:
	mov bx,si
	sub bl,0eh
	cmp bl,volta
	je fazvolt	
	mov ax,mapa[si]
	inc bp
	add si,2
	cmp ah,num
	jne tesnum
	mov bl,num
	sub bl,30h
	xor bh,bh
	cmp vector[bx],'1'
	je errorr
	mov vector[bx],'1'
	jmp tesnum
fazvolt:
	cmp num,'9'
	loopne newvolt
	ret
errorr:
	mov erro,'e'
	ret
testquad endp



limpavec proc
	xor si,si
	mov cx,10
cicle:
	mov vector[si],'0'
	inc si
	loop cicle
	ret
limpavec endp


mostraerro proc
	mov bx,15*160
	add bx,6
	cmp erro,'e'
	jne efim
	mov ah,11001111b
	mov al,'E'
	mov es:[bx],ax
	mov al,'R'
	mov es:[bx+2],ax
	mov es:[bx+4],ax
	mov al,'O'
	mov es:[bx+6],ax
	ret
efim:
	mov al,' '
	mov ah,00001111b
	mov es:[bx],ax
	mov es:[bx+2],ax
	mov es:[bx+4],ax
	mov es:[bx+6],ax
	ret
mostraerro endp


testes proc
	;; test quads 


	mov volta,28h
	call limpavec
	call testquad
	mov volta,2eh
	call limpavec
	call testquad
	mov volta,34h
	call limpavec
	call testquad

	mov volta,5eh
	call limpavec
	call testquad
	mov volta,64h
	call limpavec
	call testquad
	mov volta,6ah
	call limpavec
	call testquad

	mov volta,94h
	call limpavec
	call testquad
	mov volta,9ah
	call limpavec
	call testquad
	mov volta,0a0h
	call limpavec
	call testquad

	;;teste cols 

	mov volta,8eh
novacol:
	add volta,2h
	call limpavec
	call testcol
	cmp volta,0a0h
	jne novacol
	call limpavec

	;; teste linhas

	mov win,0
	mov volta,0h
novalinha:
	add volta,12h
	call testlin
	call incvic
	cmp volta,0a2h
	jne novalinha

	ret
testes endp



restart proc
	xor si,si
	sub si,2
resl:
	cmp si,0a0h
	je resfim
	add si,2
	mov bx,mapa[si]
	cmp bl,00101111b
	jne resnext
	cmp lastc,00001111b
	jne resl
	mov bh,' '
	mov mapa[si],bx
	jmp resl
resnext:
	cmp bl,00001111b
	jne resl
	mov bh,' '
	mov mapa[si],bx
	jmp resl
resfim:
	ret
restart endp
;---------------------------------------------------------------;
;				Fim de	Procedimentos		;
;---------------------------------------------------------------;
main proc
	mov	ax,0b800h
	mov	es,ax
	mov	ax,dados
	mov	ds,ax

inmenu:
	mov ax, 03    ;limpa ecra
	int 10h
	CALL PARAGRAFO
	MOV AH,09H		;MOSTRA UMA STRING TERMINADA EM '$'
	LEA DX,menu
	INT 21H
	mov ah,08h		;LE UMA TECLA
	int 21h
	cmp al,'3'
	je FIM
	cmp al,'2'
	jne fintro
	mov ax, 03
	int 10h
	CALL PARAGRAFO
	MOV AH,09H		;MOSTRA UMA STRING TERMINADA EM '$'
	LEA DX,intro
	INT 21H
	CALL PRESSKEY
	jmp inmenu
fintro:	
	cmp al,'1'
	jne fjoga
	call inijogo
	
	;talvez falte código

	jmp inmenu
fjoga:

	;código para apresentar classificaçoes

	jmp inmenu	
FIM:
	CALL PARAGRAFO
	CALL PARAGRAFO
	MOV	AH,09H		;MOSTRA UMA STRING TERMINADA EM '$'
	LEA	DX,adeus
	INT	21H
	CALL PRESSKEY	
	MOV	AH,4Ch
	INT	21h
main endp
cseg	ENDS
END	main