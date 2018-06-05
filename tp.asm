.8086
.model small
.stack 2048

TEsc equ 1Bh


posicaoVideoBX macro pos
mov al,pos[1]
mov bl,80
mul bl
mov bh,0
mov bl,pos[0]
add ax,bx
mov bx,2
mul bx
mov bx,ax
endm



dseg	segment para public 'data'
;						texto		db	10 dup (10 dup (' '))

;						num		db	1
	
	;POSy		db	4	; a linha pode ir de [ .. ]
	;POSx		db	10	; POSx pode ir [ .. ]	

        Erro_Open       db      'Erro ao tentar abrir o ficheiro$'
        Erro_Ler_Msg    db      'Erro ao tentar ler do ficheiro$'
        Erro_Close      db      'Erro ao tentar fechar o ficheiro$'		
        Fich	    	db      'menu.txt',0
		Fich2			db 		'tab.txt',0
		Fich3			db		'top.txt',0
        HandleFich      dw      0
        car_fich        db      ?
		postempo 		db		0,0
		time			db		0,0,0
		tempo 			db		'  :  :  $$$$'
		
		
	;Dados Tabuleiro-----------------------------------------------
ultimo_num_aleat dw 0
str_num db 5 dup(?),'$'
			linha		db	0	; Define o n?mero da linha que est? a ser desenhada
			nlinhas	db	0
;cor		db 	0
space		db	' '
;------------------------------------------------

;Dados Cursor-----------------------------------------------
string	db	"Teste pr?tico de T.I",0
Car			db	32	; Guarda um caracter do Ecran
Cor			db	7		; Guarda os atributos de cor do caracter
Car2		db	32	; Guarda um caracter do Ecran
Cor2		db	7		; Guarda os atributos de cor do caracter
POSy		db	8		; a linha pode ir de [1 .. 25]
POSx		db	30	; POSx pode ir [1..80]
POSya		db	8		; Posi??o anterior de y
POSxa		db	30	; Posi??o anterior de x
;-----------------------------------------------

;Dados Explode----------------------------------------------
valorPOSATUAL		dw		?   ;Guarda o valor da posição atual do cursor
colunaINICIAL		db		8		;Valor da coluna onde começa o tabuleiro
colunaAtual			db		?		;Valor da coluna atual
pontuacao				db 		0		;Pontuacao para o jogo
POSxtmp					db		?		;Posição temporária de x
POSytmp					db		?		;Posição temporária de y
centro					db 		0		;Booleana para o centro
;-----------------------------------------------	

	
	
dseg	ends

cseg	segment para public 'code'
assume	cs:cseg, ds:dseg



;########################################################################
goto_xy	macro	POSx,POSy
		mov	ah,02h
		mov	bh,0
		mov	dl,POSx
		mov	dh,POSy
		int	10h
endm

;########################################################################
;ROTINA PARA APAGAR ECRAN

apaga_ecran	proc
		xor	bx,bx
		mov	cx,25*80
		
apaga:		mov	byte ptr es:[bx],' '
		mov	byte ptr es:[bx+1],7
		inc	bx
		inc 	bx
		loop	apaga
		ret
apaga_ecran	endp



Imp_Fich	PROC

;abre ficheiro

        mov     ah,3dh			; vamos abrir ficheiro para leitura 
        mov     al,0			; tipo de ficheiro	
        ;lea     dx,Fich			; nome do ficheiro
        int     21h			; abre para leitura 
        jc      erro_abrir		; pode aconter erro a abrir o ficheiro 
        mov     HandleFich,ax		; ax devolve o Handle para o ficheiro 
        jmp     ler_ciclo		; depois de abero vamos ler o ficheiro 

erro_abrir:
        mov     ah,09h
        lea     dx,Erro_Open
        int     21h
        jmp     sai

ler_ciclo:
        mov     ah,3fh			; indica que vai ser lido um ficheiro 
        mov     bx,HandleFich		; bx deve conter o Handle do ficheiro previamente aberto 
        mov     cx,1			; numero de bytes a ler 
        lea     dx,car_fich		; vai ler para o local de memoria apontado por dx (car_fich)
        int     21h				; faz efectivamente a leitura
	jc	    erro_ler		; se carry é porque aconteceu um erro
	cmp	    ax,0			;EOF?	verifica se já estamos no fim do ficheiro 
	je	    fecha_ficheiro	; se EOF fecha o ficheiro 
        mov     ah,02h			; coloca o caracter no ecran
	  mov	    dl,car_fich		; este é o caracter a enviar para o ecran
	  int	    21h				; imprime no ecran
	  jmp	    ler_ciclo		; continua a ler o ficheiro

erro_ler:
        mov     ah,09h
        lea     dx,Erro_Ler_Msg
        int     21h

fecha_ficheiro:					; vamos fechar o ficheiro 
        mov     ah,3eh
        mov     bx,HandleFich
        int     21h
        jnc     sai

        mov     ah,09h			; o ficheiro pode não fechar correctamente
        lea     dx,Erro_Close
        Int     21h
sai:	  RET
Imp_Fich	endp



bloc_teclas	PROC

	mov ah,08h  ;le char
	int 21h
	mov ah,0
	cmp al,0
	jne SAI_TECLA
	mov ah,08h
	int 21h
	mov ah,1 ;ESC
	SAI_TECLA:	RET
bloc_teclas	endp


;########################################################################
;########################################################################
;########################################################################


tabulSIMPLES PROC
	mov	cx,10		; Faz o ciclo 10 vezes
ciclo4:
		call	CalcAleat
		pop	ax 		; vai buscar 'a pilha o n?mero aleat?rio

		mov	dl,cl
		mov	dh,70
		;push	dx		; Passagem de par?metros a impnum (posi??o do ecran)
		;push	ax		; Passagem de par?metros a impnum (n?mero a imprimir)
		;call	impnum		; imprime 10 aleat?rios na parte direita do ecran
		loop	ciclo4		; Ciclo de impress?o dos n?meros aleat?rios

		mov   	ax, 0b800h	; Segmento de mem?ria de v?deo onde vai ser desenhado o tabuleiro
		mov   	es, ax
		mov	linha, 	8	; O Tabuleiro vai come?ar a ser desenhado na linha 8
		mov	nlinhas, 6	; O Tabuleiro vai ter 6 linhas

ciclo2:		mov	al, 160
		mov	ah, linha
		mul	ah
		add	ax, 60
		mov 	bx, ax		; Determina Endere?o onde come?a a "linha". bx = 160*linha + 60

		mov	cx, 9		; S?o 9 colunas
ciclo:
		mov 	dh,	space	; vai imprimir o caracter "SAPCE"
		mov	es:[bx],dh	;

novacor:
		call	CalcAleat	; Calcula pr?ximo aleat?rio que ? colocado na pinha
		pop	ax ; 		; Vai buscar 'a pilha o n?mero aleat?rio
		and 	al,01110000b	; posi??o do ecran com cor de fundo aleat?rio e caracter a preto
		cmp	al, 0		; Se o fundo de ecran ? preto
		je	novacor		; vai buscar outra cor

		mov 	dh,	   space	; Repete mais uma vez porque cada pe?a do tabuleiro ocupa dois carecteres de ecran
		mov	es:[bx],   dh
		mov	es:[bx+1], al	; Coloca as caracter?sticas de cor da posi??o atual
		inc	bx
		inc	bx		; pr?xima posi??o e ecran dois bytes ? frente

		mov 	dh,	   space	; Repete mais uma vez porque cada pe?a do tabuleiro ocupa dois carecteres de ecran
		mov	es:[bx],   dh
		mov	es:[bx+1], al
		inc	bx
		inc	bx

		; mov	di,1 ;delay de 1 centesimo de segundo
		; call	delay
		loop	ciclo		; continua at? fazer as 9 colunas que correspondem a uma liha completa

		inc	linha		; Vai desenhar a pr?xima linha
		dec	nlinhas		; contador de linhas
		mov	al, nlinhas
		cmp	al, 0		; verifica se j? desenhou todas as linhas
		jne	ciclo2		; se ainda h? linhas a desenhar continua
tabulSIMPLES ENDP

;------------------------------------------------------
;CalcAleat - calcula um numero aleatorio de 16 bits
;Parametros passados pela pilha
;entrada:
;n?o tem parametros de entrada
;saida:
;param1 - 16 bits - numero aleatorio calculado
;notas adicionais:
; deve estar definida uma variavel => ultimo_num_aleat dw 0
; assume-se que DS esta a apontar para o segmento onde esta armazenada ultimo_num_aleat
CalcAleat proc near

	sub	sp,2		;
	push	bp
	mov	bp,sp
	push	ax
	push	cx
	push	dx
	mov	ax,[bp+4]
	mov	[bp+2],ax

	mov	ah,00h
	int	1ah

	add	dx,ultimo_num_aleat	; vai buscar o aleat?rio anterior
	add	cx,dx
	mov	ax,65521
	push	dx
	mul	cx
	pop	dx
	xchg	dl,dh
	add	dx,32749
	add	dx,ax

	mov	ultimo_num_aleat,dx	; guarda o novo numero aleat?rio

	mov	[BP+4],dx		; o aleat?rio ? passado por pilha

	pop	dx
	pop	cx
	pop	ax
	pop	bp
	ret
CalcAleat endp

;------------------------------------------------------
;impnum - imprime um numero de 16 bits na posicao x,y
;Parametros passados pela pilha
;entrada:
;param1 -  8 bits - posicao x
;param2 -  8 bits - posicao y
;param3 - 16 bits - numero a imprimir
;saida:
;n?o tem parametros de sa?da
;notas adicionais:
; deve estar definida uma variavel => str_num db 5 dup(?),'$'
; assume-se que DS esta a apontar para o segmento onde esta armazenada str_num
; sao eliminados da pilha os parametros de entrada
impnum proc near
	push	bp
	mov	bp,sp
	push	ax
	push	bx
	push	cx
	push	dx
	push	di
	mov	ax,[bp+4] ;param3
	lea	di,[str_num+5]
	mov	cx,5
prox_dig:
	xor	dx,dx
	mov	bx,10
	div	bx
	add	dl,'0' ; dh e' sempre 0
	dec	di
	mov	[di],dl
	loop	prox_dig

	mov	ah,02h
	mov	bh,00h
	mov	dl,[bp+7] ;param1
	mov	dh,[bp+6] ;param2
	int	10h
	mov	dx,di
	mov	ah,09h
	int	21h
	pop	di
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	pop	bp
	ret	4 ;limpa parametros (4 bytes) colocados na pilha
impnum endp




;recebe em di o n?mero de milisegundos a esperar
delay proc
	pushf
	push	ax
	push	cx
	push	dx
	push	si

	mov	ah,2Ch
	int	21h
	mov	al,100
	mul	dh
	xor	dh,dh
	add	ax,dx
	mov	si,ax


ciclo:	mov	ah,2Ch
	int	21h
	mov	al,100
	mul	dh
	xor	dh,dh
	add	ax,dx

	cmp	ax,si
	jnb	naoajusta
	add	ax,6000 ; 60 segundos
naoajusta:
	sub	ax,si
	cmp	ax,di
	jb	ciclo

	pop	si
	pop	dx
	pop	cx
	pop	ax
	popf
	ret
delay endp
;########################################################################


;Cursor------------------------------------------------------------------------
;########################################################################
goto_xy	macro		POSx,POSy
		mov		ah,02h
		mov		bh,0		; numero da p?gina
		mov		dl,POSx
		mov		dh,POSy
		int		10h
endm

;########################################################################

;########################################################################
; LE UMA TECLA

LE_TECLA	PROC

		mov		ah,08h
		int		21h
		mov		ah,0
		cmp		al,0
		jne		SAI_TECLA
		mov		ah, 08h
		int		21h
		mov		ah,1
SAI_TECLA:	RET
LE_TECLA	endp
;########################################################################


resetTime proc
	mov time[0],0
	mov time[1],0
	mov time[2],0
	ret
resetTime endp

atualizaHora proc
	mov cx,0
	mov dx,10000
	mov ah,86H
	int 15h
	inc time[0]
	cmp time[0],100
	jb fim
	mov time[0],0
	inc time[1]
	cmp time[1],60
	jb fim
	mov time[1],0
	inc time[2]
	cmp time[2],60

	fim:
	 ret
atualizaHora endp

horaToString proc
;minutos
	mov ah,0
	mov al,time[2]
	mov bl,10
	div bl
	add ah,'0'
	add al,'0'
	mov tempo[0],al
	mov tempo[1],ah
;segundos
	mov ah,0
  mov al,time[1]
  mov bl,10
  div bl
	add ah,'0'
	add al,'0'
  mov tempo[3],al
	mov tempo[4],ah
	;milisegundos
	mov ah,0
	mov al,time[0]
	mov bl,10
	div bl
	add ah,'0'
	add al,'0'
	mov tempo[6],al
	mov tempo[7],ah

	ret
horaToString endp

imprimehora proc
	mov postempo[0],35
	mov postempo[1],1
	mov si,0
	posicaoVideoBX postempo
lop:
	cmp tempo[si],'$'
	je fim
	mov dl,tempo[si]
	mov byte ptr es:[bx],dl
	mov byte ptr es:[bx+1],00001111b
	inc bx
	inc bx
	inc si
	jmp lop
fim:
	ret
imprimehora endp



;########################################################################
cursor  proc
		mov		ax,0B800h
		mov		es,ax

		goto_xy		POSx,POSy	; Vai para nova possi??o
		mov 		ah, 08h	; Guarda o Caracter que est? na posi??o do Cursor
		mov		bh,0		; numero da p?gina
		int		10h
		mov		Car, al	; Guarda o Caracter que est? na posi??o do Cursor
		mov		Cor, ah	; Guarda a cor que est? na posi??o do Cursor

		inc		POSx
		goto_xy		POSx,POSy	; Vai para nova possi??o2
		mov 		ah, 08h		; Guarda o Caracter que est? na posi??o do Cursor
		mov		bh,0		; numero da p?gina
		int		10h
		mov		Car2, al	; Guarda o Caracter que est? na posi??o do Cursor
		mov		Cor2, ah	; Guarda a cor que est? na posi??o do Cursor
		dec		POSx
		

;#########################	
		call resetTime
		
		
		

CICLO:
		
		
;#########################		
			
		goto_xy	POSxa,POSya	; Vai para a posi??o anterior do cursor
		mov		ah, 02h
		mov		dl, Car	; Repoe Caracter guardado
		int		21H

		inc		POSxa
		goto_xy		POSxa,POSya
		mov		ah, 02h
		mov		dl, Car2	; Repoe Caracter2 guardado
		int		21H
		dec 		POSxa

		goto_xy	POSx,POSy	; Vai para nova possi??o
		mov 		ah, 08h
		mov		bh,0		; numero da p?gina
		int		10h
		mov		Car, al	; Guarda o Caracter que est? na posi??o do Cursor
		mov		Cor, ah	; Guarda a cor que est? na posi??o do Cursor

		inc		POSx
		goto_xy		POSx,POSy	; Vai para nova possi??o
		mov 		ah, 08h
		mov		bh,0		; numero da p?gina
		int		10h
		mov		Car2, al	; Guarda o Caracter2 que est? na posi??o do Cursor2
		mov		Cor2, ah	; Guarda a cor que est? na posi??o do Cursor2
		dec		POSx


		goto_xy		77,0		; Mostra o caractr que estava na posi??o do AVATAR
		mov		ah, 02h		; IMPRIME caracter da posi??o no canto
		mov		dl, Car
		int		21H

		goto_xy		78,0		; Mostra o caractr2 que estava na posi??o do AVATAR
		mov		ah, 02h		; IMPRIME caracter2 da posi??o no canto
		mov		dl, Car2
		int		21H


		goto_xy		POSx,POSy	; Vai para posi??o do cursor
IMPRIME:	

		
		mov		ah, 02h
		mov		dl, '('	; Coloca AVATAR1
		int		21H

		inc		POSx
		goto_xy		POSx,POSy
		mov		ah, 02h
		mov		dl, ')'	; Coloca AVATAR2
		int		21H
		dec		POSx

		goto_xy		POSx,POSy	; Vai para posi??o do cursor

		mov		al, POSx	; Guarda a posi??o do cursor
		mov		POSxa, al
		mov		al, POSy	; Guarda a posi??o do cursor
		mov 		POSya, al

LER_SETA:	

	
		call atualizaHora		
		call horaToString
		call imprimehora	
	
		
		call 		LE_TECLA
		cmp		ah, 1
		je		ESTEND
		;Verificar se encontrou um limite!
		cmp 	al, 13  ;ENTER
		mov   centro, 0		;Coloca a boolena do centro a 0
		je 		EXPLODE_SIMPLES		;Salta para as verificações
		CMP 	AL, 27	; ESCAPE
		JE		Main
		jmp		LER_SETA

EXPLODE_PUXA:
		call preenche_preto
		inc pontuacao

DESLIZA:
		mov dh, POSytmp
		mov colunaAtual, dh			;Coluna atual fica com a posição de Y
		mov dl, colunaINICIAL
		sub dh, dl		;Número de vezes que se tem que fazer o ciclo
		inc dh				;Tem de fazer uma vez extra por causa da ultima posicao ficar a preto

		COLUNA:
		sub bx, 160				;Sobe uma linha
		mov	al, es:[bx+1]	; Coloca as caracter?sticas de cor da posição superior
		add bx, 160 			;Volta posição original

		mov cx, 2
		COR_ANTERIOR:
		mov ah,	   space	; Repete mais uma vez porque cada pe?a do tabuleiro ocupa dois carecteres de ecran
		mov	es:[bx],   ah	; Coloca o carater espaço na posição atual
		mov	es:[bx+1], al	; Coloca as caracter?sticas de cor da posi??o atual
		inc	bx
		inc	bx		; próxima posição e ecran dois bytes ? frente
		loop COR_ANTERIOR		;Falta otimizar e ir para o sítio de onde o chamou

		mov	di,2 ;delay de 2 centesimo de segundo
		call	delay

		sub bx, 164   ;Sobe uma linha mais 4 por causa do ciclo
		dec dh
		cmp dh, 0
		jne COLUNA

		;mov bx, valorPOSATUAL

PREENCHE_TOPO:
		mov al, 160		;Valor de uma linha completa
		mov ah, colunaINICIAL	;Linha inicial
		mul ah				;Valor da linha inicial guardado em ax
		mov bx, ax

		mov al, 2			;Valor de uma coluna
		mov ah, POSxtmp	;Coluna atual
		mul ah				;Calcula a coluna atual
		add bx, ax		;Valor da posicao atual

		mov	di, 2 ;delay de 2 centesimo de segundo
		call	delay

		preenche_aleatorio:
			call	CalcAleat	; Calcula pr?ximo aleat?rio que ? colocado na pinha
			pop	ax ; 		; Vai buscar 'a pilha o n?mero aleat?rio
			and 	al,01110000b	; posi??o do ecran com cor de fundo aleat?rio e caracter a preto
		  cmp	al, 0		; Se o fundo de ecran ? preto
	    je	preenche_aleatorio		; vai buscar outra cor

		call preenche_cor	;Preenche o quadrado com a nova cor aleatória
		;Está mal verificar para dar loop
		cmp al, 0					;Verifica se a posição atual tem a cor preta
		je  PREENCHE_TOPO					;Se tiver repete
		jmp EXPLODE_SIMPLES

EXPLODE:
		call preenche_preto
		inc pontuacao


EXPLODE_SIMPLES:
		mov al, 160		;Valor de uma linha completa
		mov ah, POSy	;Linha atual
		mov POSytmp, ah  ;Colaca a posição de Y na temporária
		mul ah				;Valor da linha atual guardado em ax
		mov bx, ax

		mov al, 2			;Valor de uma coluna
		mov ah, POSx	;Coluna atual
		mov POSxtmp, ah		;Colaca a posição de X na temporária
		mul ah				;Calcula a coluna atual
		add bx, ax		;Valor da posicao atual

		mov cl, cor		;Coloca a cor atual em cl
		;mov centro, 0 ;Coloca o valor do centro a 0
		mov valorPOSATUAL, bx

		sub bx, 160		;Verifica a linha em cima
		cmp	es:[bx+1], cl
		je	EXPLODE_PUXA

		mov bx, valorPOSATUAL
		add bx, 4			;Verifica linha à direita
		cmp	es:[bx+1], cl
		je  EXPLODE_DIREITA

		mov bx, valorPOSATUAL
		sub bx, 4			;Verifica linha à esquerda
		cmp	es:[bx+1], cl
		je	EXPLODE_ESQUERDA

		mov bx, valorPOSATUAL
		cmp centro, 0	;Verifica se tem de explodir o centro
		je	EXPLODE_CENTRO

		mov bx, valorPOSATUAL
		add bx, 160   ;Verifica a linha a baixo
		cmp	es:[bx+1], cl
		je  EXPLODE_BAIXO
		jmp CICLO		;Saltar para imprimir a pontuacao e depois para o ciclo
		;Adicionar o count à pontuaçao

		EXPLODE_DIREITA:
			add POSxtmp, 2		;Faz com que deslize a coluna da direita
			jmp EXPLODE_PUXA

		EXPLODE_ESQUERDA:
			sub POSxtmp, 2		;Faz com que deslize a coluna da esquerda
			jmp EXPLODE_PUXA

		EXPLODE_CENTRO:
			mov centro, 1 ;Faz com que não se tenha de voltar a explodir o centro
			jmp EXPLODE_PUXA

		EXPLODE_BAIXO:
			cmp centro, 1	;Se o centro já tiver preenchido é só puxar
			je	EXPLODE_PUXA
			mov centro, 1	;Faz com que não se tenha de voltar a explodir o centro
			mov bx,valorPOSATUAL
			jmp EXPLODE_PUXA

ESTEND:
		cmp 		al,48h
		jne		SETA_BAIXO
		dec		POSy		;cima
		jmp		CICLO

SETA_BAIXO:
		cmp		al,50h
		jne		SETA_ESQUERDA
		inc 		POSy		;Baixo
		jmp		CICLO

SETA_ESQUERDA:
		cmp		al,4Bh
		jne		SETA_DIREITA
		dec		POSx		;Esquerda
		dec		POSx		;Esquerda

		jmp		CICLO

SETA_DIREITA:
		cmp		al,4Dh
		jne		LER_SETA
		inc		POSx		;Direita
		inc		POSx		;Direita

		jmp		CICLO
cursor	endp
;########################################################################

preenche_preto PROC
		mov cx, 2			;Repete duas vezes para fazer um quadrado
		preenche:
			mov al, 0Fh	;5fh ;Mete o fundo a preto e a cor do carater a branco
			mov ah,	   space	; Repete mais uma vez porque cada pe?a do tabuleiro ocupa dois carecteres de ecran
			mov	es:[bx],   ah	; Coloca o carater espaço na posição atual
			mov	es:[bx+1], al	; Coloca as caracter?sticas de cor da posi??o atual
			inc	bx
			inc	bx		; próxima posição e ecran dois bytes ? frente
		loop preenche		;Falta otimizar e ir para o sítio de onde o chamou
		sub bx, 4 	;Volta à posição original
		ret
preenche_preto ENDP

preenche_cor PROC
		mov cx, 2			;Repete duas vezes para fazer um quadrado
		preenche:
			mov ah,	   space	; Repete mais uma vez porque cada pe?a do tabuleiro ocupa dois carecteres de ecran
			mov	es:[bx],   ah	; Coloca o carater espaço na posição atual
			mov	es:[bx+1], al	; Coloca as caracter?sticas de cor da posi??o atual
			inc	bx
			inc	bx		; próxima posição e ecran dois bytes ? frente
	  loop preenche		;Falta otimizar e ir para o sítio de onde o chamou
	  sub bx, 4 ;Volta à posição original
	  ret
preenche_cor ENDP


;########################################################################
;########################################################################






;########################################################################

	Main  proc
		mov	ax, dseg
		mov	ds,ax
		mov	ax,0B800h
		mov	es,ax

	Menu:
		call apaga_ecran
		goto_xy	1,1
		lea dx,Fich
		call Imp_Fich
		
	ler:
		call bloc_teclas
		mov dl,al

		cmp ah,1	;ESC
		je ler
		cmp al,TEsc		; ESCAPE
		je fim
		cmp al,'4'
		je fim
		cmp al,'1'
		je jogar
		cmp al,'2'
		je top
		cmp al,'3'
		je config
		jne ler
	
	jogar:
		call apaga_ecran
		lea dx, Fich2
		call Imp_Fich
		call tabulSIMPLES
		call cursor
		je ler
		
	
	
	top:
		call apaga_ecran
	
	
	
	config:
		call apaga_ecran
	
		
		
		
	fim:
		call apaga_ecran
		goto_xy	2,22
		mov	ah,4CH
		INT	21H
Main	endp
Cseg	ends
end	Main
