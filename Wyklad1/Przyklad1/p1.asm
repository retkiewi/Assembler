data1	segment
txt1	db		"To jest test! :)", 10, 13, '$'
data1	ends


code1	segment

wypisz:
	mov		ah, 9	;wypisz tekst z DS:DX
	int 	21h
	ret

start1:
		;inicjowanie stosu -> SS SP
		mov		ax, seg top1 ;ax=adres segmentowy stosu
		mov		ss, ax
		mov		sp, offset top1
		
		mov		ax, seg txt1
		mov		ds, ax
		mov		dx, offset txt1	;ds:dx
		call wypisz
		
		mov		al, 0
		mov		ah, 04ch	;koniec programu
		int		21h
		
code1	ends

stos1	segment stack
		dw		100 dup(?)
top1	dw		?
stos1	ends

end start1