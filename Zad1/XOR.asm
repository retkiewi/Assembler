 data1	segment

;dane używane przy parsowaniu
buf					db	260 dup('$')
input_buf			db	1001 dup(0)
input_file_name		db	21 dup(0)
input_pointer		dw	?
output_file_name	db	21 dup(0)
output_pointer		dw	?
cypher				db	220	dup(0)

;komunikaty i błędy
to_few_arguments_msg	db	"Podano za mala liczbe argumentow.", 10, 13, '$'	
to_long_file_name_msg	db	"Podana nazwa pliku jest zbyt dluga.", 10, 13, '$'
no_cypher_msg			db	"Brak klucza szyfrujacego", 10, 13, '$'
incorrect_cypher_msg	db	"Niepoprawny klucz szyfrujacy", 10, 13, '$'
wrong_input_msg			db	"Niepoprawna nazwa pliku wejsciowego", 10, 13, '$'
wrong_output_msg		db	"Niepoprawna nazwa pliku wyjsciowego", 10, 13, '$'
override_warning_msg	db	"Nadpisac plik wyjsciowy? Y/N", 10, 13, '$'
read_msg				db	"Blad wczytania z pliku wejsciowego", 10, 13, '$'
write_msg				db	"Blad zapisania do pliku wyjsciowego", 10, 13, '$'
correct_format_msg		db	"Poprawna skladnia to: xor.exe plik_wejsciowy(20) plik_wyjsciowy(20) ", '"', "klucz szyfrujacy", '"', ". ", 10, 13, "Nawiasy oznaczaja maksymalna ilosc znakow dla danego argumentu.", '$'

data1	ends

code1 segment

start1:
	;-INICJOWANIE-STOSU------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	mov	ax, seg top1
	mov	ss, ax
	mov	sp, offset top1
	;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	;-WCZYTYWANIE-PARAMETRÓW-DO-BUFORA---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	;inicjowanie iteratora
	mov	cx, 0
	cld

	;inicjowanie źródła:
	mov	si, 082h
	mov	cl, ds:[080h]
	cmp cl, 2
	jb	to_few_arguments_err
	
	
	;inicjowanie bufora:
	mov		ax, seg buf
	mov 	es, ax
	mov 	di, offset buf
	

	p1:		;przenosi string parametrów do bufora 
		push cx
		mov	al, byte ptr ds:[si]
		mov byte ptr es:[di], al
		inc si
		inc di
		pop	cx
		loop p1
	;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


	;-PARSOWANIE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	;ładowanie bufora jako źródła
	mov ax, seg buf
	mov ds, ax
	mov si, offset buf

	;ładowanie input_file_name jako destynacja(mam wrażenie, że cel brzmi nawet dziwnej w tym kontekście)
	mov ax, seg input_file_name
	mov es, ax
	mov di, offset input_file_name

	mov ch, 1
	mov cl, 0

	call space_destroyer
	cmp al, '"'
	je to_few_arguments_err

	;przenoszenie danych z bufora do odpowiedniego pliku(in/out)
	get_file_name:
		mov	al, byte ptr ds:[si]
		mov byte ptr es:[di], al
		cmp	al, ' '
		je load_next_argument
		cmp	al, '$'
		je no_cypher_err
		inc cl
		inc di
		inc si
		cmp cl, 20
		jg	to_long_file_name_err
		jmp get_file_name
	
	;ładowanie cypher jako destynacja(mam wrażenie, że cel brzmi nawet dziwnej w tym kontekście)
	load_cypher:
		mov ax, seg cypher
		mov es, ax
		mov	di, offset cypher

		mov	al, byte ptr ds:[si]
		cmp	al,	'"'
		jne	incorrect_cypher_err
		inc si
		;pętla ładująca
		cypher_loader:
			mov	al, byte ptr ds:[si]
			mov byte ptr es:[di], al
			inc si
			inc di
			inc cl
			cmp	al,	'$'
			je	incorrect_cypher_err
			cmp	al,	'"'
			jne	cypher_loader
		cmp	cl, 0	;jeśli nie załadowaliśmy żadnego znaku to klucz nie istnieje
		je	no_cypher_err
		dec di		;cofamy di żeby znaleźć się na końcu klucza
		mov	byte ptr es:[di], 0		;zmieniamy '$' na końcu klucza na 0, będzie to sygnał że należy "zawinąć" klucz

	jmp load_input_file

	load_next_argument:
		mov byte ptr es:[di], 0
		cmp cl, 0
		mov cl, 0
		call space_destroyer
		inc ch
		cmp	ch, 3
		jge	load_cypher
		cmp al, '"'
		je	to_few_arguments_err
		mov	ax, seg output_file_name
		mov	es,	ax
		mov	di,	offset output_file_name
		jmp	get_file_name
	;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	;-OTWIERANIE-PLIKÓW-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	load_input_file:
		mov dx, offset input_file_name	;ds cały czas wskazuje na data1, więc ustawiam tylko offset
		xor	ax, ax	;ustawiam ax tak aby plik był tylko do odczytu
		mov al,	0
		mov	ah,	3dh
		int 21h
		jc	wrong_input_err

	;ładuje pliik wyjściowy
		mov word ptr ds:[input_pointer], ax	;zapisuje uchwyt

		;wczytuje output file
		mov dx, offset output_file_name
		xor ax, ax
		mov al, 1
		mov ah, 3dh
		int 21h
		jc	override_output
		
	override_warning:
		mov dx, offset override_warning_msg
		call prints_dx
		ask_again:
		xor ax, ax
		mov ah, 01h
		int 21h
		cmp al, 'Y'
		je override_output
		cmp al, 'y'
		je override_output
		cmp al, 'N'
		je wrong_output_err
		cmp al, 'n'
		je wrong_output_err
		jmp ask_again

	;czyszcze plik wyjściowy
	override_output:
		push cx
		xor cx, cx
		mov dx, offset output_file_name
		xor ax, ax
		mov ah, 3ch
		int 21h
		mov word ptr ds:[output_pointer], ax
		pop cx
	jmp	xor_and_save_to_output
	
	
	
	
	;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	
	;-XOROWANIE-I-ZAPISYWANIE-DO-PLIKU-WYJŚCIOWEGO----------------------------------------------------------------------------------------------------------------------------------------------------------------------
	xor_and_save_to_output:
		call load_a_portion_of_input
		call xor_input_buf
		call save_to_output
		jmp	 xor_and_save_to_output

	load_a_portion_of_input:
		push cx
		mov cx, 1000
		mov bx, ds:[input_pointer]
		mov dx, offset input_buf
		xor ax, ax
		mov ah, 3fh
		int 21h
		jc	read_err
		cmp ax, 0
		je	end_program
		pop cx
		ret

	xor_input_buf:
		mov	 cx, ax

		push ax
		mov	 ax, seg cypher
		mov	 ds, ax
		mov	 si, offset cypher

		mov	 ax, seg input_buf
		mov  es, ax
		mov  di, offset input_buf
		

		xor_p:
			cmp byte ptr ds:[si], 0
			je	loop_cypher
			cypher_ok:
			mov al, byte ptr ds:[si]
			xor	byte ptr es:[di], al
			inc di
			inc si
			loop xor_p
		
		pop ax
		ret

		loop_cypher:
			mov si, offset cypher
			jmp cypher_ok

	save_to_output:
		mov bx, word ptr ds:[output_pointer]
		
		mov cx, ax

		mov ax, seg input_buf
		mov	ds, ax
		mov dx, offset input_buf

		mov ah, 40h
		int 21h

		jc	write_err

		ret

	;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	
	;-KONIEC-PROGRAMU----			+------+.      +------+       +------+       +------+      .+------+			+------+.      +------+       +------+       +------+      .+------+
	end_program:		;			|`.    | `.    |\     |\      |      |      /|     /|    .' |    .'|			|`.    | `.    |\     |\      |      |      /|     /|    .' |    .'|
		xor ax, ax		;			|  `+--+---+   | +----+-+     +------+     +-+----+ |   +---+--+'  |			|  `+--+---+   | +----+-+     +------+     +-+----+ |   +---+--+'  |
		mov ah, 04ch	;			|   |  |   |   | |    | |     |      |     | |    | |   |   |  |   |			|   |  |   |   | |    | |     |      |     | |    | |   |   |  |   |
		int 21h			;			+---+--+.  |   +-+----+ |     +------+     | +----+-+   |  .+--+---+			+---+--+.  |   +-+----+ |     +------+     | +----+-+   |  .+--+---+
						;			 `. |    `.|    \|     \|     |      |     |/     |/    |.'    | .'				 `. |    `.|    \|     \|     |      |     |/     |/    |.'    | .'
	;--------------------			   `+------+     +------+     +------+     +------+     +------+'				   `+------+     +------+     +------+     +------+     +------+'
		 

	;-ERROR HANDLERS----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	to_few_arguments_err:
		mov dx, offset to_few_arguments_msg
		call prints_dx
		mov	dx, offset correct_format_msg
		call prints_dx
		call end_program

	to_long_file_name_err:
		mov dx, offset to_long_file_name_msg
		call prints_dx
		mov	dx, offset correct_format_msg
		call prints_dx
		call end_program

	no_cypher_err:
		mov dx, offset no_cypher_msg
		call prints_dx
		mov	dx, offset correct_format_msg
		call prints_dx
		call end_program

	incorrect_cypher_err:
		mov dx, offset incorrect_cypher_msg
		call prints_dx
		mov	dx, offset correct_format_msg
		call prints_dx
		call end_program

	wrong_input_err:
		mov dx, offset wrong_input_msg
		call prints_dx
		mov dx, offset correct_format_msg
		call prints_dx
		call end_program

	wrong_output_err:
		call new_line
		mov dx, offset wrong_output_msg
		call prints_dx
		mov dx, offset correct_format_msg
		call prints_dx
		call end_program

	read_err:
		mov dx, offset read_msg
		call prints_dx
		mov dx, offset correct_format_msg
		call prints_dx
		call end_program

	write_err:
		mov dx, offset write_msg
		call prints_dx
		mov dx, offset correct_format_msg
		call prints_dx
		call end_program
	;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	;-HELPER FUNCTIONS--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	prints_dx:			;wypisuje string zakończony '$' z ds:dx
		push ax
		push ds
		mov ax, seg data1
		mov	ds, ax
		mov ah, 09h
		int 021h
		pop ds
		pop ax
		ret

	printc_dl:			;wypisuje char z ds:dl
		push	ax
		mov		ah, 2h
		int		21h
		pop		ax
		ret

	new_line:			;przechodzi na nową linie
		push	dx
		mov		dl, 10
		call printc_dl
		mov		dl, 13
		call printc_dl
		pop dx
		ret

	space_destroyer:	;usuwa ciąg spacji
		mov al, byte ptr ds:[si]
		cmp al, ' '
		jne do_not_destroy
		destroy:
			inc si
			mov al, byte ptr ds:[si]
			cmp al, ' '
			je destroy
		do_not_destroy:
			ret
	
	;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


code1 ends

stack1	segment	stack
		dw	100 dup(?)
top1	dw	?
stack1 ends

end start1