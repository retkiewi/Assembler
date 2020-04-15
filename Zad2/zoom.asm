data1 segment
alphabet_filename       db  "alphabet.txt", 0
alphabet_pointer        dw  ?
letter_buf              db  76 dup(0)
centered_y              dw  ?

;komunikaty i błędy
incorrect_zoom_msg          db "Podano niepoprawny zoom", 10, 13, '$'
to_few_arguments_msg        db "Podano za malo argumentow", 10, 13, '$'
alphabet_doesnt_exist_msg   db "Brak zestawu bitmap", 10, 13, '$'
corrupted_file_msg          db "Bitmapa litery zawiera niepoprawne znaki", 10, 13, '$'
read_msg                    db "Nie mozna przeczytac bitmapy", 10, 13, '$'
correct_format_msg          db "Poprawna skladnia to: zoom wartosc_zoom(MAX 25) tekst_do_powiekszenia", 10, 13, '$'
data1 ends

code1 segment
start1:
;-INICJOWANE-STOSU---------------------------------------------------------------------
    mov ax, seg top1
    mov ss, ax
    mov sp, offset top1
;--------------------------------------------------------------------------------------

;-WEJŚCIE-W-TRYB-GRAFICZNY-------------------------------------------------------------
    call enter_graphic_mode
;--------------------------------------------------------------------------------------

;-PARSOWANIE---------------------------------------------------------------------------
    ;inicjowanie_iteratora
    mov	cx, 0
	cld

    ;inicjowanie źródła:
	mov	si, 082h
	mov	cl, ds:[080h]
	cmp cl, 2
	jb	to_few_arguments_err



    call space_destroyer
    ;W dx agregujemy zoom(zamiana ciągu znaków na liczbę dziesiętną)
    xor dx, dx
    xor ax, ax
    get_zoom:
        mov al, ds:[si]
        cmp al, '$'
        je to_few_arguments_err
        ;Przechodzimy do dalszej części wczytywania jeśli napotkamy spacje
        cmp al, ' '
        je  start_drawing
        cmp al, 48
        jl  incorrect_zoom_err
        cmp al, 57
        jg  incorrect_zoom_err
        ;Jeśli napotkaliśmy liczbę to mnożymy dotychczasowy dx*10
        push ax
        mov al, 10
        mul dl
        mov dl, al
        pop ax
        ;Zamieniamy kod znaku na liczbę i dodajemy ją do dx
        sub al, 48
        add dl, al
        inc si
        dec cl
        ;Czy zoom nie przekroczył odpowiedniego zakresu
        cmp dl, 25
        jg  incorrect_zoom_err
        jmp get_zoom

    start_drawing:
        ;Zapamiętujemy zoom
        mov byte ptr cs:[zoom], dl
        call init_centered_y
        call reset_y
        call space_destroyer
        call open_alphabet
        draw_loop:
            ;call wait_for_key
            call check_left_space
            cmp cl, 0
            jl end_prog
            mov al, byte ptr ds:[si]
            cmp al, '$'
            je end_prog
            cmp al, ' '
            jne no_space
            call add_8_to_x
            inc si
            dec cl
            jmp draw_loop
            no_space:
            call draw_letter
            call add_8_to_x
            call reset_y
            inc si
            jmp draw_loop




        
        
;--------------------------------------------------------------------------------------

;-KONIEC-PROGRAMU----------------------------------------------------------------------
    end_prog:
        call close_letter_bitmap
        call wait_for_key
        call enter_text_mode
    end_prog_err:
        mov ax, 4c00h
        int 21h
;--------------------------------------------------------------------------------------

;-FUNKCJE-POMOCNICZE-------------------------------------------------------------------
    light_pixel:
        push ax
        push bx
        push dx
        push es
        push ds

        mov ax, 0a000h
        mov es, ax

        ;Ustawianie współrzędnych w bx
        mov ax, word ptr cs:[y]
        mov bx, 320
        mul bx

        mov bx, word ptr cs:[x]
        add bx, ax

        ;Ustawianie koloru w al
        mov al, byte ptr cs:[col]

        ;Zapalanie punktu o współrzędnych z bx na kolor z al
        mov byte ptr es:[bx], al
        
        pop ds
        pop es
        pop dx
        pop bx
        pop ax

        ret

    light_zoomed_pixel:
        push ax
        push dx
        push cx
        push ds

        

        ;Sprawdzenie czy nie wychodzimy poza zakres
        xor cx, cx
        mov cl, byte ptr cs:[zoom]

        mov ax, word ptr cs:[x]
        cmp ax, 319
        jg out_of_bounds
        add ax, cx
        cmp ax, 319
        jg out_of_bounds
        
        mov dx, word ptr cs:[y]
        cmp dx, 269
        jg out_of_bounds
        add dx, cx
        cmp dx, 269
        jg out_of_bounds

        ;Zapisywanie oryginalnych współrzędnych
        mov ax, word ptr cs:[x]
        mov dx, word ptr cs:[y]
        
        

        xor cx, cx
        mov ch, byte ptr cs:[zoom]
        lzp_p1:
            mov cl, byte ptr cs:[zoom]
            mov word ptr cs:[x], ax
            lzp_p2:
                call light_pixel
                inc x
                dec cl
                cmp cl, 0
                jg lzp_p2
            inc y
            dec ch
            cmp ch, 0
            jg lzp_p1

        out_of_bounds:       
        mov word ptr cs:[x], ax
        mov word ptr cs:[y], dx

        pop ds
        pop cx
        pop dx
        pop ax

        ret

    increment_x:
        push dx
        push ax
        push ds

        xor dx, dx
        mov ax, word ptr cs:[x]
        cmp ax, 319
        jg dont_increment
        mov dl, byte ptr cs:[zoom]
        add ax, dx
        cmp ax, 319
        jg dont_increment

        xor ax, ax
        mov dx, word ptr cs:[x]
        mov al, byte ptr cs:[zoom]
        add dx, ax
        mov word ptr cs:[x], dx

        dont_increment:
        pop ds
        pop ax
        pop dx

        ret
    
    increment_y:
        push dx
        push ax
        push ds

        xor ax, ax
        mov dx, word ptr cs:[y]
        mov al, byte ptr cs:[zoom]
        add dx, ax
        mov word ptr cs:[y], dx

        pop ds
        pop ax
        pop dx

        ret
    
    open_alphabet:
        push dx
        push ds
        push ax
        
        mov ax, seg alphabet_filename
        mov ds, ax
        mov dx, offset alphabet_filename

        mov al, 0
        mov ah, 3dh
        int 21h
        jc alphabet_doesnt_exist_err

        save_handle:
        mov word ptr ds:[alphabet_pointer], ax

        pop ax
        pop ds
        pop dx
        ret

    close_letter_bitmap:
        push ds
        push dx
        push ax
        
        mov ax, seg alphabet_pointer
        mov ds, ax
        mov dx, offset alphabet_pointer
        xor ax, ax
        mov ah, 10h
        int 21h

        pop ax
        pop dx
        pop ds
        ret

    draw_letter:
        call load_letter_to_buf
        
        push ds
        push si
        push ax
        push bx
        mov bx, word ptr cs:[x]

        mov ax, seg letter_buf
        mov ds, ax
        mov si, offset letter_buf
        add si, 2

        letter_drawing_loop:
            inc si
            mov al, byte ptr ds:[si]
            cmp al, 48
            je  draw_black
            cmp al, 49
            je  draw_white
            cmp al, 10
            je next_line
            cmp al, 0
            je drawing_letter_finished
            jmp corrupted_file_err
            draw_black:
                mov byte ptr cs:[col], 00h
                call light_zoomed_pixel
                call increment_x
                jmp pixel_drawn

            draw_white:
                mov byte ptr cs:[col], 0ah
                call light_zoomed_pixel
                call increment_x
                jmp pixel_drawn

            next_line:
                mov word ptr cs:[x], bx
                call increment_y
                jmp pixel_drawn

            pixel_drawn:
                jmp letter_drawing_loop

        drawing_letter_finished:

        pop bx
        pop ax
        pop si
        pop ds
        ret

    load_letter_to_buf:
        push cx
        push bx
        push ds
        push si

        mov dx, seg alphabet_pointer
        mov ds, dx

        push ax

        ;Wyzeruj file pointer
        mov bx, word ptr ds:[alphabet_pointer]
        xor cx, cx
        xor dx, dx
        mov ah, 42h
        mov al, 00h
        int 21h
        jc file_pointer_err

        pop ax

        find_letter:
            push ax

            mov cx, 75
            mov dx, offset letter_buf
            mov bx, word ptr ds:[alphabet_pointer]
            mov ah, 3fh
            int 21h
            jc letter_doesnt_exist_err
            cmp ax, 0
            je end_prog

            pop ax
            mov si, offset letter_buf
            inc si
            cmp al, byte ptr ds:[si]
            jne find_letter

        pop si
        pop ds
        pop bx
		pop cx
		ret

    sub_8_from_x:
        push cx
        push ds

        mov cl, byte ptr cs:[zoom]
        s8fxl:
            sub word ptr cs:[x], 8
            dec cl
            cmp cl, 0
            jne s8fxl

        pop ds
        pop cx
        ret

    add_8_to_x:
        push cx
        push ds

        mov cl, byte ptr cs:[zoom]
        a8txl:
            add word ptr cs:[x], 8
            dec cl
            cmp cl, 0
            jne a8txl

        pop ds
        pop cx
        ret

    check_left_space:
        push ax
        push dx

        mov al, byte ptr cs:[zoom]
        mov dx, 8
        mul dx

        mov bx, word ptr cs:[x]
        add bx, ax

        cmp bx, 320
        jl space_left

        jmp end_prog

        space_left:
        pop dx
        pop ax
        ret

    init_centered_y:
        push ax
        push dx
        push ds
        push di

        
        mov dl, byte ptr cs:[zoom]
        mov dh, 0
        mov al, 8
        mul dx
        mov dx, ax

        mov ax, 200
        sub ax, dx
        
        mov dl, 2

        div dl

        mov ah, 0

        mov dx, seg centered_y
        mov ds, dx
        mov di, offset centered_y

        mov word ptr ds:[di], ax

        pop di
        pop ds
        pop dx
        pop ax
        ret

    reset_y:
        push ds
        push dx
        push si

        mov dx, seg centered_y
        mov ds, dx
        mov si, offset centered_y

        mov dx, word ptr ds:[si]
        mov word ptr cs:[y], dx

        pop si
        pop dx
        pop ds
        ret

    space_destroyer:	;usuwa ciąg spacji
        push ds
		mov al, byte ptr ds:[si]
		cmp al, ' '
		jne do_not_destroy
		destroy:
			inc si
            dec cl
			mov al, byte ptr ds:[si]
			cmp al, ' '
			je destroy
		do_not_destroy:
        pop ds
		ret

    enter_text_mode:
        push ax
        mov al, 3h      ;tryb tekstowy
        mov ah, 0       ;zmiana trybu karty VGA
        int 10h
        pop ax
        ret
    
    enter_graphic_mode:
        push ax
        mov al, 13h      ;tryb graficzny 320x200 256col
        mov ah, 0       ;zmiana trybu karty VGA
        int 10h
        pop ax
        ret

    prints_dx:			;wypisuje string zakończony '$' z ds:dx
		push ax
		push ds
		mov ax, seg alphabet_filename
		mov	ds, ax
		mov ah, 09h
		int 021h
		pop ds
		pop ax
		ret
    
    printc_dl:			;wypisuje char z ds:dl
		push	ax
        push    ds
        mov     ax, seg alphabet_filename
		mov	    ds, ax
		mov		ah, 2h
		int		21h
        pop     ds
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

    wait_for_key:
        push ax
        xor ax, ax
        int 16h         ;czekaj na dowolny klawisz
        pop ax
        ret

;--------------------------------------------------------------------------------------

;-ERROR-HANDLERS-----------------------------------------------------------------------
to_few_arguments_err:
    call enter_text_mode
    mov dx, offset to_few_arguments_msg
    call prints_dx
    mov dx, offset correct_format_msg
    call prints_dx
    jmp end_prog_err

incorrect_zoom_err:
    call enter_text_mode
    mov dx, offset incorrect_zoom_msg
    call prints_dx
    mov dx, offset correct_format_msg
    call prints_dx
    jmp end_prog_err

alphabet_doesnt_exist_err:
    call enter_text_mode
    call new_line
    mov dx, offset alphabet_doesnt_exist_msg
    call prints_dx
    mov dx, offset correct_format_msg
    call prints_dx
    jmp end_prog_err

corrupted_file_err:
    call enter_text_mode
    mov dl, al
    add dl, 48
    call printc_dl
    call new_line
    mov dx, offset corrupted_file_msg
    call prints_dx
    mov dx, offset correct_format_msg
    call prints_dx
    jmp end_prog_err

file_pointer_err:
    jmp end_prog_err

letter_doesnt_exist_err:
    jmp end_prog_err

read_err:
    call enter_text_mode
    mov dl, al
    call printc_dl
    call new_line
    mov dx, offset read_msg
    call prints_dx
    mov dx, offset correct_format_msg
    call prints_dx
    jmp end_prog_err
;--------------------------------------------------------------------------------------

;-ZMIENNE------------------------------------------------------------------------------
x       dw  10
y       dw  0
col     db  0ah
zoom    db  100
;--------------------------------------------------------------------------------------
code1 ends

stack1 segment stack
        dw  200 dup(?)
top1    dw  ?
stack1 ends

end start1