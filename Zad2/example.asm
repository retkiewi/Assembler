code1 segment
start1:
    mov ax, seg top1
    mov ss, ax
    mov sp, offset top1

    mov al, 13h     ;tryb graficzny 320x200 256col
    mov ah, 0       ;zmiana trybu karty VGA
    int 10h         ;przerwanie BIOS

;-------------------------------------------------------------------------------------------------------------------
    xor cx, cx
    mov cx, 13
    
    p2:
        push cx
        push dx
        push ax
        mov  cx, 0
        mov  dx, 0FFh
        mov  ah, 86H
        int  15h
        pop ax
        pop dx
        call light_zoomed_pixel
        call increment_x
        inc col
        pop cx
        loop p2
;-------------------------------------------------------------------------------------------------------------------
    xor ax, ax
    int 16h         ;czekaj na dowolny klawisz

    mov al, 3h      ;tryb tekstowy
    mov ah, 0       ;zmiana trybu karty VGA
    int 10h

    mov ax, 4c00h
    int 21h

;--------------------------------------------------------------------------------------------------------------------
x   dw  0
y   dw  0
col db  0
zoom db 20
;--------------------------------------------------------------------------------------------------------------------

;-ZAPAL-PIXEL-O-ZADANYM-ROZMIARZE------------------------------------
light_zoomed_pixel:
    push cx
    push ax
    push cs:[y]
    xor cx, cx
    mov cl, byte ptr cs:[zoom]
    mov ax, word ptr cs:[x]
    add ax, cx
    cmp ax, 320
    jge skip_draw
    xor cx, cx
    mov cl, byte ptr cs:[zoom]
    mov ax, word ptr cs:[x]
    zoom_p1:
        push cx
        mov byte ptr cs:[x], al
        mov cl, byte ptr cs:[zoom]
        inc y
        zoom_p2:
            call light_pixel
            inc x
            loop zoom_p2
        pop cx
        loop zoom_p1
    mov byte ptr cs:[x], al
    skip_draw:
    pop cs:[y]
    pop ax
    pop cx
    ret

;--------------------------------------------------------------------

;---------------------------------------
light_pixel:
    push ax
    push es
    push bx
    mov ax, 0a000h  ;segment obrazu
    mov es, ax
    mov ax, word ptr cs:[y]     ;ax = Y
    mov bx, 320
    mul bx                      ;dx:ax = ax*bx = 320*y => ax=320*y, dx=0
    mov bx, word ptr cs:[x]     ;bx=x
    add bx, ax
    mov al, byte ptr cs:[col]
    mov byte ptr es:[bx], al    ;zapal punkt (x, y) na kolor col
    pop bx
    pop es
    pop ax
    ret
;---------------------------------------

;-HELPER-FUNCTIONS----------------------
increment_x:
    push ax
    mov al, byte ptr cs:[zoom]
    add byte ptr cs:[x], al
    pop ax
    ret

increment_y:
    push ax
    mov al, byte ptr cs:[zoom]
    add byte ptr cs:[y], al
    pop ax
    ret
;---------------------------------------
code1 ends

stack1 segment stack
        dw  100 dup(?)
top1    dw  ?
stack1 ends

end start1
