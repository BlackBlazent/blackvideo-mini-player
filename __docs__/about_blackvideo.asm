; about_blackvideo.com - simple DOS .com info program

org 100h          ; .com start address

start:
    mov dx,msg
    mov ah,09h    ; DOS print string function
    int 21h

    mov ax,4C00h  ; exit program
    int 21h

msg db 'BlackVideo Mini Player',13,10
     db 'Version: 1.1.0',13,10
     db 'License: MIT',13,10
     db 'Repo: https://github.com/BlackBlazent/blackvideo-mini-player.git',13,10
     db 'Website: https://blackvideo-centric-site.onrender.com/',13,10
     db 'Potential Domain: blackvideo.com or blackvideo.video',13,10
     db '© 2026 BlackVideo Inc.',13,10,'$'