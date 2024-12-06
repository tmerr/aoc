; Some x86-64 calling conventions used in this code.
;   rdi, rsi, rdx: First, second, third args.
;   r10, r11: Temporary registers
;   r12, ..., r15: Saved registers.
; There are also other names for accessing just lower bits of registers.

          global    _start

          section   .text

; openfile() -> file descriptor. crash on error.
openfile:
          mov       rax, 2                  ; open syscall
          mov       rdi, fname              ; path parameter
          mov       rsi, 0                  ; flags = O_RDONLY
          mov       rdx, 0                  ; mode that is ignored
          syscall                           ; rax = open(...)
          cmp       rax, 0                  ; open syscall puts a negative value in rax on error. (i think contains the errno).
          jl        crash
          ret

; rdbyte(fd) -> uint64
; reads and returns a character (in the lowest order byte), or 0 on EOF. crashes for other errors.
rdbyte:
          sub       rsp, 8                  ; initialize 8 bytes on the stack to 0.
          mov       qword [rsp], 0
          mov       rax, 0                  ; read syscall
                                            ; note, rdi is already set
          mov       rsi, rsp                ; read the byte into the stack.
          mov       rdx, 1                  ; read just 1 byte.
          syscall                           ; rax = read(...)
          cmp       rax, -1                 ; check error
          je        crash
          cmp       rax, 0                  ; check EOF
          jne       read_done
          mov       qword [rsp], 0          ; EOF is as-if a 0 byte was read.
read_done:
          mov       rax, [rsp]
          add       rsp, 8
          ret

; prbyte(uint64)
prbyte:
          push      rdi                     ; push onto the stack to give it a memory addr.

          mov       rax, 1                  ; write syscall
          mov       rdi, 1                  ; stdout file handle
          mov       rsi, rsp                ; address of string to output
          mov       rdx, 1                  ; number of bytes
          syscall

          pop       rdi
          cmp       rax, 0
          je        crash
          ret

prhello:
          mov       rax, 1                  ; write syscall
          mov       rdi, 1                  ; stdout file handle
          mov       rsi, message            ; address of string to output
          mov       rdx, 12                 ; number of bytes
          syscall
          cmp       rax, 0
          je        crash
          ret

prgoodbye:
          mov       rax, 1                  ; write syscall
          mov       rdi, 1                  ; stdout file handle
          mov       rsi, goodbye            ; address of string to output
          mov       rdx, 14                 ; number of bytes
          syscall
          cmp       rax, 0
          je        crash
          ret

crash:
          mov       rax, 1                  ; write syscall
          mov       rdi, 2                  ; stderr file handle
          mov       rsi, errmsg             ; address of string to output
          mov       rdx, 16                 ; number of bytes
          syscall

          mov       rax, 60                 ; system call for exit
          xor       rdi, rdi                ; exit code 0
          syscall                           ; invoke operating system to exit

; scan_num(fd, terminator) -> uint64
;   scans a 1 to 3 digit number followed by some character.
;   returns
;     a value from 0 to 999 on success.
;     1000 + last read byte on failure.
scan_num:
          push      r12                     ; r12 holds local var fd
          push      r13                     ; r13 holds the terminator.
          push      r14                     ; r14 holds local var num

          mov       r12, rdi
          mov       r13, rsi

          ; reference http://www.unixwiz.net/techtips/x86-jumps.html
          mov       rdi, r12
          call      rdbyte
          cmp       rax, 48                 ; check 48 <= num  <= 57
          jl        bad_num
          cmp       rax, 57
          jg        bad_num
          sub       rax, 48                 ; ascii byte to int
          mov       r14, rax

          mov       rdi, r12
          call      rdbyte
          cmp       rax, r13                ; check terminator
          je        good_num
          cmp       rax, 48                 ; check 48 <= num  <= 57
          jl        bad_num
          cmp       rax, 57
          jg        bad_num
          sub       rax, 48                 ; ascii byte to int
          mov       r10, rax
          mov       rax, r14                ; put num into rax to multiply
          mov       r11, 10                 ; put 10 into a temporary register.
          mul       r11                     ; rax *= 10
          add       rax, r10                ; rax += digit
          mov       r14, rax                ; apply the changes.

          mov       rdi, r12
          call      rdbyte
          cmp       rax, r13                ; check terminator
          je        good_num
          cmp       rax, 48                 ; check 48 <= num  <= 57
          jl        bad_num
          cmp       rax, 57
          jg        bad_num
          sub       rax, 48                 ; ascii byte to int
          mov       r10, rax
          mov       rax, r14                ; put num into rax to multiply
          mov       r11, 10                 ; put 10 into a temporary register.
          mul       r11                     ; rax *= 10
          add       rax, r10                ; rax += digit
          mov       r14, rax                ; apply the changes.

          ; after 3 digits, require the terminator.
          mov       rdi, r12
          call      rdbyte
          cmp       rax, r13                ; check terminator
          jne       bad_num
good_num: mov       rax, r14
          jmp       end_num
bad_num:  add       rax, 1000
end_num:  pop       r14
          pop       r13
          pop       r12
          ret

; solution() -> uint64
solution:
          push      r12                     ; r12 holds local var fd
          push      r13                     ; r13 holds local var sum
          push      r14                     ; r14 holds left num in pair
          push      r15                     ; r15 holds 0 iff in "do()" mode.
          mov       r15, 0                  ; initially do() is enabled.

          call      openfile                ; ...
          mov       r12, rax                ; fd = openfile()
find_m:   mov       rdi, r12                ; scan m
          call      rdbyte
check_m:  cmp       rax, 0
          je        eof
          cmp       rax, "d"
          je        got_d
          cmp       r15, 0
          jne       find_m
          cmp       rax, "m"
          je        got_m
          jmp       find_m

got_d:    mov       rdi, r12                ; scan o
          call      rdbyte
          cmp       rax, "o"
          jne       check_m

          mov       rdi, r12                ; scan next char
          call      rdbyte
          cmp       rax, "n"                ; check n
          je        got_n
          cmp       rax, 40                 ; check left paren
          je        is_do
          jmp       check_m

is_do:    mov       rdi, r12                ; scan right paren
          call      rdbyte
          cmp       rax, 41
          jne       check_m
          mov       r15, 0                  ; begin do() mode
          jmp       find_m

got_n:    mov       rdi, r12                ; scan '
          call      rdbyte
          cmp       rax, 39
          jne       check_m 

          mov       rdi, r12                ; scan t
          call      rdbyte
          cmp       rax, "t"
          jne       check_m 

          mov       rdi, r12                ; scan left paren
          call      rdbyte
          cmp       rax, 40
          jne       check_m 

          mov       rdi, r12                ; scan right paren
          call      rdbyte
          cmp       rax, 41
          jne       check_m
          mov       r15, 1                  ; begin don't() mode
          jmp       find_m

got_m:    mov       rdi, r12                ; scan u
          call      rdbyte
          cmp       rax, "u"
          jne       check_m

          mov       rdi, r12                ; scan l
          call      rdbyte
          cmp       rax, "l"
          jne       check_m

          mov       rdi, r12                ; scan left paren
          call      rdbyte
          cmp       rax, 40
          jne       check_m

          mov       rdi, r12                ; scan comma-terminated num
          mov       rsi, 44
          call      scan_num
          cmp       rax, 1000
          jl        left_ok
          sub       rax, 1000
          jmp       check_m
left_ok:  mov       r14, rax

          mov       rdi, r12                ; scan right-paren-terminated num
          mov       rsi, 41
          call      scan_num
          cmp       rax, 1000
          jl        right_ok
          sub       rax, 1000
          jmp       check_m
right_ok: mul       r14                     ; rax = left * right
          add       r13, rax                ; sum += rax
          jmp       find_m
eof:
          mov       rax, r13
          pop       r15
          pop       r14
          pop       r13
          pop       r12
          ret

; itoa(val: uint64, out: *char) -> uint64
;   writes an int to the given character buffer followed by newline. Buffer must be >= 21 chars.
;   returns num bytes written.
pr_itoa:
          push      r12                     ; char position

          mov       r12, rsi
          mov       byte [r12], 10          ; write the newline char
          add       r12, 1                  ; advance

          mov       rax, rdi
          mov       r10, 10
itoa_loop:
          cmp       rax, 10
          jl        last_char
          mov       rdx, 0
          div       r10                     ; rax /= 10, remainder to rdx
          add       rdx, 48                 ; convert to ascii
          mov       byte [r12], dl          ; write the char
          add       r12, 1                  ; advance
          jmp       itoa_loop
last_char:
          add       rax, 48                 ; convert to ascii
          mov       byte [r12], al          ; write the char
          mov       rax, r12
          sub       rax, rsi                ; bytes written = end - start + 1.
          add       rax, 1
reverse:
          cmp       rsi, r12
          jge       itoa_done
          mov       r10b, [rsi]              ; tmp1 = *left
          mov       r11b, [r12]              ; tmp2 = *right
          mov       [r12], r10b
          mov       [rsi], r11b
          add       rsi, 1
          sub       r12, 1
          jmp       reverse
itoa_done:
          pop       r12
          ret


_start:
          sub       rsp, 24                 ; enough bytes to store itoa result.
          call      prhello

          call solution
          mov       rdi, rax                ; print the solution in ascii.
          mov       rsi, rsp
          call      pr_itoa

          mov       rdi, 1                  ; stdout file handle
          mov       rsi, rsp                ; address of string to output
          mov       rdx, rax                ; number of bytes
          mov       rax, 1                  ; write syscall
          syscall

          call      prgoodbye

          add       rsp, 24
          mov       rax, 60                 ; exit syscall
          mov       rdi, 0                  ; exit code 0
          syscall

          section   .data
message:  db        "Hello Day 3", 10    ; note the newline at the end
goodbye:  db        "Goodbye Day 3", 10
errmsg    db        "Uh oh! Crashing", 10
fname:    db        "input.txt", 0