(module
  (import "env" "_fwrite"
          (func $__fwrite (param i32 i32 i32 i32) (result i32)))
  (import "env" "_stdout"
          (global $stdoutPtr i32))
  (import "env" "memory"
          (memory 1))
  (export "main"
          (func $main))

  (data (i32.const 8) "Teste de console\nTexto plano.\n")

  (func (export "establishStackSpace") (param i32 i32) (nop))

  (func $main (result i32)
    (local $stdout i32)
    (local.set $stdout
               (i32.load align=4 (global.get $stdoutPtr)))
    (return
      (call $__fwrite
            (i32.const 8)  ;; String address (void *ptr)
            (i32.const 1)  ;; Size of data (character -- size_t size
            (i32.const 30) ;; Size of string
            (local.get $stdout)))))

