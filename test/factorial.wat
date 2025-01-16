(module
  ;; name: factorial
  (func $factorial (param $n i32) (result i32)
    (local $o i32)
    (local.set $o
      (i32.const 1)
    )
    (block $exit
      (loop $factorial_loop
        (br_if $exit
          (i32.eqz
            (local.get $n)
          )
        )
        (local.set $o
          (i32.mul
            (local.get $o)
            (local.get $n)
          )
        )
        (local.set $n
          (i32.sub
            (local.get $n)
            (i32.const 1)
          )
        )
        (br $factorial_loop)
      )
    )
    (local.get $o)
  )
)
