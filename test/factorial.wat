(module
  ;; name: factorial
  (func $factorial (param $n i32) (result i32)
    (local $o i32)
    (i32.const 1)
    (local.set $o)
    (loop $factorial_loop
      (i32.add
        (local.get $n)
        (i32.const 0)
      )
      (if
        (then
          (i32.mul
            (local.get $o)
            (local.get $n)
          )
          (local.set $o)
          (i32.sub
            (local.get $n)
            (i32.const 1)
          )
          (local.set $n)
        )
      )
    )
    (local.get $o)
    (i32.const 1)
    (local.set $o)
    (loop $factorial_loop
      (i32.add
        (local.get $n)
        (i32.const 0)
      )
      (if
        (then
          (i32.mul
            (local.get $o)
            (local.get $n)
          )
          (local.set $o)
          (i32.sub
            (local.get $n)
            (i32.const 1)
          )
          (local.set $n)
        )
      )
    )
    (local.get $o)
  )
)
