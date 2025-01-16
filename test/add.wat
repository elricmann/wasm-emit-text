(module
  ;; name: add
  (import "console" "log" (func (param $log i32)))
  (memory 1 2)
  (export "add" (func $add))
  (func $add (param $a i32) (param $b i32) (result i32)
    (i32.add
      (local.get $a)
      (local.get $b)
    )
  )
)
