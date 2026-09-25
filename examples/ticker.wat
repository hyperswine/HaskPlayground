;; the original "ticker" demo, now as WAT: 4 x (print "tick", burn fuel)
(module
  (import "wasi" "fd_write" (func $fd_write (param i32 i32 i32) (result i32)))
  (memory 1)
  (data (i32.const 0) "tick\n")
  (export "_start" (func $main))
  (func $main (local $i i32) (local $j i32)
    (block $done (loop $outer
      (br_if $done (i32.eq (local.get $i) (i32.const 4)))
      (drop (call $fd_write (i32.const 1) (i32.const 0) (i32.const 5)))
      (local.set $j (i32.const 25))
      (loop $busy (br_if $busy (local.tee $j (i32.sub (local.get $j) (i32.const 1)))))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $outer)))))
