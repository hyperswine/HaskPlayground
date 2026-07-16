;; ============================================================================
;; Minimal hand-written WASM/WAT calculator against the model's simplified
;; WASI-like ABI (NOT binary wasi_snapshot_preview1 -- no iovecs, path_open
;; takes (ptr, len, rights) and fd_read/fd_write take (fd, ptr, len)).
;;
;; Behaviour:
;;   fd = path_open("expr.txt", read)     -- capability-gated, may prompt user
;;   read line "A op B" into memory
;;   compute A op B for op in + - * /     (integers)
;;   print decimal result + newline to stdout, or an error message
;;
;; Memory layout:
;;   100..107   "expr.txt"
;;   200..263   input buffer
;;   300..      error strings
;;   400..420   output digit buffer (digits written right-to-left ending 419,
;;              '\n' at 420)
;; ============================================================================
(module
  (import "wasi" "path_open" (func $path_open (param i32 i32 i32) (result i32)))
  (import "wasi" "fd_read"   (func $fd_read   (param i32 i32 i32) (result i32)))
  (import "wasi" "fd_write"  (func $fd_write  (param i32 i32 i32) (result i32)))
  (memory 1)
  (data (i32.const 100) "expr.txt")
  (data (i32.const 300) "error: open failed\n")   ;; len 19
  (data (i32.const 320) "error: div by zero\n")   ;; len 19
  (data (i32.const 340) "error: bad op\n")        ;; len 14

  (func $main
    (local $fd i32) (local $i i32) (local $c i32)
    (local $a i32) (local $b i32) (local $op i32)
    (local $r i32) (local $neg i32) (local $p i32)

    (block $exit

      ;; ---- fd = path_open("expr.txt", len 8, rights R=0) ------------------
      (i32.const 100) (i32.const 8) (i32.const 0)
      (call $path_open)
      (local.tee $fd)
      (i32.const 1) (i32.add) (i32.eqz)          ;; fd == -1 ?
      (if (then
        (i32.const 1) (i32.const 300) (i32.const 19) (call $fd_write) (drop)
        (br $exit)))

      ;; ---- read up to 64 bytes into 200 -----------------------------------
      (local.get $fd) (i32.const 200) (i32.const 64)
      (call $fd_read) (drop)

      (i32.const 200) (local.set $i)

      ;; ---- skip spaces -----------------------------------------------------
      (block $s1e (loop $s1
        (local.get $i) (i32.load8_u) (i32.const 32) (i32.sub)
        (i32.eqz) (i32.eqz) (br_if $s1e)          ;; not a space -> stop
        (local.get $i) (i32.const 1) (i32.add) (local.set $i)
        (br $s1)))

      ;; ---- parse A ---------------------------------------------------------
      (i32.const 0) (local.set $a)
      (block $p1e (loop $p1
        (local.get $i) (i32.load8_u) (local.set $c)
        (local.get $c) (i32.const 48) (i32.lt_s) (br_if $p1e)   ;; c < '0'
        (i32.const 57) (local.get $c) (i32.lt_s) (br_if $p1e)   ;; c > '9'
        (local.get $a) (i32.const 10) (i32.mul)
        (local.get $c) (i32.const 48) (i32.sub) (i32.add)
        (local.set $a)
        (local.get $i) (i32.const 1) (i32.add) (local.set $i)
        (br $p1)))

      ;; ---- skip spaces, grab op byte, skip spaces --------------------------
      (block $s2e (loop $s2
        (local.get $i) (i32.load8_u) (i32.const 32) (i32.sub)
        (i32.eqz) (i32.eqz) (br_if $s2e)
        (local.get $i) (i32.const 1) (i32.add) (local.set $i)
        (br $s2)))

      (local.get $i) (i32.load8_u) (local.set $op)
      (local.get $i) (i32.const 1) (i32.add) (local.set $i)

      (block $s3e (loop $s3
        (local.get $i) (i32.load8_u) (i32.const 32) (i32.sub)
        (i32.eqz) (i32.eqz) (br_if $s3e)
        (local.get $i) (i32.const 1) (i32.add) (local.set $i)
        (br $s3)))

      ;; ---- parse B ---------------------------------------------------------
      (i32.const 0) (local.set $b)
      (block $p2e (loop $p2
        (local.get $i) (i32.load8_u) (local.set $c)
        (local.get $c) (i32.const 48) (i32.lt_s) (br_if $p2e)
        (i32.const 57) (local.get $c) (i32.lt_s) (br_if $p2e)
        (local.get $b) (i32.const 10) (i32.mul)
        (local.get $c) (i32.const 48) (i32.sub) (i32.add)
        (local.set $b)
        (local.get $i) (i32.const 1) (i32.add) (local.set $i)
        (br $p2)))

      ;; ---- dispatch on op ('+'=43 '-'=45 '*'=42 '/'=47) --------------------
      (block $done
        (local.get $op) (i32.const 43) (i32.sub) (i32.eqz)
        (if (then
          (local.get $a) (local.get $b) (i32.add) (local.set $r)
          (br $done)))

        (local.get $op) (i32.const 45) (i32.sub) (i32.eqz)
        (if (then
          (local.get $a) (local.get $b) (i32.sub) (local.set $r)
          (br $done)))

        (local.get $op) (i32.const 42) (i32.sub) (i32.eqz)
        (if (then
          (local.get $a) (local.get $b) (i32.mul) (local.set $r)
          (br $done)))

        (local.get $op) (i32.const 47) (i32.sub) (i32.eqz)
        (if (then
          (local.get $b) (i32.eqz)
          (if (then
            (i32.const 1) (i32.const 320) (i32.const 19) (call $fd_write) (drop)
            (br $exit)))
          (local.get $a) (local.get $b) (i32.div_s) (local.set $r)
          (br $done)))

        ;; unknown operator
        (i32.const 1) (i32.const 340) (i32.const 14) (call $fd_write) (drop)
        (br $exit))

      ;; ---- print r as decimal + '\n' ---------------------------------------
      (i32.const 420) (i32.const 10) (i32.store8)     ;; trailing newline

      (local.get $r) (i32.eqz)
      (if
        (then
          (i32.const 419) (i32.const 48) (i32.store8) ;; "0\n"
          (i32.const 1) (i32.const 419) (i32.const 2) (call $fd_write) (drop))
        (else
          (i32.const 0) (local.set $neg)
          (local.get $r) (i32.const 0) (i32.lt_s)
          (if (then
            (i32.const 1) (local.set $neg)
            (i32.const 0) (local.get $r) (i32.sub) (local.set $r)))

          (i32.const 420) (local.set $p)
          (block $de (loop $dl
            (local.get $r) (i32.eqz) (br_if $de)
            (local.get $p) (i32.const 1) (i32.sub) (local.set $p)
            (local.get $p)
            (local.get $r) (i32.const 10) (i32.rem_s) (i32.const 48) (i32.add)
            (i32.store8)
            (local.get $r) (i32.const 10) (i32.div_s) (local.set $r)
            (br $dl)))

          (local.get $neg)
          (if (then
            (local.get $p) (i32.const 1) (i32.sub) (local.set $p)
            (local.get $p) (i32.const 45) (i32.store8)))     ;; '-'

          ;; fd_write(1, p, 421 - p)
          (i32.const 1) (local.get $p)
          (i32.const 421) (local.get $p) (i32.sub)
          (call $fd_write) (drop)))
    ) ;; $exit
  )
)
