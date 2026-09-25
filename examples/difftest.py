import subprocess, sys
mine = subprocess.run(["./tests","--diff"], capture_output=True, text=True).stdout.strip().splitlines()
ok = fail = 0
for line in mine:
    name, args, res = line.split("|")
    p = subprocess.run(["wasmtime","run","--invoke",name,"tests.wat",*args.split()], capture_output=True, text=True)
    if p.returncode != 0:
        trap = [l for l in p.stderr.splitlines() if "wasm trap:" in l]
        ref = "trap: " + trap[0].split("wasm trap:")[1].strip() if trap else "ERROR " + p.stderr[:80]
    else:
        ref = p.stdout.strip()
    def norm(s):
        if s.startswith("trap:"): return s
        out=[]
        for tok in s.split():
            try: out.append(repr(float(tok)) if ("." in tok or "e" in tok) else tok)
            except: out.append(tok)
        return " ".join(out)
    same = norm(res) == norm(ref) or (not res.startswith("trap") and not ref.startswith("trap") and all(abs(float(a)-float(b))<=1e-12*max(1,abs(float(b))) for a,b in zip(res.split(),ref.split())))
    ok += same; fail += (not same)
    print(("  same  " if same else "  DIFF  ") + f"{name}({args}): mine={res!r}  wasmtime={ref!r}")
print(f"{ok} agree, {fail} differ")
