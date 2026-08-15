"""pkgstore client — publish, resolve, fetch-with-cache, install.

Usage:
  client.py publish <dir> <name> <version>     tar+gzip dir, upload, register
  client.py resolve                            versions in pkg.json -> hashes in pkg.lock.json
  client.py install                            fetch lockfile deps into cache, link into deps/
  client.py cache                              show local cache contents

Files (in cwd):
  pkg.json        {"dependencies": {"name": "version", ...}}
  pkg.lock.json   {"name": {"version": ..., "hash": ...}, ...}
  deps/<name>     symlink -> cache entry

Cache: ~/.cache/pkgstore/<hash>/  extracted content, verified on every use.
Server: PKGSTORE_URL env var, default http://127.0.0.1:8323
"""

import gzip
import hashlib
import io
import json
import os
import shutil
import sys
import tarfile
from pathlib import Path

import httpx

SERVER = os.environ.get("PKGSTORE_URL", "http://127.0.0.1:8323")
CACHE = Path(os.environ.get("PKGSTORE_CACHE",
             Path.home() / ".cache" / "pkgstore"))


def die(msg: str) -> None:
    print(f"error: {msg}", file=sys.stderr)
    sys.exit(1)


def make_blob(src: Path) -> bytes:
    """Deterministic gzip'd tarball: sorted names, zeroed metadata.

    Determinism means re-publishing identical content yields the identical
    hash, so the store dedups it for free.
    """
    buf = io.BytesIO()
    with tarfile.open(fileobj=buf, mode="w") as tf:
        for p in sorted(src.rglob("*")):
            if not p.is_file():
                continue
            ti = tarfile.TarInfo(str(p.relative_to(src)))
            data = p.read_bytes()
            ti.size = len(data)
            ti.mtime, ti.uid, ti.gid, ti.uname, ti.gname = 0, 0, 0, "", ""
            tf.addfile(ti, io.BytesIO(data))
    return gzip.compress(buf.getvalue(), mtime=0)


def publish(src: str, name: str, version: str) -> None:
    blob = make_blob(Path(src))
    local_hash = "sha256:" + hashlib.sha256(blob).hexdigest()
    r = httpx.post(f"{SERVER}/upload", content=blob)
    r.raise_for_status()
    server_hash = r.json()["hash"]
    if server_hash != local_hash:
        die(f"server hash mismatch: {server_hash} != {local_hash}")
    r = httpx.post(f"{SERVER}/index/{name}/{version}",
                   json={"hash": local_hash})
    if r.status_code == 409:
        die(r.json()["detail"])
    r.raise_for_status()
    print(f"published {name}@{version} ({len(blob)} bytes) -> {local_hash}")


def resolve() -> None:
    spec = json.loads(Path("pkg.json").read_text())
    lock = {}
    for name, version in spec.get("dependencies", {}).items():
        r = httpx.get(f"{SERVER}/index/{name}")
        r.raise_for_status()
        match = [v for v in r.json()["versions"] if v["version"] == version]
        if not match:
            die(f"{name}@{version} not found in index")
        lock[name] = {"version": version, "hash": match[0]["hash"]}
        print(f"resolved {name}@{version} -> {match[0]['hash'][:19]}…")
    Path("pkg.lock.json").write_text(json.dumps(lock, indent=2) + "\n")
    print("wrote pkg.lock.json")


def fetch_into_cache(h: str) -> Path:
    """Return cache dir for hash, fetching + verifying if absent."""
    dest = CACHE / h.replace(":", "_")
    if (dest / ".ok").exists():
        print(f"  cache hit  {h[:19]}…")
        return dest
    print(f"  fetching   {h[:19]}…")
    r = httpx.get(f"{SERVER}/blob/{h}")
    r.raise_for_status()
    blob = r.content
    got = "sha256:" + hashlib.sha256(blob).hexdigest()
    if got != h:  # never trust the server's claim
        die(f"integrity failure: wanted {h}, got {got}")
    if dest.exists():
        shutil.rmtree(dest)
    dest.mkdir(parents=True)
    with tarfile.open(fileobj=io.BytesIO(gzip.decompress(blob))) as tf:
        tf.extractall(dest, filter="data")
    (dest / ".ok").touch()
    return dest


def install() -> None:
    lock = json.loads(Path("pkg.lock.json").read_text())
    deps = Path("deps")
    deps.mkdir(exist_ok=True)
    for name, entry in lock.items():
        print(f"{name}@{entry['version']}")
        cached = fetch_into_cache(entry["hash"])
        link = deps / name
        if link.is_symlink() or link.exists():
            link.unlink() if link.is_symlink() else shutil.rmtree(link)
        link.symlink_to(cached)
    print(f"installed {len(lock)} package(s) into deps/")


def show_cache() -> None:
    if not CACHE.exists():
        print("(empty)")
        return
    for d in sorted(CACHE.iterdir()):
        n = sum(1 for _ in d.rglob("*") if _.is_file())
        print(f"{d.name}  ({n} files)")


if __name__ == "__main__":
    cmds = {"publish": publish, "resolve": resolve,
            "install": install, "cache": show_cache}
    if len(sys.argv) < 2 or sys.argv[1] not in cmds:
        die(f"usage: {sys.argv[0]} {{publish <dir> <name> <ver> | resolve | install | cache}}")
    cmds[sys.argv[1]](*sys.argv[2:])
