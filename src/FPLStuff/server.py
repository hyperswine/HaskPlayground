"""pkgstore server — a small dumb content-addressed package store.

Model:
  store/<sha256>          gzip'd tarball blobs, immutable, dedup'd by content
  index.db (sqlite)       append-only (name, version) -> hash bindings

Endpoints:
  POST /upload                        raw gzip tarball body -> {"hash": ...}
  GET  /blob/{hash}                   stream blob back
  POST /index/{name}/{version}        bind version to an uploaded hash (append-only)
  GET  /index/{name}                  all versions of a package
  GET  /index                         all packages
  GET  /browse/{name}/{version}       full file map for a version, one call
  GET  /diff/{name}/{va}/{vb}         per-file unified diffs between two versions
  GET  /                              tiny HTML browser (browse + side-by-side diff)

No auth, no policy, no builds. Policy lives on top.
"""

import difflib
import gzip
import hashlib
import io
import json
import sqlite3
import tarfile
from pathlib import Path

from fastapi import FastAPI, HTTPException, Request
from fastapi.responses import HTMLResponse, Response

ROOT = Path(__file__).parent / "data"
STORE = ROOT / "store"
DB = ROOT / "index.db"
MAX_BLOB = 256 * 1024 * 1024  # 256 MB, adjust to taste

app = FastAPI(title="pkgstore")


def db() -> sqlite3.Connection:
    conn = sqlite3.connect(DB)
    conn.row_factory = sqlite3.Row
    return conn


@app.on_event("startup")
def startup() -> None:
    STORE.mkdir(parents=True, exist_ok=True)
    with db() as conn:
        conn.execute(
            """CREATE TABLE IF NOT EXISTS packages (
                 name        TEXT NOT NULL,
                 version     TEXT NOT NULL,
                 hash        TEXT NOT NULL,
                 size        INTEGER NOT NULL,
                 uploaded_at TEXT NOT NULL DEFAULT (datetime('now')),
                 PRIMARY KEY (name, version)
               )"""
        )


def blob_path(h: str) -> Path:
    if not (h.startswith("sha256:") and len(h) == 71 and h[7:].isalnum()):
        raise HTTPException(400, "malformed hash")
    return STORE / h.replace(":", "_")


# ---------------------------------------------------------------- blobs

@app.post("/upload")
async def upload(request: Request):
    body = await request.body()
    if not body:
        raise HTTPException(400, "empty body")
    if len(body) > MAX_BLOB:
        raise HTTPException(413, "blob too large")
    h = "sha256:" + hashlib.sha256(body).hexdigest()
    p = blob_path(h)
    if not p.exists():
        tmp = p.with_suffix(".tmp")
        tmp.write_bytes(body)
        tmp.rename(p)  # atomic-ish; content-addressed so a race writes same bytes
    return {"hash": h, "size": len(body), "existed": p.stat().st_size == len(body)}


@app.get("/blob/{h}")
def get_blob(h: str):
    p = blob_path(h)
    if not p.exists():
        raise HTTPException(404, "no such blob")
    return Response(p.read_bytes(), media_type="application/octet-stream")


# ---------------------------------------------------------------- index

@app.post("/index/{name}/{version}")
async def register(name: str, version: str, request: Request):
    payload = await request.json()
    h = payload.get("hash", "")
    p = blob_path(h)
    if not p.exists():
        raise HTTPException(400, "hash not in store; upload first")
    try:
        with db() as conn:
            conn.execute(
                "INSERT INTO packages (name, version, hash, size) VALUES (?,?,?,?)",
                (name, version, h, p.stat().st_size),
            )
    except sqlite3.IntegrityError:
        row = _lookup(name, version)
        if row["hash"] == h:
            return {"ok": True, "note": "already registered with same hash"}
        # THE invariant: version->hash bindings are append-only, never repointed.
        raise HTTPException(
            409,
            f"{name}@{version} is already bound to {row['hash']}; "
            "bindings are immutable — publish a new version",
        )
    return {"ok": True}


def _lookup(name: str, version: str) -> sqlite3.Row:
    with db() as conn:
        row = conn.execute(
            "SELECT * FROM packages WHERE name=? AND version=?", (name, version)
        ).fetchone()
    if row is None:
        raise HTTPException(404, f"no such package version: {name}@{version}")
    return row


@app.get("/index")
def index_all():
    with db() as conn:
        rows = conn.execute(
            "SELECT name, version, hash, size, uploaded_at FROM packages ORDER BY name, uploaded_at"
        ).fetchall()
    return [dict(r) for r in rows]


@app.get("/index/{name}")
def index_name(name: str):
    with db() as conn:
        rows = conn.execute(
            "SELECT version, hash, size, uploaded_at FROM packages WHERE name=? ORDER BY uploaded_at",
            (name,),
        ).fetchall()
    if not rows:
        raise HTTPException(404, f"no such package: {name}")
    return {"name": name, "versions": [dict(r) for r in rows]}


# ---------------------------------------------------------------- browse / diff

def _files_of(h: str) -> dict[str, str | None]:
    """One blob fetch -> full file map. Text decoded; binary marked None."""
    raw = blob_path(h).read_bytes()
    files: dict[str, str | None] = {}
    with tarfile.open(fileobj=io.BytesIO(gzip.decompress(raw))) as tf:
        for m in tf.getmembers():
            if not m.isfile():
                continue
            data = tf.extractfile(m).read()
            try:
                files[m.name] = data.decode("utf-8")
            except UnicodeDecodeError:
                files[m.name] = None  # binary; viewer shows a placeholder
    return files


@app.get("/browse/{name}/{version}")
def browse(name: str, version: str):
    row = _lookup(name, version)
    return {
        "name": name,
        "version": version,
        "hash": row["hash"],
        "uploaded_at": row["uploaded_at"],
        "files": _files_of(row["hash"]),
    }


@app.get("/diff/{name}/{va}/{vb}")
def diff(name: str, va: str, vb: str):
    fa = _files_of(_lookup(name, va)["hash"])
    fb = _files_of(_lookup(name, vb)["hash"])
    out = {"name": name, "a": va, "b": vb, "files": []}
    for path in sorted(set(fa) | set(fb)):
        in_a, in_b = path in fa, path in fb
        status = "added" if not in_a else "removed" if not in_b else (
            "unchanged" if fa[path] == fb[path] else "changed"
        )
        entry = {"path": path, "status": status}
        if status == "changed" and fa[path] is not None and fb[path] is not None:
            entry["diff"] = list(
                difflib.unified_diff(
                    fa[path].splitlines(), fb[path].splitlines(),
                    fromfile=f"{path}@{va}", tofile=f"{path}@{vb}", lineterm="",
                )
            )
        out["files"].append(entry)
    return out


# ---------------------------------------------------------------- viewer

@app.get("/", response_class=HTMLResponse)
def viewer():
    return (Path(__file__).parent / "viewer.html").read_text()
