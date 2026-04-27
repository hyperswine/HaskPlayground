#!/usr/bin/env python3
"""
fpr - FP-RISC module registry and build tool

REGISTRY FORMAT  (fpr.reg — JSON in project root)
--------------------------------------------------
{
  "version": 1,
  "modules": {
    "std": {
      "version": 3,
      "hash":    "a1b2c3d4...",   # SHA-1 of committed content
      "path":    "std.fpr",        # relative path of source file
      "snapshot":"std.fpr.v3"      # filename inside .fpr/libs/
    },
    "mymod": {
      "version": 1,
      "hash":    "...",
      "path":    "mymod.fpr",
      "snapshot":"mymod.fpr.v1"
    }
  }
}

DIRECTORY LAYOUT
----------------
project/
  fpr.reg                  ← registry (committed to VCS)
  std.fpr                  ← source files
  mymod.fpr
  .fpr/
    libs/
      std.fpr.v1           ← immutable snapshots (never overwritten)
      std.fpr.v2
      std.fpr.v3           ← latest committed version
      mymod.fpr.v1
    cache/                 ← compiled output cache (keyed by name@vN)
      std@v3.core
      mymod@v1.core

COMMANDS
--------
  fpr init                 initialise fpr.reg and .fpr/ in current dir
  fpr status               show which files have changed since last commit
  fpr update               bump version for all changed files, write snapshots
  fpr update <file.fpr>    bump version for a specific file
  fpr resolve <spec>       print path of module snapshot for use "name@vN"
  fpr info                 print registry contents
  fpr clean                remove .fpr/cache/

USE RESOLUTION  (called by the compiler)
-----------------------------------------
  use "std@v1"   → .fpr/libs/std.fpr.v1
  use "std"      → .fpr/libs/std.fpr.v<current>  (latest committed)
  use "std@head" → std.fpr (source, uncommitted, dev only)
"""

import argparse
import hashlib
import json
import os
import shutil
import sys
from pathlib import Path
from typing import Optional

# ── constants ──────────────────────────────────────────────────────────────

REG_FILE    = "fpr.reg"
LIBS_DIR    = ".fpr/libs"
CACHE_DIR   = ".fpr/cache"
REG_VERSION = 1          # schema version of fpr.reg itself


# ── registry helpers ───────────────────────────────────────────────────────

def find_project_root() -> Optional[Path]:
    """Walk up from cwd until we find fpr.reg or hit filesystem root."""
    p = Path.cwd()
    while True:
        if (p / REG_FILE).exists():
            return p
        if p.parent == p:
            return None
        p = p.parent


def load_registry(root: Path) -> dict:
    reg_path = root / REG_FILE
    if not reg_path.exists():
        error(f"No {REG_FILE} found. Run 'fpr init' first.")
    with open(reg_path) as f:
        return json.load(f)


def save_registry(root: Path, reg: dict) -> None:
    reg_path = root / REG_FILE
    with open(reg_path, "w") as f:
        json.dump(reg, f, indent=2)
        f.write("\n")


def sha1_file(path: Path) -> str:
    h = hashlib.sha1()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(65536), b""):
            h.update(chunk)
    return h.hexdigest()


def sha1_str(s: str) -> str:
    return hashlib.sha1(s.encode()).hexdigest()


def error(msg: str) -> None:
    print(f"error: {msg}", file=sys.stderr)
    sys.exit(1)


def info(msg: str) -> None:
    print(msg)


def module_name_from_path(path: Path) -> str:
    """mymod.fpr → mymod"""
    return path.stem


def snapshot_name(module: str, version: int) -> str:
    return f"{module}.fpr.v{version}"


def ensure_dirs(root: Path) -> None:
    (root / LIBS_DIR).mkdir(parents=True, exist_ok=True)
    (root / CACHE_DIR).mkdir(parents=True, exist_ok=True)


# ── commands ───────────────────────────────────────────────────────────────

def cmd_init(args) -> None:
    root = Path.cwd()
    reg_path = root / REG_FILE

    if reg_path.exists():
        error(f"{REG_FILE} already exists. Already initialised.")

    ensure_dirs(root)

    # Discover .fpr files in the project root (files only, not dirs)
    fpr_files = sorted(p for p in root.glob("*.fpr") if p.is_file())
    modules = {}

    for fpr_path in fpr_files:
        name = module_name_from_path(fpr_path)
        h = sha1_file(fpr_path)
        snap = snapshot_name(name, 1)
        snap_path = root / LIBS_DIR / snap
        shutil.copy2(fpr_path, snap_path)
        modules[name] = {
            "version":  1,
            "hash":     h,
            "path":     str(fpr_path.relative_to(root)),
            "snapshot": snap,
        }
        info(f"  registered {name} @ v1  ({fpr_path.name})")

    reg = {"version": REG_VERSION, "modules": modules}
    save_registry(root, reg)
    info(f"\nInitialised {REG_FILE} with {len(modules)} module(s).")
    info(f"Snapshots written to {LIBS_DIR}/")


def cmd_status(args) -> None:
    root = find_project_root()
    if root is None:
        error("No project found. Run 'fpr init'.")

    reg = load_registry(root)
    modules = reg.get("modules", {})
    any_changed = False

    # Check registered modules
    for name, meta in sorted(modules.items()):
        src = root / meta["path"]
        if not src.exists():
            print(f"  MISSING   {name} (expected {meta['path']})")
            any_changed = True
            continue
        current_hash = sha1_file(src)
        if current_hash != meta["hash"]:
            print(f"  CHANGED   {name} @ v{meta['version']}  ({meta['path']})")
            any_changed = True
        else:
            print(f"  ok        {name} @ v{meta['version']}  ({meta['path']})")

    # Check for new .fpr files not yet registered
    for fpr_path in sorted(p for p in root.glob("*.fpr") if p.is_file()):
        name = module_name_from_path(fpr_path)
        if name not in modules:
            print(f"  NEW       {name}  ({fpr_path.name})  — not yet registered")
            any_changed = True

    if not any_changed:
        info("\nAll modules up to date.")


def _bump_module(root: Path, reg: dict, name: str, src: Path) -> bool:
    """
    Bump version for a single module.
    Returns True if a bump was performed.
    """
    modules = reg["modules"]
    current_hash = sha1_file(src)

    if name in modules:
        meta = modules[name]
        if meta["hash"] == current_hash:
            info(f"  unchanged {name} @ v{meta['version']}")
            return False
        new_version = meta["version"] + 1
    else:
        new_version = 1

    snap = snapshot_name(name, new_version)
    snap_path = root / LIBS_DIR / snap

    # Snapshots are immutable — never overwrite
    if snap_path.exists():
        error(f"Snapshot {snap} already exists. This is a bug.")

    shutil.copy2(src, snap_path)

    modules[name] = {
        "version":  new_version,
        "hash":     current_hash,
        "path":     str(src.relative_to(root)),
        "snapshot": snap,
    }
    info(f"  bumped    {name} v{new_version - 1 if name in modules else 0} → v{new_version}  ({snap})")
    return True


def cmd_update(args) -> None:
    root = find_project_root()
    if root is None:
        error("No project found. Run 'fpr init'.")

    ensure_dirs(root)
    reg = load_registry(root)
    modules = reg.get("modules", {})

    if args.files:
        # Update specific files
        targets = [Path(f) for f in args.files]
    else:
        # Update all .fpr files in project root
        targets = sorted(p for p in root.glob("*.fpr") if p.is_file())

    bumped = 0
    for fpr_path in targets:
        if not fpr_path.is_absolute():
            fpr_path = root / fpr_path
        if not fpr_path.exists():
            info(f"  warning: {fpr_path} not found, skipping")
            continue
        name = module_name_from_path(fpr_path)
        if _bump_module(root, reg, name, fpr_path):
            bumped += 1

    save_registry(root, reg)

    if bumped:
        info(f"\n{bumped} module(s) updated.")
    else:
        info("\nNothing to update.")


def cmd_resolve(args) -> None:
    """
    Resolve a use-spec like 'std@v1' or 'mymod' to an absolute path.
    This is called by the compiler to find the source for a use declaration.
    """
    root = find_project_root()
    if root is None:
        error("No project found.")

    reg = load_registry(root)
    spec = args.spec  # e.g. "std@v1", "std@head", "std"

    if "@" in spec:
        name, vtag = spec.split("@", 1)
    else:
        name, vtag = spec, None

    modules = reg.get("modules", {})

    if name not in modules:
        error(f"Module '{name}' not in registry.")

    meta = modules[name]

    if vtag is None or vtag == "latest":
        # Latest committed version
        snap_path = root / LIBS_DIR / meta["snapshot"]
        print(str(snap_path.resolve()))
    elif vtag == "head":
        # Live source file (uncommitted, dev mode)
        src_path = root / meta["path"]
        if not src_path.exists():
            error(f"Source file {meta['path']} not found.")
        print(str(src_path.resolve()))
    else:
        # Specific version: v1, v2, ...
        if not vtag.startswith("v"):
            error(f"Invalid version tag '{vtag}'. Expected vN (e.g. v1, v3).")
        try:
            vnum = int(vtag[1:])
        except ValueError:
            error(f"Invalid version number in '{vtag}'.")
        snap = snapshot_name(name, vnum)
        snap_path = root / LIBS_DIR / snap
        if not snap_path.exists():
            error(f"Snapshot {snap} not found. Available versions: 1..{meta['version']}")
        print(str(snap_path.resolve()))


def cmd_info(args) -> None:
    root = find_project_root()
    if root is None:
        error("No project found.")

    reg = load_registry(root)
    modules = reg.get("modules", {})

    info(f"Registry: {root / REG_FILE}")
    info(f"Libs:     {root / LIBS_DIR}")
    info(f"Schema:   v{reg.get('version', '?')}")
    info(f"Modules ({len(modules)}):")

    for name, meta in sorted(modules.items()):
        src   = root / meta["path"]
        snap  = root / LIBS_DIR / meta["snapshot"]
        dirty = ""
        if src.exists():
            if sha1_file(src) != meta["hash"]:
                dirty = "  [CHANGED — run fpr update]"
        else:
            dirty = "  [SOURCE MISSING]"

        info(f"  {name}")
        info(f"    version:  v{meta['version']}")
        info(f"    source:   {meta['path']}{dirty}")
        info(f"    snapshot: {LIBS_DIR}/{meta['snapshot']}"
             + (" ✓" if snap.exists() else "  [SNAPSHOT MISSING]"))
        info(f"    hash:     {meta['hash'][:16]}...")

        # List all available snapshots
        snaps = sorted((root / LIBS_DIR).glob(f"{name}.fpr.v*"))
        if len(snaps) > 1:
            versions = [s.name.split(".v")[1] for s in snaps]
            info(f"    history:  v{', v'.join(versions)}")


def cmd_clean(args) -> None:
    root = find_project_root()
    if root is None:
        error("No project found.")

    cache = root / CACHE_DIR
    if cache.exists():
        shutil.rmtree(cache)
        cache.mkdir()
        info(f"Cleared {CACHE_DIR}/")
    else:
        info("Cache already empty.")


def cmd_link(args) -> None:
    """
    Print the full resolution table for all use declarations in a .fpr file.
    Used by the compiler front-end to know what to include.
    Output: JSON  { "std": "/abs/path/to/.fpr/libs/std.fpr.v1", ... }
    """
    root = find_project_root()
    if root is None:
        error("No project found.")

    reg   = load_registry(root)
    src   = Path(args.file)
    if not src.exists():
        error(f"File not found: {src}")

    # Parse use declarations from the file (simple text scan, no full parse)
    uses = _scan_uses(src)
    result = {}

    for spec in uses:
        if "@" in spec:
            name, vtag = spec.split("@", 1)
        else:
            name, vtag = spec, "latest"

        modules = reg.get("modules", {})
        if name not in modules:
            print(f"  warning: module '{name}' not in registry, skipping", file=sys.stderr)
            continue

        meta    = modules[name]
        if vtag == "latest":
            snap_path = root / LIBS_DIR / meta["snapshot"]
        elif vtag == "head":
            snap_path = root / meta["path"]
        else:
            if not vtag.startswith("v"):
                error(f"Invalid vtag '{vtag}'")
            vnum = int(vtag[1:])
            snap  = snapshot_name(name, vnum)
            snap_path = root / LIBS_DIR / snap
            if not snap_path.exists():
                error(f"Snapshot {snap} not found.")

        result[name] = str(snap_path.resolve())

    print(json.dumps(result, indent=2))


def _scan_uses(path: Path) -> list:
    """
    Quick scan for `use "name@vN"` declarations without full parsing.
    Returns list of spec strings like ["std@v1", "mymod@v3"].
    """
    import re
    pattern = re.compile(r'\buse\s+"([^"]+)"')
    uses = []
    with open(path) as f:
        for line in f:
            line = line.split("#")[0]  # strip comments
            for m in pattern.finditer(line):
                uses.append(m.group(1))
    return uses


# ── compiler integration helpers ──────────────────────────────────────────

def resolve_use_spec(spec: str, root: Optional[Path] = None) -> Optional[str]:
    """
    Python API for use by the compiler.
    Given a spec like "std@v1", returns the absolute path to the snapshot.
    Returns None if not found.
    """
    if root is None:
        root = find_project_root()
    if root is None:
        return None

    try:
        reg = load_registry(root)
    except SystemExit:
        return None

    if "@" in spec:
        name, vtag = spec.split("@", 1)
    else:
        name, vtag = spec, "latest"

    modules = reg.get("modules", {})
    if name not in modules:
        return None

    meta = modules[name]

    if vtag in ("latest", None):
        p = root / LIBS_DIR / meta["snapshot"]
    elif vtag == "head":
        p = root / meta["path"]
    else:
        if not vtag.startswith("v"):
            return None
        try:
            vnum = int(vtag[1:])
        except ValueError:
            return None
        snap = snapshot_name(name, vnum)
        p = root / LIBS_DIR / snap

    return str(p.resolve()) if p.exists() else None


def all_module_versions(name: str, root: Optional[Path] = None) -> list:
    """
    Return list of (version_number, snapshot_path) for a module.
    """
    if root is None:
        root = find_project_root()
    if root is None:
        return []

    snaps = sorted((root / LIBS_DIR).glob(f"{name}.fpr.v*"),
                   key=lambda p: int(p.suffix[2:]) if p.suffix[2:].isdigit() else 0)
    result = []
    for s in snaps:
        try:
            parts = s.name.split(".v")
            vnum = int(parts[-1])
            result.append((vnum, str(s.resolve())))
        except (ValueError, IndexError):
            continue
    return result


# ── CLI entry point ────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        prog="fpr",
        description="FP-RISC module registry and build tool",
    )
    sub = parser.add_subparsers(dest="command", metavar="COMMAND")

    sub.add_parser("init",   help="Initialise fpr.reg in current directory")

    sub.add_parser("status", help="Show which modules have changed")

    p_update = sub.add_parser("update", help="Bump version for changed modules")
    p_update.add_argument("files", nargs="*", metavar="FILE.fpr",
                          help="Specific files to update (default: all changed)")

    p_resolve = sub.add_parser("resolve",
                                help="Resolve a module spec to a file path")
    p_resolve.add_argument("spec", metavar="SPEC",
                           help="e.g. 'std@v1', 'mymod', 'std@head'")

    p_link = sub.add_parser("link",
                             help="Print use-resolution table for a .fpr file")
    p_link.add_argument("file", metavar="FILE.fpr")

    sub.add_parser("info",  help="Print registry contents")
    sub.add_parser("clean", help="Clear compiled cache")

    args = parser.parse_args()

    if args.command is None:
        parser.print_help()
        sys.exit(0)

    dispatch = {
        "init":    cmd_init,
        "status":  cmd_status,
        "update":  cmd_update,
        "resolve": cmd_resolve,
        "link":    cmd_link,
        "info":    cmd_info,
        "clean":   cmd_clean,
    }

    dispatch[args.command](args)


if __name__ == "__main__":
    main()
