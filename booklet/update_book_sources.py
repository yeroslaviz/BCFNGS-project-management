#!/usr/bin/env python3
from __future__ import annotations

import pathlib
import re

ROOT = pathlib.Path(__file__).resolve().parent.parent
OUT_DIR = ROOT / "booklet" / "chapters"

SOURCES = [
    ("Server Preparations", ROOT / "Server_preparations.qmd", OUT_DIR / "server-preparations.qmd"),
    ("LDAP Setup Summary (2026-02-06)", ROOT / "notes" / "ldap-setup-summary-2026-02-06.md", OUT_DIR / "ldap-setup-summary-2026-02-06.qmd"),
    ("VM Rollout Summary (2026-02-09)", ROOT / "notes" / "vm-rollout-summary-2026-02-09.md", OUT_DIR / "vm-rollout-summary-2026-02-09.qmd"),
    ("VM Troubleshooting Guide", ROOT / "notes" / "vm-troubleshooting-guide.md", OUT_DIR / "vm-troubleshooting-guide.qmd"),
    ("Ubuntu Bare Server Setup HOWTO", ROOT / "notes" / "ubuntu-bare-server-setup-howto.md", OUT_DIR / "ubuntu-bare-server-setup-howto.qmd"),
]

FENCE_RE = re.compile(r"^(```|~~~)")
HEADING_RE = re.compile(r"^(#{1,6})\s")


def strip_yaml(text: str) -> str:
    if text.startswith("---\n"):
        end = text.find("\n---", 4)
        if end != -1:
            return text[end + 4 :].lstrip("\n")
    return text


def shift_headings(text: str) -> str:
    out_lines = []
    in_code = False
    for line in text.splitlines(keepends=True):
        if FENCE_RE.match(line):
            in_code = not in_code
            out_lines.append(line)
            continue
        if not in_code:
            m = HEADING_RE.match(line)
            if m:
                line = "#" + line
        out_lines.append(line)
    return "".join(out_lines)


def build_chapter(title: str, body: str) -> str:
    return (
        "---\n"
        f"title: \"{title}\"\n"
        "execute:\n"
        "  eval: false\n"
        "---\n\n"
        f"{body}"
    )


def force_eval_false(text: str) -> str:
    # Ensure no chunk explicitly enables eval
    text = re.sub(r"^#\|\s*eval\s*:\s*.*$", "#| eval: false", text, flags=re.IGNORECASE | re.MULTILINE)
    # Normalize any knitr-style eval=TRUE in chunk headers
    text = re.sub(r"eval\\s*=\\s*TRUE", "eval=FALSE", text, flags=re.IGNORECASE)
    return text


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    for title, src, dst in SOURCES:
        if not src.exists():
            raise SystemExit(f"Missing source file: {src}")
        raw = src.read_text(encoding="utf-8")
        body = strip_yaml(raw)
        body = shift_headings(body)
        body = force_eval_false(body)
        dst.write_text(build_chapter(title, body), encoding="utf-8")


if __name__ == "__main__":
    main()
