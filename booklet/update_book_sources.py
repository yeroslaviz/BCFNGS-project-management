#!/usr/bin/env python3
from __future__ import annotations

import pathlib
import re

ROOT = pathlib.Path(__file__).resolve().parent.parent
OUT_DIR = ROOT / "booklet" / "chapters"
QUARTO_FILE = ROOT / "booklet" / "_quarto.yml"
SITE_URL = "https://yeroslaviz.github.io/BCFNGS-project-management/"

FENCE_RE = re.compile(r"^(```|~~~)")
HEADING_RE = re.compile(r"^(#{1,6})\s")
TITLE_RE = re.compile(r'^\s*title:\s*["\']?(.+?)["\']?\s*$', re.IGNORECASE | re.MULTILINE)
H1_RE = re.compile(r"^#\s+(.+?)\s*$", re.MULTILINE)
SLUG_RE = re.compile(r"[^a-z0-9]+")


def discover_sources() -> tuple[list[pathlib.Path], list[pathlib.Path]]:
    root_qmd = sorted(path for path in ROOT.glob("*.qmd") if path.is_file())
    notes_md = sorted(path for path in (ROOT / "notes").glob("*.md") if path.is_file())
    return root_qmd, notes_md


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


def extract_yaml_title(text: str) -> str | None:
    if not text.startswith("---\n"):
        return None
    end = text.find("\n---", 4)
    if end == -1:
        return None
    header = text[: end + 4]
    match = TITLE_RE.search(header)
    if not match:
        return None
    return match.group(1).strip()


def extract_first_h1(text: str) -> str | None:
    match = H1_RE.search(text)
    if not match:
        return None
    return match.group(1).strip()


def title_from_filename(path: pathlib.Path) -> str:
    return re.sub(r"[-_]+", " ", path.stem).strip().title()


def resolve_title(path: pathlib.Path, raw_text: str) -> str:
    title = extract_yaml_title(raw_text)
    if title:
        return title
    h1 = extract_first_h1(raw_text)
    if h1:
        return h1
    return title_from_filename(path)


def slugify(path: pathlib.Path) -> str:
    slug = SLUG_RE.sub("-", path.stem.lower()).strip("-")
    return slug or "chapter"


def unique_slug(base_slug: str, used: set[str], prefix: str) -> str:
    slug = base_slug
    if slug in used:
        slug = f"{prefix}-{slug}"
    i = 2
    candidate = slug
    while candidate in used:
        candidate = f"{slug}-{i}"
        i += 1
    used.add(candidate)
    return candidate


def build_chapter(title: str, body: str) -> str:
    return (
        "---\n"
        f'title: "{title}"\n'
        "execute:\n"
        "  eval: false\n"
        "---\n\n"
        f"{body}"
    )


def force_eval_false(text: str) -> str:
    # Ensure no chunk explicitly enables eval.
    text = re.sub(r"^#\|\s*eval\s*:\s*.*$", "#| eval: false", text, flags=re.IGNORECASE | re.MULTILINE)
    # Normalize knitr-style eval=TRUE in chunk headers.
    text = re.sub(r"eval\s*=\s*TRUE", "eval=FALSE", text, flags=re.IGNORECASE)
    return text


def render_chapter(raw: str, title: str) -> str:
    body = strip_yaml(raw)
    body = shift_headings(body)
    body = force_eval_false(body)
    return build_chapter(title, body)


def build_quarto_config(root_chapters: list[str], note_chapters: list[str]) -> str:
    lines = [
        "project:",
        "  type: book",
        "  output-dir: _site",
        "",
        "book:",
        '  title: "NGS Sequencing App Ops Booklet"',
        '  subtitle: "Shiny + LDAP + VM rollout"',
        '  author: "Assa Yeroslaviz"',
        f'  site-url: "{SITE_URL}"',
        "  chapters:",
        "    - index.qmd",
    ]

    if root_chapters:
        lines.extend([
            '    - part: "Project QMD Documents"',
            "      chapters:",
        ])
        lines.extend(f"        - chapters/{name}" for name in root_chapters)

    if note_chapters:
        lines.extend([
            '    - part: "Operational Notes"',
            "      chapters:",
        ])
        lines.extend(f"        - chapters/{name}" for name in note_chapters)

    lines.extend([
        "",
        "format:",
        "  html:",
        "    theme: cosmo",
        "    toc: true",
        "    toc-depth: 3",
        "    number-sections: true",
        "    code-fold: true",
        "",
    ])
    return "\n".join(lines)


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    root_qmd, notes_md = discover_sources()
    if not root_qmd and not notes_md:
        raise SystemExit("No source files found (root .qmd or notes/*.md).")

    used_slugs: set[str] = set()
    root_chapter_files: list[str] = []
    note_chapter_files: list[str] = []
    expected_files: set[str] = set()

    for source in root_qmd:
        raw = source.read_text(encoding="utf-8")
        title = resolve_title(source, raw)
        slug = unique_slug(slugify(source), used_slugs, "root")
        destination = OUT_DIR / f"{slug}.qmd"
        destination.write_text(render_chapter(raw, title), encoding="utf-8")
        root_chapter_files.append(destination.name)
        expected_files.add(destination.name)

    for source in notes_md:
        raw = source.read_text(encoding="utf-8")
        title = resolve_title(source, raw)
        slug = unique_slug(slugify(source), used_slugs, "note")
        destination = OUT_DIR / f"{slug}.qmd"
        destination.write_text(render_chapter(raw, title), encoding="utf-8")
        note_chapter_files.append(destination.name)
        expected_files.add(destination.name)

    # Remove stale generated chapter files.
    for chapter_file in OUT_DIR.glob("*.qmd"):
        if chapter_file.name not in expected_files:
            chapter_file.unlink()

    QUARTO_FILE.write_text(build_quarto_config(root_chapter_files, note_chapter_files), encoding="utf-8")


if __name__ == "__main__":
    main()
