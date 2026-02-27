#!/usr/bin/env python3
"""
Generate a QMD report that maps Shiny action buttons to observeEvent handlers
and highlights the main function calls/side effects in each handler.
"""

from __future__ import annotations

import argparse
import datetime as dt
import re
from pathlib import Path


ACTION_BUTTON_RE = re.compile(r'actionButton\(\s*"([A-Za-z0-9_]+)"\s*(?:,\s*"([^"]*)")?')
OBSERVE_EVENT_RE = re.compile(r'observeEvent\(\s*input\$([A-Za-z0-9_]+)\s*,')
FUNCTION_DEF_RE = re.compile(r"^\s*([A-Za-z][A-Za-z0-9_.]*)\s*<-\s*function\s*\(")
FUNCTION_CALL_RE = re.compile(r"\b([A-Za-z][A-Za-z0-9_.]*)\s*\(")


KEYWORDS = {
    "if",
    "for",
    "while",
    "repeat",
    "function",
    "switch",
    "return",
    "tryCatch",
    "local",
    "list",
    "c",
    "NULL",
    "TRUE",
    "FALSE",
    "paste",
    "paste0",
    "sprintf",
    "nrow",
    "ncol",
    "as.numeric",
    "as.character",
    "as.logical",
    "trimws",
    "which",
    "unique",
    "sort",
    "setNames",
    "class",
    "style",
    "data.frame",
    "reactive",
    "reactiveVal",
    "observeEvent",
    "observe",
    "req",
    "is.null",
    "is.na",
    "length",
}


IMPORTANT_CALLS = {
    "dbExecute",
    "dbGetQuery",
    "dbDisconnect",
    "showModal",
    "removeModal",
    "showNotification",
    "send_project_creation_email",
    "send_project_cost_notification_email",
    "send_data_released_email",
    "load_projects",
    "load_admin_data",
    "validate_and_repair_database",
}

CORE_WORKFLOW_IDS = {
    "new_project_btn",
    "create_project_btn",
    "edit_project_btn",
    "update_project_btn",
    "send_notification_btn",
    "delete_project_btn",
    "confirm_delete_btn",
    "update_status_btn",
    "confirm_status_update_btn",
    "manage_announcements_btn",
}


def find_block_end(lines: list[str], start_idx: int) -> int:
    """Find matching closing brace for an observeEvent block."""
    depth = 0
    started = False
    for idx in range(start_idx, len(lines)):
        line = lines[idx]
        for ch in line:
            if ch == "{":
                depth += 1
                started = True
            elif ch == "}":
                depth -= 1
        if started and depth == 0:
            return idx
    return len(lines) - 1


def ordered_unique(items: list[str]) -> list[str]:
    seen = set()
    out = []
    for item in items:
        if item not in seen:
            seen.add(item)
            out.append(item)
    return out


def safe_mermaid_id(raw: str) -> str:
    return re.sub(r"[^A-Za-z0-9_]", "_", raw)


def collect_action_buttons(lines: list[str]) -> dict[str, dict[str, str | int]]:
    result = {}
    for i, line in enumerate(lines, start=1):
        m = ACTION_BUTTON_RE.search(line)
        if not m:
            continue
        btn_id = m.group(1)
        label = (m.group(2) or "").strip()
        if btn_id not in result:
            result[btn_id] = {"line": i, "label": label}
    return result


def collect_custom_functions(lines: list[str]) -> set[str]:
    fns = set()
    for line in lines:
        m = FUNCTION_DEF_RE.search(line)
        if m:
            fns.add(m.group(1))
    return fns


def side_effects_from_calls(calls: list[str]) -> list[str]:
    effects = []
    callset = set(calls)
    if "dbExecute" in callset:
        effects.append("Writes DB")
    if "dbGetQuery" in callset:
        effects.append("Reads DB")
    if (
        "send_project_creation_email" in callset
        or "send_project_cost_notification_email" in callset
        or "send_data_released_email" in callset
    ):
        effects.append("Sends email")
    if "showModal" in callset:
        effects.append("Opens modal")
    if "removeModal" in callset:
        effects.append("Closes modal")
    if "showNotification" in callset:
        effects.append("Shows notification")
    if "load_projects" in callset or "load_admin_data" in callset:
        effects.append("Refreshes UI data")
    return effects


def extract_other_inputs(block: str, trigger_input: str) -> list[str]:
    ids = ordered_unique(re.findall(r"\binput\$([A-Za-z0-9_]+)\b", block))
    return [x for x in ids if x != trigger_input]


def extract_sql_targets(block: str) -> list[str]:
    targets: list[str] = []
    patterns = [
        (r"\bSELECT\b.*?\bFROM\s+([A-Za-z_][A-Za-z0-9_.]*)", "SELECT"),
        (r"\bINSERT\s+INTO\s+([A-Za-z_][A-Za-z0-9_.]*)", "INSERT"),
        (r"\bUPDATE\s+([A-Za-z_][A-Za-z0-9_.]*)", "UPDATE"),
        (r"\bDELETE\s+FROM\s+([A-Za-z_][A-Za-z0-9_.]*)", "DELETE"),
    ]
    for pattern, op in patterns:
        for m in re.finditer(pattern, block, flags=re.IGNORECASE | re.DOTALL):
            table = m.group(1).strip()
            targets.append(f"{op} {table}")
    return ordered_unique(targets)


def extract_notification_messages(block: str) -> list[str]:
    msgs = []
    for m in re.finditer(r"showNotification\(\s*([\"'])(.*?)\1", block, flags=re.DOTALL):
        msg = re.sub(r"\s+", " ", m.group(2)).strip()
        if msg:
            msgs.append(msg)
    # Keep short; just the first 2 unique messages.
    return ordered_unique(msgs)[:2]


def infer_purpose(input_id: str, label: str, calls: list[str], effects: list[str]) -> str:
    # Specific first
    if input_id == "new_project_btn":
        return "Open the Create Project modal and prefill lookup choices."
    if input_id == "create_project_btn":
        return "Validate form inputs, insert a new project, and send initial project email."
    if input_id == "edit_project_btn":
        return "Open the Edit Project modal for the selected project."
    if input_id == "update_project_btn":
        return "Save edits to selected project (including additional cost for admins)."
    if input_id == "send_notification_btn":
        return "Send admin-triggered cost notification email for the selected project."
    if input_id == "update_status_btn":
        return "Open status update modal for selected project."
    if input_id == "confirm_status_update_btn":
        return "Persist new project status and trigger release email when applicable."
    if input_id == "delete_project_btn":
        return "Open delete confirmation for selected project."
    if input_id == "confirm_delete_btn":
        return "Delete selected project from the database."
    if input_id == "manage_announcements_btn":
        return "Open Landing Text admin panel."
    if input_id == "validate_db_btn":
        return "Run database validation checks and report issues."
    if input_id == "restore_db_btn":
        return "Start database restore flow from uploaded backup."
    if input_id == "confirm_restore_btn":
        return "Execute restore after confirmation and reopen app state."
    if input_id == "login_btn":
        return "Authenticate local login and initialize session state."
    if input_id == "logout_btn":
        return "End local session or show LDAP end-session guidance."
    if input_id == "register_btn":
        return "Validate registration fields and create a new user."

    # Pattern-based
    if input_id.startswith("manage_"):
        return "Open admin management modal for this reference table."
    if input_id.startswith("add_"):
        return "Validate new entry fields and insert a new row."
    if input_id.startswith("edit_") and input_id.endswith("_btn"):
        return "Open edit modal for currently selected row."
    if input_id.startswith("update_") and input_id.endswith("_btn"):
        return "Apply update from modal form to the selected row."
    if input_id.startswith("delete_") and input_id.endswith("_btn"):
        return "Open delete confirmation dialog for selected row."
    if input_id.startswith("confirm_delete_") and input_id.endswith("_btn"):
        return "Delete selected row after confirmation."
    if "send" in input_id and "notification" in input_id:
        return "Send notification to configured recipients."

    # Fallback based on effects
    if "Writes DB" in effects:
        return "Processes input and writes changes to the database."
    if "Opens modal" in effects:
        return "Opens an interaction modal for user/admin action."
    return "Handle this UI event and update app state."


def collect_handlers(
    lines: list[str],
    action_buttons: dict[str, dict[str, str | int]],
    custom_functions: set[str],
) -> list[dict]:
    handlers = []
    for i, line in enumerate(lines, start=1):
        m = OBSERVE_EVENT_RE.search(line)
        if not m:
            continue
        input_id = m.group(1)
        start_idx = i - 1
        end_idx = find_block_end(lines, start_idx)
        block = "\n".join(lines[start_idx : end_idx + 1])

        raw_calls = FUNCTION_CALL_RE.findall(block)
        calls = [c for c in raw_calls if c not in KEYWORDS]
        calls = ordered_unique(calls)

        custom_calls = [c for c in calls if c in custom_functions and c != "observeEvent"]
        important_calls = [c for c in calls if c in IMPORTANT_CALLS]
        key_calls = ordered_unique(custom_calls + important_calls)[:10]

        effects = side_effects_from_calls(calls)
        other_inputs = extract_other_inputs(block, input_id)
        sql_targets = extract_sql_targets(block)
        notif_messages = extract_notification_messages(block)

        button_meta = action_buttons.get(input_id, {})
        purpose = infer_purpose(
            input_id=input_id,
            label=str(button_meta.get("label", "")),
            calls=key_calls,
            effects=effects,
        )
        handlers.append(
            {
                "input_id": input_id,
                "button_line": button_meta.get("line", "-"),
                "button_label": button_meta.get("label", ""),
                "handler_line": i,
                "handler_end_line": end_idx + 1,
                "key_calls": key_calls,
                "side_effects": effects,
                "other_inputs": other_inputs,
                "sql_targets": sql_targets,
                "notification_messages": notif_messages,
                "purpose": purpose,
            }
        )
    return handlers


def build_mermaid(handlers: list[dict]) -> str:
    # Keep graph readable: focus on core project workflow buttons + notification flow.
    selected = [h for h in handlers if h["input_id"] in CORE_WORKFLOW_IDS]

    lines = ["graph LR"]
    lines.append('  classDef btn fill:#e8f0fe,stroke:#356ac3,color:#1b3f7f,stroke-width:1px;')
    lines.append('  classDef evt fill:#f0f7e8,stroke:#5b8f29,color:#2f5c12,stroke-width:1px;')
    lines.append('  classDef fn fill:#fff3e6,stroke:#cc7a00,color:#7a4a00,stroke-width:1px;')
    lines.append('  classDef fx fill:#f5e9ff,stroke:#7b49b0,color:#4d2b73,stroke-width:1px;')

    effect_nodes = {}
    for h in selected:
        bid = safe_mermaid_id(f"btn_{h['input_id']}")
        hid = safe_mermaid_id(f"evt_{h['input_id']}_{h['handler_line']}")
        blabel = h["input_id"]
        hlabel = f"observeEvent @ {h['handler_line']}"
        lines.append(f'  {bid}["{blabel}"] --> {hid}["{hlabel}"]')
        lines.append(f"  class {bid} btn;")
        lines.append(f"  class {hid} evt;")

        for call in h["key_calls"][:4]:
            fid = safe_mermaid_id(f"fn_{h['input_id']}_{call}")
            lines.append(f'  {hid} --> {fid}["{call}()"]')
            lines.append(f"  class {fid} fn;")

        for effect in h["side_effects"]:
            if effect not in effect_nodes:
                effect_nodes[effect] = safe_mermaid_id(f"fx_{effect}")
                lines.append(f'  {effect_nodes[effect]}["{effect}"]')
                lines.append(f"  class {effect_nodes[effect]} fx;")
            lines.append(f"  {hid} --> {effect_nodes[effect]}")

    if not selected:
        lines.append('  A["No core handlers found"]')
    return "\n".join(lines)


def render_qmd(
    app_path: Path,
    handlers: list[dict],
    action_buttons: dict[str, dict[str, str | int]],
    out_path: Path,
) -> None:
    now = dt.datetime.now().strftime("%Y-%m-%d %H:%M")
    mapped_buttons = {h["input_id"] for h in handlers if h["button_line"] != "-"}
    unmapped_buttons = sorted(set(action_buttons) - mapped_buttons)
    mermaid = build_mermaid(handlers)
    summary_handlers = sorted(handlers, key=lambda x: (x["handler_line"], x["input_id"]))
    core_summary = [h for h in summary_handlers if h["input_id"] in CORE_WORKFLOW_IDS]

    lines = []
    lines.append("---")
    lines.append('title: "Button to Function Map (app.R)"')
    lines.append("execute:")
    lines.append("  echo: false")
    lines.append("  warning: false")
    lines.append("format:")
    lines.append("  html:")
    lines.append("    mermaid-format: svg")
    lines.append("---")
    lines.append("")
    lines.append("# Overview")
    lines.append("")
    lines.append(f"- Source file: `{app_path}`")
    lines.append(f"- Generated: `{now}`")
    lines.append(f"- Total `actionButton()` controls detected: **{len(action_buttons)}**")
    lines.append(f"- Total `observeEvent(input$...)` handlers detected: **{len(handlers)}**")
    lines.append(f"- Buttons with direct handler mapping: **{len(mapped_buttons)}**")
    lines.append("")
    lines.append("`Main Impacts` means what the handler changes, for example: reads/writes DB, sends email, opens/closes modal, shows notifications, refreshes UI data.")
    lines.append("")
    lines.append("## Regenerate")
    lines.append("")
    lines.append("```bash")
    lines.append(
        "python3 /Users/yeroslaviz/Documents/Github/BCFNGS-project-management/scripts/generate_button_function_map.py"
    )
    lines.append("```")
    lines.append("")
    lines.append("## Explorer (Split View)")
    lines.append("")
    lines.append("```{=html}")
    lines.append("<style>")
    lines.append("main.content { max-width: 98vw !important; width: 98vw !important; }")
    lines.append("#quarto-document-content { max-width: none !important; width: 100% !important; }")
    lines.append(".page-columns { grid-template-columns: minmax(0, 1fr) !important; }")
    lines.append(".diagram-toolbar { display:flex; align-items:center; gap:8px; margin: 0 0 8px 0; }")
    lines.append(".diagram-toolbar button { border:1px solid #c6d0db; background:#f8fafc; padding:4px 10px; border-radius:6px; cursor:pointer; }")
    lines.append(".diagram-toolbar button:hover { background:#eef3f8; }")
    lines.append(".diagram-zoom-level { min-width:60px; text-align:center; font-weight:600; color:#2f3b4a; }")
    lines.append("#diagram-pane { border:1px solid #d8dee6; border-radius:8px; padding:8px; overflow:auto; max-height:80vh; background:#ffffff; }")
    lines.append("#diagram-pane svg { display:block; width:auto !important; max-width:none !important; height:auto !important; }")
    lines.append(".summary-table-wrap { max-height:80vh; overflow:auto; border:1px solid #d8dee6; border-radius:8px; padding:8px; background:#ffffff; }")
    lines.append("</style>")
    lines.append("<script>")
    lines.append("document.addEventListener('DOMContentLoaded', function () {")
    lines.append("  const pane = document.getElementById('diagram-pane');")
    lines.append("  if (!pane) return;")
    lines.append("  const svg = pane.querySelector('svg');")
    lines.append("  const zoomLabel = document.getElementById('diagram-zoom-level');")
    lines.append("  const zoomInBtn = document.getElementById('diagram-zoom-in');")
    lines.append("  const zoomOutBtn = document.getElementById('diagram-zoom-out');")
    lines.append("  const zoomResetBtn = document.getElementById('diagram-zoom-reset');")
    lines.append("  if (!svg) { if (zoomLabel) zoomLabel.textContent = 'n/a'; return; }")
    lines.append("  let zoom = 1.0;")
    lines.append("  const minZoom = 0.30;")
    lines.append("  const maxZoom = 3.00;")
    lines.append("  const step = 0.10;")
    lines.append("  function applyZoom() {")
    lines.append("    svg.style.transformOrigin = 'top left';")
    lines.append("    svg.style.transform = `scale(${zoom})`;")
    lines.append("    if (zoomLabel) zoomLabel.textContent = `${Math.round(zoom * 100)}%`;")
    lines.append("  }")
    lines.append("  if (zoomInBtn) zoomInBtn.addEventListener('click', function () { zoom = Math.min(maxZoom, +(zoom + step).toFixed(2)); applyZoom(); });")
    lines.append("  if (zoomOutBtn) zoomOutBtn.addEventListener('click', function () { zoom = Math.max(minZoom, +(zoom - step).toFixed(2)); applyZoom(); });")
    lines.append("  if (zoomResetBtn) zoomResetBtn.addEventListener('click', function () { zoom = 1.0; applyZoom(); });")
    lines.append("  applyZoom();")
    lines.append("});")
    lines.append("</script>")
    lines.append("```")
    lines.append("")
    lines.append(":::: {.columns}")
    lines.append("::: {.column width=\"40%\"}")
    lines.append("### Diagram")
    lines.append("")
    lines.append("```{=html}")
    lines.append("<div class=\"diagram-toolbar\">")
    lines.append("  <button id=\"diagram-zoom-out\" type=\"button\">-</button>")
    lines.append("  <span id=\"diagram-zoom-level\" class=\"diagram-zoom-level\">100%</span>")
    lines.append("  <button id=\"diagram-zoom-in\" type=\"button\">+</button>")
    lines.append("  <button id=\"diagram-zoom-reset\" type=\"button\">Reset</button>")
    lines.append("</div>")
    lines.append("<div id=\"diagram-pane\">")
    lines.append("```")
    lines.append("")
    lines.append("```{mermaid}")
    lines.append("%%| fig-width: 22")
    lines.append("%%| fig-height: 28")
    lines.append(mermaid)
    lines.append("```")
    lines.append("")
    lines.append("```{=html}")
    lines.append("</div>")
    lines.append("```")
    lines.append(":::")
    lines.append("::: {.column width=\"60%\"}")
    lines.append("### Handler Summary")
    lines.append("")
    lines.append("```{=html}")
    lines.append("<div class=\"summary-table-wrap\">")
    lines.append("```")
    lines.append("")
    lines.append("| Button ID | Purpose | Uses Inputs | Main Impacts |")
    lines.append("|---|---|---|---|")
    for h in summary_handlers:
        inputs = ", ".join(f"`{x}`" for x in h["other_inputs"][:5]) if h["other_inputs"] else "-"
        impacts = ", ".join(h["side_effects"]) if h["side_effects"] else "-"
        lines.append(f"| `{h['input_id']}` | {h['purpose']} | {inputs} | {impacts} |")
    lines.append("")
    lines.append("```{=html}")
    lines.append("</div>")
    lines.append("```")
    lines.append("")
    lines.append("### Core Buttons")
    lines.append("")
    lines.append("| Button ID | Label | Responsibility | Data/Inputs used | Key calls | DB targets |")
    lines.append("|---|---|---|---|---|---|")
    for h in core_summary:
        key_calls = ", ".join(f"`{c}()`" for c in h["key_calls"][:4]) if h["key_calls"] else "-"
        btn_label = h["button_label"] if h["button_label"] else "-"
        inputs = ", ".join(f"`{x}`" for x in h["other_inputs"][:6]) if h["other_inputs"] else "-"
        sql = ", ".join(f"`{x}`" for x in h["sql_targets"][:4]) if h["sql_targets"] else "-"
        lines.append(
            f"| `{h['input_id']}` | {btn_label} | {h['purpose']} | {inputs} | {key_calls} | {sql} |"
        )

    lines.append("")
    lines.append("### Notification Text (Detected)")
    lines.append("")
    lines.append("| Button ID | Notification message sample(s) |")
    lines.append("|---|---|")
    with_messages = [h for h in summary_handlers if h["notification_messages"]]
    if with_messages:
        for h in with_messages:
            msgs = "<br>".join(h["notification_messages"])
            lines.append(f"| `{h['input_id']}` | {msgs} |")
    else:
        lines.append("| - | No direct literal `showNotification()` text detected in handlers. |")
    lines.append(":::")
    lines.append("::::")
    lines.append("")
    lines.append("## Full Mapping Table")
    lines.append("")
    lines.append(
        "| Button ID | Label | Purpose | Button line | Handler line | Handler block | Uses inputs | Key function calls | Impacts | SQL targets |"
    )
    lines.append("|---|---|---|---:|---:|---|---|---|---|---|")
    for h in summary_handlers:
        key_calls = ", ".join(f"`{c}()`" for c in h["key_calls"]) if h["key_calls"] else "-"
        sidefx = ", ".join(h["side_effects"]) if h["side_effects"] else "-"
        inputs = ", ".join(f"`{x}`" for x in h["other_inputs"]) if h["other_inputs"] else "-"
        sql = ", ".join(f"`{x}`" for x in h["sql_targets"]) if h["sql_targets"] else "-"
        block = f"`{h['handler_line']}-{h['handler_end_line']}`"
        label = h["button_label"] if h["button_label"] else "-"
        lines.append(
            f"| `{h['input_id']}` | {label} | {h['purpose']} | {h['button_line']} | {h['handler_line']} | {block} | {inputs} | {key_calls} | {sidefx} | {sql} |"
        )
    lines.append("")
    lines.append("## Buttons Without Direct observeEvent(input$...) Handler")
    lines.append("")
    if unmapped_buttons:
        for btn in unmapped_buttons:
            meta = action_buttons[btn]
            label = meta.get("label") or "-"
            lines.append(f"- `{btn}` (line {meta['line']}, label: `{label}`)")
    else:
        lines.append("- None")

    out_path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--app",
        default="/Users/yeroslaviz/Documents/Github/BCFNGS-project-management/sequencing-app/app.R",
        help="Path to app.R",
    )
    parser.add_argument(
        "--out",
        default="/Users/yeroslaviz/Documents/Github/BCFNGS-project-management/Documentation/app-button-function-map.qmd",
        help="Output QMD file path",
    )
    args = parser.parse_args()

    app_path = Path(args.app)
    out_path = Path(args.out)
    text = app_path.read_text(encoding="utf-8")
    lines = text.splitlines()

    action_buttons = collect_action_buttons(lines)
    custom_functions = collect_custom_functions(lines)
    handlers = collect_handlers(lines, action_buttons, custom_functions)
    render_qmd(app_path, handlers, action_buttons, out_path)

    print(f"[OK] Wrote {out_path}")
    print(f"[INFO] actionButton count: {len(action_buttons)}")
    print(f"[INFO] observeEvent(input$...) count: {len(handlers)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
