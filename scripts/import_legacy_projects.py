#!/usr/bin/env python3
import argparse
import csv
import os
import sqlite3
from typing import Dict, Optional, Tuple


REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
DEFAULT_DB_PATH = os.path.join(REPO_ROOT, "sequencing-app", "sequencing_projects.db")
DEFAULT_CSV_PATH = os.path.join(REPO_ROOT, "exports", "legacy_projects.csv")


def col_exists(conn: sqlite3.Connection, table: str, column: str) -> bool:
    try:
        rows = conn.execute(f"PRAGMA table_info({table})").fetchall()
        return column in {r[1] for r in rows}
    except sqlite3.Error:
        return False


def ensure_full_name_column(conn: sqlite3.Connection) -> None:
    if not col_exists(conn, "users", "full_name"):
        conn.execute("ALTER TABLE users ADD COLUMN full_name TEXT")


def projects_sql_has_legacy_status(conn: sqlite3.Connection) -> bool:
    row = conn.execute(
        "SELECT sql FROM sqlite_master WHERE type='table' AND name='projects'"
    ).fetchone()
    if not row or not row[0]:
        return False
    return "Legacy project" in row[0]


def rebuild_projects_table_with_legacy_status(conn: sqlite3.Connection) -> None:
    conn.execute("PRAGMA foreign_keys=OFF")
    conn.execute("ALTER TABLE projects RENAME TO projects_old")

    conn.execute(
        """
        CREATE TABLE projects (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          project_id INTEGER UNIQUE,
          project_name TEXT NOT NULL,
          user_id INTEGER NOT NULL,
          responsible_user TEXT NOT NULL,
          reference_genome TEXT NOT NULL,
          service_type_id INTEGER NOT NULL,
          budget_id INTEGER NOT NULL,
          description TEXT,
          num_samples INTEGER,
          sequencing_platform TEXT,
          sequencing_depth_id INTEGER NOT NULL,
          sequencing_cycles_id INTEGER NOT NULL,
          kickoff_meeting INTEGER,
          type_id INTEGER,
          total_cost REAL,
          status TEXT DEFAULT 'Created' CHECK(status IN (
            'Created',
            'Samples received',
            'Library preparation',
            'QC done',
            'Data analysis',
            'Data released',
            'Legacy project'
          )),
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
          updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
          FOREIGN KEY (user_id) REFERENCES users (id),
          FOREIGN KEY (type_id) REFERENCES types (id),
          FOREIGN KEY (service_type_id) REFERENCES service_types (id),
          FOREIGN KEY (budget_id) REFERENCES budget_holders (id),
          FOREIGN KEY (sequencing_depth_id) REFERENCES sequencing_depths (id),
          FOREIGN KEY (sequencing_cycles_id) REFERENCES sequencing_cycles (id)
        )
        """
    )

    cols = [
        "id",
        "project_id",
        "project_name",
        "user_id",
        "responsible_user",
        "reference_genome",
        "service_type_id",
        "budget_id",
        "description",
        "num_samples",
        "sequencing_platform",
        "sequencing_depth_id",
        "sequencing_cycles_id",
        "kickoff_meeting",
        "type_id",
        "total_cost",
        "status",
        "created_at",
        "updated_at",
    ]
    col_list = ", ".join(cols)
    conn.execute(
        f"INSERT INTO projects ({col_list}) SELECT {col_list} FROM projects_old"
    )
    conn.execute("DROP TABLE projects_old")

    conn.execute("DROP TRIGGER IF EXISTS auto_project_id")
    conn.execute(
        """
        CREATE TRIGGER IF NOT EXISTS auto_project_id
        AFTER INSERT ON projects
        FOR EACH ROW
        WHEN NEW.project_id IS NULL
        BEGIN
          UPDATE projects
          SET project_id = (SELECT COALESCE(MAX(project_id), 0) + 1 FROM projects)
          WHERE id = NEW.id;
        END;
        """
    )
    conn.execute("PRAGMA foreign_keys=ON")


def ensure_sequencing_depth(conn: sqlite3.Connection) -> int:
    row = conn.execute(
        "SELECT id FROM sequencing_depths WHERE depth_description = ?",
        ("Legacy (unspecified)",),
    ).fetchone()
    if row:
        return row[0]
    cur = conn.execute(
        """
        INSERT INTO sequencing_depths (depth_description, cost_upto_150_cycles, cost_upto_300_cycles)
        VALUES (?, ?, ?)
        """,
        ("Legacy (unspecified)", 0, 0),
    )
    return cur.lastrowid


def get_or_create_simple(
    conn: sqlite3.Connection, table: str, column: str, value: str
) -> int:
    row = conn.execute(
        f"SELECT id FROM {table} WHERE {column} = ?",
        (value,),
    ).fetchone()
    if row:
        return row[0]
    cur = conn.execute(
        f"INSERT INTO {table} ({column}) VALUES (?)",
        (value,),
    )
    return cur.lastrowid


def get_or_create_service_type(
    conn: sqlite3.Connection, service_type: str
) -> int:
    row = conn.execute(
        "SELECT id FROM service_types WHERE service_type = ?",
        (service_type,),
    ).fetchone()
    if row:
        return row[0]
    cur = conn.execute(
        """
        INSERT INTO service_types (service_type, kit, costs_per_sample)
        VALUES (?, ?, ?)
        """,
        (service_type, "legacy", 0),
    )
    return cur.lastrowid


def find_or_create_budget_holder(
    conn: sqlite3.Connection,
    group_name: str,
    budget_group: str,
    missing_groups: set,
) -> int:
    group_name = (group_name or "").strip()
    budget_group = (budget_group or "").strip()

    if budget_group:
        row = conn.execute(
            "SELECT id FROM budget_holders WHERE cost_center = ?",
            (budget_group,),
        ).fetchone()
        if row:
            return row[0]

    if group_name:
        row = conn.execute(
            "SELECT id FROM budget_holders WHERE surname = ? OR name = ?",
            (group_name, group_name),
        ).fetchone()
        if row:
            return row[0]

    if group_name:
        missing_groups.add(group_name)
    placeholder_name = group_name or "Legacy"
    placeholder_cc = budget_group or "Legacy"
    cur = conn.execute(
        """
        INSERT INTO budget_holders (name, surname, cost_center, email)
        VALUES (?, ?, ?, ?)
        """,
        (placeholder_name, "Legacy", placeholder_cc, "ngs@biochem.mpg.de"),
    )
    return cur.lastrowid


def ensure_user(
    conn: sqlite3.Connection,
    username: str,
    full_name: str,
    email: str,
    research_group: str,
) -> int:
    row = conn.execute(
        "SELECT id, full_name, email FROM users WHERE username = ?",
        (username,),
    ).fetchone()
    if row:
        user_id, existing_full_name, existing_email = row
        updates = []
        params = []
        if col_exists(conn, "users", "full_name") and (not existing_full_name):
            updates.append("full_name = ?")
            params.append(full_name)
        if existing_email in (None, "") and email:
            updates.append("email = ?")
            params.append(email)
        if updates:
            params.append(username)
            conn.execute(
                f"UPDATE users SET {', '.join(updates)} WHERE username = ?",
                params,
            )
        return user_id

    password_placeholder = "ldap-only"
    if col_exists(conn, "users", "full_name"):
        cur = conn.execute(
            """
            INSERT INTO users (username, full_name, password, email, research_group, is_admin)
            VALUES (?, ?, ?, ?, ?, 0)
            """,
            (username, full_name, password_placeholder, email, research_group),
        )
    else:
        cur = conn.execute(
            """
            INSERT INTO users (username, password, email, research_group, is_admin)
            VALUES (?, ?, ?, ?, 0)
            """,
            (username, password_placeholder, email, research_group),
        )
    return cur.lastrowid


def normalize_reference_genome(value: str) -> str:
    v = (value or "").strip()
    if v == "" or v.upper() == "NA":
        return "NA (legacy)"
    return v


def normalize_text(value: Optional[str]) -> str:
    return (value or "").strip()


def main() -> int:
    parser = argparse.ArgumentParser(description="Import legacy projects into the Shiny SQLite DB.")
    parser.add_argument("--db", default=DEFAULT_DB_PATH, help="Path to sequencing_projects.db")
    parser.add_argument("--csv", default=DEFAULT_CSV_PATH, help="Path to legacy_projects.csv")
    args = parser.parse_args()

    if not os.path.exists(args.csv):
        print(f"[ERROR] CSV not found: {args.csv}")
        return 1

    if not os.path.exists(args.db):
        print(f"[ERROR] DB not found: {args.db}")
        return 1

    conn = sqlite3.connect(args.db)
    conn.row_factory = sqlite3.Row

    ensure_full_name_column(conn)
    if not projects_sql_has_legacy_status(conn):
        rebuild_projects_table_with_legacy_status(conn)

    legacy_depth_id = ensure_sequencing_depth(conn)

    missing_budget_groups = set()
    inserted = 0
    skipped = 0

    with open(args.csv, newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            legacy_id = normalize_text(row.get("legacy_id"))
            if not legacy_id.isdigit():
                continue
            legacy_id_int = int(legacy_id)

            exists = conn.execute(
                "SELECT 1 FROM projects WHERE project_id = ?",
                (legacy_id_int,),
            ).fetchone()
            if exists:
                skipped += 1
                continue

            project_name = normalize_text(row.get("project_name"))
            username = normalize_text(row.get("login"))
            full_name = normalize_text(row.get("user_full_name")) or username
            group_name = normalize_text(row.get("group_name"))
            reference_genome = normalize_reference_genome(row.get("reference_genome"))
            type_label = normalize_text(row.get("type_label")) or "Legacy"
            sample_type = normalize_text(row.get("sample_type")) or type_label
            sequencing_length = normalize_text(row.get("sequencing_length"))
            budget_group = normalize_text(row.get("budget_group"))
            sequencing_platform = normalize_text(row.get("sequencing_platform"))
            note = normalize_text(row.get("note"))

            if not project_name or not username:
                continue

            user_email = f"{username}@biochem.mpg.de"
            user_id = ensure_user(conn, username, full_name, user_email, group_name)
            budget_id = find_or_create_budget_holder(conn, group_name, budget_group, missing_budget_groups)
            type_id = get_or_create_simple(conn, "types", "name", type_label)
            service_type_id = get_or_create_service_type(conn, sample_type)
            cycles_label = sequencing_length if sequencing_length else "Legacy"
            cycles_id = get_or_create_simple(conn, "sequencing_cycles", "cycles_description", cycles_label)

            conn.execute(
                """
                INSERT INTO projects (
                  project_id, project_name, user_id, responsible_user, reference_genome,
                  service_type_id, budget_id, description, sequencing_platform,
                  sequencing_depth_id, sequencing_cycles_id, type_id, status
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    legacy_id_int,
                    project_name,
                    user_id,
                    full_name,
                    reference_genome,
                    service_type_id,
                    budget_id,
                    note if note else None,
                    sequencing_platform if sequencing_platform else None,
                    legacy_depth_id,
                    cycles_id,
                    type_id,
                    "Legacy project",
                ),
            )
            inserted += 1

    conn.commit()
    conn.close()

    print(f"[OK] Inserted {inserted} legacy projects.")
    print(f"[OK] Skipped {skipped} (already present).")
    if missing_budget_groups:
        print("[WARN] Missing budget holder groups (placeholders created):")
        for name in sorted(missing_budget_groups):
            print(f"  - {name}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
