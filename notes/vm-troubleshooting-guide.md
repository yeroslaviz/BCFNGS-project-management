# VM Troubleshooting Guide (GitHub, LDAP, Apache, Shiny)

## Quick Start (How to run the app)

### Local dev — local mode

```r
Sys.setenv(APP_ENV = "dev", AUTH_MODE = "local")
shiny::runApp("sequencing-app")
```

### Local dev — LDAP simulation

```r
Sys.setenv(
  APP_ENV = "dev",
  AUTH_MODE = "ldap",
  DEV_REMOTE_USER = "your_username",
  LDAP_URI = "ldaps://ldapserv1.biochem.mpg.de:636,ldaps://ldapserv2.biochem.mpg.de:636,ldaps://ldapserv3.biochem.mpg.de:636",
  LDAP_BASE_DN = "dc=biochem,dc=mpg,dc=de"
)
shiny::runApp("sequencing-app")
```

### Local dev — unset

```r
Sys.unsetenv(c(
  "APP_ENV","AUTH_MODE","DEV_REMOTE_USER",
  "LDAP_BIND_DN","LDAP_BIND_PW",
  "LDAP_URI","LDAP_BASE_DN","LDAP_DEBUG"
))
```

### VM — LDAP mode (production)

```bash
sudo systemctl edit shiny-server
```

```ini
[Service]
Environment=AUTH_MODE=ldap
Environment=LDAP_URI=ldaps://ldapserv1.biochem.mpg.de:636,ldaps://ldapserv2.biochem.mpg.de:636,ldaps://ldapserv3.biochem.mpg.de:636
Environment=LDAP_BASE_DN=dc=biochem,dc=mpg,dc=de
```

```bash
sudo systemctl restart shiny-server
```

### VM — local mode (maintenance)

```ini
[Service]
Environment=AUTH_MODE=local
```

```bash
sudo systemctl restart shiny-server
```

### VM — unset (clear overrides)

```bash
sudo systemctl edit shiny-server
# remove AUTH_MODE/LDAP_* lines, save, then:
sudo systemctl restart shiny-server
```


This checklist covers the most common issues we hit and how to fix them quickly.

## AI session restart (quick resume)

If I need to resume work in a new AI session, I point it to:

1. `notes/vm-rollout-summary-2026-02-09.md` for the current state and validation.
2. `notes/ubuntu-bare-server-setup-howto.md` for the full setup flow.

## A) Git / GitHub sync problems

**1) Permission denied (publickey)**

Symptoms:

- `git@github.com: Permission denied (publickey)`
- `fatal: Could not read from remote repository`

Fix:

1. Verify SSH auth:

```
ssh -T git@github.com
```

2. Confirm repo URL:

```
git remote -v
```

3. Ensure the correct SSH key is loaded (or add it to `~/.ssh/config`).

**2) Divergent branches during pull**

Symptoms:

```
fatal: Need to specify how to reconcile divergent branches.
```

Fix:

```
git config pull.rebase false
# or git config pull.rebase true
```

**3) Local changes block pull**

Symptoms:

```
error: Your local changes ... would be overwritten by merge
```

Fix:

```
git stash push -m "vm local changes"
git pull origin main
git stash pop
```

Resolve conflicts if they appear.

**4) Merge conflict in config files**

Fix options:

```
# Keep repo version
git checkout --ours <file>

# Keep local version
git checkout --theirs <file>
```

**5) Untracked files in the VM**

Symptoms:

```
?? sequencing-app/test_detailed.R
```

Fix:

- Ignore, delete, or add to `.gitignore`.

## B) Apache / LDAP authentication issues

**1) Apache won’t restart after LDAP change**

Symptoms:

```
AH00526: Syntax error ... Bad LDAP URL while parsing.
```

Fix:

Use a single, complete LDAP URL:

```
AuthLDAPURL "ldaps://ldapserv1.biochem.mpg.de:636/dc=biochem,dc=mpg,dc=de?uid?sub?(objectClass=posixAccount)"
```

**2) Invalid command 'LDAPTLS_CACERTDIR'**

Cause:

`LDAPTLS_CACERTDIR` is **not** an Apache directive.

Fix:

Remove from Apache config. Put TLS trust in `/etc/ldap/ldap.conf` instead.

**3) ldap_simple_bind() failed: Can't contact LDAP server**

Fix:

1. Check port:

```
nc -vz ldapserv1.biochem.mpg.de 636
```

2. Test TLS bypass:

```
LDAPTLS_REQCERT=never ldapsearch -x -H ldaps://ldapserv1.biochem.mpg.de:636 \
  -b "dc=biochem,dc=mpg,dc=de" "(uid=USER)" mail
```

3. Fix `/etc/ldap/ldap.conf`:

```
TLS_CACERTDIR /etc/ssl/certs
```

**4) LDAPTrustedGlobalCert error**

Symptoms:

```
LDAPTrustedGlobalCert cannot occur within <VirtualHost> section
```

Fix:

Move it to a global Apache config or comment it out (if not needed).

## C) Admin access in LDAP mode

**Problem**

Local login is disabled when `AUTH_MODE=ldap`, so the built‑in `admin` user cannot log in.

**Fix**

Promote the LDAP user in SQLite.

**Option 1: Pre‑create the user as admin (before first login)**

```
sudo -u shiny sqlite3 /srv/shiny-server/sequencing-app/sequencing_projects.db \
"INSERT OR IGNORE INTO users (username, is_admin) VALUES ('soyer', 1);"
```

If the user row does **not** insert, the table likely has `NOT NULL` columns (e.g., `password`, `email`). In that case use:

```
sudo -u shiny sqlite3 /srv/shiny-server/sequencing-app/sequencing_projects.db \
"INSERT OR IGNORE INTO users (username, password, email, is_admin) VALUES ('soyer', 'ldap-only', 'soyer@biochem.mpg.de', 1);"
```

**Option 2: Promote after first login**

```
sudo -u shiny sqlite3 /srv/shiny-server/sequencing-app/sequencing_projects.db \
"UPDATE users SET is_admin=1 WHERE username='soyer';"
```

If the user already exists and you want to update email at the same time, use:

```
sudo -u shiny sqlite3 /srv/shiny-server/sequencing-app/sequencing_projects.db \
"UPDATE users SET is_admin=1, email='soyer@biochem.mpg.de' WHERE username='soyer';"
```

**Verify**

```
sudo -u shiny sqlite3 /srv/shiny-server/sequencing-app/sequencing_projects.db \
"SELECT username, is_admin FROM users WHERE username IN ('yeroslaviz','soyer');"
```

## D) SQLite quick reference (inspect DB structure)

**Show all tables**

```
sudo -u shiny sqlite3 /srv/shiny-server/sequencing-app/sequencing_projects.db ".tables"
```

**Show table schema**

```
sudo -u shiny sqlite3 /srv/shiny-server/sequencing-app/sequencing_projects.db \
"PRAGMA table_info(users);"
```

**Show full CREATE statement**

```
sudo -u shiny sqlite3 /srv/shiny-server/sequencing-app/sequencing_projects.db \
".schema users"
```

**Preview rows**

```
sudo -u shiny sqlite3 /srv/shiny-server/sequencing-app/sequencing_projects.db \
"SELECT username, email, is_admin FROM users LIMIT 20;"
```

## E) Update budget holders (CSV vs DB)

The app reads **budget holders from the SQLite DB**, not directly from the CSV.  
Editing the Excel/CSV file does **not** change the running app unless you rebuild the DB.

**Option A — Update via the app (safe, no rebuild)**  
Use **Administer Projects → Manage Budget Holders**. Changes are written to the DB immediately.

**Option B — Update via SQL (safe, no rebuild)**

```
# Update cost center
sudo -u shiny sqlite3 /srv/shiny-server/sequencing-app/sequencing_projects.db \
"UPDATE budget_holders SET cost_center='P350' WHERE name='Cox' AND surname='Juergen';"

# Add a new PI
sudo -u shiny sqlite3 /srv/shiny-server/sequencing-app/sequencing_projects.db \
"INSERT INTO budget_holders (name, surname, cost_center, email) VALUES ('NewPI','Lastname','P999','newpi@biochem.mpg.de');"
```

**Option C — Rebuild DB from CSV (destructive)**

```
cd /srv/shiny-server/sequencing-app
sudo -u shiny Rscript setup_database.R
```

## F) Switching between LDAP and local auth

**LDAP mode**

```
sudo systemctl edit shiny-server
```

```
[Service]
Environment=AUTH_MODE=ldap
Environment=LDAP_URI=ldaps://ldapserv1.biochem.mpg.de:636,ldaps://ldapserv2.biochem.mpg.de:636,ldaps://ldapserv3.biochem.mpg.de:636
Environment=LDAP_BASE_DN=dc=biochem,dc=mpg,dc=de
```

**Local mode**

```
[Service]
Environment=AUTH_MODE=local
```

Apply changes:

```
sudo systemctl restart shiny-server
sudo systemctl show shiny-server --property=Environment
```

## F) Apache proxy issues

**1) `/sequencing-app/` returns 404**

Symptoms:

- `curl -Ik -u USER https://host/sequencing-app/` returns 404
- `curl -I http://localhost:3838/sequencing-app/` returns 200

Cause:

Conflicting `ProxyPass /` rules or backup config files in `sites-enabled`.

Fix:

1. Ensure only these lines are active:

```
ProxyPass /sequencing-app/ http://localhost:3838/sequencing-app/
ProxyPassReverse /sequencing-app/ http://localhost:3838/sequencing-app/
```

2. Remove or move backup files from `/etc/apache2/sites-enabled`.

3. Restart Apache:

```
sudo apache2ctl -t
sudo systemctl restart apache2
```

**2) Redirect to `/` shows "No UI defined"**

Cause:

Root path is not proxied correctly.

Fix:

Use `/sequencing-app/` as the app path and update redirects accordingly.

## G) Shiny app issues

**1) App works locally but fails via Apache**

Check:

- Apache logs for LDAP errors
- Shiny logs for app startup errors

**2) LDAP mode enabled but no user**

Cause:

Shiny does not receive a user identity.

Fix:

- Use `/sequencing-app/?auth_user=<user>` redirect fallback.
- Confirm redirect works with `curl -Ik -u USER https://host/`.

**3) Missing environment variables in Shiny**

Check:

```
sudo systemctl show shiny-server --property=Environment
```

Restart after changes:

```
sudo systemctl restart shiny-server
```

## H) Deployment issues

**1) Deploy script ran but app unchanged**

Fix:

- Verify `deploy.sh` ran successfully.
- Confirm files in `/srv/shiny-server/sequencing-app/` updated.

**2) Database overwritten**

Fix:

- Ensure `deploy.sh` excludes `sequencing_projects.db`.
- Keep `*.db` ignored in git.

## Quick validation commands

```
# Apache
sudo apache2ctl -t
sudo apache2ctl -S

# LDAP
ldapsearch -x -H ldaps://ldapserv1.biochem.mpg.de:636 \
  -b "dc=biochem,dc=mpg,dc=de" "(uid=USER)" mail

# App (proxy)
curl -Ik -u USER https://ngs-testing-vm.biochem.mpg.de/sequencing-app/

# App (backend)
curl -I http://localhost:3838/sequencing-app/
```

## References

- `notes/ubuntu-bare-server-setup-howto.md`
- `notes/vm-rollout-summary-2026-02-09.md`
