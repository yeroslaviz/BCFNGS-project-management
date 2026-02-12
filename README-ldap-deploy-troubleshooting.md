# LDAP Deploy Smoke Tests and Troubleshooting

This document covers:

1. How deploy + smoke tests work
2. Expected outputs
3. What to do for common failures (`too many redirects`, `disconnected`, LDAP auth fallback)

## 1) Deploy with built-in smoke tests

Run on VM:

```bash
cd ~/BCFNGS-project-management
./deploy.sh
```

The script now:

1. Deploys app files to `/srv/shiny-server/sequencing-app/`
2. Restarts `shiny-server`
3. Runs LDAP smoke tests:
   - `/sequencing-app/` returns `302` to canonical `?auth_user=<logged-user>`
   - tampered `?auth_user=other` is rewritten to `?auth_user=<logged-user>`
   - final app load returns `200`

It exits with non-zero status and prints remediation hints if any check fails.

Note:

- `deploy.sh` syncs app code only. If you changed Apache config (`01-main.conf`), apply/restart Apache separately, then run the curl checks in section 4.

## 2) Non-interactive usage (optional)

If you do not want password prompts:

```bash
LDAP_TEST_USER="yeroslaviz-test" LDAP_TEST_PASSWORD="YOUR_PASSWORD" ./deploy.sh
```

Skip smoke tests only when needed:

```bash
SKIP_SMOKE_TEST=1 ./deploy.sh
```

Override target base URL:

```bash
PUBLIC_BASE_URL="https://ngs-testing-vm.biochem.mpg.de" ./deploy.sh
```

## 3) Baseline environment checks

### Shiny environment must include:

```bash
sudo systemctl show shiny-server --property=Environment
```

Required values:

- `AUTH_MODE=ldap`
- `TRUST_PROXY_AUTH_USER_QUERY=1`

If missing, update:

```bash
sudo systemctl edit shiny-server
```

Then restart:

```bash
sudo systemctl daemon-reload
sudo systemctl restart shiny-server
```

## 4) Manual curl validation (reference)

These should match the smoke test behavior.

```bash
curl -ki -u yeroslaviz-test "https://ngs-testing-vm.biochem.mpg.de/sequencing-app/" | sed -n '1,20p'
```

Expected:

- `HTTP/1.1 302 Found`
- `Location: .../sequencing-app/?auth_user=yeroslaviz-test`

```bash
curl -ki -u yeroslaviz-test "https://ngs-testing-vm.biochem.mpg.de/sequencing-app/?auth_user=yeroslaviz" | sed -n '1,20p'
```

Expected:

- `HTTP/1.1 302 Found`
- `Location: .../sequencing-app/?auth_user=yeroslaviz-test`

```bash
curl -k -L -u yeroslaviz-test -o /dev/null -s -w "%{http_code}\n" "https://ngs-testing-vm.biochem.mpg.de/sequencing-app/"
```

Expected:

- `200`

## 5) Troubleshooting by symptom

### A) `ERR_TOO_MANY_REDIRECTS`

Likely cause:

- Apache rewrite rule loop in `/etc/apache2/sites-enabled/01-main.conf`.

Fix:

```bash
sudo cp ~/BCFNGS-project-management/01-main.conf /etc/apache2/sites-available/01-main.conf
sudo ln -sf /etc/apache2/sites-available/01-main.conf /etc/apache2/sites-enabled/01-main.conf
sudo apache2ctl configtest
sudo systemctl restart apache2
sudo systemctl restart shiny-server
```

Then re-run curl checks from section 4.

### B) App shows local login page + `LDAP mode enabled but no authenticated user found`

Likely cause:

- Shiny is in LDAP mode, but proxy trust/query fallback settings are missing.

Checks:

```bash
sudo systemctl show shiny-server --property=Environment
```

Required:

- `AUTH_MODE=ldap`
- `TRUST_PROXY_AUTH_USER_QUERY=1`

### C) `Disconnected from the server. Reload`

Likely causes:

- Shiny worker restarted/terminated
- Apache websocket/proxy path mismatch

Checks:

```bash
sudo journalctl -u shiny-server -n 200 --no-pager
sudo tail -n 200 /var/log/shiny-server/$(sudo ls -t /var/log/shiny-server | head -n 1)
sudo tail -n 200 /var/log/apache2/sequencing-app-ssl-error.log
```

Also ensure Apache modules are enabled:

```bash
sudo apache2ctl -M | egrep 'headers_module|rewrite_module|proxy_module|proxy_http_module|proxy_wstunnel_module|ldap_module|authnz_ldap_module'
```

### D) User can impersonate another user via URL query

Expected fixed behavior:

- tampered `auth_user` should always be rewritten to the authenticated LDAP username.

If not fixed, re-apply `01-main.conf` from repo and restart Apache + Shiny (section 5A).

## 6) Certificate warning note

If Apache restart shows:

- `AH01909: server certificate does NOT include an ID which matches the server name`

This is a certificate/SAN mismatch warning for host naming. It is separate from LDAP auth logic and redirect rewrite behavior.
