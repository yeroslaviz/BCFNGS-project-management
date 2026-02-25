#!/usr/bin/env bash
set -euo pipefail

# Cutover ngs-vm main hostname from Perl (:443) to Shiny (:443),
# while keeping Perl reachable on :9443 as fallback.
#
# Usage:
#   ./scripts/cutover_ngs_vm_ports.sh cutover
#   ./scripts/cutover_ngs_vm_ports.sh rollback
#
# Optional overrides:
#   PERL_443_SITE=02-testing.conf
#   SHINY_443_SITE=ngs-vm-shiny-443.conf
#   PERL_9443_SITE=ngs-vm-perl-9443.conf
#   SHINY_8443_SITE=ngs-vm-shiny.conf

MODE="${1:-}"
if [[ "${MODE}" != "cutover" && "${MODE}" != "rollback" ]]; then
  echo "Usage: $0 {cutover|rollback}"
  exit 1
fi

PERL_443_SITE="${PERL_443_SITE:-02-testing.conf}"
SHINY_443_SITE="${SHINY_443_SITE:-ngs-vm-shiny-443.conf}"
PERL_9443_SITE="${PERL_9443_SITE:-ngs-vm-perl-9443.conf}"
SHINY_8443_SITE="${SHINY_8443_SITE:-ngs-vm-shiny.conf}"

enable_modules() {
  sudo a2enmod ssl proxy proxy_http proxy_wstunnel rewrite headers ldap authnz_ldap >/dev/null
}

ensure_listen_9443() {
  if ! grep -q '^Listen 9443$' /etc/apache2/ports.conf; then
    echo 'Listen 9443' | sudo tee -a /etc/apache2/ports.conf >/dev/null
  fi
}

restart_apache() {
  sudo apache2ctl -t
  sudo systemctl restart apache2
}

disable_competing_443_sites() {
  local conf site
  local enabled_sites=(/etc/apache2/sites-enabled/*.conf)

  for conf in "${enabled_sites[@]}"; do
    [[ -f "${conf}" ]] || continue
    if grep -Eq '^[[:space:]]*<VirtualHost[[:space:]]+\*:443>' "${conf}"; then
      site="$(basename "${conf}")"
      if [[ "${site}" != "${SHINY_443_SITE}" ]]; then
        echo "[cutover] disabling competing :443 site: ${site}"
        sudo a2dissite "${site}" >/dev/null || true
      fi
    fi
  done
}

reenable_legacy_443_site() {
  local site
  local candidates=("${PERL_443_SITE}" "02-testing.conf" "default-ssl.conf")

  for site in "${candidates[@]}"; do
    if [[ -f "/etc/apache2/sites-available/${site}" ]]; then
      echo "[rollback] re-enabling legacy :443 site: ${site}"
      sudo a2ensite "${site}" >/dev/null || true
      return
    fi
  done

  echo "[rollback] warning: no legacy :443 site file found in /etc/apache2/sites-available" >&2
}

show_checks() {
  cat <<'EOF'

Validation commands:
  curl -kI https://ngs-vm.biochem.mpg.de/
  curl -kI -u <ldap_user> https://ngs-vm.biochem.mpg.de/sequencing-app/
  curl -kI https://ngs-vm.biochem.mpg.de:9443/
EOF
}

if [[ "${MODE}" == "cutover" ]]; then
  echo "[cutover] enabling modules and 9443 listener"
  enable_modules
  ensure_listen_9443

  echo "[cutover] enabling Shiny on 443: ${SHINY_443_SITE}"
  sudo a2ensite "${SHINY_443_SITE}" >/dev/null
  echo "[cutover] enabling Perl fallback on 9443: ${PERL_9443_SITE}"
  sudo a2ensite "${PERL_9443_SITE}" >/dev/null

  echo "[cutover] disabling configured Perl on 443: ${PERL_443_SITE}"
  sudo a2dissite "${PERL_443_SITE}" >/dev/null || true

  disable_competing_443_sites

  echo "[cutover] keeping 8443 site unchanged: ${SHINY_8443_SITE}"
  restart_apache
  echo "[cutover] done"
  show_checks
  exit 0
fi

reenable_legacy_443_site
echo "[rollback] disabling Shiny on 443: ${SHINY_443_SITE}"
sudo a2dissite "${SHINY_443_SITE}" >/dev/null || true
echo "[rollback] disabling Perl fallback on 9443: ${PERL_9443_SITE}"
sudo a2dissite "${PERL_9443_SITE}" >/dev/null || true

restart_apache
echo "[rollback] done"
show_checks
