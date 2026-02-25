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

  echo "[cutover] disabling Perl on 443: ${PERL_443_SITE}"
  sudo a2dissite "${PERL_443_SITE}" >/dev/null || true

  echo "[cutover] keeping 8443 site unchanged: ${SHINY_8443_SITE}"
  restart_apache
  echo "[cutover] done"
  show_checks
  exit 0
fi

echo "[rollback] re-enabling Perl on 443: ${PERL_443_SITE}"
sudo a2ensite "${PERL_443_SITE}" >/dev/null
echo "[rollback] disabling Shiny on 443: ${SHINY_443_SITE}"
sudo a2dissite "${SHINY_443_SITE}" >/dev/null || true
echo "[rollback] disabling Perl fallback on 9443: ${PERL_9443_SITE}"
sudo a2dissite "${PERL_9443_SITE}" >/dev/null || true

restart_apache
echo "[rollback] done"
show_checks
