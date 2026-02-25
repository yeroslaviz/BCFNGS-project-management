#!/usr/bin/env bash
set -euo pipefail

# Legacy switch helper (hostname-based strategy).
# This script keeps the older flow (perl hostname -> shiny hostname redirect).
# Current production cutover for ngs-vm uses:
#   scripts/cutover_ngs_vm_ports.sh
# with Shiny on :443 and Perl fallback on :9443.

PERL_SITE="${PERL_SITE:-ngs-vm.conf}"
SHINY_SITE="${SHINY_SITE:-ngs-vm-shiny.conf}"
SHINY_HOSTNAME="${SHINY_HOSTNAME:-ngs-vm-shiny.biochem.mpg.de}"
REDIRECT_FROM="${REDIRECT_FROM:-ngs-vm.biochem.mpg.de}"

echo "Disabling perl vhost: ${PERL_SITE}"
sudo a2dissite "${PERL_SITE}" || true

echo "Enabling shiny vhost: ${SHINY_SITE}"
sudo a2ensite "${SHINY_SITE}"

echo "Reloading Apache"
sudo systemctl reload apache2

cat <<EOF
DONE. Shiny should now be served from:
  https://${SHINY_HOSTNAME}

If you want the old hostname to redirect, add a 301 redirect block
inside the perl vhost file (or a dedicated redirect vhost), e.g.:

  Redirect permanent / https://${SHINY_HOSTNAME}/

Then reload Apache again.
EOF
