#!/bin/bash

set -e

########################################################################################
# This script will do the following steps to ensure the smooth running of the app.
# #
# 1. syncs the app code to the Shiny Server directory using rsync:
#     - Source: /home/yeroslaviz/BCFNGS-project-management/sequencing-app/
#     - Target: /srv/shiny-server/sequencing-app/
#     - Uses --delete to remove files in the target that no longer exist in the source.
#     - Excludes sequencing_projects.db so the production DB is not overwritten.
#
# 2. Fixes ownership of the deployed folder:
#
#     - shiny:shiny on everything under /srv/shiny-server/sequencing-app
#
# 3. Makes the DB writable:
#    - sequencing_projects.db
#
# 4. Restarts Shiny Server
########################################################################################


echo "Deploying Shiny app..."

if sudo rsync -av --delete --exclude 'sequencing_projects.db' \
  /home/yeroslaviz/BCFNGS-project-management/sequencing-app/ \
  /srv/shiny-server/sequencing-app/
then
  sudo chown -R shiny:shiny /srv/shiny-server/sequencing-app
if [ -f /srv/shiny-server/sequencing-app/sequencing_projects.db ]; then
  sudo chmod 666 /srv/shiny-server/sequencing-app/sequencing_projects.db
else
  echo "Note: DB file not found at /srv/shiny-server/sequencing-app/sequencing_projects.db (skipping chmod)."
fi
  sudo systemctl restart shiny-server
  echo "Deployment complete!"
else
  echo "Deploy failed: rsync error. Shiny server was NOT restarted."
  exit 1
fi
