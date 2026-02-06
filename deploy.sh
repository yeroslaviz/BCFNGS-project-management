#!/bin/bash
echo "Deploying Shiny app..."
sudo rsync -av --delete --exclude 'sequencing_projects.db' /home/yeroslaviz/BCFNGS-project-management/sequencing-app/ /srv/shiny-server/sequencing-app/
sudo chown -R shiny:shiny /srv/shiny-server/sequencing-app
sudo chmod 666 /srv/shiny-server/sequencing-app/sequencing_projects.db
sudo systemctl restart shiny-server
echo "Deployment complete!"
