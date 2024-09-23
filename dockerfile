apt-get update
apt-get upgrade
apt-get install -y curl git locales libpng-dev libssl-dev libcurl4-openssl-dev xdg-utils libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
locale-gen en_US.UTF-8

curl -L https://rig.r-pkg.org/deb/rig.gpg -o /etc/apt/trusted.gpg.d/rig.gpg
sh -c 'echo "deb http://rig.r-pkg.org/deb rig main" > /etc/apt/sources.list.d/rig.list'
apt-get update
apt-get install -y r-rig
rig add 4.4.1
rig system add-pak 4.4.1

Rscript -e 'install.packages(c("languageserver", "httpgd", "httpuv", "renv", "tidyverse", "shiny", "reactable"))'
