#!/bin/bash
read -r -e -p "Install kde updates?" -N 1 install

if [[ "$install" == [Yy]* ]]; then
    paru -S --needed plasma-meta spectacle kmix kdeconnect ktorrent kmail kde-system-meta ark isoimagewriter kgpg konsole kwayland-integration plasma5-integration oxygen5 packagekit-qt6 gocryptfs flatpak-kcm geoip2-database kdegraphics-thumbnailers ffmpegthumbs unarchiver unrar
fi
