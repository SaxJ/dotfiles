#!/bin/bash
read -r -e -p "Install kde updates?" -N 1 install

if [[ "$install" == [Yy]* ]]; then
    paru -S plasma-meta spectacle kmix kdeconnect ktorrent kmail kde-system-meta ark isoimagewriter kgpg konsole
fi
