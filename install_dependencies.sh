#!/bin/bash

if [[ $(id -u) -ne 0 ]] ; 
then 
  echo "sudo is required"; 
  exit 1; 
fi

apt install gnome-terminal;
apt install dmenu;
apt install imagemagick;
apt install nautilus;
apt install fish;

chsh -s /usr/bin/fish

