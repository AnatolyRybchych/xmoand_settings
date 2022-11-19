#/usr/bin/sh

if [[ $(id -u) -ne 0 ]] ; 
then 
  echo "Please run as root" ; 
  exit 1 ; 
fi

apt install tilda;
apt insall dmenu;
apt install Imagemagick;
apt install nautilus;
apt install fish;
