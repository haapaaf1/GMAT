# !/bin/sh
# This file is a shell script that you can use to launch gmat from a Gnome
# desktop.  Copy it into the directory containing gmat, edit it as indicated 
# below, and then create a launched for it on your desktop.

# Edit this path to specify where the GMAT executable, gmat, resides:
cd /home/djc/TS_Code/GMAT_PropDesign/bin

# Set the library path for your shared libraries (wx and the GMAT libraries), 
# and start GMAT running:
LD_LIBRARY_PATH=.:./lib:/usr/local/lib:$LD_LIBRARY_PATH ./gmat
