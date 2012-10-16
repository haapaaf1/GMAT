#!/bin/bash
# Author: 	Jfisher
# Project:	Gmat
# Title:	configure.sh
# Purpose:	Windows Script to configure and easily build gmat for end users
# Usage: 	-arch [x86 | x64]

# Clear the screen
clear

# Intro Text
echo "........................................................"
echo "Starting GMAT Build"
echo "........................................................"
echo

# Set default variables
arch="x86"

# ***********************************
# Input Args
# ***********************************
if [ "$1" = "-arch" ]
then
	arch=$2
fi

# ***********************************
# Make File Generation and Build
# ***********************************

# Change to msw directory
cd ../build/linux

# Generate unix makefiles
if [ "$arch" = "x86" ]
then
	cmake -G "Unix Makefiles" ../../src/
else
	cmake -G "Unix Makefiles" -D 64_BIT=true ../../src/
fi

# Make Gmat
make

# Change back to build directory
cd ../

echo ""
echo "*************************************"
echo "Gmat Build Finished Succesfully!"
echo "*************************************"
