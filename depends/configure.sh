# Author: 	Jfisher
# Project:	Gmat
# Title:	configure.sh
# Purpose:	This script allows developers to quickly and easily 
#		configure the gmat development environment on linux.

# Clear the screen
clear

# Set default variables
gmat_path="/media/jfish-store/gmat-buildbranch"
use_latest=false

# ***********************************
# Input System
# ***********************************
function set_input {
	if [ $1=="-p" ]
	then
		#gmat_path=$2
		echo $2
	fi

	if [ $3=="-l" ] 
	then
		#use_latest=$4
		echo $4
	fi
}

# ***********************************
# Download Library Dependencies
# ***********************************
function download_depends {

	# Set Variables
	bin_path=$gmat_path/depends/bin
	f2c_path=$gmat_path/depends/f2c
	cspice_path=$gmat_path/depends/cspice
	wxWidgets_path=$gmat_path/depends/wxWidgets
	sofa_path=$gmat_path/depends/sofa
	tsplot_path=$gmat_path/depends/tsPlot
	pcre_path=$gmat_path/depends/pcre

	# Display the variables
	echo $bin_path
	echo $f2c_path
	echo $cspice_path
	echo $wxWidgets_path
	echo $sofa_path
	echo $tsplot_path

	# Create directories and download f2c if it does not already exist.
	if [ ! -f $f2c_path ]
	then
		# Create Directories
		mkdir $f2c_path
	
		# Change to f2c directory
		cd $f2c_path
	
		# Use wget to download software
		wget -nH --cut-dirs=1 -r ftp://netlib.org/f2c/
	fi

	# Create directories and download cspice if it does not already exist.
	if [ ! -f $cspice_path ]
	then
		# Create Directories
		mkdir $cspice_path
	
		# Change to cspice directory
		cd $cspice_path
	
		# Download and extract Spice (32 and 64), finally remove archive
		wget ftp://naif.jpl.nasa.gov/pub/naif/toolkit//C/PC_Linux_GCC_32bit/packages/cspice.tar.Z
		gzip -d cspice.tar.Z
		tar xfv cspice.tar
		mv cspice cspice32
		rm cspice.tar
	
		wget ftp://naif.jpl.nasa.gov/pub/naif/toolkit//C/PC_Linux_GCC_64bit/packages/cspice.tar.Z
		gzip -d cspice.tar.Z
		tar xfv cspice.tar
		mv cspice cspice64
		rm cspice.tar
	fi	

	# Create directories and download wxwidgets if it does not already exist.
	if [ ! -f $wxWidgets_path ]
	then
		# Create Directories
		mkdir $wxWidgets_path
	
		# Change to wxwidgets directory
		cd $wxWidgets_path
	
		# Checkout wxWidget source.
		svn co http://svn.wxwidgets.org/svn/wx/wxWidgets/tags/WX_2_8_12/ wxWidgets-2.8.12
	
		# Copy modified wxWidget setup.h file to downloaded source path (Preconfigured to use
		# ODBC and GLCanvas
		cp $bin_path/wx/setup.h $wxWidgets_path/wxWidgets-2.8.12/include/wx/msw/setup.h
		cp $bin_path/wx/setup.h $wxWidgets_path/wxWidgets-2.8.12/include/wx/msw/setup0.h
	
		if [ $use_latest == true ]
		then
			# Change to wxwidgets directory
			cd $wxWidgets_path
	
			# Checkout latest wxWidget source.
			svn co http://svn.wxwidgets.org/svn/wx/wxWidgets/trunk/ wxWidgets-latest
		
			# Copy modified wxWidget setup.h file to downloaded source path (Preconfigured to use
			# ODBC and GLCanvas
			cp $bin_path/wx/setup.h $wxWidgets_path/wxWidgets-latest/include/wx/msw/setup0.h
			cp $bin_path/wx/setup.h $wxWidgets_path/wxWidgets-latest/include/wx/msw/setup0.h
		
		fi
	fi

	# Create directories and download sofa if it does not already exist.
	if [ ! -f $sofa_path ]
	then
		# Change to depends directory
		cd $gmat_path/depends/

		# Download and extract Sofa Source, finally remove archive
		wget http://www.iausofa.org/2012_0301_C/sofa_c_a.zip
		unzip sofa_c_a.zip
		rm sofa_c_a.zip	
	fi

	# Create directories and download tsplot if it does not already exist.
	if [ ! -f $tsplot_path ]
	then
		# Create Directories
		mkdir $tsplot_path
	
		# Change to tsplot directory
		cd $tsplot_path
	
		# Checkout tsplot source.
		svn co svn://svn.code.sf.net/p/tsplot/code/trunk tsplot-latest
	fi

	# Create directories and download pcre if it does not already exist.
	if [ ! -f $pcre_path ]
	then
		# Create Directories
		mkdir $pcre_path
	
		# Change to pcre directory
		cd $pcre_path
	
		# Checkout pcre source.
		svn co svn://vcs.exim.org/pcre/code/tags/pcre-8.31 pcre-8.31
	
		if [ $use_latest == true ]
		then
		
			# Change to pcre directory
			cd $pcre_path
	
			# Checkout latest pcre source.
			svn co svn://vcs.exim.org/pcre/code/trunk pcre-latest
		fi
	fi
}

function build_wxWidgets {
	# Set build path based on version
	if [ $use_latest == true ]
	then
		wx_build_path=$gmat_path/depends/wxWidgets/latest
	else
		wx_build_path=$gmat_path/depends/wxWidgets/wxWidgets-2.8.12
	fi

	# Check if dependencies have already been built
	depend_x86_path=$gmat_path/depends/wxWidgets/wxWidgets-2.8.12/lib/libwx_gtk2_core-2.8.so
	depend_amd64_path=$gmat_path/depends/wxWidgets/wxWidgets-2.8.12/lib/libwx_gtk2_core-2.8.so

	cd $wx_build_path

	if [ ! -f $depend_x86_path ]
	then
		lib_x86_folder=$gmat_path/depends/wxWidgets/wxWidgets-2.8.12/lib/
		output_x86_folder=$gmat_path/application/bin/linux/x86/
		
		#Clean
		make distclean		
		
		# Configure 32bit wxWidget build
		./configure --with-opengl

		# Compile 32bit wxWidget build
		make 
	
		# Change to contrib directory
		cd contrib
	
		# Compile 32 bit wxWidget contrib
		make

		# Clean output path
		rm -r $output_x86_folder/*
	
		# Copy wxWidget dll's to gmat application/bin/arch directory
		cp $lib_x86_folder/*.so $output_x86_folder
	fi
}

# Run Script Functions
set_input
download_depends
build_wxWidgets

# ***********************************
# End of script
# ***********************************
echo $gmat_path
echo Dependency Configuration Complete!
exit 1
