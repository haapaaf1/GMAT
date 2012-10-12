GMAT Dependency Configuration README
---

This readme file provides instructions for configuring the gmat library dependencies.


AUTOMATIC CONFIGURATION

The configuration script "configure.bat" for windows and "configure.sh" for Linux/Mac will take care of all dependency downloads and dependency configurations. In order to use these scripts you must have the proper prerequisite software installed.

The required software is as follows.

All OS's:

	Wget
	Subversion

Linux:

	gnome-core-devel 
	build-essential 
	libgtk2.0-dev 
	libgtk2.0-doc 
	devhelp 
	libgl1-mesa-dev 
	libglu1-mesa-dev 
	freeglut3 
	freeglut3-dev 


Windows:

	7Zip
	Visual Studio or Windows SDK


In order to run either script you should first open up a terminal window and navigate to the /gmat/depends/ folder. The configuration.sh and configuration.bat usage is as follows.

Linux:

	sudo configure.sh –p [ /path/to/gmat/folder/ ] –latest

Sudo is necessary because of the symlink modifications to the opengl libraries and the install location of wxWidget dependency. If you do not wish to use sudo than unfortunatlly you will need to manually configure and build the wxWidget Dependency.


Windows:

	configure.bat –p [ /path/to/gmat/folder/ ] 
		      –wget [ /path/to/wget/install/ ] 
		      –svn [/path/to/svn/install/ ] 
		      –sevenz [ /path/to/7zip/install/ ] 
		      –vsversion [vsversion ie. 9 | 10 | 11 ] 
		      –vspath [ /path/to/visual/studio/or/windows/sdk/ ] 
		      –latest [ true | false ]
	

If you do not wish to enter the arguments on the command line simply modify the "Default Variables" section of the configure script and run the script without them.

Please Note: You must be connected to the internet in for for these scripts to properly work.


MANUAL CONFIGURATION

For manual configuration please consult the build system documentation listed under Addtional Support.


ADDITIONAL SUPPORT

For additional help configuring the GMAT Dependencies, please visit the Build System documenation on the GMAT Wiki.

	GMAT Wiki Pages:  http://gmat.ed-pages.com/wiki/tiki-index.php