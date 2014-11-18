# Recipe: installing a virtual machine

This recipe describes all the steps necessary to install a virtual machine that runs MOSSCO.  

## OpenSuSE distribution image

We will choose OpenSuSE 13.1; it is a fairly recent Linux distribution that is widely used.

You can obtain a small ISO image from <http://software.opensuse.org/131/en,>.  We choose this network distribution as we do not require all the graphical libraries that come with the general OpenSuSE installers.  You may choose any other disk image, if you like. 

So select Network, Direct link, and then your processor,  most likely 64-bit PC, and download the image to your hard disk. 



## Virtual machine

This recipe uses Oracle "Virtual Box" as a virtual machine, which is free software and can be obtained from <https://www.virtualbox.org/wiki/Downloads>.  The installation procedure will be very similar with other virtual machine drivers like VirtualPC.

 

### Setting up a new virtual machine


Open VirtualBox, then select "New".  Choose a name (e.g. ""MOSSCO Suse"), your operating system (Linux) and your distribution (OpenSuSE 64-bit). Click continue twice, check the box "create hard drive" and click Create and Continue (any of the choices will do, so stick with the default if there is no good reason to deviate). Choosing 4 Gb should be sufficient for this demonstration. Choose a location for this hard drive image to reside and confirm.

In a next step, the downloaded OpenSuSE ISO image will be added to your virtual machine. Select "MOSSCO Suse" in the left column of Virtual Box, press Change and select the Mass Storage pane. Select the plus-sign next to IDE Controller, confirm, and select your  ISO file as a CD. Press okay.

### Installing OpenSuSE on the new virtual machine

Select Installation, wait, 
Select Keyboard and language, Press Next, Select New installation, selct your time zone.  For the graphical Desktop, choose "Other" and then "Minimal Server (Text mode)".  


In Virtual Box, doubleclick your new virtual machine named "MOSSCO Suse".  Select Installation and go through the language setup, select New Installation, select the time zone. For the graphical desktop,  do not choose the default, but choose "Other" and then "Minimal Server (Text mode)". Confirm the default partitioning, and provide your user details on the next screen.  

Wait for the downloads to be finished (plan 30 mins) and press install. 

### Starting and setting up your new operating system

In Virtual Box, select "MOSSCO Suse" in the left column and click "Change".  On the "System" pane, uncheck CD and floppy disk from the start order, then click OK, and doubleclick your virtual machine to start it. 

@todo from here:

Enter your login details, and obtain adminstrator right by typing

	sudo su
	
Enter your password again.  We will use the tools `apt-cache` and `apt-get` to search and install software, you might also use the graphical `aptitude` interface.

	apt-get install git

	apt-get install gfortran libmpich-dev libxerces-c2-dev 
	
	apt-get install make cmake
	
	apt-get install libnetcdff5 libnetcdf-dev netcdf-bin
	
	apt-get install python-yaml
	
### Install ESMF

Create a directory esmf-code, and check out the ESMF software

    git clone git://git.code.sf.net/p/esmf/esmf esmf-code
	export ESMF_DIR=$HOME/esmf-code	
	cd $ESMF_DIR
	
	
Set some environment variables for ESMF

	export ESMF_COMM=mpich2
	export ESMF_NETCDF=split	
	export ESMF_PIO=internal
	
	make
	
Make the location of your `esmf.mk` file known to your system
	
	export ESMFMKFILE=$ESMF_DIR/lib/libO/Linux.gfortran.64.default/esmf.mk
	
## Obtaining and compiling MOSSCO

Follow the instructions on <http://www.sf.net/p/mossco> for downloading and installing MOSSCO

