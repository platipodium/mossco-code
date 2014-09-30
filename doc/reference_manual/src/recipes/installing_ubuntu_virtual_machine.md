# Recipe: installing a virtual machine

This recipe describes all the steps necessary to install a virtual machine that runs MOSSCO.  

## Ubuntu distribution image

We will choose Ubuntu 14.04, known as "Trusty Thar"; it is a fairly recent Linux distribution that requires minimal effort to run MOSSCO.

You can obtain a minimal Ubuntu as an ISO image from <https://help.ubuntu.com/community/Installation/MinimalCD>.  We choose this minimal distribution as we do not require the graphical libraries that come with the general Ubuntu installers.  You may choose any other Ubuntu image, if you like. 

Please select your processor, most likely 64-bit PC, and download the image to your hard disk. I used <http://archive.ubuntu.com/ubuntu/dists/trusty/main/installer-amd64/current/images/netboot/mini.iso> for this recipe.

## Virtual machine

This recipe uses Oracle "Virtual Box" as a virtual machine, which is free software and can be obtained from <https://www.virtualbox.org/wiki/Downloads>.  The installation procedure will be very similar with other virtual machine drivers like VirtualPC.

 

### Setting up a new virtual machine

Open VirtualBox, then select "New".  Choose a name (e.g. ""MOSSCO Thar"), your operating system (Linux) and your distribution (Ubuntu 64-bit). Click continue twice, check the box "create hard drive" and click Create and Continue (any of the choices will do, so stick with the default if there is no good reason to deviate). Choosing 4 Gb should be sufficient for this demonstration. Choose a location for this hard drive image to reside and confirm.

In a next step, the downloaded Ubuntu ISO image will be added to your virtual machine. Select "MOSSCO Thar" in the left column of Virtual Box, press Change and select the Mass Storage pane. Select the plus-sign next to IDE Controller, confirm, and select your "mini.iso" Ubuntu ISO file as a CD. Press okay.

### Installing Ubuntu on the new virtual machine

In Virtual Box, doubleclick your new virtual machine named "MOSSCO Thar".  Select Install and go through the language setup and the keyboard setup. Type "mossco-thar" as your hostname.  Select your download location and wait for basic packages to be downloaded by the setup.

Create a name/user/password of your choice when asked, and confirm the follwoing dialog up to partitioning.  There, choose "Guided setup" and follow the default recommendations.  Press "Save" and wait for the installation of the base system (it will take 10 min to 1 h)

You will be asked to allow automatic updates (select automatic security updates).  At last, you will be asked for the software you want to install.  Check base system.

Say yes to installing the master boot record, to using the system clock.  Finish the installation by pressing continue.  The system will restart from the CD; close the virtual machine window and select "Power-off this virtual machine"

### Starting and setting up your new operating system

In Virtual Box, select "MOSSCO Thar" in the left column and click "Change".  On the "System" pane, uncheck CD and floppy disk from the start order, then click OK, and doubleclick your virtual machine to start it. Enter your login details, and obtain adminstrator right by typing

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

