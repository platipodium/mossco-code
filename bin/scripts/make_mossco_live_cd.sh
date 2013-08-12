#!/bin/bash
# 
# This script follows the instructions from https://help.ubuntu.com/community/LiveCDCustomizationFromScratch
# to create a Ubuntu Remix Live CD tailored for MOSSCO
# 
# This script can only be run within a Debian-based (e.g. Ubuntu) Linux system, you need to have sudo or root rights

# Set architecture to create the live-cd for (choose i386 or x86_64)
ARCH=i386

# Set Ubuntu distribution to create the live-cd with (choose raring, quantal or any other of those)
RELEASE=quantal

# Set the working directory
CHROOT=chroot-$RELEASE-$ARCH

# 1. Install the required host system packages

sudo echo "Creating live cd for $RELEASE-$ARCH ..." 

if ! [ -x /usr/sbin/debootstrap ]; then
  for P in debootstrap syslinux squashfs-tools genisoimage ; do
    echo "Installing $P ..."
    sudo apt-get install $P
    if [ $? == 100 ]; then
      echo "Your system does not have the required $P package. Exit" 
    exit
    fi 
  done
fi

if ! [ -d $CHROOT/etc ]; then
  sudo debootstrap --arch=$ARCH $RELEASE $CHROOT 
fi

if ! [ -d $CHROOT/etc ]; then
  echo "Could not install a bootstrap version of Ubuntu on $CHROOT. Exit"
  exit
fi
 
echo "Completed host side preparations." 
    
# 2. Mount dev and get internet in chroot

sudo mount | grep $CHROOT/dev || sudo mount --bind /dev/$CHROOT/dev
sudo cp /etc/hosts $CHROOT/etc/hosts
sudo cp /etc/resolv.conf $CHROOT/etc/resolv.conf
sudo cp /etc/apt/sources.list $CHROOT/etc/apt/sources.list
sudo cp /sbin/initctl /sbin/initctl.bak

# 3. Enter the chroot, make system mounts and initctl diversion

sudo chroot chroot
mount none -t proc /proc
mount none -t sysfs /sys
mount none -t devpts /dev/pts
export HOME=/root
export LC_ALL=C
apt-get update
apt-get install --yes dbus
dbus-uuidgen > /var/lib/dbus/machine-id
dpkg-divert --local --rename --add /sbin/initctl

# 4. Upgrade your system and install live system requirements
apt-get --yes upgrade
apt-get install --yes ubuntu-standard casper lupin-casper
apt-get install --yes discover laptop-detect os-prober
apt-get install --yes linux-generic 

# 5. The filesystem could be some 650 MB large already by now ....
# Then install the desktop to make this installable, add another 700 MB
apt-get install --yes ubiquity-frontend-gtk

# 6. Configure here the user packages to install
apt-get install --yes gfortran-4.7 gcc-4.7 gcc gcc-multilib
for P in cpp gcc gcc-ar gcc-nm gcc-ranlib gcov gfortran; do
  ln -s /usr/bin/$P-4.7 /usr/bin/$P
done
ln -s /usr/bin/gcc-4.7 /usr/bin/g++
ln -s /usr/bin/gcc-4.7 /usr/bin/c++

apt-get install --yes mpich2
apt-get install --yes git
#apt-get install --yes doxygen  # costs another 1 GB
apt-get install --yes libnetcdf-dev netcdf-bin

# 7. Add MOSSCO software
mkdir -p /opt/src
cd /opt/src
export MOSSCODIR=/opt/src/mossco
export FABMDIR=/opt/src/fabm
export ESMFDIR=/opt/src/esmf
git clone git://git.code.sf.net/p/mossco/code mossco
git clone git://git.code.sf.net/p/fabm/code fabm
git clone git://esmf.git.sourceforge.net/gitroot/esmf/esmf esmf
cd $FABMDIR && git checkout mossco
cd $ESMFDIR && git checkout ESMF_6_2_0:0

cat <<EOT > $HOME/.fabm
export FABMDIR=$FABMDIR
export FABM_F2003=true
export FORTRAN_COMPILER=GFORTRAN
EOT

echo "Your FABM configuration resides in $HOME/.fabm, please source it before using FABM".

cat <<EOT > $HOME/.esmf
export ESMF_DIR=$ESMFDIR
export ESMFMKFILE=$ESMF_DIR/lib/libg/Linux.gfortran.64.mpich2.default/esmf.mk
export ESMF_BOPT=g
export ESMF_OPTLEVEL=2
export ESMF_COMM=mpich2
export ESMF_INSTALL_PREFIX=/opt
export ESMF_NETCDF=standard
EOT

echo "Your ESMF configuration resides in $HOME/.esmf, please source it before using ESMF".

cd $ESMFDIR
make


# 8. Cleanup
make -C $FABMDIR/src distclean
make -C $FAMBDIR distclean
make -C $MOSSCODIR distclean

rm /var/lib/dbus/machine-id
rm /sbin/initctl
dpkg-divert --rename --remove /sbin/initctl

ls /boot/vmlinuz-3.5.**-**-generic > list.txt
export sum=$(cat list.txt | grep '[^ ]' | wc -l)
if [ $sum -gt 1 ]; then
dpkg -l 'linux-*' | sed '/^ii/!d;/'"$(uname -r | sed "s/\(.*\)-\([^0-9]\+\)/\1/")"'/d;s/^[^ ]* [^ ]* \([^ ]*\).*/\1/;/[0-9]/!d' | xargs sudo apt-get -y purge
fi
rm list.txt

apt-get clean

rm -rf /tmp/*

rm /etc/resolv.conf

umount -lf /proc
umount -lf /sys
umount -lf /dev/pts
exit

# 9. Clean up on the host
sudo mount | grep $CHROOT/dev && sudo umount $CHROOT/dev

# 10. Create cd image directory
mkdir -p image/{casper,isolinux,install}
cp $CHROOT/boot/vmlinuz-3.5.**-**-generic image/casper/vmlinuz
cp $CHROOT/boot/initrd.img-3.5.**-**-generic image/casper/initrd.lz
cp /usr/lib/syslinux/isolinux.bin image/isolinux/
cp /boot/memtest86+.bin image/install/memtest

cat <<EOT > image/isolinux/isolinux.txt
************************************************************************

This is an Ubuntu Remix Live CD.

For the default live system, enter "live".  To run memtest86+, enter "memtest"

************************************************************************
EOT

cat <<EOT > image/isolinux/isolinux.cfg
DEFAULT live
LABEL live
  menu label ^Start or install Ubuntu Remix
  kernel /casper/vmlinuz
  append  file=/cdrom/preseed/ubuntu.seed boot=casper initrd=/casper/initrd.lz quiet splash --
LABEL check
  menu label ^Check CD for defects
  kernel /casper/vmlinuz
  append  boot=casper integrity-check initrd=/casper/initrd.lz quiet splash --
LABEL memtest
  menu label ^Memory test
  kernel /install/memtest
  append -
LABEL hd
  menu label ^Boot from first hard disk
  localboot 0x80
  append -
DISPLAY isolinux.txt
TIMEOUT 300
PROMPT 1 
EOT

sudo chroot $CHROOT dpkg-query -W --showformat='${Package} ${Version}\n' | sudo tee image/casper/filesystem.manifest
sudo cp -v image/casper/filesystem.manifest image/casper/filesystem.manifest-desktop
REMOVE='ubiquity ubiquity-frontend-gtk ubiquity-frontend-kde casper lupin-casper live-initramfs user-setup discover1 xresprobe os-prober libdebian-installer4'
for i in $REMOVE ; do
  sudo sed -i "/${i}/d" image/casper/filesystem.manifest-desktop
done

sudo mksquashfs $CHROOT image/casper/filesystem.squashfs 
printf $(sudo du -sx --block-size=1 chroot | cut -f1) > image/casper/filesystem.size
sudo mksquashfs $CHROOT image/casper/filesystem.squashfs -e boot

echo <<EOT > image/README.diskdefines
#define DISKNAME  Ubuntu Remix
#define TYPE  binary
#define TYPEbinary  1
#define ARCH  i386
#define ARCHi386  1
#define DISKNUM  1
#define DISKNUM1  1
#define TOTALNUM  0
#define TOTALNUM0  1
EOT

touch image/ubuntu

mkdir image/.disk
cd image/.disk
touch base_installable
echo "full_cd/single" > cd_type
echo "Ubuntu Remix" > info
echo "http//your-release-notes-url.com" > release_notes_url
cd ../..

sudo -s
(cd image && find . -type f -print0 | xargs -0 md5sum | grep -v "\./md5sum.txt" > md5sum.txt)
exit

cd image
sudo mkisofs -r -V "$IMAGE_NAME" -cache-inodes -J -l -b isolinux/isolinux.bin -c isolinux/boot.cat -no-emul-boot -boot-load-size 4 -boot-info-table -o ../ubuntu-remix.iso .
cd ..
 



