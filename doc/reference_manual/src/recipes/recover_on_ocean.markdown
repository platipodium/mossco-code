# Recipe #15: Recover on ocean

After a crash of the filesystems on ocean, it may be necessary to Reinstall
everything from scratch (if you're a group administrator)

 - libyaml, PyYaml, cmake, ESMF

If you can rely on your adminstrator to recover the above, you only have to
delete and reinstall MOSSCO from the repository; then add the updated `PYTHONPATH` and
`ESMFMKFILE` variables to your system, and expand your `PATH`


## Reinstall mossco

    rm -rf $MOSSCO_DIR
    git clone ssh://<username>@git.code.sf.net/p/mossco/code
    (cd $MOSSCO_DIR; make external; make)

Be careful if you have not saved your setups!

    rm -rf $MOSSCO_SETUPDIR
    git clone ssh://<username>@git.code.sf.net/p/mossco/setups
    (cd $MOSSCO_SETUPDIR; make external)

## Reinstall PyYAML

    mkdir -p ${HOME}/temp; cd temp

    wget http://pyyaml.org/download/libyaml/yaml-0.1.5.tar.gz.
    tar xzf yaml-0.1.5.tar.gz; cd yaml-0.1.5
    ./configure --prefix=${HOME}/opt  # you can choose a different path here
    make && make install

    wget http://pyyaml.org/download/pyyaml/PyYAML-3.11.tar.gz
    tar xzf PyYAML-3.11.tar.gz
    cd PyYAML-3.11

Manipulate `setup.cfg`  and set the `STATIC` option as well as the Library
and include paths, e.g.,  `include_dirs=${HOME}/opt/include` and
`library_dirs=${HOME}/opt/lib`

    python setup.py  --with-libyaml install --prefix=$HOME/opt
    export PYTHONPATH=$HOME/opt/lib64/python2.6/site-packages  # adjust to your system

## Reinstall esmf

    rm -rf $ESMF_DIR
    git clone git://esmf.git.sourceforge.net/gitroot/esmf/esmf $ESMF_DIR
    $MOSSCO_DIR/scripts/installation/install_esmf_versions.sh

## Reinstall cmake

    mkdir -p ${HOME}/temp; cd temp
    wget --no-check-certificate https://cmake.org/files/v3.4/cmake-3.4.1.tar.gz
    tar xzf cmake-3.4.1.tar.gz ; cd cmake-3.4.1
    ./bootstrap ; ./configure --prefix=$HOME/opt

    export PATH=${HOME}/opt/bin:${PATH}
