# -*- mode: python -*-
a = Analysis(['main.pyw'],
             pathex=['/home/ak2stud/Nick/python_scripts/dev/nc_info_gui/wrk/exe'],
             hiddenimports=['/usr/lib64/python2.7/site-packages/netCDF4_utils.py',
                            '/usr/lib64/python2.7/site-packages/netcdftime.py'],
             hookspath=['.'],
             runtime_hooks=None)
pyz = PYZ(a.pure)
exe = EXE(pyz,
          a.scripts,
          a.binaries,
          a.zipfiles,
          a.datas,
          name='main',
          debug=False,
          strip=None,
          upx=True,
          console=True )
