import os, sys
import subprocess
import re

def replace_fabm_symbols_objcopy(MOSSCO_INSTALL_PREFIX, OBJC):

    cmd = 'nm libmossco.a'
    symbols = subprocess.check_output([cmd], shell=True).decode("utf-8")

    symbols = re.split("\s+", symbols)
    symbols = [s for s in symbols if 'mossco_strings' in s]
    symbols = [s for s in symbols if 'mossco_stringcopy' in s]
    symbols = re.split('mossco_strings',symbols[0])
    prefix = symbols[0]
    symbols = re.split('mossco_stringcopy',symbols[1])
    infix = symbols[0]
    postfix = symbols[1]

    cmd = 'nm libmossco.a'
    symbols = subprocess.check_output([cmd], shell=True).decode("utf-8")
    symbols = re.split("\s+", symbols)
    cmd = 'nm libmossco_fabm.a'
    items = subprocess.check_output([cmd], shell=True).decode("utf-8")
    items = re.split("\s+", items)
    symbols.extend(items)

    items = ['fabm','fabm_types','fabm_properties','fabm_config','fabm_driver',
             'fabm_standard_variables','fabm_expressions', 'fabm_builtin_models', 'fabm_particle','fabm_coupling','fabm_library']

    itemsymbols=[]
    for item in items:
        search = prefix + item + infix
        itemsymbols.extend([s for s in symbols if s.startswith(search)])

    itemsymbols = set(itemsymbols)

    with open('objcopy_replace_symbols.tsv','w') as f:
        for item in list(itemsymbols):
            replace = prefix + 'mossco_' + item[len(prefix):]
            f.write(' '.join([item,replace,'\n']))

    for lib in ['libmossco.a', 'libmossco_fabm.a']:

        library = os.path.join(MOSSCO_INSTALL_PREFIX,'lib',lib)
        cmd = OBJC + ' --redefine-syms=objcopy_replace_symbols.tsv ' + library
        subprocess.call([cmd], shell=True)

def replace_fabm_symbols_objconv(MOSSCO_INSTALL_PREFIX, OBJC):

    cmd=OBJC + ' -ds ' + os.path.join(MOSSCO_INSTALL_PREFIX,'lib','libmossco.a')
    symbols=subprocess.check_output([cmd], shell=True).decode("utf-8")

    symbols = re.split("\s+", symbols)
    symbols = [s for s in symbols if 'mossco_strings' in s]
    symbols = [s for s in symbols if 'mossco_stringcopy' in s]
    symbols = re.split('mossco_strings',symbols[0])
    prefix = symbols[0]
    symbols = re.split('mossco_stringcopy',symbols[1])
    infix = symbols[0]
    postfix = symbols[1]

    items = ['fabm','fabm_types','fabm_properties','fabm_config','fabm_driver',
             'fabm_standard_variables','fabm_expressions', 'fabm_builtin_models', 'fabm_particle','fabm_coupling','fabm_library']

    for lib in ['libmossco.a', 'libmossco_fabm.a']:

        library = os.path.join(MOSSCO_INSTALL_PREFIX,'lib',lib)
        cmd = OBJC
        for item in items:
            cmd += ' -np:' + prefix + item + infix + ':' + prefix + 'mossco_' + item + infix

        cmd += ' ' + library
        cmd += ' ' + library + '.tmp'

        subprocess.call([cmd], shell=True)
        subprocess.call(['mv ' + library + '.tmp ' + library], shell=True)

if __name__ == '__main__':

    if (len(sys.argv) > 1):
        MOSSCO_INSTALL_PREFIX=sys.argv[1]
    else:
        MOSSCO_INSTALL_PREFIX=os.environ['MOSSCO_DIR']

    OBJCs = ['objcopy','objconv', 'gobjcopy']

    if (len(sys.argv) > 2):
        OBJC=sys.argv[2]
        rc=subprocess.call(['which',OBJC],stderr=None)

        if rc != 0:
            print('Could not find requested ' + OBJC + ' utility')
            sys.exit(1)

    else:
        for OBJC in OBJCs:
            rc=subprocess.call(['which',OBJC],stderr=None)
            if rc == 0: break

        if rc != 0:
            print('Could not find required objconv/objcopy utility')
            sys.exit(1)

    if 'objcopy' in OBJC:
        replace_fabm_symbols_objcopy(MOSSCO_INSTALL_PREFIX, OBJC)
    else:
        replace_fabm_symbols_objconv(MOSSCO_INSTALL_PREFIX, OBJC)

    sys.exit(0)
