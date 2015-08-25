from PyQt4 import QtCore
import logging
import sys


class EmittingStream(QtCore.QObject):
    ''' Solution 1
    # see 
    # http://stackoverflow.com/questions/8356336/how-to-capture-output-of-pythons-interpreter-and-show-in-a-text-widget
    '''
    textWritten = QtCore.pyqtSignal(str)

    def write(self, text):
        self.textWritten.emit(str(text))






logger = logging.getLogger(__name__)

class XStream(QtCore.QObject):
    ''' Solution 2
    # see 
    # http://stackoverflow.com/questions/11465971/redirecting-output-in-pyqt
    '''
    _stdout = None
    _stderr = None

    messageWritten = QtCore.pyqtSignal(str)

    def flush( self ):
        pass

    def fileno( self ):
        return -1

    def write( self, msg ):
        if ( not self.signalsBlocked() ):
            #_msg = '<font color=red><br>{0}<br></font>'.format(msg)
            _msg = '<font color=red>{0}</font>'.format(msg)
            self.messageWritten.emit(unicode(_msg))

    @staticmethod
    def stdout():
        if ( not XStream._stdout ):
            XStream._stdout = XStream()
            sys.stdout = XStream._stdout
        return XStream._stdout

    @staticmethod
    def stderr():
        if ( not XStream._stderr ):
            XStream._stderr = XStream()
            sys.stderr = XStream._stderr
        return XStream._stderr
