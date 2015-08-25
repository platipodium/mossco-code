'''
this code is taken from 
http://www.ster.kuleuven.be/~pieterd/python/html/plotting/interactive_colorbar.html

and modified further
'''

import pylab as plt
import numpy as np
import time



class DraggableColorbar(object):
    def __init__(self, cbar, mappable, parent=None):
        self.cbar = cbar
        self.parent = parent
        self.mappable = mappable
        self.press = None
        self._defaultRange = True
        self.cycle = sorted([i for i in dir(plt.cm) if hasattr(getattr(plt.cm, i), 'N')])
        self.index = self.cycle.index(cbar.get_cmap().name)
        self.default_index = self.cycle.index(cbar.get_cmap().name)
        self.connect()

    def connect(self):
        """connect to all the events we need"""
        self.cidpress = self.cbar.patch.figure.canvas.mpl_connect(
            'button_press_event', self.on_press)
        self.cidrelease = self.cbar.patch.figure.canvas.mpl_connect(
            'button_release_event', self.on_release)
        self.cidmotion = self.cbar.patch.figure.canvas.mpl_connect(
            'motion_notify_event', self.on_motion)
        self.keypress = self.cbar.patch.figure.canvas.mpl_connect(
            'key_press_event', self.key_press)

    def on_press(self, event):
        """on button press we will see if the mouse is over us and store some data"""
        if event.inaxes != self.cbar.ax: return
        self.press = event.x, event.y

    def key_press(self, event):
        #print 'you pressed:', event.key
        if event.key == ' ':
            self.resetRange()
        else:
            if event.key == 'r':
                self.index = self.default_index
            elif event.key == 'down':
                self.index += 1
            elif event.key == 'up':
                self.index -= 1
            if self.index < 0:
                self.index = len(self.cycle)
            elif self.index >= len(self.cycle):
                self.index = 0
            cmap = self.cycle[self.index]
            self.cbar.set_cmap(cmap)
            self.mappable.set_cmap(cmap)
            self.redraw()


    def on_motion(self, event):
        'on motion we will move the rect if the mouse is over us'
        if self.press is None: return
        if event.inaxes != self.cbar.ax: return
        xprev, yprev = self.press
        dx = event.x - xprev
        dy = event.y - yprev
        self.press = event.x, event.y
        #print 'x0=%f, xpress=%f, event.xdata=%f, dx=%f, x0+dx=%f'%(x0, xpress, event.xdata, dx, x0+dx)
        scale = self.cbar.norm.vmax - self.cbar.norm.vmin
        perc = 0.03
        if event.button == 1:
            self.cbar.norm.vmin -= (perc*scale)*np.sign(dy)
            self.cbar.norm.vmax -= (perc*scale)*np.sign(dy)
        elif event.button == 3:
            self.cbar.norm.vmin -= (perc*scale)*np.sign(dy)
            self.cbar.norm.vmax += (perc*scale)*np.sign(dy)
        self.redraw()


    def on_release(self, event):
        """on release we reset the press data"""
        self.press = None
        self.mappable.set_norm(self.cbar.norm)
        self.cbar.patch.figure.canvas.draw()
        self.set_defaultRange()

    def disconnect(self):
        """disconnect all the stored connection ids"""
        self.cbar.patch.figure.canvas.mpl_disconnect(self.cidpress)
        self.cbar.patch.figure.canvas.mpl_disconnect(self.cidrelease)
        self.cbar.patch.figure.canvas.mpl_disconnect(self.cidmotion)


    def redraw(self):
        start1 = time.clock()
        print '\t\t-------starting cb.redraw()()-------'
        start = time.clock()
        self.cbar.draw_all()
        end = time.clock(); print '\t\t{0:.6f}'.format(end - start), 'self.cbar.draw_all()'
        start = time.clock()
        self.mappable.set_norm(self.cbar.norm)
        end = time.clock(); print '\t\t{0:.6f}'.format(end - start), 'self.mappable.set_norm(self.cbar.norm)'
        start = time.clock()
        

        self.cbar.patch.figure.canvas.draw()
        #self.parent.pltWidget.fast_redraw(self.parent.plot_setup['im'])


        end = time.clock(); print '\t\t{0:.6f}'.format(end - start), 'self.cbar.patch.figure.canvas.draw()'
        end = time.clock(); print '\t\t{0:.6f}'.format(end - start1), 'altogether() > seconds:'
        print '\t\t-------finishing cb.redraw()()-------'

    def set_defaultRange(self):
        data_min, data_max = self.parent.selected_data['vmin'], self.parent.selected_data['vmax']
        if abs(data_min-self.cbar.norm.vmin) < 0.1 and abs(data_max-self.cbar.norm.vmax) < 0.1:
            self._defaultRange = True
            #print 'default range set True'
        else:
            self._defaultRange = False
            #print 'default range set False'

    def resetRange(self):
        data_min, data_max = self.parent.selected_data['vmin'], self.parent.selected_data['vmax']
        self.set_min(data_min)
        self.set_max(data_max)

    def set_min(self, val, redraw=True):
        start1 = time.clock()
        print '\t-------starting set_min()-------'
        start = time.clock()
        self.cbar.norm.vmin = val
        end = time.clock(); print '\t{0:.6f}'.format(end - start), 'self.cbar.norm.vmin = val > seconds:'
        start = time.clock()
        if redraw: self.redraw()
        end = time.clock(); print '\t{0:.6f}'.format(end - start), 'self.redraw() > seconds:'
        start = time.clock()
        self.set_defaultRange()
        end = time.clock(); print '\t{0:.6f}'.format(end - start), 'self.set_defaultRange() > seconds:'
        end = time.clock(); print '\t{0:.6f}'.format(end - start1), 'altogether() > seconds:'
        print '\t-------finishing set_min()-------'

    def set_max(self, val, redraw=True):
        start1 = time.clock()
        print '\t\t-------starting set_mmax()-------'
        start = time.clock()
        self.cbar.norm.vmax = val
        end = time.clock(); print '\t{0:.6f}'.format(end - start), 'self.cbar.norm.vmax = val > seconds:'
        start = time.clock()
        if redraw: self.redraw()
        end = time.clock(); print '\t{0:.6f}'.format(end - start), 'self.redraw() > seconds:'
        start = time.clock()
        self.set_defaultRange()
        end = time.clock(); print '\t{0:.6f}'.format(end - start), 'self.set_defaultRange() > seconds:'
        end = time.clock(); print '\t{0:.6f}'.format(end - start1), 'altogether() > seconds:'
        print '\t------finishing set_max()-------'


    def set_cmap(self, cmap):
        self.cbar.set_cmap(cmap)
        self.redraw()

    def get_min(self):
        return self.cbar.norm.vmin
    
    def get_max(self):
        return self.cbar.norm.vmax
   
    def get_cmap(self):
        return self.cbar.get_cmap()


    def isDefaultRange(self):
        return self._defaultRange
