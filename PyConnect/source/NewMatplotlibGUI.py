#!/usr/bin/env python

import wx
import sys
import matplotlib.pyplot as plt
#matplotlib.use('WXAgg')
#from matplotlib.figure import Figure
#from matplotlib.backends.backend_wxagg import \
#    FigureCanvasWxAgg as FigCanvas, \
#    NavigationToolbar2WxAgg as NavigationToolbar
from matplotlib import rc
from matplotlib.backends.backend_pdf import PdfPages
import numpy as np
#import matplotlib.pylab as plt
import matplotlib.lines as lns
from mpl_toolkits.mplot3d import Axes3D
from KeywordInit import Keywords
from DisconnectPlot import DisconnectPlot

rc('text', usetex=True)
rc('font', family='serif')




                
def init_plot(self):
    self.fig = Figure()

    self.axDG = self.fig.add_subplot(121)
    self.axMDG = self.fig.add_subplot(122, projection='3d')
        
    self.axDG.set_title('Disconnectivity Graph')
    self.axMDG.set_title('Metric Disconnectivity Graph')
        
    # Turns off axes and ticks for in the top and right hand portions
        # of the plot, and sets the bottom and left hand axis back from the
        # plot.
    for loc, spine in self.axDG.spines.iteritems():
        if loc in ['left','bottom']:    
             spine.set_position(('outward',10)) # outward by 10 points                                                                               
        elif loc in ['right','top']:
            spine.set_color('none') # don't draw spine                                                                                              
        else:
            raise ValueError('unknown spine location: %s'%loc)
            
    self.axDG.xaxis.set_ticks_position('bottom')
    self.axDG.yaxis.set_ticks_position('left')


    self.PlotDG()
    self.PlotMDG()

#    def create_menu(self):
#        self.menubar = wx.MenuBar()
#        
#        menu_file = wx.Menu()
# 
#	m_expt = menu_file.Append(-1, "&Save plot\tCtrl-S", "Save plot \
#to file")
#        self.Bind(wx.EVT_MENU, self.on_save_plot, m_expt)
#        menu_file.AppendSeparator()
#        m_exit = menu_file.Append(-1, "E&xit\tCtrl-X", "Exit")
#        self.Bind(wx.EVT_MENU, self.on_exit, m_exit)
#
#        self.menubar.Append(menu_file, "&File")
#	self.SetMenuBar(self.menubar)
#
#
#    def add_toolbar(self):
#        self.toolbar = NavigationToolbar(self.canvas)
#        self.toolbar.Realize()
#        #if wx.Platform == '__WXMAC__':
#            # Mac platform (OSX 10.3, MacPython) does not seem to cope with
#            # having a toolbar in a sizer. This work-around gets the buttons
#            # back, but at the expense of having the toolbar at the top
#        #    self.SetToolBar(self.toolbar)
#        #else:
#            # On Windows platform, default window size is incorrect, so set
#            # toolbar width to figure width.
#            #tw, th = self.toolbar.GetSizeTuple()
#            #fw, fh = self.canvas.GetSizeTuple()
#            # By adding toolbar in sizer, we are able to put it at the bottom
#            # of the frame - so appearance is closer to GTK version.
#            # As noted above, doesn't work for Mac.
#            #self.toolbar.SetSize(wx.Size(fw, th))
#            #self.sizer.Add(self.toolbar, 0, wx.LEFT | wx.EXPAND)
#        # update the axes menu on the toolbar
#        self.toolbar.update()
#
#       
#    def create_main_panel(self):
#        self.panel = wx.Panel(self)
#
#        self.init_plot()
#        self.canvas = FigCanvas(self.panel, -1, self.fig)
#        self.add_toolbar()
#        self.axMDG.mouse_init()        
#        self.vbox = wx.BoxSizer(wx.VERTICAL)
#        self.vbox.Add(self.canvas, 1, flag=wx.LEFT | wx.TOP | wx.GROW)        
#        self.hbox2 = wx.BoxSizer(wx.HORIZONTAL)
#        self.hbox2.Add(self.toolbar, border=5, flag=wx.ALL)
#        self.vbox.Add(self.hbox2, 0, flag=wx.ALIGN_LEFT | wx.TOP)
#        
#        self.panel.SetSizer(self.vbox)
#        self.vbox.Fit(self)
#    
#    def create_status_bar(self):
#        self.statusbar = self.CreateStatusBar()
#
#        
def PlotDG(self):
    for l in self.disc.basin_index['Level']:
        if l == 1: continue
        for b in self.disc.basin_index['Level'][l]['Basin']:
            c = self.disc.basin_index['Level'][l]['Basin'][b]\
                ['Children']
            p = self.disc.basin_index['Level'][l]['Basin'][b]\
                ['Parents']
            self.LinesDG(l,b,c,p)


def PlotMDG(self):
    for l in self.disc.basin_index['Level']:
        if l == 1: continue
        for b in self.disc.basin_index['Level'][l]['Basin']:
            c = self.disc.basin_index['Level'][l]['Basin'][b]\
                ['Children']
            p = self.disc.basin_index['Level'][l]['Basin'][b]\
                ['Parents']
            self.LinesMDG(l,b,c,p)


def LinesDG(self,l,b,c,p):
    rgb = self.disc.basin_index['Level'][l]['Basin'][b]['RGB']

    x1 = self.disc.basin_index['Level'][l-1]['Basin'][p]['X']

    z1 = 1.0*(self.disc.basin_index['Level'][l-1]['Energy'])
    x2 = self.disc.basin_index['Level'][l]['Basin'][b]['X']

    if not c:
        z2 = self.disc.basin_index['Level'][l]['Basin'][b]['Energy']
    else:
        z2 = self.disc.basin_index['Level'][l]['Energy']      
    self.plot_dataDG = self.axDG.plot([x1,x2],[z1,z2], color =rgb, linewidth=0.2)
    

def LinesMDG(self,l,b,c,p):
    rgb = self.disc.basin_index['Level'][l]['Basin'][b]['RGB']

    x1 = self.disc.basin_index['Level'][l-1]['Basin'][p]['GLX']
    y1 = self.disc.basin_index['Level'][l-1]['Basin'][p]['GLY']
    z1 = 1.0*(self.disc.basin_index['Level'][l-1]['GLZ'])

    x2 = self.disc.basin_index['Level'][l]['Basin'][b]['GLX']
    y2 = self.disc.basin_index['Level'][l]['Basin'][b]['GLY']
    if not c:
        z2 = self.disc.basin_index['Level'][l]['Basin'][b]['GLZ']

    else:
        z2 = self.disc.basin_index['Level'][l]['GLZ']#+0.5          
        self.plot_dataMDG = self.axMDG.plot([x1,x2],[y1,y2],[z1,z2], color =rgb, alpha = 0.75,
                                            linewidth=0.2)
    
#    def on_save_plot(self, event):
#        file_choices = "PNG (*.png)|*.png"
#        
#        dlg = wx.FileDialog(
#            self, 
#            message="Save plot as...",
#            defaultDir=os.getcwd(),
#            defaultFile="plot.png",
#            wildcard=file_choices,
#            style=wx.SAVE)
#        
#        if dlg.ShowModal() == wx.ID_OK:
#            path = dlg.GetPath()
#            self.canvas.print_figure(path, dpi=self.dpi)
#            self.flash_status_message("Saved to %s" % path)
#    
#    def on_exit(self, event):
#        self.Destroy()
#    
#
#    

if __name__ == '__main__':

    kw = Keywords()

    disc = DisconnectPlot(kw)
    # Initialisation                                                   
    disc.InitialiseMin()
    disc.InitialiseTS()

    disc.CountMin()
    disc.CountTS()
    disc.RemoveThreshold()
    disc.RemoveUnderConnect()
    disc.RemoveDisjoint()
    disc.InitialiseBasin()
    disc.AssignBasins()
    disc.PruneBasins()
    disc.ReNumberBasins()
    disc.GgetParentsAndChildren()
    disc.GetNodeSize()
    # End initialisation
                                               
    disc.PositionBasins()
    #disc.GetMetric3D()                                                 

#    app = wx.PySimpleApp()
#    app.frame = CanvasFrame(disc)
#    app.frame.Show()
#    app.MainLoop()
 #   GUI.MainWin(disc)
