#!/usr/bin/env python

import wx
import sys
import matplotlib
matplotlib.use('WXAgg')
from matplotlib.figure import Figure
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigCanvas, \
    NavigationToolbar2WxAgg as NavigationToolbar
import numpy as np
#import matplotlib.pylab as plt
import matplotlib.lines as lns
from mpl_toolkits.mplot3d import Axes3D
from KeywordInit import Keywords
from DisconnectPlot import DisconnectPlot

class BoundControlBox(wx.Panel):
        """ A static box with a couple of radio buttons and a text
        box. Allows to switch between an automatic mode and a 
        manual mode with an associated value.
    """
        def __init__(self, parent, ID, label, initval):
            super(BoundControlBox,self).__init__(parent, ID)
            
            self.value = initval
            
            box = wx.StaticBox(self, -1, label)
            sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
            
        
        def on_update_manual_text(self, event):
            self.manual_text.Enable(self.radio_manual.GetValue())
    
        def on_text_enter(self, event):
            self.value = self.manual_text.GetValue()
    
        def is_auto(self):
            return self.radio_auto.GetValue()
        
        def manual_value(self):
            return self.value



class CanvasFrame(wx.Frame):
    def __init__(self,disc,parent=None,id=-1):
        
        super(CanvasFrame,self).__init__(parent=None,id=-1)
        
        self.disc = disc
        #self.panel = wx.Panel(self)
        #self.fig = Figure()
        #self.ax = self.fig.add_subplot(111)
        
        self.create_menu()
        self.create_status_bar()
        self.create_main_panel()
        self.add_toolbar()
                
    def init_plot(self):
        self.fig = Figure()

        self.axDG = self.fig.add_subplot(121)
        self.axMDG = self.fig.add_subplot(122, projection='3d')
        
        self.axDG.set_title('Disconnectivity Graph')
        self.axMDG.set_title('Metric Disconnectivity Graph')
        
        self.PlotDG()
        self.PlotMDG()

    def create_menu(self):
        self.menubar = wx.MenuBar()
        
        menu_file = wx.Menu()
        
    def create_main_panel(self):
        self.panel = wx.Panel(self)

        self.init_plot()
        self.canvas = FigCanvas(self.panel, -1, self.fig)

        self.axMDG.mouse_init()        
        self.vbox = wx.BoxSizer(wx.VERTICAL)
        self.vbox.Add(self.canvas, 1, flag=wx.LEFT | wx.TOP | wx.GROW)        
        
        #self.vbox.Add(self.hbox2, 0, flag=wx.ALIGN_LEFT | wx.TOP)
        
        self.panel.SetSizer(self.vbox)
        self.vbox.Fit(self)
    
    def create_status_bar(self):
        self.statusbar = self.CreateStatusBar()

        
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

        z1 = 1.0*(self.disc.basin_index['Level'][l-1]['GLZ'])

        x2 = self.disc.basin_index['Level'][l]['Basin'][b]['X']

        if not c:
            z2 = self.disc.basin_index['Level'][l]['Basin'][b]['GLZ']

        else:
            z2 = self.disc.basin_index['Level'][l]['GLZ']#+0.5          
        self.plot_dataDG = self.axDG.plot([x1,x2],[z1,z2], color =rgb)
    

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
            self.plot_dataMDG = self.axMDG.plot([x1,x2],[y1,y2],[z1,z2], color =rgb, alpha = 0.75)
    


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

    app = wx.PySimpleApp()
    app.frame = CanvasFrame(disc)
    app.frame.Show()
    app.MainLoop()
 #   GUI.MainWin(disc)
