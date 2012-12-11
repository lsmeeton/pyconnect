#!/usr/bin/env python

#import wx
#import sys
import os
import matplotlib
#matplotlib.use('WXAgg')
#from matplotlib.figure import Figure
#from matplotlib.backends.backend_wxagg import \
#    FigureCanvasWxAgg as FigCanvas, \
#    NavigationToolbar2WxAgg as NavigationToolbar
from matplotlib import rc
#from matplotlib import colors, cm
from matplotlib.collections import LineCollection
#from matplotlib.backends.backend_pdf import PdfPages
import numpy as np
import matplotlib.pylab as plt
#import matplotlib.lines as lns
from mpl_toolkits.mplot3d import Axes3D
from KeywordInit import Keywords
from DisconnectPlot import DisconnectPlot
#import time
import sys
from mayavi import mlab


class DGCanvasFrame():
    def __init__(self,disc):
        
#        super(DGCanvasFrame,self).__init__()
        
        self.disc = disc
   
        self.init_plot()
                
    def init_plot(self):
        
        # self.line_array is input for linecollection object
        self.line_array = []#np.zeros((2,2))
        self.rgba_array = []
        self.fig = plt.figure()

        self.ax = self.fig.add_subplot(111)
#        self.colbar = self.fig.add_subplot(122)
        
#        self.ax.set_title('Disconnectivity Graph')

        self.FormatAxes()
        
        self.FormatPlot()
        
        self.PlotDG()
        
        
        
        if self.disc.kw.trval['trval_file']:
            self.AddColourBar()

        self.IdentifyMin()
            
        

    def FormatAxes(self):
        '''
        Format axes (ie. determine location and add labels)
        '''
        for loc, spine in self.ax.spines.iteritems():
            if loc in ['left']:    
                spine.set_position(('outward',10)) # outward by 10 points                                                                               
            elif loc in ['right','top','bottom']:
                spine.set_color('none') # don't draw spine                                                                                              
            else:
                raise ValueError('unknown spine location: %s'%loc)
            
        self.ax.xaxis.set_ticks([])
        self.ax.yaxis.set_ticks_position('left')
        if self.disc.kw['energy_label']['label']: 
            self.ax.set_ylabel(self.disc.kw['energy_label']['label'])
            
        self.ax.set_xlim(-0.5,0.5)
        self.ax.set_ylim(self.disc.kw.first['E1'] - (self.disc.kw.levels['n'])*self.disc.kw.delta['dE'],self.disc.kw.first['E1'])#(-48,-52)
 
 
    def FormatPlot(self):
        '''
        Formats latex labels
        '''
        if self.disc.kw.tex:
            rc('text', usetex=True)
            rc('font', family='serif')

 
    def AddColourBar(self):
        '''
        Adds a colour bar to the plot
        '''
        self.cax, kw = matplotlib.colorbar.make_axes(self.ax)
            
        cb1 = matplotlib.colorbar.ColorbarBase(self.cax,
                                                cmap=self.disc.col_map,
                                                norm=self.disc.norm,
                                                orientation='vertical')
        cb1.set_label(self.disc.kw['colour_bar_label']['label'])
        
    def IdentifyMin(self):
        '''
        Identifies min specified by IDMIN keyword
        '''
        for i in self.disc.kw.idmin['Min']:
            label = str(self.disc.kw.idmin['Min'][i])
            # Find location of minima
            level_list = []
            for l in self.disc.minima_index['Index'][i]['Basin']['Level']:
                b = self.disc.minima_index['Index'][i]['Basin']['Level'][l]
                if b: level_list.append(l)
            l = max(level_list)
            b = self.disc.minima_index['Index'][i]['Basin']['Level'][l]
            c = self.disc.basin_index['Level'][l]['Basin'][b]['Children']
            x = self.disc.basin_index['Level'][l]['Basin'][b]['X']
            if not c:
                z = self.disc.basin_index['Level'][l]['Basin'][b]['Energy']
            else:
                z = self.disc.basin_index['Level'][l]['Energy']      
            self.ax.text(x,z,label)
 
    def PlotDG(self):
        self.plot_listy = []
        for l in self.disc.basin_index['Level']:
            if l == 1: continue
            for b in self.disc.basin_index['Level'][l]['Basin']:
                c = self.disc.basin_index['Level'][l]['Basin'][b]\
                    ['Children']
                p = self.disc.basin_index['Level'][l]['Basin'][b]\
                    ['Parents']
                self.LinesDG(l,b,c,p)
        print 'shape of self.line_array', np.shape(self.line_array), type(self.line_array[0])
#        np.swapaxes(self.line_array, 0, 2)
        self.Line = LineCollection(self.line_array,
                                   color=self.rgba_array,
                                   linewidth=0.2)
        self.ax.add_collection(self.Line)

    def LinesDG(self,l,b,c,p):
        rgb = self.disc.basin_index['Level'][l]['Basin'][b]['RGB']
#        colour = self.disc.basin_index['Level'][l]['Basin'][b]['X']

        x1 = self.disc.basin_index['Level'][l-1]['Basin'][p]['X']

        z1 = 1.0*(self.disc.basin_index['Level'][l-1]['Energy'])
        
        x2 = self.disc.basin_index['Level'][l]['Basin'][b]['X']

        if not c:
            try: z2 = self.disc.basin_index['Level'][l]['Basin'][b]['Energy']
            except KeyError: 
                print l,b
                sys.exit()
#            z2 = self.disc.basin_index['Level'][l]['Basin'][b]['Energy']
        else:

            z2 = self.disc.basin_index['Level'][l]['Energy']      
#        self.plot_dataDG = self.ax.plot([x1,x2],[z1,z2], c=rgb, linewidth=0.2)
#        self.line_array = np.dstack((self.line_array,np.array([[x1,z1],
#                                                               [x2,z2]])))
        self.line_array.append(np.array([[x1,z1],[x2,z2]]))
        self.rgba_array.append(rgb)
    
class MDGCanvasFrame():
    def __init__(self,disc,Q):
        
        
        self.Q = Q
        self.disc = disc
      
        self.init_plot()
        
                
    def init_plot(self):
        self.line_array = []#np.zeros((2,2))
        self.rgba_array = []
        self.fig = plt.figure()
        self.ax = self.fig.add_subplot(111)
        
#        self.ax.set_title('Metric Disconnectivity Graph')
        
        # Turns off axes and ticks for in the top and right hand portions
        # of the plot, and sets the bottom and left hand axis back from the
        # plot.
        
        self.FormatAxes()
        


#        self.PlotDG()
        self.PlotMDG()
        
        if self.disc.kw.trval['trval_file']:
            self.cax, kw = matplotlib.colorbar.make_axes(self.ax)
            
            cb1=matplotlib.colorbar.ColorbarBase(self.cax,cmap=self.disc.col_map,
                                                 norm=self.disc.norm,
                                                 orientation='vertical')
            cb1.set_label(self.disc.kw['colour_bar_label']['label'])
        
    def FormatAxes(self):
        '''
        Format axes (ie. determine location and add labels)
        '''
        for loc, spine in self.ax.spines.iteritems():
            if loc in ['left','bottom']:    
                spine.set_position(('outward',10)) # outward by 10 points                                                                               
            elif loc in ['right','top']:
                spine.set_color('none') # don't draw spine                                                                                              
            else:
                raise ValueError('unknown spine location: %s'%loc)
            
        self.ax.xaxis.set_ticks_position('bottom')
        self.ax.yaxis.set_ticks_position('left')
        
        if self.Q == 'X':
            self.ax.set_xlim(self.disc.basin_index['MinX'],
                             self.disc.basin_index['MaxX'])
        else:
            self.ax.set_xlim(self.disc.basin_index['MinY'],
                             self.disc.basin_index['MaxY'])
            
        self.ax.set_ylim(self.disc.kw.first['E1'] - (self.disc.kw.levels['n'])*self.disc.kw.delta['dE'],self.disc.kw.first['E1'])#(-48,-52)
 

        if self.disc.kw['energy_label']['label']: 
            self.ax.set_ylabel(self.disc.kw['energy_label']['label'])
        
        if self.Q == 'X':    
            if self.disc.kw['q1']['label']:
                self.ax.set_xlabel(self.disc.kw['q1']['label'])
        else:
            if self.disc.kw['q2']['label']:
                self.ax.set_xlabel(self.disc.kw['q2']['label'])

    def PlotMDG(self):
        self.plot_listy = []
        for l in self.disc.basin_index['Level']:
            if l == 1: continue
            for b in self.disc.basin_index['Level'][l]['Basin']:
                c = self.disc.basin_index['Level'][l]['Basin'][b]\
                    ['Children']
                p = self.disc.basin_index['Level'][l]['Basin'][b]\
                    ['Parents']
                self.LinesMDG(l,b,c,p)
        self.Line = LineCollection(self.line_array,
                                   color=self.rgba_array,
                                   linewidth=0.2)
        self.ax.add_collection(self.Line)

    def LinesMDG(self,l,b,c,p):
        if self.Q == 'X': metric = 'MetricX'
        elif self.Q == 'Y': metric = 'MetricY'
        
        rgb = self.disc.basin_index['Level'][l]['Basin'][b]['RGB']

        x1 = self.disc.basin_index['Level'][l-1]['Basin'][p][metric]

        z1 = 1.0*(self.disc.basin_index['Level'][l-1]['Energy'])

        x2 = self.disc.basin_index['Level'][l]['Basin'][b][metric]

        if not c:
            z2 = self.disc.basin_index['Level'][l]['Basin'][b]['Energy']

        else:
            z2 = self.disc.basin_index['Level'][l]['Energy']#+0.5          
        self.plot_dataMDG = self.ax.plot([x1,x2],[z1,z2], color =rgb,
                                                linewidth=0.2)
        self.line_array.append(np.array([[x1,z1],[x2,z2]]))
        self.rgba_array.append(rgb)

class MDG3DCanvasFrame():
    def __init__(self,disc):

        
        self.disc = disc

        self.init_plot()
        
                
    def init_plot(self):
#        self.fig = plt.figure()
        self.rgba_array = []
        self.fig = mlab.figure(1,bgcolor=(0.5, 0.5, 0.5))
        mlab.clf()

#        self.ax = self.fig.add_subplot(111, projection='3d')
        

#        self.ax.set_title('Metric Disconnectivity Graph')
        
#        self.FormatAxes()

        self.PlotMDG()
#        if self.disc.kw.trval['trval_file']:
#            self.cax, kw = matplotlib.colorbar.make_axes(self.ax)
#            
#            cb1=matplotlib.colorbar.ColorbarBase(self.cax,cmap=self.disc.col_map,
#                                                 norm=self.disc.norm,
#                                                 orientation='vertical')
#            cb1.set_label(self.disc.kw['colour_bar_label']['label'])
        
    def FormatAxes(self):
        '''
        Format axes (ie. determine location and add labels)
        '''
        

        if self.disc.kw['energy_label']['label']: 
            self.ax.set_zlabel(self.disc.kw['energy_label']['label'])
            
        if self.disc.kw['q1']['label']:
            self.ax.set_xlabel(self.disc.kw['q1']['label'])
        
        if self.disc.kw['q2']['label']:
            self.ax.set_ylabel(self.disc.kw['q2']['label'])
   
    def PlotMDG(self):
        indx = 0
        self.plot_index = {} # key: level, basin tuple, value: individual basin index
        self.list_x = []
        self.list_y = []
        self.list_z = []
        self.connections = []
        
        for b in self.disc.basin_index['Level'][1]['Basin']:
            self.plot_index[(1,b)] = indx
            rgb = self.disc.basin_index['Level'][1]['Basin'][b]['RGB']

            x1 = self.disc.basin_index['Level'][1]['Basin'][b]['MetricX']
            y1 = self.disc.basin_index['Level'][1]['Basin'][b]['MetricY']

            z1 = self.disc.basin_index['Level'][1]['Energy']
            self.rgba_array.append(rgb)
            self.list_x.append(x1)
            self.list_y.append(y1)
            self.list_z.append(z1)
              
            indx += 1
        
        for l in self.disc.basin_index['Level']:
            if l == 1: continue
            for b in self.disc.basin_index['Level'][l]['Basin']:
                c = self.disc.basin_index['Level'][l]['Basin'][b]\
                    ['Children']
                p = self.disc.basin_index['Level'][l]['Basin'][b]\
                    ['Parents']
                if not self.plot_index.has_key((l,b)):
                    self.plot_index[(l,b)] = indx
                self.LinesMDG(l,b,c,p)
                indx += 1
#        mlab.points3d(self.list_x[:],self.list_y[:],self.list_z[:],
#                      color = self.rgba_array)
        self.list_x = np.hstack(self.list_x)
        self.list_y = np.hstack(self.list_y)
        self.list_z = np.hstack(self.list_z)
        self.connections = np.vstack(self.connections)
        print np.shape(self.list_x), np.shape(self.plot_index)
#        self.s = self.list_x**2 + self.list_y**2 + self.list_z**2
        self.src = mlab.pipeline.scalar_scatter(self.list_x, 
                                           self.list_y, 
                                           self.list_z,
                                           self.list_x) 
#                                           np.zeros(len(self.list_x)))
        
        self.src.mlab_source.dataset.lines = self.connections
#        self.lines = mlab.pipeline.stripper(self.src)
        mlab.pipeline.surface(self.src, line_width=1, 
                              opacity = 0.4)
        mlab.axes(self.src)
        mlab.show() 


    def LinesMDG(self,l,b,c,p):
        rgb = self.disc.basin_index['Level'][l]['Basin'][b]['RGB']

        x1 = self.disc.basin_index['Level'][l]['Basin'][b]['MetricX']
        y1 = self.disc.basin_index['Level'][l]['Basin'][b]['MetricY']
#        z1 = 1.0*(self.disc.basin_index['Level'][l]['Energy'])

        
#        x2 = self.disc.basin_index['Level'][l]['Basin'][b]['MetricX']
#        y2 = self.disc.basin_index['Level'][l]['Basin'][b]['MetricY']
        if not c:
            z1 = self.disc.basin_index['Level'][l]['Basin'][b]['Energy']

        else:
            z1 = self.disc.basin_index['Level'][l]['Energy']#+0.5          
        
#        self.plot_dataMDG = self.ax.plot([x1,x2],[y1,y2],[z1,z2], color =rgb,
#                                                linewidth=0.2)
#        mlab.plot3d([x1,x2],[y1,y2],[z1,z2], color =rgb)
        self.rgba_array.append(rgb)
        self.list_x.append(x1)
        self.list_y.append(y1)
        self.list_z.append(z1)
        indx1 = self.plot_index[(l,b)]
        indx2 = self.plot_index[(l-1,p)]
        self.connections.append(np.array([indx1,indx2]))


if __name__ == '__main__':
    kw = Keywords()
    
    disc = DisconnectPlot(kw)
                       
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
    
    disc.DumpNumbers()
    
    disc.PositionBasins()
    #t1 = time.time()
    #print 'Initialise disconnect %2.6f'%(#t1-#t0)
    #print 'Total %2.6f'%(#t1-#t00)
    #disc.GetMetric3D()                                                 
    #t0 = time.time()
#    #print disc.minima_index['Index'][5]
    #t1 = time.time()
    #print '#print disc.minima_index %2.6f'%(#t1-#t0)
    #t0 = time.time()
#    DG = DGCanvasFrame(disc)
    #t1 = time.time()
    #print 'Initialise DGframe %2.6f'%(#t1-#t0)
    #t0 = time.time()
#    plt.ylim(-48,-52)
#    plt.show()
#    plt.savefig("tree.eps",format="eps")
    #t1 = time.time()
    #print 'Initialise disconnect %2.6f'%(#t1-#t0)
#    MDG = MDGCanvasFrame(disc,Q='X')
#    plt.savefig("metrictreeX.eps",format="eps")
#    MDG = MDGCanvasFrame(disc,Q='Y')
#    plt.savefig("metrictreeY.eps",format="eps")
#    MDG = MDG3DCanvasFrame(disc)
#    plt.show()#savefig("metrictree.eps",format="eps")
#    app = wx.PySimpleApp()
#    if disc.kw.metric3d['present']:
#        t0 = time.time()
#        app.DGframe = DGCanvasFrame(disc)
#        t1 = time.time()
#        print 'DG %2.6f'%(t1-t0)
#        t0 = time.time()
#        app.MDG1frame = MDGCanvasFrame(disc,Q='X')
#        t1 = time.time()
#        print 'MDG1 %2.6f'%(t1-t0)
#        t0 = time.time()
#        app.MDG2frame = MDGCanvasFrame(disc,Q='Y')
#        t1 = time.time()
#        print 'MDG2 %2.6f'%(t1-t0)
#        t0 = time.time()
#        app.MDG3Dframe = MDG3DCanvasFrame(disc)
#        t1 = time.time()
#        print 'MDG3D %2.6f'%(t1-t0)
#        
#        app.DGframe.Show()
#        app.MDG1frame.Show()
#        app.MDG2frame.Show()
#        app.MDG3Dframe.Show()
#    elif disc.kw.metric['present']:
#        app.DGframe = DGCanvasFrame(disc)
#        app.MDGframe = MDGCanvasFrame(disc,Q='X')
#        
#        app.DGframe.Show()
#        app.MDGframe.Show()
##        app.MDG3Dframe.Show()
#    else:
#        app.DGframe = DGCanvasFrame(disc)
#        
#        app.DGframe.Show()
##    app.MDGframe.Show()
##    app.MDG3Dframe.Show()
##    app.DGframe.Show()
##    app.MDGframe.Show()
##    app.MDG3Dframe.Show()
#    app.MainLoop()
# #   GUI.MainWin(disc)
