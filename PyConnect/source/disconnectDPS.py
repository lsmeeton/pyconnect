#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
from KeywordInit import Keywords
#from Disconnect import Disconnect
from DisconnectPlot import DisconnectPlot
import wx
#import ProxyGUI as GUI
from MatplotlibGUI import DGCanvasFrame, MDGCanvasFrame, MDG3DCanvasFrame
from mayavi import mlab

if __name__ == '__main__':
    
    print 'Disconnectivity Graphs'
    print '--------------- ------\n'
    print 'Reading keyword file\n'
    
    kw = Keywords()

    disc = DisconnectPlot(kw)
    
    # Initialisation
    print 'Reading minima from %s'%disc.kw.minima['data_file']
    disc.InitialiseMin()
    print 'Reading TS from %s'%disc.kw.ts['data_file']
    disc.InitialiseTS()
    disc.CountMin()
    disc.CountTS()
    print '%d Minima read,\t%d TS read\n'%(disc.minima_index['Size'],disc.ts_index['Size'])

    print 'Removing minima with less than %d connections'%disc.kw.connectmin['connectmin']
    disc.RemoveUnderConnect()
    disc.CountMin()
    disc.CountTS()
    print 'Remaining minima = %d\nRemaining TS = %d\n'%(disc.minima_index['Size'],disc.ts_index['Size'])
    print 'Removing Disjoint minima and TS'
    disc.RemoveDisjoint()
    disc.CountMin()
    disc.CountTS()
    print 'Remaining minima = %d\nRemaining TS = %d\n'%(disc.minima_index['Size'],disc.ts_index['Size'])

    print 'Initialising Basins'
    disc.InitialiseBasin()
    disc.AssignBasins()
#    for l in disc.basin_index['Level']:
#        print '%d basins at energy %2.2f'%(disc.basin_index['Level'][l]['No. of Basins'],
#                                           disc.basin_index['Level'][l]['Energy'])

    disc.PruneBasins()

    disc.ReNumberBasins()
    print 'Calculating Parents and Children\n'
    disc.GgetParentsAndChildren()
    disc.GetNodeSize()
    disc.DumpNumbers()
    disc.DumpSizes()
    print 'Final Basin Count'
    for l in disc.basin_index['Level']:
        print '%d basins at energy %2.2f'%(disc.basin_index['Level'][l]['No. of Basins'],
                                           disc.basin_index['Level'][l]['Energy'])
    print '\n'
    # End initialisation
    print 'Positioning Basins'
    disc.PositionBasins()
    #disc.GetMetric3D()

    if disc.kw.metric3d['present']:
        
        DGframe = DGCanvasFrame(disc)
        MDG1frame = MDGCanvasFrame(disc,Q='X')
        MDG2frame = MDGCanvasFrame(disc,Q='Y')
        
#        plt.draw()
        plt.show()
        MDG3Dframe = MDG3DCanvasFrame(disc)
        
        mlab.show()
    elif disc.kw.metric['present']:
        DGframe = DGCanvasFrame(disc)
        MDGframe = MDGCanvasFrame(disc,Q='X')
        plt.show()
       
    else:
        
        DGframe = DGCanvasFrame(disc)
#        plt.savefig("tree.pdf",format="pdf")
        
        plt.show()

#    GUI.MainWin(disc)
    
#    MainLoop()
