#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
from KeywordInit import Keywords
#from Disconnect import Disconnect
from DisconnectPlot import DisconnectPlot
import wx
#import ProxyGUI as GUI
from MatplotlibGUI import DGCanvasFrame, MDGCanvasFrame, MDG3DCanvasFrame

if __name__ == '__main__':
    
    kw = Keywords()
#    print kw
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
    
    if disc.kw.metric3d['present']:

        DGframe = DGCanvasFrame(disc)
        MDG1frame = MDGCanvasFrame(disc,Q='X')
        MDG2frame = MDGCanvasFrame(disc,Q='Y')
        MDG3Dframe = MDG3DCanvasFrame(disc)
        plt.show()

    elif disc.kw.metric['present']:
        DGframe = DGCanvasFrame(disc)
        MDGframe = MDGCanvasFrame(disc,Q='X')
        plt.show()
       
    else:
        DGframe = DGCanvasFrame(disc)
#        plt.savefig("tree.eps",format="eps")
        plt.show()

#    GUI.MainWin(disc)
    
#    MainLoop()
