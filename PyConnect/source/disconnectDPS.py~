#!/usr/bin/env python

import numpy as np
from KeywordInit import Keywords
#from Disconnect import Disconnect
from DisconnectPlot import DisconnectPlot
import wx
import ProxyGUI as GUI

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
    
    app = wx.App()
    GUI.MainWin(disc)
    #for l in disc.basin_index['Level']:
    #    for b in disc.basin_index['Level'][l]['Basin']:
    #        print l, b, disc.basin_index['Level'][l]['Basin'][b]['Min']
    app.MainLoop()
