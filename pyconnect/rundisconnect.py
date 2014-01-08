import matplotlib.pyplot as plt

from pyconnect.keywords import Keywords
from pyconnect.disconnectplot import DisconnectPlot
from pyconnect.matplotlibgui import DGCanvasFrame, MDGCanvasFrame,MDG3DCanvasFrame

# from mayavi import mlab

if __name__ == '__main__':
    
    print 'Disconnectivity Graphs'
    print '--------------- ------\n'
    print 'Reading keyword file\n'
    
    kw = Keywords(dinfo="/home/lewis/DISCONNECTIO/DISCONNECTinput/BLN69/dinfo")

    disc = DisconnectPlot(kw)
    
    # Initialisation
    print 'Reading minima from %s'%disc.kw.minima['data_file']
    disc.InitialiseMin()
    print 'Reading TS from %s'%disc.kw.ts['data_file']
    disc.InitialiseTS()
    disc.CountMin()
    disc.CountTS()
    print '%d Minima read,\t%d TS read\n'%(disc.minima_index['Size'],disc.ts_index['Size'])

    disc.RemoveInvalidTS()
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
    disc.PruneBasins()

    disc.ReNumberBasins()
    print 'Calculating Parents and Children\n'
    disc.GetParentsAndChildren()
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

    if disc.kw.metric3d['present']:
        
        DGframe = DGCanvasFrame(disc)
        MDG1frame = MDGCanvasFrame(disc,Q='X')
        MDG2frame = MDGCanvasFrame(disc,Q='Y')
        
#        plt.draw()
#        plt.show()
#         if kw.maya:
#             plt.show()
#             MDG3Dframe = MDG3DMayaCanvasFrame(disc)
#             mlab.show()
#         else:
        MDG3Dframe = MDG3DCanvasFrame(disc)
        plt.show()

    elif disc.kw.metric['present']:
        DGframe = DGCanvasFrame(disc)
        MDGframe = MDGCanvasFrame(disc,Q='X')
        plt.show()
       
    else:
        DGframe = DGCanvasFrame(disc)
        
        plt.show()
