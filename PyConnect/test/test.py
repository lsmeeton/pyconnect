#!/usr/bin/env python

import unittest
from Disconnect import Disconnect
from DisconnectPlot import DisconnectPlot
from KeywordInit import Keywords


class ProductTestCase(unittest.TestCase):

    def setUp(self):
        kw = Keywords()
        self.disc = DisconnectPlot(kw)
        self.disc.InitialiseMin()
        self.disc.InitialiseTS()
    
    def testCountMin1(self):
        n = 132356
        self.disc.CountMin()
        self.assertEqual(self.disc.minima_index['Size'], n)

    def testMinList1(self):
        test_list = {}
        minima_list = {}

        for lines in open('minima_energy'):
            indice = int(lines.split()[0])
            energy = float(lines.split()[1])
            test_list[indice] = energy

        for i in self.disc.minima_index['Index']:
            minima_list[i] = self.disc.minima_index['Index'][i]['Energy']

        self.assertDictEqual(minima_list, test_list)

    def testCountTS1(self):
        n = 117791

        self.disc.CountTS()
        self.assertEqual(self.disc.ts_index['Size'], n)

    def testTSListEnergy1(self):
        #self.disc.RemoveDegenerateTS()
        test_list = {}
        ts_list = {}
        for lines in open('ts_energy'):
            indice = int(lines.split()[0])
            energy = float(lines.split()[1])
            test_list[indice] = energy

        for i in self.disc.ts_index['Index']:
            ts_list[i] = self.disc.ts_index['Index'][i]['Energy']

        self.assertDictEqual(ts_list, test_list)

    def testMinList2(self):
        test_list = {}
        minima_list = {}
        self.disc.RemoveThreshold()
        for lines in open('minima_threshold_energy'):
            indice = int(lines.split()[0])
            energy = float(lines.split()[1])
            test_list[indice] = energy

        for i in self.disc.minima_index['Index']:
            minima_list[i] = self.disc.minima_index['Index'][i]['Energy']

        self.assertDictEqual(minima_list, test_list)


    def testTSListEnergy2(self):
        #self.disc.RemoveDegenerateTS()
        test_list = {}
        ts_list = {}
        self.disc.RemoveThreshold()
        for lines in open('ts_threshold_energy'):
            indice = int(lines.split()[0])
            energy = float(lines.split()[1])
            test_list[indice] = energy

        for i in self.disc.ts_index['Index']:
            ts_list[i] = self.disc.ts_index['Index'][i]['Energy']

        self.assertDictEqual(ts_list, test_list)

        
    def testHighestTS(self):
        x = 19.1133463797
        self.disc.HighTS()
        self.assertEqual(self.disc.ts_index['HighTS']['Energy'], x)

    def testLowestTS(self):
        x = -52.5702347522
        self.disc.LowTS()
        self.assertEqual(self.disc.ts_index['LowTS']['Energy'], x)

    
    def testMinimaDegree(self):
        '''
        Tests if node degree calculations are correct. Note, only
        compares minima from PyConnect which are present in 
        FORTRAN after using GETNCONN call.
        '''
        
        test_list = {}
        minima_list = {}
        self.disc.RemoveThreshold()
        self.disc.CalcDegree()
        for lines in open('minima_degree'):
            indice = int(lines.split()[0])
            degree = int(lines.split()[1])
            test_list[indice] = degree
        
        for i in test_list:
            minima_list[i] = (self.disc.minima_index
                              ['Index'][i]['Degree'])
        
        self.assertDictEqual(minima_list, test_list)

    def testFindGM(self):
        '''
        Tests if the global minimum can be located
        '''
#        self.disc.RemoveThreshold()
        self.disc.CountMin()
        self.disc.CountTS()
        self.disc.RemoveThreshold()
        self.disc.RemoveUnderConnect()
        self.disc.RemoveDisjoint()
#        print self.disc.minima_index['GM']
        self.disc.InitialiseBasin()
            
        #print self.disc.basin_index
        self.assertEqual(1,1)


    def testMinimaDegreeRemove(self):
        '''
        Tests if minima are correctly removed based on 
        their degree
        '''
        test_list = {}
        minima_list = {}
        self.disc.RemoveThreshold()
        self.disc.RemoveUnderConnect()
        for lines in open('minima_degree'):
            indice = int(lines.split()[0])
            degree = int(lines.split()[1])
            test_list[indice] = degree
            
        for i in test_list:
            minima_list[i] = (self.disc.minima_index
                              ['Index'][i]['Degree'])
        
        self.assertDictEqual(minima_list, test_list)

    def testRemoveDisjointMin(self):
        '''
        Tests if disjoint minima are correctly removed
        from the database
        '''
        test_list = []
        minima_list = []
        self.disc.CountMin()
        self.disc.CountTS()
        self.disc.RemoveThreshold()
        self.disc.RemoveUnderConnect()
        self.disc.RemoveDisjoint()
        for lines in open('minima_disjoint_removed'):
            indice = int(lines.split()[0])
            
            test_list.append(indice)
            
        minima_list = self.disc.minima_index['Index'].keys()
#        print 'min', self.disc.minima_index['Index'][3]
        self.assertEqual(minima_list, test_list)

    def testRemoveDisjointTS(self):
        '''
        Tests if disjoint minima are correctly removed
        from the database
        '''
        test_list = []
        minima_list = []
        self.disc.CountMin()
        self.disc.CountTS()
        self.disc.RemoveThreshold()
        self.disc.RemoveUnderConnect()
        self.disc.RemoveDisjoint()
        for lines in open('ts_disjoint_removed'):
            indice = int(lines.split()[0])
            
            test_list.append(indice)
            
        minima_list = self.disc.ts_index['Index'].keys()
        
        
        self.assertEqual(minima_list, test_list)

    def testBasinAssignment(self):
        '''
        Tests if disjoint minima are correctly removed
        from the database
        '''
        test_list = {}
        minima_list = {}
        self.disc.CountMin()
        self.disc.CountTS()
        self.disc.RemoveThreshold()
        self.disc.RemoveUnderConnect()
        self.disc.RemoveDisjoint()
#        print self.disc.minima_index['GM']
        self.disc.InitialiseBasin()
        self.disc.AssignBasins()
        for lines in open('basin_at_level'):
            level = int(lines.split()[0])
            num = int(lines.split()[1])
            test_list[level] = num
            
        for l in self.disc.basin_index['Level']:
            minima_list[l] = self.disc.basin_index['Level'][l]['No. of Basins']
        #print 'min', self.disc.minima_index['Index'][3]
        #print 'min', self.disc.minima_index['Index'][4]
        #print self.disc.minima_index['Index'][127634]['Basin']['Level']
        #print self.disc.basin_index['Level'][15]
        self.assertDictEqual(minima_list, test_list)


#def testBasinAssignmentHalf(self):
#    '''
#    Tests if disjoint minima are correctly removed
#    from the database
#    '''
#    test_list = {}
#    minima_list = {}
#    self.disc.CountMin()
#    self.disc.CountTS()
#    self.disc.RemoveThreshold()
#    self.disc.RemoveUnderConnect()
#    self.disc.RemoveDisjoint()
#    print self.disc.minima_index['GM']
#    self.disc.InitialiseBasin()
#    for lines in open('min_in_basin_num'):
#        level = int(lines.split()[0])
#        num = int(lines.split()[1])
#        test_list[level] = num
#        
#    for l in self.disc.basin_index['Level']:
#        minima_list[l] = self.disc.basin_index['Level'][l]['No. of Basins']
#    print 'min', self.disc.minima_index['Index'][3]
#    print 'min', self.disc.minima_index['Index'][4]
#    #print self.disc.basin_index
#    self.assertDictEqual(minima_list, test_list)
#

    def testRenumberBasin(self):
        '''
        Tests if disjoint minima are correctly removed
        from the database
        '''
        test_list = {}
        minima_list = {}
        self.disc.CountMin()
        self.disc.CountTS()
        self.disc.RemoveThreshold()
        self.disc.RemoveUnderConnect()
        self.disc.RemoveDisjoint()
        #print self.disc.minima_index['GM']
        self.disc.InitialiseBasin()
        self.disc.AssignBasins()
        self.disc.PruneBasins()
        
        self.disc.ReNumberBasins()
        for lines in open('min_in_basin_renumber'):
            level = int(lines.split()[0])
            num = int(lines.split()[1])
            test_list[level] = num
            
        for l in self.disc.basin_index['Level']:
            minima_list[l] = self.disc.basin_index['Level'][l]['No. of Basins']
        #print 'min', self.disc.minima_index['Index'][3]
        #print 'min', self.disc.minima_index['Index'][4]
        #print self.disc.basin_index
        self.assertDictEqual(minima_list, test_list)



    def testBasinAssignmentNum(self):
        '''
        Tests if disjoint minima are correctly removed
        from the database
        '''
        test_list = {}
        minima_list = {}
        self.disc.CountMin()
        self.disc.CountTS()
        self.disc.RemoveThreshold()
        self.disc.RemoveUnderConnect()
        self.disc.RemoveDisjoint()
        #print self.disc.minima_index['GM']
        self.disc.InitialiseBasin()
        self.disc.AssignBasins()
        
        for lines in open('min_in_basin'):
            m = int(lines.split()[0])
            l = int(lines.split()[1])
            b = int(lines.split()[2])
            if b != 0:
                try:
                    test_list[l][b].append(m)
                except KeyError:
                    try:
                        test_list[l][b] = [m]
                    except KeyError:
                        test_list[l] = {}
                        test_list[l][b] = [m]
            
        for m in self.disc.minima_index['Index']:
            for l in self.disc.basin_index['Level']:
                
                b = self.disc.minima_index['Index'][m]['Basin']['Level'][l]
         #       print m, l, b
                if b:
                    try:
                        minima_list[l][b].append(m)
          #              print 'try', m,l,b,minima_list
                    except KeyError:
                        try:
                        #minima_list[l] = {}
                            minima_list[l][b] = [m]
                        except KeyError:
                            minima_list[l] = {}
                            minima_list[l][b] = [m]
           #             print 'except', m,l,b,minima_list
        #print 'min', self.disc.minima_index['Index'][3]
        #print 'min', self.disc.minima_index['Index'][4]
        
#        print minima_list
#        print len(minima_list), len(test_list), 'len'
#        print test_list
        #print 'hello'
        for i in self.disc.minima_index['Index']:
            for j in self.disc.minima_index['Index'][i]['Basin']['Level']:
                k =  self.disc.minima_index['Index'][i]['Basin']['Level'][j]
         #       print i, j, k
        #print self.disc.minima_index['Index']
        #print self.disc.basin_index
        self.assertDictEqual(minima_list, test_list)

    def testRenumberBasinNum(self):
        '''
        Tests if disjoint minima are correctly removed
        from the database
        '''
        test_list = {}
        minima_list = {}
        self.disc.CountMin()
        self.disc.CountTS()
        self.disc.RemoveThreshold()
        self.disc.RemoveUnderConnect()
        self.disc.RemoveDisjoint()
        #print self.disc.minima_index['GM']
        self.disc.InitialiseBasin()
        self.disc.AssignBasins()
        self.disc.PruneBasins()
        self.disc.ReNumberBasins()
        for lines in open('min_in_basin_parent'):
            m = int(lines.split()[0])
            l = int(lines.split()[1])
            b = int(lines.split()[2])
            if b != 0:
                try:
                    test_list[l][b].append(m)
                except KeyError:
                    try:
                        test_list[l][b] = [m]
                    except KeyError:
                        test_list[l] = {}
                        test_list[l][b] = [m]
        for l in self.disc.basin_index['Level']:
            minima_list[l] = {}
            for b in self.disc.basin_index['Level'][l]['Basin']:
                
                minima_list[l][b] = self.disc.basin_index['Level'][l]['Basin'][b]['Min']
        #print minima_list
        #print len(minima_list), len(test_list), 'len'
        #print test_list
        
        
        self.assertDictEqual(minima_list, test_list)

    def testRenumberBasinMin(self):
        '''
        Tests if  minima are in correct basins
        after renumbering
        '''
        #self.maxDiff = None
        test_list = {}
        minima_list = {}
        ref = {}
        self.disc.CountMin()
        self.disc.CountTS()
        self.disc.RemoveThreshold()
        self.disc.RemoveUnderConnect()
        self.disc.RemoveDisjoint()
        #print self.disc.minima_index['GM']
        self.disc.InitialiseBasin()
        self.disc.AssignBasins()
        self.disc.PruneBasins()
        self.disc.ReNumberBasins()
        for lines in open('min_in_basin_parent'):
            m = int(lines.split()[0])
            l = int(lines.split()[1])
            b = int(lines.split()[2])
            if b != 0:
                try:
                    test_list[l][m] = b
                except KeyError:
                    test_list[l] = {}
                    test_list[l][m] = b
        for l in self.disc.basin_index['Level']:
            minima_list[l] = {}
        for m in self.disc.minima_index['Index']:
            for l in self.disc.basin_index['Level']:
                if self.disc.minima_index['Index'][m]['Basin']['Level'][l]:
                    minima_list[l][m] = self.disc.minima_index['Index'][m]['Basin']['Level'][l]
                
                #minima_list[l][b] = self.disc.basin_index['Level'][l]['Basin'][b]['Min']
        for l in self.disc.basin_index['Level']:
            ref[l] = {}
            for m in test_list[l]:
                ref[l][m] = {}
                b = test_list[l][m]
                if minima_list[l][m]:
                    ref[l][m] = {b:minima_list[l][m]}
                if not ref[l][m][b]: print 'FUCKING HELL!'
#        print minima_list[3]
        #print len(minima_list), len(test_list), 'len'
#        print ref[3], len(ref[3])
#        print test_list[3]
        
        
        self.assertDictEqual(minima_list, test_list)


    def testBasinContent(self):
        '''
        Tests if object basin_dict['Level'][l]['Node'][n]
        contains the correct list of minima.
        '''
        test_list = {}
        minima_list = {}
        self.disc.CountMin()
        self.disc.CountTS()
        self.disc.RemoveThreshold()
        self.disc.RemoveUnderConnect()
        self.disc.RemoveDisjoint()
        #print self.disc.minima_index['GM']
        self.disc.InitialiseBasin()
        self.disc.AssignBasins()
        self.disc.PruneBasins()
        #self.disc.ReNumberBasins()
        for lines in open('min_in_basin'):
            m = int(lines.split()[0])
            l = int(lines.split()[1])
            b = int(lines.split()[2])
            if b != 0:
                try:
                    test_list[l][b].append(m)
                except KeyError:
                    try:
                        test_list[l][b] = [m]
                    except KeyError:
                        test_list[l] = {}
                        test_list[l][b] = [m]
            for l in self.disc.basin_index['Level']:
                minima_list[l] = {}
                for b in self.disc.basin_index['Level'][l]['Basin']:
                
                    minima_list[l][b] = self.disc.basin_index['Level'][l]['Basin'][b]['Min']
        
        self.assertDictEqual(minima_list, test_list)

    def testParents(self):
        self.maxDiff = None
        test_list = {}
        minima_list = {}
        parent_list = {}
        pmin_list = {}
        ref = {}
        self.disc.CountMin()
        self.disc.CountTS()
        self.disc.RemoveThreshold()
        self.disc.RemoveUnderConnect()
        self.disc.RemoveDisjoint()
        self.disc.InitialiseBasin()
        self.disc.AssignBasins()
        self.disc.PruneBasins()
        self.disc.ReNumberBasins()
        self.disc.GetParentsAndChildren()
        for lines in open('min_in_basin_parent'):
            m = int(lines.split()[0])
            l = int(lines.split()[1])
            b = int(lines.split()[2])
            p = int(lines.split()[3])
            #c = int(lines.split()[4])
            if b != 0:
                try:
                    test_list[l][b].append(m)
                except KeyError:
                    try:
                        test_list[l][b] = [m]
                    except KeyError:
                        test_list[l] = {}
                        test_list[l][b] = [m]
                    #test_list[l][b]
            if l != 1 and b != 0:
                try:
                    parent_list[l][b] = p
                except KeyError:
                    parent_list[l] = {}
                    parent_list[l][b] = p
            
        for l in self.disc.basin_index['Level']:
            minima_list[l] = {}
            for b in self.disc.basin_index['Level'][l]['Basin']:
                
                minima_list[l][b] = self.disc.basin_index['Level'][l]['Basin'][b]['Min']
            if l !=1:
                pmin_list[l] = {}
                for b in self.disc.basin_index['Level'][l]['Basin']:
                    pmin_list[l][b] = self.disc.basin_index['Level'][l]['Basin'][b]['Parents']
        # Re-number test list to correspond to minima_list
        
        #print self.disc.basin_index['Level']
        #print self.disc.basin_index['Level'][2]['Basin'][13]
        #print 'python'
        #print minima_list[2]
        #print 'FORTRAN'
        #print test_list[2]
        #
        #print 'python'
        #print minima_list[3]
        #print 'FORTRAN'
        #print test_list[3]

        #print parent_list
        #print pmin_list
        self.assertDictEqual(pmin_list, parent_list)

    def testChildren(self):
        self.maxDiff = None
        test_list = {}
        minima_list = {}
        children_list = {}
        pmin_list = {}
        ref = {}
        self.disc.CountMin()
        self.disc.CountTS()
        self.disc.RemoveThreshold()
        self.disc.RemoveUnderConnect()
        self.disc.RemoveDisjoint()
        self.disc.InitialiseBasin()
        self.disc.AssignBasins()
        self.disc.PruneBasins()
        self.disc.ReNumberBasins()
        self.disc.GgetParentsAndChildren()
        for lines in open('min_in_basin_parent'):
            m = int(lines.split()[0])
            l = int(lines.split()[1])
            b = int(lines.split()[2])
            p = int(lines.split()[3])
            #c = int(lines.split()[4])
            if b != 0:
                try:
                    test_list[l][b].append(m)
                except KeyError:
                    try:
                        test_list[l][b] = [m]
                    except KeyError:
                        test_list[l] = {}
                        test_list[l][b] = [m]
                    #test_list[l][b]
            if l != 1 and b != 0:
                try:
                    children_list[l-1][p].append(b)
                except KeyError:
                    try:
                        children_list[l-1][p] = [b]
                    except KeyError:
                        children_list[l-1] = {}
                        children_list[l-1][p] = [b]
        # Remove duplicates from childre_list
        for l in children_list:
            for p in children_list[l]:
                #print 'hello',children_list[l][p]
                children_list[l][p] = list(set(children_list[l][p]))
                #print 'hello again', children_list[l][p]
                #list(children_list[l][p])
                #print 'hello hello again', children_list[l][p]
        for l in self.disc.basin_index['Level']:
            minima_list[l] = {}
            for b in self.disc.basin_index['Level'][l]['Basin']:
                
                minima_list[l][b] = self.disc.basin_index['Level'][l]['Basin'][b]['Min']
            if l !=self.disc.kw.levels['n']:
                pmin_list[l] = {}
                for b in self.disc.basin_index['Level'][l]['Basin']:
                    c = self.disc.basin_index['Level'][l]['Basin'][b]['Children']
                    if c:
                        pmin_list[l][b] = c
        # Re-number test list to correspond to minima_list
        
        #print self.disc.basin_index['Level']
        #print self.disc.basin_index['Level'][2]['Basin'][13]
        #print 'python'
        #print minima_list[2]
        #print 'FORTRAN'
        #print test_list[2]
        #
        #print 'python'
        #print minima_list[3]
        #print 'FORTRAN'
        #print test_list[3]
        #print self.disc.basin_index['Level'][8]['Basin']
#        print children_list
#        print pmin_list
        
        #print parent_list
        #print pmin_list
        self.assertDictEqual(pmin_list, children_list)


    
if __name__ == '__main__': unittest.main()

