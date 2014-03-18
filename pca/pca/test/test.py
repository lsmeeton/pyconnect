#!/usr/bin/env python

import unittest
import numpy as np
from PCAinit import PCAinit
from PCAprep import MyPCAprep
from PCA import PCA
import matplotlib.pyplot as plt

class ProductTestCase(unittest.TestCase):
    
    def setUp(self):
        '''
        The following data is for the BLN 46 model protein with the minimaindex
        file "~/workspace/PrincipalComponentAnalysis/minimaindex"
        '''
        self.n_min = 1342
        self.n_atoms = 46
        self.total_min = 132356
        self.cartesianIn_shape = (self.n_min,3,self.n_atoms)
        self.initial_shape = 3*self.n_min*self.n_atoms
        self.cartesianOut_shape = (self.n_min,3*self.n_atoms)
        self.dihedralIn_shape = (self.n_min,2*(self.n_atoms - 3))
        
    def testCountLines(self):
        pca = PCAinit()
        #n_min = 132356 # For BLN46 model
        pca.CountLines()
        self.assertEqual(pca.total_min, self.total_min, 
                         'Minima not counted correctly %d %d'
                         %(pca.total_min, self.total_min))
        
    def testCheckBasis(self):

        pca = PCAinit()
        pca.CheckFiles()
        pca.basis = 'nothing'
        with self.assertRaises(RuntimeError):
            pca.BasisCheck()
            
    def testReadMinimaIndex(self):
        pca = PCAinit()
        pca.CheckFiles()
        pca.ReadMinimaIndex()
        
        self.assertEqual(pca.n_min, self.n_min, 
                         "n_min: %d pca.n_min: %d"%(self.n_min, pca.n_min))
        
    def testCheckShapeCorrect(self):
        pca = PCAinit()
        pca.CheckFiles()
        pca.ReadMinimaIndex()
        pca.ReadConfigurationSpace()
        pca.ShapeCheck()
        
        self.assertEqual(pca.shape[0], self.initial_shape, 'Shapes are not equivalent!')

    def testReshapeCartesianIn(self):
        pca = MyPCAprep()
        pca.CheckFiles()
        pca.ReadMinimaIndex()
        pca.ReadConfigurationSpace()
        pca.ShapeCheck()
        pca.basis = 'cartesian'
        
        pca.ReshapeCartesianIn()
        
        self.assertEqual(pca.shape
                         , self.cartesianIn_shape,
                          'Shapes are not equivalent!')
        
    def testReshapeCartesianOut(self):
        pca = MyPCAprep()
        pca.CheckFiles()
        pca.ReadMinimaIndex()
        pca.ReadConfigurationSpace()
        pca.ShapeCheck()
        pca.basis = 'cartesian'
        
        pca.ReshapeCartesianIn()
        pca.ReshapeCartesianOut()
        
        self.assertEqual(pca.shape
                         , self.cartesianOut_shape,
                          'Shapes are not equivalent!')
        
    def testReshapeDihedralIn(self):
        pca = MyPCAprep()
        pca.CheckFiles()
        pca.ReadMinimaIndex()
        pca.ReadConfigurationSpace()
        pca.ShapeCheck()
        pca.basis = 'dihedral'
        
        pca.ReshapeDihedral()
        
        self.assertEqual(pca.shape
                         , self.dihedralIn_shape,
                          'Shapes are not equivalent!')
        
    def testConvertDihedrals(self):
        pca = MyPCAprep()
        pca.n_min = 1
        pca.n_atoms = 12
        pca.config_space = np.zeros([pca.n_min,3,pca.n_atoms])

        pca.config_space[0,:,0] = np.array([ 4.5862716,   3.7086541,   2.4275676])
        pca.config_space[0,:,1] = np.array([5.6076207 ,  4.4390252 ,  1.7038494])
        pca.config_space[0,:,2] = np.array([5.2474499 ,  4.5191011 ,  0.2272263])
        pca.config_space[0,:,3] = np.array([6.1437839 ,  5.0894422 , -0.5825547])
        pca.config_space[0,:,4] = np.array([7.3934538 ,  5.6393035 , -0.0931457])
        pca.config_space[0,:,5] = np.array([8.3803778 ,  4.5086754 ,  0.1622337])
        pca.config_space[0,:,6] = np.array([9.6440337 ,  4.8463342 ,  0.4360941])
        pca.config_space[0,:,7] = np.array([10.1171163,   6.2173446,   0.4407248])
        pca.config_space[0,:,8] = np.array([ 9.8283835,   6.8604155,   1.7905271])
        pca.config_space[0,:,9] = np.array([10.7365668,   7.7223495,   2.2563117])
        pca.config_space[0,:,10]= np.array([11.9815505,   7.9891157,   1.5628325])
        pca.config_space[0,:,11]= np.array([11.8350792,   9.2428067,   0.7127979])
 

        pca.basis = 'dihedral'
        
        print pca.config_space  
        
        dihedrals = np.array([np.cos(np.deg2rad(-175.697530)),
                              np.sin(np.deg2rad(-175.697530)),
                              np.cos(np.deg2rad(-1.351148)),
                              np.sin(np.deg2rad(-1.351148)),
                              np.cos(np.deg2rad(78.228332)),
                              np.sin(np.deg2rad(78.228332)),
                              np.cos(np.deg2rad(170.764306)),
                              np.sin(np.deg2rad(170.764306)),
                              np.cos(np.deg2rad(-2.804173)),
                              np.sin(np.deg2rad(-2.804173)),
                              np.cos(np.deg2rad(84.357578)),
                              np.sin(np.deg2rad(84.357578)),
                              np.cos(np.deg2rad(144.048356)),
                              np.sin(np.deg2rad(144.048356)),
                              np.cos(np.deg2rad(-3.486604)),
                              np.sin(np.deg2rad(-3.486604)),
                              np.cos(np.deg2rad(96.591853)),
                              np.sin(np.deg2rad(96.591853)),])
        
        pca.ConvertDihedrals()
        
        
        for i in range(18):
            self.assertAlmostEqual(dihedrals[i], pca.config_space[0][i], places=7)
        
        
    def testPCAorthog(self):
        '''
        Test to ensure that I understand how PCA works and how it is implemented
        in numpy.
        '''
        pca = PCA()
        
        mux, sigmax = 30, 1
        muy, sigmay = 20, 0.5
        
        xprime = (sigmax * np.random.normal(loc=0,size=10000)) #mux
        yprime = (sigmay * np.random.normal(loc=0,size=10000)) # muy
        
        for i in range(10):
            
            theta = float(i)*0.2*np.pi#np.random.random()*2.0*np.pi
            print 'theta', theta
#        mux, sigmax = 30, 1
#        muy, sigmay = 20, 0.5
#        theta = -1.0*np.pi/3.0
#
#        xprime = (sigmax * np.random.normal(loc=0,size=10000)) #mux
#        yprime = (sigmay * np.random.normal(loc=0,size=10000)) # muy
            x = np.cos(theta)*xprime - np.sin(theta)*yprime
            y = np.sin(theta)*xprime + np.cos(theta)*yprime
            
#            analytic_PCs = np.array([[np.cos(theta) - np.sin(theta),
#                                     np.sin(theta) + np.cos(theta)],
#                                   [np.sin(theta) + np.cos(theta),
#                                    np.sin(theta) - np.cos(theta)]])
            analytic_PCs = np.array([[np.cos(theta),
                                      np.sin(theta)],
                                     [np.sin(theta) ,
                                      -np.cos(theta)]])
            
#        
            x += mux
            y += muy
#
#        
            pca.config_space = np.array([x,y]).swapaxes(0,1)
#        cov = np.cov(pca.config_space.T)
#        eigh =  np.linalg.eigh(cov)
            fig = plt.figure()
            ax = fig.add_subplot(1,1,1)
    #        
            ax.scatter(pca.config_space[:,0],pca.config_space[:,1],c=xprime, alpha = 0.2, marker = 'o')
#        
            pca.runPCA()
            print 'analytic\n',analytic_PCs
            print 'calc\n', pca.PCs, '\n', np.dot(pca.PCs[0][:],pca.PCs[1][:])
#        print 'pca.config_mu', pca.config_mu
#        print 'pca.config_sigma', pca.config_sigma
#        print 'pca.PCs', pca.PCs
#        print 'pca.PCs[1]', pca.PCs[1]
#        print 'pca.PCs[0]', pca.PCs[0]
#        print 'cov', pca.cov
#        print 'variance', pca.variance
#        print 'std', pca.std
#        
#        
#        ax.scatter(pca.Y[:,0], pca.Y[:,1], alpha = 0.2, c=xprime, marker = 'o')
#        
            plt.plot([pca.config_mu[0], pca.config_mu[0] + (pca.PCs[0][0])*5.0*pca.std[0]]
                     ,[pca.config_mu[1], pca.config_mu[1] + (pca.PCs[0][1])*5.0*pca.std[0]], 
                     c = 'red')
            plt.plot([pca.config_mu[0], pca.config_mu[0] + (pca.PCs[1][0])*5.0*pca.std[1]]
                     ,[pca.config_mu[1], pca.config_mu[1] + (pca.PCs[1][1])*5.0*pca.std[1]], 
                     c = 'blue')
            ax.grid(True)
               
            
    
            #plt.show()
            
            self.assertAlmostEqual(np.dot(pca.PCs[0][:],pca.PCs[1][:]), 0.0, places=2, )
            
            
    def testPCAanalytic(self):
        '''
        Test to ensure that I understand how PCA works and how it is implemented
        in numpy.
        '''
        pca = PCA()
        
        mux, sigmax = 30, 1
        muy, sigmay = 20, 0.5
        
        xprime = (sigmax * np.random.normal(loc=0,size=10000)) #mux
        yprime = (sigmay * np.random.normal(loc=0,size=10000)) # muy
        
        for i in range(10):
            
            theta = np.random.random()*2.0*np.pi
            print 'theta', theta
#        mux, sigmax = 30, 1
#        muy, sigmay = 20, 0.5
#        theta = -1.0*np.pi/3.0
#
#        xprime = (sigmax * np.random.normal(loc=0,size=10000)) #mux
#        yprime = (sigmay * np.random.normal(loc=0,size=10000)) # muy
            x = np.cos(theta)*xprime - np.sin(theta)*yprime
            y = np.sin(theta)*xprime + np.cos(theta)*yprime
            
#            analytic_PCs = np.array([[np.cos(theta) - np.sin(theta),
#                                     np.sin(theta) + np.cos(theta)],
#                                   [np.sin(theta) + np.cos(theta),
#                                    np.sin(theta) - np.cos(theta)]])
            analytic_PCs = np.array([[-np.cos(theta),
                                      -np.sin(theta)],
                                     [np.sin(theta) ,
                                      -np.cos(theta)]])
            
#        
            x += mux
            y += muy
#
#        
            pca.config_space = np.array([x,y]).swapaxes(0,1)
#        cov = np.cov(pca.config_space.T)
#        eigh =  np.linalg.eigh(cov)
            fig = plt.figure()
            ax = fig.add_subplot(1,1,1)
    #        
            ax.scatter(pca.config_space[:,0],pca.config_space[:,1],c=xprime, alpha = 0.2, marker = 'o')
#        
            pca.runPCA()
            print 'analytic\n',analytic_PCs
            print 'calc\n', pca.PCs, '\n', np.dot(pca.PCs[0][:],pca.PCs[1][:])
#        print 'pca.config_mu', pca.config_mu
#        print 'pca.config_sigma', pca.config_sigma
#        print 'pca.PCs', pca.PCs
#        print 'pca.PCs[1]', pca.PCs[1]
#        print 'pca.PCs[0]', pca.PCs[0]
#        print 'cov', pca.cov
#        print 'variance', pca.variance
#        print 'std', pca.std
#        
#        
            ax.scatter(pca.Y[:,0], pca.Y[:,1], alpha = 0.2, c=xprime, marker = 'o')
#        
            plt.plot([pca.config_mu[0], pca.config_mu[0] + (pca.PCs[0][0])*5.0*pca.std[0]]
                     ,[pca.config_mu[1], pca.config_mu[1] + (pca.PCs[0][1])*5.0*pca.std[0]], 
                     c = 'red')
            plt.plot([pca.config_mu[0], pca.config_mu[0] + (pca.PCs[1][0])*5.0*pca.std[1]]
                     ,[pca.config_mu[1], pca.config_mu[1] + (pca.PCs[1][1])*5.0*pca.std[1]], 
                     c = 'blue')
            ax.grid(True)
               
            
    
#            plt.show()
            for l in range(2):
                for j in range(2):
                    
                    self.assertAlmostEqual(np.abs(pca.PCs[l][j]), 
                                           np.abs(analytic_PCs[l][j]), 
                                           delta=0.051,
                                           msg = 'l: %d j: %d pca: %1.3f analytic: %1.3f'
                                           %(l,j,pca.PCs[l][j], analytic_PCs[l][j]) )
                    
if __name__ == '__main__': unittest.main()
