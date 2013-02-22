#!/usr/bin/env python

from PCA import PCA
import numpy as np
#import matplotlib.pyplot as plt
import sys
import os

__metaclass__ = type

class PCAprint(PCA):
    '''
    PCAprint contains all the functions and methods for saving data to from the
    PCA to disk.
    '''
    def __init__(self,*args):
        super(PCAprint, self).__init__(*args)

        
        #self.

    def PrintPCACartesianCoords(self):
        '''
        Writes the principal components to file. Each PC, PC1, PC2, ..., PC"N" 
        is written to file "PC_coords1", "PC_coords2" etc in an xyz format 
        of co-ordinates. 
        Components are each written to a seperate file, which are stored within 
        their own directory.
        '''
        try: os.mkdir(self.kw.PC_coords_dir_name)
        except OSError: sys.exit('WARNING: directory "%s" already exists - \nAborting calculation'
                                 %self.kw.PC_coords_dir_name)
        
        for i in range(self.numPCs):
            f = open(self.kw.PC_coords_dir_name + '/' + self.kw.PC_coords_dir_name +
                     '%d'%(i+1), 'w')
            f.write('# Principal Component: %d\n# Fraction of Variance Captured: %2.5f\n'
                    %(i+1,self.norm_variance[i]))
            for j in range(0, 3*(self.kw.n_atoms),3):
                f.write('%2.6f\t%2.6f\t%2.6f\n'
                        %(self.PCs[i,j], self.PCs[i,j+1], self.PCs[i,j+2]))
            f.close()
                
    def PrintPCAProjections(self):
        '''
        Writes the projection of each minima in the data set onto each principal
        component, PC1, PC2, .. in the file "PC_projections1", "PC_projections2"
        in a two coloumn format: minima    value
        '''
        try: os.mkdir(self.kw.PC_project_dir_name)
        except OSError: sys.exit('WARNING: directory "%s" already exists - \nAborting calculation'
                                 %self.kw.PC_project_dir_name)
        
        
        for i in range(self.numPCs):
            f = open(self.kw.PC_project_dir_name + '/' + self.kw.PC_project_dir_name +
                     '%d'%(i+1), 'w')
            f.write('# %d minima projected onto PC%d\n# Min\tPC\n'%
                    (self.n_min, (i+1)))
            for j in range(self.n_min):
                f.write('%d\t%1.6f\n'%(self.min_index[j],self.Y[j,i]))
            f.close()

    def PrintPCAVariance(self):
        '''
        Writes the mean, variance and cumulative variance of each PC to the file
        "PC_variance".
        '''
        
        for i in range(self.numPCs):
            f = open(self.kw.PC_variance, 'w')
            f.write('No. of Atoms: %i,\t No. of Structures in min.data: %i,\t No. of Structures used to Calculate PCA: %i\n'
                    %(self.kw.n_atoms, self.total_min, len(self.min_index)))
            for i in range(self.numPCs):
                f.write('PCA No. %i,\t mean = %4.6g,\t std dev = %4.6g,\t frac = %1.6g,\t cum. frac = %1.6g\n'
                        %(i+1, self.config_mu[i], self.config_sigma[i], 
                          self.norm_variance[i], self.cum_variance[i]))
            f.close()
    
    def SavePCMatrix(self):
        '''
        Save self.PCs as a .npy file called "PCs.npy"
        '''
        np.save(self.kw.PC_coords_dir_name + '/PCs.npy', self.PCs)
        
    def SaveEnsembleAverage(self):
        '''
        save self.ensemble_average as a .npy file called "ensemble.structure.npy"
        '''
        np.save(self.kw.PC_coords_dir_name + 
                '/ensemble.structure.npy', self.ensemble_average)
        
    def SaveMeanSigma(self):
        '''
        save self.config_mu and self.config_sigma as a .npx file called "mu_sigma.npz"
        '''
        np.savez(self.kw.PC_coords_dir_name +
                 '/mu_sigma.npz', self.config_mu, self.config_sigma)
        
    def SaveConfigSpace(self):
        '''
        save self.config_space as a .npy file called "config_space.npy"
        '''
        np.save('config_space.npy',self.config_space)
        
    def PrintStructureXYZ(self,structure):
        '''
        Takes as input a 2D [3][n_atoms] np array and converts it to a string 
        for printing or writing to file.
        '''
        m, n = structure.shape
        if m == 3 and n == self.kw.n_atoms:
            structure = np.swapaxes(structure,0,1)
            structure_str = np.array_str(structure, precision=6)
            structure_str = structure_str.replace('[[', ' ')
            structure_str = structure_str.replace('[', '')
            structure_str = structure_str.replace(']', '')
            
            return structure_str
        
            
    def PrintPCMatrix(self):
        '''
        Print self.PCs as a matrix
        '''
        f = open('self.PCsMatrix','w')
        m,n = np.shape(self.PCs)
        for i in range(m):
            for j in range(n):
                f.write('%2.6f\t'%self.PCs[i,j])
            f.write('\n')
        f.close()