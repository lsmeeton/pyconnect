#!/usr/bin/env python
'''
Written by Lewis Smeeton, 2012
'''


import numpy as np
from PCAprep import MyPCAprep
#----------------------------------------------#
__metaclass__ = type


class PCA(MyPCAprep):
    def __init__(self,*args):
        super(PCA, self).__init__(*args)
        """
        compute the SVD of self.config_space and store data for PCA.  Use 
        project to project the data onto a reduced set of dimensions

        Inputs:

          *self.config_space*: a numobservations x numdims array
          
          *self.weight_array*: array containing the boltzmann weighting of each 
                               structure
    
        Attrs:

          *a* a centered unit sigma version of input a

          *c* a centred, weighted version of input a

          *numrows*, *numcols*: the dimensions of a

          *mu* : a numdims array of means of a

          *sigma* : a numdims array of atandard deviation of a

          *fracs* : the proportion of variance of each of the principal components

          *PCs* : the weight vector for projecting a numdims point or array into PCA space

          *Y* : a projected into PCA space


        The factor loadings are in the Wt factor, ie the factor
        loadings for the 1st principal component are given by Wt[0]

        """
#        self.runPCA()
        return
    
    def runPCA(self):
        

        n, m = self.config_space.shape
        if n<m:
            raise RuntimeError('we assume data in a is organised with numrows>numcols')

        self.numrows, self.numPCs = n, m
             
        # Calculate weighted mean
        self.CalcConfigData()
        
        # Centre c on its mean, and scale by its standard deviation
        self.config_space = self.center(self.config_space)
     
        # Calculate the covariance matrix, self.cov
        self.CalcCov()
        
        # Calculate the eigenvalues self.variance, and eigenvalues self.PCs, of 
        # self.cov
        self.variance, self.PCs = np.linalg.eigh(self.cov)
        
        # Note, temp_* is because vectors are ordered in c differently than they are in the inbuilt PCA solver
        # Therefore, for the sake of consistency with previously written code, one must re-organise temp_PCs
        #
        #  (||||)    (---1st PC---)
        #  (|"21) ==>(---2nd PC---)
        #  (|"PP)    (---"    "---)
        #  (||||)    (---LastPC---)
        #
        # They also point in opposite direction, so we have to multiply by -1.0
        self.Reorder()
        
        # Normalise variance
        self.Normalise()
        
        self.Y = np.dot(self.PCs, self.config_space.T).T
 
        self.Wt = self.PCs

    def Reorder(self):
        '''
        Re-order self.PCs and self.variance to make consistent with legacy code
        '''
        # Order temp_variance from largest ---> smallest
        PC_index_order = np.argsort(self.variance[::-1]) 
        # Initialise variance vector
        temp_variance = self.variance[PC_index_order[0]]
        # Initialise PCs vector
        temp_PCs = self.PCs[:,PC_index_order[0]].reshape(1,self.numPCs)
        for i in range( 1, self.numPCs):
            
            temp_variance = np.append(temp_variance,self.variance[PC_index_order[i]])
            temp_PCs = np.vstack((temp_PCs, self.PCs[:,PC_index_order[i]].reshape(1,self.numPCs)))
        
        self.variance = temp_variance
        self.PCs = temp_PCs
        
    def CalcConfigData(self):
        '''
        Calculates the mean, self.config_mu, and standard deviation, 
        self.config_sigma, of each row (degree of freedom) in self.config_space.
        '''
        mean = np.zeros(self.numPCs)
        
        if self.kw.beta:
            for i in range(self.numrows):
                mean = mean + self.weight_array[i]*self.config_space[i,:]
            
            mean = mean / sum(self.weight_array)
        
            self.config_mu = mean
            
            std = np.zeros(self.numPCs)
            for i in range(self.numrows):
                std = std + self.weight_array[i]*((self.config_space[i,:] 
                                                   - mean)**2) 
            std = np.sqrt(std / sum(self.weight_array))
            self.config_sigma = std
        
        else:
            self.config_mu = self.config_space.mean(axis=0)
            self.config_sigma = self.config_space.std(axis=0)
                
    def CalcCov(self):
        '''
        Calculates the covariance matrix, self.cov, of self.config_space
        '''
        # Create the Covariance matrix, self.cov
        # First, initialise self.cov
        self.cov = np.zeros([self.numrows,self.numrows])
        # Then add to self.cov the vectors in self.config_space, multiplied by their 
        # corresponding weights from self.weights_array (if applicable)
        if self.kw.beta:
            for i in range(self.numrows):
                self.cov += self.weight_array[i]*np.dot(np.matrix(self.config_space[i,:]).T,
                                                   np.matrix(self.config_space[i,:]))
        else:

            self.cov = np.cov(self.config_space.T)
           
    def Normalise(self):
        '''
        Calculates the following PC properties:
        Standard Deviation: self.std
        Normalised variance: self.norm_variance
        Normalised Standard Deviation: self.norm_std
        Cumulative Normalised variance: self.cum_variance
        '''
        
        self.norm_variance = self.variance / sum(self.variance)
        self.std = self.norm_std = np.zeros(len(self.variance))
        for i in range(self.numPCs):
            if self.variance[i] >= 0: self.std[i] = np.sqrt(self.variance[i])
            else:
                print 'WARNING: variance of PC %d of %d is < 0, calculating sqrt of its modulus'%(i+1,self.numPCs)
                self.std[i] = np.sqrt(np.abs(self.variance[i]))

        self.norm_std = np.sqrt(self.std)/sum(self.std)
        
        self.cum_variance = np.cumsum(self.norm_variance)

        
    def project(self, x, minfrac=0.):
        '''
        project x onto the principle axes, dropping any axes where fraction of variance<minfrac
        '''
        x = np.asarray(x)

        ndims = len(x.shape)

        if (x.shape[-1]!=self.numPCs):
            raise ValueError('Expected an array with dims[-1]==%d'%self.numPCs)


        Y = np.dot(self.PCs, self.center(x).T).T
        mask = self.fracs>=minfrac
        if ndims==2:
            Yreduced = Y[:,mask]
        else:
            Yreduced = Y[mask]
        return Yreduced

    def center(self, x):
        '''
        center the data using the mean
        '''
        return (x - self.config_mu)/self.config_sigma

    @staticmethod
    def _get_colinear():
        c0 = np.array([
            0.19294738,  0.6202667 ,  0.45962655,  0.07608613,  0.135818  ,
            0.83580842,  0.07218851,  0.48318321,  0.84472463,  0.18348462,
            0.81585306,  0.96923926,  0.12835919,  0.35075355,  0.15807861,
            0.837437  ,  0.10824303,  0.1723387 ,  0.43926494,  0.83705486])

        c1 = np.array([
            -1.17705601, -0.513883  , -0.26614584,  0.88067144,  1.00474954,
            -1.1616545 ,  0.0266109 ,  0.38227157,  1.80489433,  0.21472396,
            -1.41920399, -2.08158544, -0.10559009,  1.68999268,  0.34847107,
            -0.4685737 ,  1.23980423, -0.14638744, -0.35907697,  0.22442616])

        c2 = c0 + 2*c1
        c3 = -3*c0 + 4*c1
        a = np.array([c3, c0, c1, c2]).T
        return a

#===========================================================#