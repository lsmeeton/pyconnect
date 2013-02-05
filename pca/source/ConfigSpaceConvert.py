#!/usr/bin/env python

from PCAprint import PCAprint
#from PCA import PCA
#from PCAprep import MyPCAprep
#from PCAinit import PCAinit
from KeywordInit import Keyword

if __name__ == '__main__':
    kw = Keyword()
    pca = PCAprint(kw)
    
    pca.DirectoryCheck()
    pca.BasisCheck()
    pca.ShapeCheck()
    pca.ReadMinimaIndex()
    pca.ReadConfigurationSpace()
#    print pca.kw.beta
    if pca.kw.beta:
        pca.ReadMinimaEnergies()   
        pca.FindGM()
        pca.CalculateWeighting()
#    pca.basis = 'dihedral'
    # Convert config_space to selected basis set
    if pca.kw.basis == 'cartesian':
        pca.ReshapeCartesianIn()
        pca.config_space_copy = pca.config_space.copy()
        pca.LstSqrStructureFit()
        pca.ReshapeCartesianOut()

    if pca.kw.basis == 'dihedral':
        print 'Converting to Internal Dihedrals'
        pca.ReshapeDihedral()
        print 'Done'
    
pca.SaveConfigSpace()
#    pca.runPCA()
#    pca.PrintPCACartesianCoords()
#    pca.PrintPCAProjections()
#    pca.PrintPCAVariance()
#    pca.SavePCMatrix()
#    pca.SaveEnsembleAverage()
#    pca.SaveMeanSigma()
#    pc1 = pca.ReshapeVector(pca.PCs[0])
#    print pca.PrintStructureXYZ(pc1)
#    pca.PrintPCMatrix()
#    print pca.PCs[0,1:3]