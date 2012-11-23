#!/usr/bin/env python

from PCAprint import PCAprint
from PCA import PCA
from PCAprep import MyPCAprep
from PCAinit import PCAinit
from KeywordInit import Keyword

if __name__ == '__main__':
    kw = Keyword()
    pca = PCAprint(kw)
    
    pca.CheckFiles()
    pca.BasisCheck()
    pca.ShapeCheck()
    pca.ReadMinimaIndex()
    pca.ReadConfigurationSpace()
    print pca.kw.beta
    if pca.kw.beta:
        pca.ReadMinimaEnergies()   
        pca.FindGM()
        pca.CalculateWeighting()
#    pca.basis = 'dihedral'
    # Convert config_space to selected basis set
    if pca.kw.basis == 'cartesian':
        pca.ReshapeCartesianIn()
        pca.LstSqrStructureFit()
        pca.ReshapeCartesianOut()

    if pca.kw.basis == 'dihedral':
        print 'Converting to Internal Dihedrals'
        pca.ReshapeDihedral()
        print 'Done'
    
    pca.runPCA()
    pca.PrintPCACartesianCoords()
    pca.PrintPCAProjections()
    pca.PrintPCAVariance()
