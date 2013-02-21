import cPickle
import numpy as np
import sys
sys.path.append('/home/lewis/git/pyconnect/PyConnect')
#import MatplotlibGUI
#import MatplotlibGUI
f = open('minima.obj','r')
minima = cPickle.load(f)
f.close()

f = open('basin.obj','r')
basin = cPickle.load(f)
f.close()

#f = open('disc.obj','r')
#disc = cPickle.load(f)
#f.close()

