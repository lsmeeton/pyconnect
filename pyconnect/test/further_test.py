#!/usr/bin/env python

import unittest
from Disconnect import Disconnect
from DisconnectPlot import DisconnectPlot
from KeywordInit import Keywords


class ProductTestCase(unittest.TestCase):

    def setUp(self):
        kw = Keywords()
        self.disc = DisconnectPlot(kw)

    def testParents(self):
        '''
        Tests whether the parents of each basin are accurately
        identified, whilst accounting for differences in the numbering
        of the basins in the FORTRAN and python codes.
        '''

        
