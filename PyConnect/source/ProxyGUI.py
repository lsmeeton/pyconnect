import wx
import numpy as np

from wx.glcanvas import GLCanvas
from OpenGL.GLUT import *
from OpenGL.GLU import *
from OpenGL.GL import *

__metaclass__ = type

class MyGLCanvas(GLCanvas):
    def __init__(self,parent, *args):
        super(MyGLCanvas, self).__init__(parent, attribList=[wx.glcanvas.WX_GL_DOUBLEBUFFER])
        self.lastx = self.x = 30
        self.lasty = self.y = 30
        self.size = None
        self.parent = parent
        self.MouseRotate = False
        wx.EVT_PAINT(self, self.OnPaint)
        wx.EVT_SIZE(self, self.OnSize)
        
        wx.EVT_LEFT_DOWN(self, self.OnMouseDown)
        wx.EVT_LEFT_UP(self, self.OnMouseUp)
        #wx.EVT_BUTTON(self, )
        wx.EVT_WINDOW_DESTROY(self, self.OnDestroy)
        
        self.init = True

    #ef ColourCheck(self, allow):
    #   if allow:
    #       self.red = 1.0
    #   else:
    #       self.red = 0.0
    #-----------------------------------------------------------------------------------------------
    def OnPaint(self, event):
        dc = wx.PaintDC(self)
        self.SetCurrent()
        if not self.init:
            self.InitGL()
            self.init = True
        self.OnDraw()

    #-----------------------------------------------------------------------------------------------        
    def OnSize(self, event):
        size = self.size = self.GetClientSize()
        if self.GetContext():
            self.SetCurrent()
            glViewport(0, 0, size.width, size.height)
        event.Skip()

       
    #-----------------------------------------------------------------------------------------------
    def OnMouseDown(self, evt):
        self.CaptureMouse()
        #print self.CaptureMouse()
        self.x, self.y = self.lastx, self.lasty = evt.GetPosition()
    #-----------------------------------------------------------------------------------------------
    def OnMouseUp(self, evt):
        self.ReleaseMouse()
    
    def OnDestroy(self, event):
        print "Destroying Window"

#====================================================================
class DisconnectCanvas(MyGLCanvas):
    def __init__(self, parent):
        super(DisconnectCanvas, self).__init__(parent)
        self.disc = parent.disc

    def OnDraw(self):
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        glOrtho(-1,1,-1,1,-1,1)
        glMatrixMode(GL_MODELVIEW)
        glLoadIdentity()
    
        glClearColor(1.0,1.0,1.0,0.0)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    
        self.DrawAxes()
        self.LabelEnergyAxes()
    
        self.Plot()
        if self.size is None:
            self.size = self.GetClientSize()
        
            
        self.SwapBuffers()
    

    def DrawAxes(self):
        '''
        Draw axes for DG
        '''
        glBegin(GL_LINES)
        glColor3f(0.0, 0.0, 0.0)
        glVertex2f(-0.55, 0.5)
        glVertex2f(-0.55, -0.5)
        glEnd()


    def LabelEnergyAxes(self):
        '''
        
        '''
    # Calculate no. of levels, and location of dashes
        n = self.disc.kw.levels['n']
        delta = self.disc.kw.delta['dE']
        first = self.disc.kw.first['E1']
        label_sep = 0.1
        tick_sep = 1.0 / (n-1)
        half_tick_width = 0.005
    
        for i in range(n):
            height = 0.5 - tick_sep*i
            glBegin(GL_LINES)
            glColor3f(0.0, 0.0, 0.0)
            glVertex2f(-0.55 - half_tick_width, height)
            glVertex2f(-0.55 + half_tick_width, height)
            glEnd()
            
            glutInit()
            glRasterPos2d(-0.55 - half_tick_width - label_sep, height)
            glColor3f(0,0,0)
            label = first - delta*i
            
            for char in str(label):
                glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, ord(char))
    
    def Plot(self):
        '''

        '''
        for l in self.disc.basin_index['Level']:
            if l == 1: continue
            for b in self.disc.basin_index['Level'][l]['Basin']:
                c = self.disc.basin_index['Level'][l]['Basin'][b]\
                    ['Children']
                p = self.disc.basin_index['Level'][l]['Basin'][b]\
                    ['Parents']
                self.Lines(l,b,c,p)
        
    def Lines(self, l, b, c, p):
        red, green, blue = self.disc.basin_index['Level'][l]['Basin'][b]['RGB']
                
        x1 = self.disc.basin_index['Level'][l-1]['Basin'][p]['X']
        
        z1 = 1.0*(self.disc.basin_index['Level'][l-1]['GLZ'])
        
        x2 = self.disc.basin_index['Level'][l]['Basin'][b]['X']

        if not c:
            z2 = self.disc.basin_index['Level'][l]['Basin'][b]['GLZ']
            
        else:
            z2 = self.disc.basin_index['Level'][l]['GLZ']#+0.5
        
        glBegin(GL_LINES)
        glColor3f(red, green, blue)
        glVertex2f(x1, z1)
        glVertex2f(x2, z2)
        
        glEnd()


class MetricDisconnect2DCanvas(MyGLCanvas):
    def __init__(self, parent):
        super(MetricDisconnect2DCanvas, self).__init__(parent)
        self.disc = parent.disc
    
    def OnDraw(self):
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        glOrtho(-1,1,-1,1,-1,1)
        glMatrixMode(GL_MODELVIEW)
        glLoadIdentity()
        
        #glTranslatef(-0.5*np.sqrt(2.0), -0.5, 0.0)
        #glutInit()
        #glRasterPos2d(0,0)
        #glColor3f(0,0,0)
        #glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24, ord('h'))
        glClearColor(1.0,1.0,1.0,0.0)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        #glutInit()
        #glRasterPos2d(0,0)
        #glColor3f(0,0,0)
        #for char in 'hello':
        #    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_18, ord(char))
        self.DrawAxes()
        self.LabelEnergyAxes()
        self.LabelMetricAxis()
        self.Plot()
        if self.size is None:
            self.size = self.GetClientSize()
            
 
 
        
        self.SwapBuffers()
    

    def DrawAxes(self):
        '''
        Draw axes for MDG
        '''
        glBegin(GL_LINES)
        glColor3f(0.0, 0.0, 0.0)
        glVertex2f(-0.55, 0.5)
        glVertex2f(-0.55, -0.5)

        glVertex2f(-0.5,-0.5)
        glVertex2f(0.5, -0.5)
        glEnd()


    def LabelEnergyAxes(self):
        '''
        
        '''
    # Calculate no. of levels, and location of dashes
        n = self.disc.kw.levels['n']
        delta = self.disc.kw.delta['dE']
        first = self.disc.kw.first['E1']
        label_sep = 0.1
        tick_sep = 1.0 / (n-1)
        half_tick_width = 0.005
    #label_list = []
        for i in range(n):
            height = 0.5 - tick_sep*i
            glBegin(GL_LINES)
            glColor3f(0.0, 0.0, 0.0)
            glVertex2f(-0.55 - half_tick_width, height)
            glVertex2f(-0.55 + half_tick_width, height)
            glEnd()
            
            glutInit()
            glRasterPos2d(-0.55 - half_tick_width - label_sep, height)
            glColor3f(0,0,0)
            label = first - delta*i
            
            for char in str(label):
                glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, ord(char))
    # Place dashes along length of axes

    def LabelMetricAxis(self):
        '''
        
        '''
    # Calculate no. of levels, and location of dashes
        maxX = self.disc.basin_index['MaxX']
        minX = self.disc.basin_index['MinX']
        n = self.disc.basin_index['MetXTickNo']
        delta = (maxX - minX)/n
        
        tick_sep = 1.0/(n)
        label_sep = 0.1
        #tick_sep = 1.0 / (n-1)
        half_tick_width = 0.005
    #label_list = []
        for i in range(n+1):
            x_len = -0.5 + tick_sep*i
            glBegin(GL_LINES)
            glColor3f(0.0, 0.0, 0.0)
            glVertex2f(x_len, -0.5 - half_tick_width)
            glVertex2f(x_len, -0.5 + half_tick_width)
            glEnd()
            
            glutInit()
            glRasterPos2d(x_len,-0.5 - half_tick_width - label_sep)
            glColor3f(0,0,0)
            label = minX + delta*i
            
            for char in str(label):
                glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, ord(char))
    


    def Plot(self):
        '''

        '''
        for l in self.disc.basin_index['Level']:
            if l == 1: continue
            for b in self.disc.basin_index['Level'][l]['Basin']:
                c = self.disc.basin_index['Level'][l]['Basin'][b]\
                    ['Children']
                p = self.disc.basin_index['Level'][l]['Basin'][b]\
                    ['Parents']
                self.Lines(l,b,c,p)
                #print l, b, c, p
                #print self.disc.basin_index

    def Lines(self, l, b, c, p):
        red, green, blue = self.disc.basin_index['Level'][l]['Basin'][b]['RGB']
        #red = self.disc.basin_index['Level'][l]['Basin'][b]['R']
        #green = self.disc.basin_index['Level'][l]['Basin'][b]['G']
        #blue = self.disc.basin_index['Level'][l]['Basin'][b]['B']
        
        
        x1 = self.disc.basin_index['Level'][l-1]['Basin'][p]['GLX']
        
        z1 = 1.0*(self.disc.basin_index['Level'][l-1]['GLZ'])
        
        x2 = self.disc.basin_index['Level'][l]['Basin'][b]['GLX']

        if not c:                                              
            #z2 = self.disc.basin_index['Level'][l]['GLZ']#+0.5
            z2 = self.disc.basin_index['Level'][l]['Basin'][b]['GLZ']

        else:
            z2 = self.disc.basin_index['Level'][l]['GLZ']#+0.5

        glBegin(GL_LINES)
        glColor3f(red, green, blue)
        glVertex2f(x1, z1)
        glVertex2f(x2, z2)
        
        glEnd()

class MetricDisconnect3DCanvas(MyGLCanvas):
    def __init__(self, parent):
        super(MetricDisconnect3DCanvas, self).__init__(parent)
        self.disc = parent.disc
        wx.EVT_MOTION(self, self.OnMouseMotion)


    def OnDraw(self):
        #glFogi(GL_FOG_MODE, GL_EXP)
        #glFogf(GL_FOG_DENSITY, 2.0)
        #glEnable(GL_FOG)
        glClearColor(1.0,1.0,1.0,0.0)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        
        self.WireCube()
        self.Plot()
        self.LabelEnergyAxes()
        self.LabelMetricXAxis()
        self.LabelMetricYAxis()
        if self.size is None:
            self.size = self.GetClientSize()
            
        if self.MouseRotate:
            self.Rotate()
        
        self.SwapBuffers()


    def snapX(self):
        glMatrixMode(GL_PROJECTION)
        glFrustum(-0.5, 0.5, -0.5, 0.5, 0.0, 3.0)

        # position viewer
        glMatrixMode(GL_MODELVIEW)
        glTranslatef(0.0, 0.0, -2.0)

        # position object
        glRotatef(0.0, 1.0, 0.0, 0.0)
        glRotatef(0.0, 0.0, 1.0, 0.0)

        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        glShadeModel(GL_SMOOTH)
    

    def Plot(self):
        '''

        '''
        for l in self.disc.basin_index['Level']:
            if l == 1: continue
            for b in self.disc.basin_index['Level'][l]['Basin']:
                c = self.disc.basin_index['Level'][l]['Basin'][b]\
                    ['Children']
                p = self.disc.basin_index['Level'][l]['Basin'][b]\
                    ['Parents']
                self.Lines(l,b,c,p)
    
    def Lines(self, l, b, c, p):
        red, green, blue = self.disc.basin_index['Level'][l]['Basin'][b]['RGB']
        #green = self.disc.basin_index['Level'][l]['Basin'][b]['G']
        #blue = self.disc.basin_index['Level'][l]['Basin'][b]['B']
        #red = np.random.random()
        #blue = np.random.random()
        #green = np.random.random()
        
        x1 = self.disc.basin_index['Level'][l-1]['Basin'][p]['GLX']
        y1 = self.disc.basin_index['Level'][l-1]['Basin'][p]['GLY']
        z1 = 1.0*(self.disc.basin_index['Level'][l-1]['GLZ'])
        
        x2 = self.disc.basin_index['Level'][l]['Basin'][b]['GLX']
        y2 = self.disc.basin_index['Level'][l]['Basin'][b]['GLY']
        
        if not c:                                              
            #z2 = self.disc.basin_index['Level'][l]['GLZ']#+0.5
            z2 = self.disc.basin_index['Level'][l]['Basin'][b]['GLZ']

        else:
            z2 = self.disc.basin_index['Level'][l]['GLZ']#+0.5

        glBegin(GL_LINES)
        glColor3f(red, green, blue)
        glVertex3f(x1, z1, y1)
        glVertex3f(x2, z2, y2)
        
        glEnd()



    def Rotate(self):
        
        w, h = self.size
        w = max(w, 1.0)
        h = max(h, 1.0)
        xScale = 180.0 / w
        yScale = 180.0 / h
        glRotatef((self.y - self.lasty) * yScale, 1.0, 0.0, 0.0);
        glRotatef((self.x - self.lastx) * xScale, 0.0, 1.0, 0.0);


    def OnMouseMotion(self, evt):
        if evt.Dragging() and evt.LeftIsDown():
            self.MouseRotate = True
            self.lastx, self.lasty = self.x, self.y
            self.x, self.y = evt.GetPosition()
            self.Refresh(False)
        else: self.MouseRotate = False

        
    def InitGL(self):
        '''
        Initialize GL
        '''
       

        # set viewing projection
        glMatrixMode(GL_PROJECTION)
        glFrustum(-0.5, 0.5, -0.5, 0.5, 0.0, 3.0)

        # position viewer
        glMatrixMode(GL_MODELVIEW)
        glTranslatef(0.0, 0.0, -2.0)

        # position object
        glRotatef(self.y, 1.0, 0.0, 0.0)
        glRotatef(self.x, 0.0, 1.0, 0.0)

        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        glShadeModel(GL_SMOOTH)
    

    def LabelEnergyAxes(self):
        '''
        
        '''
    # Calculate no. of levels, and location of dashes
        n = self.disc.kw.levels['n']
        delta = self.disc.kw.delta['dE']
        first = self.disc.kw.first['E1']
        label_sep = 0.1
        tick_sep = 1.0 / (n-1)
        half_tick_width = 0.005
    #label_list = []
        for i in range(n):
            height = 0.5 - tick_sep*i
            glBegin(GL_LINES)
            glColor3f(0.0, 0.0, 0.0)
            glVertex3f(-0.5 - half_tick_width, height, 0.5)
            glVertex3f(-0.5 + half_tick_width, height, 0.5)
            glEnd()
            
            glutInit()
            glRasterPos3d(-0.5 - half_tick_width - label_sep, height, 
                           0.5)
            glColor3f(0,0,0)
            label = first - delta*i
            
            for char in str(label):
                glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, ord(char))
    # Place dashes along length of axes

    def LabelMetricXAxis(self):
        '''
        
        '''
        # Calculate no. of levels, and location of dashes
        maxX = self.disc.basin_index['MaxX']
        minX = self.disc.basin_index['MinX']
        n = self.disc.basin_index['MetXTickNo']
        delta = (maxX - minX)/n
        
        tick_sep = 1.0/(n)
        label_sep = 0.1
        #tick_sep = 1.0 / (n-1)
        half_tick_width = 0.005
    #label_list = []
        for i in range(n+1):
            x_len = -0.5 + tick_sep*i
            glBegin(GL_LINES)
            glColor3f(0.0, 0.0, 0.0)
            glVertex3f(x_len,-0.5 - half_tick_width, 0.5)
            glVertex3f(x_len,-0.5 + half_tick_width, 0.5)
            glEnd()
            
            glutInit()
            glRasterPos3d(x_len,
                          -0.5 - half_tick_width - label_sep,
                          0.5 + half_tick_width + label_sep)
            glColor3f(0,0,0)
            label = minX + delta*i
            
            for char in str(label):
                glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, ord(char))
    
    def LabelMetricYAxis(self):
        '''
        
        '''
        # Calculate no. of levels, and location of dashes
        maxY = self.disc.basin_index['MaxY']
        minY = self.disc.basin_index['MinY']
        n = self.disc.basin_index['MetYTickNo']
        delta = (maxY - minY)/n
        
        tick_sep = 1.0/(n)
        label_sep = 0.1
        #tick_sep = 1.0 / (n-1)
        half_tick_width = 0.005
    #label_list = []
        for i in range(n+1):
            y_len = -0.5 + tick_sep*i
            glBegin(GL_LINES)
            glColor3f(0.0, 0.0, 0.0)
            glVertex3f(-0.5, -0.5 - half_tick_width, y_len)
            glVertex3f(-0.5, -0.5 + half_tick_width, y_len)
            glEnd()
            
            glutInit()
            glRasterPos3d(-0.5 - half_tick_width - label_sep,
                          -0.5 - half_tick_width - label_sep,
                           y_len)
            glColor3f(0,0,0)
            label = minY + delta*i
            
            for char in str(label):
                glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, ord(char))
    

    def WireCube(self):
        # clear color and depth buffers
        #glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

        glBegin(GL_LINES)
        #print self.red
        glColor3f(0,0,0)
        glVertex3f(-0.5, -0.5, -0.5)
        glVertex3f(0.5,-0.5,-0.5)

        glVertex3f(-0.5, -0.5, -0.5)
        glVertex3f(-0.5,0.5,-0.5)

        glVertex3f(-0.5, -0.5, -0.5)
        glVertex3f(-0.5,-0.5,0.5)

        glVertex3f(0.5, 0.5, 0.5)
        glVertex3f(-0.5, 0.5, 0.5)

        glVertex3f(0.5, 0.5, 0.5)
        glVertex3f(0.5, -0.5, 0.5)

        glVertex3f(0.5, 0.5, 0.5)
        glVertex3f(0.5, 0.5, -0.5)

        glVertex3f(0.5, -0.5, -0.5)
        glVertex3f(0.5, -0.5, 0.5)

        glVertex3f(0.5, -0.5, -0.5)
        glVertex3f(0.5, 0.5, -0.5)

        glVertex3f(-0.5, 0.5, -0.5)
        glVertex3f(-0.5, 0.5, 0.5)

        glVertex3f(-0.5, 0.5, -0.5)
        glVertex3f(0.5, 0.5, -0.5)

        glVertex3f(-0.5, -0.5, 0.5)
        glVertex3f(0.5, -0.5, 0.5)

        glVertex3f(-0.5, -0.5, 0.5)
        glVertex3f(-0.5, 0.5, 0.5)

        glEnd()


class AxesDraw(MyGLCanvas):
    def __init__(self, parent):
        super(AxesDraw, self).__init__(parent)
        self.disc = parent.disc

    #-----------------------------------------------------------------------------------------------
    def InitGL(self):
        '''
        Initialize GL
        '''
       

        # set viewing projection
        glMatrixMode(GL_PROJECTION)
        glFrustum(-0.5, 0.5, -0.5, 0.5, 0.0, 3.0)

        # position viewer
        glMatrixMode(GL_MODELVIEW)
        glTranslatef(0.0, 0.0, -2.0)

        # position object
        glRotatef(self.y, 1.0, 0.0, 0.0)
        glRotatef(self.x, 0.0, 1.0, 0.0)

        glEnable(GL_DEPTH_TEST)
        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        glShadeModel(GL_SMOOTH)
        
        
    def Rotate(self):
        
        w, h = self.size
        w = max(w, 1.0)
        h = max(h, 1.0)
        xScale = 180.0 / w
        yScale = 180.0 / h
        glRotatef((self.y - self.lasty) * yScale, 1.0, 0.0, 0.0);
        glRotatef((self.x - self.lastx) * xScale, 0.0, 1.0, 0.0);

#--------------------------------------------------------------------
    
    def OnDraw(self):
        glClearColor(1.0,1.0,1.0,0.0)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        self.WireCube()
        
        for l in self.disc.basin_index['Level']:
            
            if l !=1:
                for b in self.disc.basin_index['Level'][l]['Basin']:
                    c = self.disc.basin_index['Level'][l]['Basin'][b]\
                        ['Children']
                    p = self.disc.basin_index['Level'][l]['Basin'][b]\
                        ['Parents']
                    self.Lines(l,b,c,p)
        
        
        if self.size is None:
            self.size = self.GetClientSize()
            
        if self.MouseRotate:
            self.Rotate()
        
        self.SwapBuffers()

    #def AscertainLength(self):
        


    def Rotate(self):
        
        w, h = self.size
        w = max(w, 1.0)
        h = max(h, 1.0)
        xScale = 180.0 / w
        yScale = 180.0 / h
        glRotatef((self.y - self.lasty) * yScale, 1.0, 0.0, 0.0);
        glRotatef((self.x - self.lastx) * xScale, 0.0, 1.0, 0.0);


    def Lines(self, l, b, c, p):
        '''

        '''
        #print l,b,c, p
        red = self.disc.basin_index['Level'][l]['Basin'][b]['R']
        green = self.disc.basin_index['Level'][l]['Basin'][b]['G']
        blue = self.disc.basin_index['Level'][l]['Basin'][b]['B']
        
        x1 = self.disc.basin_index['Level'][l - 1]['Basin'][p]['X']-0.5
        y1 = self.disc.basin_index['Level'][l - 1]['Basin'][p]['Y']-0.5
        z1 = self.disc.basin_index['Level'][l - 1]['Z']#-0.5

        x2 = self.disc.basin_index['Level'][l]['Basin'][b]['X']-0.5
        y2 = self.disc.basin_index['Level'][l]['Basin'][b]['Y']-0.5
        if not c:                                              
            z2 = self.disc.basin_index['Level'][l]['Basin'][b]['Z']-0.5
        else:
            z2 = self.disc.basin_index['Level'][l]['Z']-0.5

        glBegin(GL_LINES)
        glColor3f(red, green, blue)
        glVertex3f(x1, y1, z1)
        glVertex3f(x2, y2, z2)
        glEnd()

    def Line(self, x_one, y_one, z_one, x_two, y_two, z_two, R, G, B):
        #
        #print x_one, y_one, z_one

        glBegin(GL_LINES)
        glColor3f(R, G, B)
        glVertex3f(x_one, y_one, z_one)
        glVertex3f(x_two, y_two, z_two)
        glEnd()

    def WireCube(self):
            # clear color and depth buffers
        #glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

        glBegin(GL_LINES)
        #print self.red
        glColor3f(0,0,0)
        glVertex3f(-0.5, -0.5, -0.5)
        glVertex3f(0.5,-0.5,-0.5)

        glVertex3f(-0.5, -0.5, -0.5)
        glVertex3f(-0.5,0.5,-0.5)

        glVertex3f(-0.5, -0.5, -0.5)
        glVertex3f(-0.5,-0.5,0.5)

        glVertex3f(0.5, 0.5, 0.5)
        glVertex3f(-0.5, 0.5, 0.5)

        glVertex3f(0.5, 0.5, 0.5)
        glVertex3f(0.5, -0.5, 0.5)

        glVertex3f(0.5, 0.5, 0.5)
        glVertex3f(0.5, 0.5, -0.5)

        glVertex3f(0.5, -0.5, -0.5)
        glVertex3f(0.5, -0.5, 0.5)

        glVertex3f(0.5, -0.5, -0.5)
        glVertex3f(0.5, 0.5, -0.5)

        glVertex3f(-0.5, 0.5, -0.5)
        glVertex3f(-0.5, 0.5, 0.5)

        glVertex3f(-0.5, 0.5, -0.5)
        glVertex3f(0.5, 0.5, -0.5)

        glVertex3f(-0.5, -0.5, 0.5)
        glVertex3f(0.5, -0.5, 0.5)

        glVertex3f(-0.5, -0.5, 0.5)
        glVertex3f(-0.5, 0.5, 0.5)

        glEnd()
    




#====================================================================

   

class ToolPanel(wx.Panel):
    def __init__(self,parent,canvas_metric_3d, id=-1,*args, **kwargs):
        super(ToolPanel, self).__init__(parent, id = -1, *args, **kwargs)
        self.canvas_metric_3d = id
        self.parent = parent
        #self.helloButton = wx.Button(self, label = 'HELLO')
        self.snapXButton = wx.Button(self, label = 'Snap X axis')
        self.snapXButton.Bind(wx.EVT_BUTTON, self.snapX)
        self.snapYButton = wx.Button(self, label = 'Snap Y axis')
        #self.snapYButton.Bind(wx.EVT_BUTTON, self.snapY)
        self.sizer = wx.BoxSizer(wx.VERTICAL)
        self.sizer.Add(self.snapXButton, flag = wx.TOP, border = 5)
        self.sizer.Add(self.snapYButton, flag = wx.TOP, border = 5)
        #self.border = wx.BoxSizer()

        self.SetSizer(self.sizer)
        
    def snapX(self, e):
        self.canvas_metric_3d.snapX()
        self.canvas_metric_3d.OnDraw()

    def snapY(self, e):
        self.canvas_metric_3d.snapY()
        self.canvas_metric_3d.OnDraw()

#===================================================================================================

class MainWin(wx.Frame):
    def __init__(self, disc,parent = None, id = -1, title = 'Disconnect'):
        super(MainWin, self).__init__(parent, id, title, size = (400,200), style = wx.DEFAULT_FRAME_STYLE | wx.NO_FULL_REPAINT_ON_RESIZE)
        #wx.Frame.__init__(self, parent, id, title, size = (400,200), style = wx.DEFAULT_FRAME_STYLE | wx.NO_FULL_REPAINT_ON_RESIZE)
        self.disc = disc
        self.parent = parent
        self.canvas_metric_3d = MetricDisconnect3DCanvas(self)
        self.canvas_disc = DisconnectCanvas(self)
        #self.canvas = MetricDisconnect2DCanvas(self)
        self.panel = ToolPanel(self, self.parent, 
                               self.canvas_metric_3d)
        

        self.sizer = wx.BoxSizer(wx.HORIZONTAL)
        self.sizer.Add(self.panel, proportion=0, flag=wx.EXPAND|wx.ALL, border=5)
        self.sizer.Add(self.canvas_disc, proportion = 1, flag=wx.EXPAND|wx.ALL, border=5)
        self.sizer.Add(self.canvas_metric_3d, proportion = 1, flag=wx.EXPAND|wx.ALL, border=5)
        self.SetSizer(self.sizer)

        self.Show()
