//$Header$
//------------------------------------------------------------------------------
//                              TrajPlotCanvas
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// ** Legal **
//
// Author: Linda Jun
// Created: 2003/11/25
/**
 * Implements TrajPlotCanvas for opengl plot.
 */
//------------------------------------------------------------------------------
#include "TrajPlotCanvas.hpp"
#include "FileManager.hpp"        // for Earth texture file
#include "ColorTypes.hpp"         // for namespace GmatColor::
#include "Rvector3.hpp"           // for Rvector3::GetMagnitude()
#include "MessageInterface.hpp"

#include <math.h>

#ifdef __WXMAC__
#  ifdef __DARWIN__
#    include <OpenGL/gl.h>
#    include <OpenGL/glu.h>
#  else
#    include <gl.h>
#    include <glu.h>
#  endif
#else
#  include <GL/gl.h>
#  include <GL/glu.h>
#  include <IL/il.h>
#  include <IL/ilu.h>
#  include <IL/ilut.h>
#endif

//#define DEBUG_TRAJCANVAS_INIT 1
//#define DEBUG_TRAJCANVAS_UPDATE 1
//#define DEBUG_TRAJCANVAS_DRAW 1
//#define DEBUG_TRAJCANVAS_BODY 1

BEGIN_EVENT_TABLE(TrajPlotCanvas, wxGLCanvas)
   EVT_SIZE(TrajPlotCanvas::OnSize)
   EVT_PAINT(TrajPlotCanvas::OnPaint)
   EVT_MOUSE_EVENTS(TrajPlotCanvas::OnMouse)
END_EVENT_TABLE()

//---------------------------------
// static data
//---------------------------------

enum TFrameMode
{
   GCI_FRAME,
   SOL_ROT_FRAME,
   SELENOCENTRIC_FRAME,
   EARTH_MOON_ROT_FRAME,
   HELIOCENTRIC_FRAME,
   ANY_BODY_FRAME,
   ANY_BODY_ROT_FRAME
};
   
struct GlColorType
{
   Byte red;
   Byte green;
   Byte blue;
   Byte not_used;
};


static int *sIntColor = new int;
static GlColorType *sGlColor = (GlColorType*)sIntColor;

using namespace GmatPlot;

//------------------------------------------------------------------------------
// TrajPlotCanvas(wxWindow *parent, wxWindowID id,
//                const wxPoint& pos, const wxSize& size, SolarSystem *ss,
//                long style, const wxString& name)
//------------------------------------------------------------------------------
/**
 * Constructor.
 *
 * @param <parent> parent window pointer
 * @param <id>     window id
 * @param <pos>    position (top, left) where the window to be placed within the
 *                 parent window
 * @param <size>   size of the window
 * @param <ss>     solar system pointer to retrieve body information
 * @param <style>  style of window
 * @param <name>   title of window
 *
 */
//------------------------------------------------------------------------------
TrajPlotCanvas::TrajPlotCanvas(wxWindow *parent, wxWindowID id,
                               const wxPoint& pos, const wxSize& size, SolarSystem *ss,
                               long style, const wxString& name)
   : wxGLCanvas(parent, id, pos, size, style, name)
{    
   // initalize data members
   mTextTrajFile = NULL;
   mGlList = 0;
   mInitialized = false;
   mNumData = 0;
   
   // view
   mCanvasSize = size;
   mDefaultViewX = 90.0;
   mDefaultViewY = 0.0;
   mDefaultViewZ = 0.0;
   mDefaultViewDist = DEFAULT_DIST;
   
   mCurrViewX = mDefaultViewX;
   mCurrViewY = mDefaultViewY;
   mCurrViewZ = mDefaultViewZ;
   mCurrViewDist = mDefaultViewDist;
   
   m_fCamTransX = 0;
   m_fCamTransY = 0;
   m_fCamTransZ = 0;
   
   mAxisLength = mCurrViewDist;
   mCurrViewFrame = GCI_FRAME;
   mCenterViewBody = GmatPlot::EARTH;
   mCurrBody = GmatPlot::EARTH;
   mRotateAboutXaxis = true;
   mRotateAboutYaxis = false;
   mRotateAboutZaxis = false;
   mRotateXy = true;

   mZoomAmount = 300.0;
   
   // projection
   ChangeProjection(size.x, size.y, mAxisLength);
   
   mEarthRadius = 6378.14; //km
   mScRadius = 200;        //km: make big enough to see

   // view options
   mDrawSpacecraft = true;
   mDrawWireFrame = false;
   mDrawEquPlane = true;
   mUseTexture = true;
   mEquPlaneColor = GmatColor::GRAY32;
   
   // solar system
   mSolarSystem = ss;
   
   // bodies
   mOtherBodyCount = 0;
   
   for (int i=0; i<MAX_BODIES; i++)
   {
      mBodyTextureIndex[i] = UNINIT_TEXTURE;
      mBodyMaxZoomIn[i] = MAX_ZOOM_IN;
      mBodyInUse[i] = false;
      mBodyHasData[i] = false;
   }
   
   // set Sun, Earth, Moon in use as default,
   // bodies' position is updated in UpdadateSpacecraft()
   mBodyInUse[SUN]   = true;
   mBodyInUse[EARTH] = true;
   mBodyInUse[MOON]  = true;
   mBodyHasData[SUN]   = true;
   mBodyHasData[EARTH] = true;
   mBodyHasData[MOON]  = true;
   
   for (int body=0; body<MAX_BODIES; body++)
      mBodyRadius[body] = 0.0;

   if (mSolarSystem != NULL)
   {
      CelestialBody *bodyPtr;
      for (int body=0; body<=MOON; body++)
      {
         if (mBodyInUse[body])
         {
            bodyPtr = mSolarSystem->GetBody(GmatPlot::BODY_NAME[body]);
            if (bodyPtr != NULL)
            {
               mBodyRadius[body] = bodyPtr->GetEquatorialRadius();
               mBodyMaxZoomIn[body] = mBodyRadius[body] * RADIUS_ZOOM_RATIO;
#if DEBUG_TRAJCANVAS_INIT
               MessageInterface::ShowMessage
                  ("TrajPlotCanvas() %s Radius=%f maxZoomIn=%f\n", GmatPlot::BODY_NAME[body].c_str(),
                   mBodyRadius[body], mBodyMaxZoomIn[body]);
#endif
            }
         }
      }
   }

   // Zoom
   mMaxZoomIn = mBodyMaxZoomIn[EARTH];
   
   // Spacecraft
   mScCount = 1; //loj: set to 1 for now
   for (int i=0; i<MAX_SCS; i++)
      mScTextureIndex[i] = UNINIT_TEXTURE;

}


//------------------------------------------------------------------------------
// ~TrajPlotCanvas()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
TrajPlotCanvas::~TrajPlotCanvas()
{
   if (mTextTrajFile)
      delete mTextTrajFile;
}

//------------------------------------------------------------------------------
// bool IsInitialized()
//------------------------------------------------------------------------------
/**
 * Retrives initialized flag.
 */
//------------------------------------------------------------------------------
bool TrajPlotCanvas::IsInitialized()
{
   return mInitialized;
}

//------------------------------------------------------------------------------
// void ClearPlot()
//------------------------------------------------------------------------------
/**
 * Clears plot.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::ClearPlot()
{
   //loj: black for now.. eventually it will use background color
   glClearColor(0.0, 0.0, 0.0, 1);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   glFlush();
   SwapBuffers();
   mNumData = 0;
   //mDrawEquPlane = false;
}

//------------------------------------------------------------------------------
// void UpdatePlot()
//------------------------------------------------------------------------------
/**
 * Updates plot.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::UpdatePlot()
{
   if (mAxisLength < mMaxZoomIn)
   {
      mAxisLength = mMaxZoomIn;
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::UpdatePlot() distance < max zoom in. distance set to %f\n",
          mAxisLength);
   }
   
   ChangeProjection(mCanvasSize.x, mCanvasSize.y, mAxisLength);
   Refresh(false);
}

//------------------------------------------------------------------------------
// void ShowDefaultView()
//------------------------------------------------------------------------------
/**
 * Shows default view.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::ShowDefaultView()
{
   int clientWidth, clientHeight;
   GetClientSize(&clientWidth, &clientHeight);

   mCurrViewX = mDefaultViewX;
   mCurrViewY = mDefaultViewY;
   mCurrViewZ = mDefaultViewZ;
   mCurrViewDist = mDefaultViewDist;
   mAxisLength = mCurrViewDist;
   m_fCamTransX = 0;
   m_fCamTransY = 0;
   m_fCamTransZ = 0;

   mCenterViewBody = EARTH;
   ChangeView(mCurrViewX, mCurrViewY, mCurrViewZ);
   ChangeProjection(clientWidth, clientHeight, mAxisLength);
   Refresh(false);
}


//------------------------------------------------------------------------------
// void ZoomIn()
//------------------------------------------------------------------------------
/**
 * Zoom in the picture.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::ZoomIn()
{
   double realDist = (mAxisLength - mZoomAmount) / log(mAxisLength);

   if (mAxisLength > mMaxZoomIn)
   {
      mAxisLength = mAxisLength - realDist;
   
      if (mAxisLength < mMaxZoomIn)
         mAxisLength = mMaxZoomIn;
   
      ChangeProjection(mCanvasSize.x, mCanvasSize.y, mAxisLength);
   }
   
   Refresh(false);
}


//------------------------------------------------------------------------------
// void ZoomOut()
//------------------------------------------------------------------------------
/**
 * Zoom out the picture.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::ZoomOut()
{
   // the further object is the faster zoom out
   double realDist = (mAxisLength + mZoomAmount) / log(mAxisLength);
   mAxisLength = mAxisLength + realDist;

   //mAxisLength = mAxisLength + mZoomAmount;
    
   ChangeProjection(mCanvasSize.x, mCanvasSize.y, mAxisLength);
    
   Refresh(false);
}


//------------------------------------------------------------------------------
// void DrawEquPlane(bool flag)
//------------------------------------------------------------------------------
/**
 * Draws equatorial plane
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawEquPlane(bool flag)
{
   mDrawEquPlane = flag;
   Refresh(false);
}

//------------------------------------------------------------------------------
// void DrawWireFrame(bool flag)
//------------------------------------------------------------------------------
/**
 * Shows objects in wire frame.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawWireFrame(bool flag)
{
   mDrawWireFrame = flag;
   Refresh(false);
}

//------------------------------------------------------------------------------
// void OnPaint(wxPaintEvent& event)
//------------------------------------------------------------------------------
/**
 * Processes wxPaintEvent.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::OnPaint(wxPaintEvent& event)
{
   // must always be here
   wxPaintDC dc(this);
    
#ifndef __WXMOTIF__
   if (!GetContext()) return;
#endif

   SetCurrent();    
    
   if (mDrawWireFrame)
   {
      glPolygonMode(GL_FRONT, GL_LINE); // for wire frame
      glPolygonMode(GL_BACK, GL_LINE);  // for wire frame
   }
   else
   {
      glPolygonMode(GL_FRONT, GL_FILL);
      glPolygonMode(GL_BACK, GL_FILL);
   }

   SetProjection();
   DrawPicture();
   glFlush();
   SwapBuffers();
}


//------------------------------------------------------------------------------
// void OnSize(wxSizeEvent& event)
//------------------------------------------------------------------------------
/**
 * Processes wxSizeEvent.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::OnSize(wxSizeEvent& event)
{
   // this is also necessary to update the context on some platforms
   wxGLCanvas::OnSize(event);
    
   // set GL viewport (not called by wxGLCanvas::OnSize on all platforms...)
   int nWidth, nHeight;
   GetClientSize(&nWidth, &nHeight);
   mCanvasSize.x = nWidth;
   mCanvasSize.y = nHeight;
#ifndef __WXMOTIF__
   if (GetContext())
#endif
   {
      //loj: need this to make picture not to stretch to canvas
      ChangeProjection(nWidth, nHeight, mAxisLength);
      SetCurrent();
      glViewport(0, 0, (GLint) nWidth, (GLint) nHeight);
   }
}


//------------------------------------------------------------------------------
// void OnMouse(wxMouseEvent& event)
//------------------------------------------------------------------------------
/**
 * Processes wxMouseEvent.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::OnMouse(wxMouseEvent& event)
{
   int flippedY;
   int clientWidth, clientHeight;
   int mouseX, mouseY;
    
   GetClientSize(&clientWidth, &clientHeight);
   ChangeProjection(clientWidth, clientHeight, mAxisLength);
    
   mouseX = event.GetX();
   mouseY = event.GetY();
        
   // First, flip the mouseY value so it is oriented right (bottom left is 0,0)
   flippedY = clientHeight - mouseY;
    
   GLfloat fEndX = m_fLeftPos + ((GLfloat)mouseX /(GLfloat)clientWidth) *
      (m_fRightPos - m_fLeftPos);
   GLfloat fEndY = m_fBottomPos + ((GLfloat)flippedY /(GLfloat)clientHeight)*
      (m_fTopPos - m_fBottomPos);
   
   //if mouse dragging
   if (event.Dragging())
   {
  
      //------------------------------
      // translating
      //------------------------------
      if (event.ShiftDown() && event.LeftIsDown())
      {
         // Do a X/Y Translate of the camera
         m_fCamTransX += (fEndX - m_fStartX);
         m_fCamTransY += (fEndY - m_fStartY);

         // repaint
         Refresh(false);
      }
      //------------------------------
      // rotating
      //------------------------------
      else if (event.LeftIsDown())
      {        
         ComputeView(fEndX, fEndY);
         ChangeView(mCurrViewX, mCurrViewY, mCurrViewZ);
            
         // repaint
         Refresh(false);
      }
      //------------------------------
      // zooming
      //------------------------------
      else if (event.RightIsDown())
      {            
         // find the length
         double x2 = pow(mouseX - mLastMouseX, 2);
         double y2 = pow(mouseY - mLastMouseY, 2);
         double length = sqrt(x2 + y2);
         mZoomAmount = length * 100;

         //loj: 11/4/04 add another dragging direction (Build 3.2)
         if (mouseX < mLastMouseX && mouseY > mLastMouseY)
         {
            // dragging from upper right corner to lower left corner
            ZoomIn();
         }
         else if (mouseX > mLastMouseX && mouseY < mLastMouseY)
         {
            // dragging from lower left corner to upper right corner
            ZoomOut();
         }
         else
         {
            // if mouse moves toward left then zoom in
            if (mouseX < mLastMouseX || mouseY < mLastMouseY)
               ZoomIn();
            else
               ZoomOut();
         }
         
         //loj: 11/4/04 commented out
         // if mouse moves toward left, then zoom in (Build 3)
         //if (mouseX < mLastMouseX)
         //   ZoomIn();
         //else
         //   ZoomOut();
         
         //loj: 7/12/04 commented out
         // if mouse moves toward the upper left, then zoom in (Build 2)
         //if (mouseX < mLastMouseX || mouseY < mLastMouseY)
         //   ZoomIn();
         //else
         //   ZoomOut();
         
      }
   } // end if (event.Dragging())
   
   // save last position
   mLastMouseX = mouseX;
   mLastMouseY = mouseY;
   
   //loj: 7/12/04 commented out
   //m_fStartX = m_fLeftPos + ((GLfloat)mouseX / (GLfloat)clientWidth) *
   //   (m_fRightPos - m_fLeftPos);
   //m_fStartY = m_fBottomPos + ((GLfloat)flippedY / (GLfloat)clientHeight) *
   //   (m_fTopPos - m_fBottomPos);
   
   m_fStartX = fEndX;
   m_fStartY = fEndY;
   
   //wxLogStatus(MdiGlPlot::mdiParentGlFrame,
   //            wxT("X = %d Y = %d"), event.GetX(), event.GetY());
   wxLogStatus(MdiGlPlot::mdiParentGlFrame,
               wxT("distance=%f"), mAxisLength);
   //loj: 6/14/04 do not show debug info
   //wxLogStatus(MdiGlPlot::mdiParentGlFrame,
   //            wxT("X = %d Y = %d lastX = %f lastY = %f Zoom amount = %f Distance = %f"),
   //            event.GetX(), event.GetY(), m_fStartX, m_fStartY, mZoomAmount, mAxisLength);
   event.Skip();
}


//------------------------------------------------------------------------------
// bool TrajPlotCanvas::InitGL()
//------------------------------------------------------------------------------
/**
 * Initializes GL and IL.
 */
//------------------------------------------------------------------------------
bool TrajPlotCanvas::InitGL()
{
   // remove back faces
   glDisable(GL_CULL_FACE);
   glEnable(GL_DEPTH_TEST);

   glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
   glDepthFunc(GL_LESS);
    
   // speedups
   glEnable(GL_DITHER);
   glShadeModel(GL_SMOOTH);
   glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
   glHint(GL_POLYGON_SMOOTH_HINT, GL_FASTEST);
    
#ifdef __WXMSW__
   // initalize devIL library
   ilInit();
   ilutRenderer(ILUT_OPENGL);
   
   // try to load textures
   if (!LoadGLTextures())
      return false;
   
   mInitialized = true;
#endif
   
   return true;
}


//------------------------------------------------------------------------------
//  bool LoadGLTextures()
//------------------------------------------------------------------------------
/**
 * Loads textures.
 */
//------------------------------------------------------------------------------
bool TrajPlotCanvas::LoadGLTextures()
{
   //use FileManager to retrieve texture file name
   FileManager *fm = FileManager::Instance();
   std::string textureFile;
   ILboolean status;
   
#ifdef __WXMSW__
   //--------------------------------------------------
   // always load Earth texture
   //--------------------------------------------------
   if (mBodyTextureIndex[EARTH] == UNINIT_TEXTURE)
   {
      textureFile = fm->GetStringParameter("FULL_EARTH_TEXTURE_FILE");
      status = ilLoadImage((char*)textureFile.c_str());
      
#if DEBUG_TRAJCANVAS_BODY
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::LoadGLTextures() status of loading %s = %d\n",
          textureFile.c_str(), status);
#endif
      
      if (status != 1)
         return false;

      mBodyTextureIndex[EARTH] = ilutGLBindTexImage();
   }
   
   //--------------------------------------------------
   // load other standard bodies texture if used
   //--------------------------------------------------
   for (int body=0; body<=MOON; body++)
   {
      if (body == EARTH)
         continue;

      if (mBodyInUse[body] && mBodyTextureIndex[body] == UNINIT_TEXTURE)
      {
         // load texture
         switch (body)
         {
         case SUN:
            textureFile = fm->GetStringParameter("FULL_SUN_TEXTURE_FILE");
            status = ilLoadImage((char*)textureFile.c_str());
            break;
         case MERCURY:
            textureFile = fm->GetStringParameter("FULL_MERCURY_TEXTURE_FILE");
            status = ilLoadImage((char*)textureFile.c_str());
            break;
         case VENUS:
            textureFile = fm->GetStringParameter("FULL_VENUS_TEXTURE_FILE");
            status = ilLoadImage((char*)textureFile.c_str());
            break;
         case MARS:
            textureFile = fm->GetStringParameter("FULL_MARS_TEXTURE_FILE");
            status = ilLoadImage((char*)textureFile.c_str());
            break;
         case JUPITER:
            textureFile = fm->GetStringParameter("FULL_JUPITER_TEXTURE_FILE");
            status = ilLoadImage((char*)textureFile.c_str());
            break;
         case SATURN:
            textureFile = fm->GetStringParameter("FULL_SATURN_TEXTURE_FILE");
            status = ilLoadImage((char*)textureFile.c_str());
            break;
         case URANUS:
            textureFile = fm->GetStringParameter("FULL_URANUS_TEXTURE_FILE");
            status = ilLoadImage((char*)textureFile.c_str());
            break;
         case NEPTUNE:
            textureFile = fm->GetStringParameter("FULL_NEPTUNE_TEXTURE_FILE");
            status = ilLoadImage((char*)textureFile.c_str());
            break;
         case PLUTO:
            textureFile = fm->GetStringParameter("FULL_PLUTO_TEXTURE_FILE");
            status = ilLoadImage((char*)textureFile.c_str());
            break;
         case MOON:
            textureFile = fm->GetStringParameter("FULL_MOON_TEXTURE_FILE");
            status = ilLoadImage((char*)textureFile.c_str());
            break;
         default:
            MessageInterface::ShowMessage
               ("TrajPlotCanvas::LoadGLTextures() Texture file is not supported for body: %s\n",
                GmatPlot::BODY_NAME[body].c_str());
            status = 0;
            break;
         }
                 
         if (status != 1)
         {
            MessageInterface::ShowMessage
               ("TrajPlotCanvas::LoadGLTextures() Unable to load texture file for %s\n"
                "file name:%s\n", GmatPlot::BODY_NAME[body].c_str(), textureFile.c_str());
         }
         else
         {
            mBodyTextureIndex[body] = ilutGLBindTexImage();
         }
      }
   }
   
   return true;
#else
   return false;
#endif
}


//------------------------------------------------------------------------------
//  void SetProjection()
//------------------------------------------------------------------------------
/**
 * Sets view projection.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetProjection()
{
   // Setup the world view
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   glMatrixMode(GL_PROJECTION); // first go to projection mode
   glPushMatrix();
   glLoadIdentity();
   SetupWorld();                // set it up
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix();
}


//------------------------------------------------------------------------------
//  void SetupWorld()
//------------------------------------------------------------------------------
/**
 * Sets world view as orthographic projection. With an orthographic projection,
 * the viewing volumn is a rectangular parallelepiped. Unlike perspective
 * projection, the size of the viewing volumn doesn't change from one end to the
 * other, so distance from the camera doesn't affect how large a object appears.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetupWorld()
{
   // Setup how we view the world
   glOrtho(m_fLeftPos, m_fRightPos, m_fBottomPos, m_fTopPos,
           m_fViewNear, m_fViewFar);
   
   //Translate Camera
   glTranslatef(m_fCamTransX, m_fCamTransY, m_fCamTransZ);

   //put camera transformations here.
   if (mRotateAboutXaxis)
   {
      glRotatef(m_fCamRotationY, 0.0, 1.0, 0.0);
      glRotatef(m_fCamRotationZ, 0.0, 0.0, 1.0);
      glRotatef(m_fCamRotationX, 1.0, 0.0, 0.0);
   }
   else if (mRotateAboutYaxis)
   {
      glRotatef(m_fCamRotationZ, 0.0, 0.0, 1.0);
      glRotatef(m_fCamRotationX, 1.0, 0.0, 0.0);
      glRotatef(m_fCamRotationY, 0.0, 1.0, 0.0);
   }
   else
   {
      glRotatef(m_fCamRotationX, 1.0, 0.0, 0.0);
      glRotatef(m_fCamRotationY, 0.0, 1.0, 0.0);
      glRotatef(m_fCamRotationZ, 0.0, 0.0, 1.0);
   }
   
   //camera moves opposite direction to center on object
   //this is the point of rotation
   switch (mCenterViewBody)
   {
   case EARTH:
      glTranslatef(-mTempEarthPos[mNumData-1][0],
                   -mTempEarthPos[mNumData-1][1],
                   -mTempEarthPos[mNumData-1][2]);
      break;
   default:
      glTranslatef(-mTempBodyPos[mCenterViewBody][mNumData-1][0],
                   -mTempBodyPos[mCenterViewBody][mNumData-1][1],
                   -mTempBodyPos[mCenterViewBody][mNumData-1][2]);
      break;
   }
} // end SetupWorld()


//------------------------------------------------------------------------------
//  void ComputeView(GLfloat fEndX, GLfloat fEndY)
//------------------------------------------------------------------------------
/**
 * Calculates a percentage of how much the mouse has moved. When moving the mouse
 * left-right, we want to rotate about the Y axis, and vice versa
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::ComputeView(GLfloat fEndX, GLfloat fEndY)
{
   float fYAmnt = 360*(fEndX - m_fStartX)/(m_fRightPos - m_fLeftPos);
   float fXAmnt = 360*(fEndY - m_fStartY)/(m_fBottomPos - m_fTopPos);

   
   // always rotate the y axis
   mCurrViewY = m_fCamRotationY + fYAmnt;

   // Are we rotating the x or the z in this case?
   if (mRotateXy)
   {
      // x axis
      mCurrViewX = m_fCamRotationX + fXAmnt - 270;
   }
   else
   {
      // z axis
      mCurrViewZ = m_fCamRotationZ + fXAmnt;
   }
}


//------------------------------------------------------------------------------
//  void DrawPicture()
//------------------------------------------------------------------------------
/**
 * Draws whole picture.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawPicture()
{
#if DEBUG_TRAJCANVAS_DRAW
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::DrawPicture() mNumData=%d\n", mNumData);
#endif
   
//     glClearColor(0.0, 0.0, 0.0, 1); // black
//     glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    
//     glPushMatrix();
//     glLoadIdentity();
   
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

   m_fViewLeft = -mAxisLength/2;
   m_fViewRight = mAxisLength/2;
   m_fViewTop = mAxisLength/2;
   m_fViewBottom = -mAxisLength/2;
   m_fViewNear = -mAxisLength/2;
   m_fViewFar = mAxisLength/2;

   // draw earth
   DrawEarthTrajectory();
   DrawSpacecraftTrajectory();

   // draw other bodies
   for (int body=0; body<MAX_BODIES; body++)
   {
      if (body == EARTH)
         continue;
      
      if (mBodyInUse[body])
          DrawBodyTrajectory(body);
   }
   
   // draw equatorial plane
   if (mDrawEquPlane)
      DrawEquatorialPlane();
   
   glPopMatrix();
   
} // end DrawPicture()


//------------------------------------------------------------------------------
//  void DrawEquatorialPlane()
//------------------------------------------------------------------------------
/**
 * Draws equatorial plane circles.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawEquatorialPlane()
{
   int i;
   float endPos[3];
   float distance;
   double angle;
   
   static const Real RAD_PER_DEG =
      3.14159265358979323846264338327950288419716939937511 / 180.0;
   
   distance = (double)mAxisLength;

   glPushMatrix();
   glLoadIdentity();
   glBegin(GL_LINES);
   
   // set color
   *sIntColor = mEquPlaneColor;
   glColor3ub(sGlColor->red, sGlColor->green, sGlColor->blue);

   //-----------------------------------
   // draw lines
   //-----------------------------------
   for (i=0; i<360; i+=15)
   {
      angle = RAD_PER_DEG * ((double)i);
      
      endPos[0] = distance * cos(angle);
      endPos[1] = distance * sin(angle);
      endPos[2] = 0.0;
      
      glVertex3f(0.0, 0.0, 0.0);
      glVertex3f(endPos[0], endPos[1], endPos[2]);
   }
   
   glEnd();
   glPopMatrix();

   //-----------------------------------
   // draw circles
   //-----------------------------------
   glPushMatrix();
   glLoadIdentity();
   
   GLUquadricObj *qobj = gluNewQuadric();

   double radius;
   double distAdd;
   
   for(i=1; i<10; i++)
   {
      gluQuadricDrawStyle(qobj, GLU_LINE  );
      gluQuadricNormals  (qobj, GLU_SMOOTH);
      gluQuadricTexture  (qobj, GL_FALSE  );
      distAdd = i*distance/10;
      
      //        if (i == 1)
      //        {
      //            // draw first circle here
      //        }
      
      radius = distAdd + (distAdd /100.0 / log10(distAdd) * exp(double(i))); //loj: looks better
      //gluDisk(qobj, i*distance/10, i*distance/10, 50, 1); // equal distance
      gluDisk(qobj, radius, radius, 50, 1);
   }
   gluDeleteQuadric(qobj);
   
   glPopMatrix();
   
} // end DrawEquPlane()


//------------------------------------------------------------------------------
//  void DrawEarth()
//------------------------------------------------------------------------------
/**
 * Draws Earth shpere and maps texture image.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawEarth()
{
   if (mCenterViewBody == EARTH)
   {
      glColor3f(1.0, 1.0, 1.0);

      if (mUseTexture)
      {
         if (mBodyTextureIndex[EARTH] != UNINIT_TEXTURE)
         {
            glBindTexture(GL_TEXTURE_2D, mBodyTextureIndex[EARTH]);
            glEnable(GL_TEXTURE_2D);
         }
      }
       
      GLUquadricObj* qobj = gluNewQuadric();
      gluQuadricDrawStyle(qobj, GLU_FILL );
      gluQuadricNormals  (qobj, GLU_SMOOTH);
      gluQuadricTexture  (qobj, GL_TRUE  );
      gluSphere(qobj, mEarthRadius, 50, 50);
      gluDeleteQuadric(qobj);
       
      glDisable(GL_TEXTURE_2D);
      glMatrixMode(GL_MODELVIEW);
   }
} // end DrawEarth()


//------------------------------------------------------------------------------
//  void DrawEarthTrajectory()
//------------------------------------------------------------------------------
/**
 * Draws Earth trajectory and draws Earth at the last point.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawEarthTrajectory()
{
   // set color
   *sIntColor = GmatPlot::BODY_COLOR[EARTH];
   glColor3ub(sGlColor->red, sGlColor->green, sGlColor->blue);

   // Draw Earth trajectory line based on points
   glPushMatrix();
   glLoadIdentity();
   glBegin(GL_LINES);

   // if current frame is not GCI
   if (mCurrViewFrame != GCI_FRAME)
   {
      for (int i=1; i<mNumData; i++)
      {
         if (mTime[i] > mTime[i-1]) //loj: 11/03/04 changed from >=
         {
            glVertex3fv(mTempEarthPos[i-1]);
            glVertex3fv(mTempEarthPos[i]);
         }
      }
   }
   glEnd();
   glPopMatrix();
   
   //-------------------------------------------------------
   //draw earth with texture
   //-------------------------------------------------------
   if (mNumData > 0) //loj: 6/22/04 changed 1 to 0
   {
      glPushMatrix();
      glLoadIdentity();
       
      // put earth at final position
      glTranslatef(mTempEarthPos[mNumData-1][0],
                   mTempEarthPos[mNumData-1][1],
                   mTempEarthPos[mNumData-1][2]);
       
      DrawEarth();
      glPopMatrix();
   }
   
} // end DrawEarthTrajectory()


//------------------------------------------------------------------------------
//  void DrawBody(int bodyIndex)
//------------------------------------------------------------------------------
/**
 * Draws body shpere and maps texture image.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawBody(int bodyIndex)
{
   //-------------------------------------------------------
   // draw anybody with texture on option
   //-------------------------------------------------------
   if (mNumData > 1 && mBodyTextureIndex[bodyIndex] != UNINIT_TEXTURE)
   {
      glPushMatrix();
      glLoadIdentity();
      glColor4f(1.0, 1.0, 1.0, 1.0);
      //loj:glColor3f(1.0, 1.0, 1.0);

      // put anybody at final position
      glTranslatef(mTempBodyPos[bodyIndex][mNumData-1][0],
                   mTempBodyPos[bodyIndex][mNumData-1][1],
                   mTempBodyPos[bodyIndex][mNumData-1][2]);

      glBindTexture(GL_TEXTURE_2D, mBodyTextureIndex[bodyIndex]);
      glEnable(GL_TEXTURE_2D);
      GLUquadricObj* qobj = gluNewQuadric();
      gluQuadricDrawStyle(qobj, GLU_FILL  );
      gluQuadricNormals  (qobj, GLU_SMOOTH);
      gluQuadricTexture  (qobj, GL_TRUE   );
      gluSphere(qobj, mBodyRadius[bodyIndex], 50, 50);
      gluDeleteQuadric(qobj);
   }
   
   glPopMatrix();
   glDisable(GL_TEXTURE_2D);

} // end DrawBody()


//------------------------------------------------------------------------------
//  void DrawBodyTrajectory(int bodyIndex)
//------------------------------------------------------------------------------
/**
 * Draws body trajectory and draws body at the last point.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawBodyTrajectory(int bodyIndex)
{
#if DEBUG_TRAJCANVAS_DRAW
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::DrawBodyTrajectory() drawing %s\n", GmatPlot::BODY_NAME[bodyIndex].c_str());
#endif
   
   // Draw anybody trajectory line based on points
   glPushMatrix();
   glLoadIdentity();
   glBegin(GL_LINES);

   // set color
   *sIntColor = GmatPlot::BODY_COLOR[bodyIndex];
   glColor3ub(sGlColor->red, sGlColor->green, sGlColor->blue);

   for (int i=1; i<mNumData; i++)
   {      
      if (mTime[i] >= mTime[i-1])
      {
         glVertex3f((mTempBodyPos[bodyIndex][i-1][0]),
                    (mTempBodyPos[bodyIndex][i-1][1]),
                    (mTempBodyPos[bodyIndex][i-1][2]));
         
         glVertex3f((mTempBodyPos[bodyIndex][i][0]),
                    (mTempBodyPos[bodyIndex][i][1]),
                    (mTempBodyPos[bodyIndex][i][2]));
      }
   }

   glEnd();
   glPopMatrix();

   //-------------------------------------------------------
   // draw anybody with texture on option
   //-------------------------------------------------------
   DrawBody(bodyIndex);
   
} // end DrawBodyTrajectory()


//------------------------------------------------------------------------------
//  void DrawSpacecraft(UnsignedInt scColor)
//------------------------------------------------------------------------------
/**
 * Draws spacecraft.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawSpacecraft(UnsignedInt scColor)
{
   //loj: 7/2/04 commented out so that each spacecraft can have different color
   //if (mGlList == 0)
   //{
   //mGlList = glGenLists( 1 );
   //glNewList( mGlList, GL_COMPILE_AND_EXECUTE );
        
   // draw six faces of a long cube
   glBegin(GL_QUADS);
   //loj: 7/2/04 *sIntColor = GmatColor::YELLOW32;
   *sIntColor = scColor;
   glColor3ub(sGlColor->red, sGlColor->green, sGlColor->blue);
        
   glNormal3f( 0.0F, 0.0F, 1.0F);
   glVertex3f( mScRadius, mScRadius, mScRadius*2);
   glVertex3f(-mScRadius, mScRadius, mScRadius*2);
   glVertex3f(-mScRadius,-mScRadius, mScRadius*2);
   glVertex3f( mScRadius,-mScRadius, mScRadius*2);

   glNormal3f( 0.0F, 0.0F,-1.0F);
   glVertex3f(-mScRadius,-mScRadius,-mScRadius*2);
   glVertex3f(-mScRadius, mScRadius,-mScRadius*2);
   glVertex3f( mScRadius, mScRadius,-mScRadius*2);
   glVertex3f( mScRadius,-mScRadius,-mScRadius*2);

   glNormal3f( 0.0F, 1.0F, 0.0F);
   glVertex3f( mScRadius, mScRadius, mScRadius*2);
   glVertex3f( mScRadius, mScRadius,-mScRadius*2);
   glVertex3f(-mScRadius, mScRadius,-mScRadius*2);
   glVertex3f(-mScRadius, mScRadius, mScRadius*2);

   glNormal3f( 0.0F,-1.0F, 0.0F);
   glVertex3f(-mScRadius,-mScRadius,-mScRadius*2);
   glVertex3f( mScRadius,-mScRadius,-mScRadius*2);
   glVertex3f( mScRadius,-mScRadius, mScRadius*2);
   glVertex3f(-mScRadius,-mScRadius, mScRadius*2);

   glNormal3f( 1.0F, 0.0F, 0.0F);
   glVertex3f( mScRadius, mScRadius, mScRadius*2);
   glVertex3f( mScRadius,-mScRadius, mScRadius*2);
   glVertex3f( mScRadius,-mScRadius,-mScRadius*2);
   glVertex3f( mScRadius, mScRadius,-mScRadius*2);

   glNormal3f(-1.0F, 0.0F, 0.0F);
   glVertex3f(-mScRadius,-mScRadius,-mScRadius*2);
   glVertex3f(-mScRadius,-mScRadius, mScRadius*2);
   glVertex3f(-mScRadius, mScRadius, mScRadius*2);
   glVertex3f(-mScRadius, mScRadius,-mScRadius*2);
   glEnd();

   // spacecraft with same color, use Display List
   if (mGlList == 0)
   {
      mGlList = glGenLists( 1 );
      glNewList( mGlList, GL_COMPILE_AND_EXECUTE );
      
      // draw six faces of a thin wide cube
      glBegin(GL_QUADS);
      *sIntColor = GmatColor::YELLOW32;
      glColor3ub(sGlColor->red, sGlColor->green, sGlColor->blue);

      glNormal3f( 0.0F, 0.0F, 1.0F);
      glVertex3f( mScRadius/4, mScRadius*4, mScRadius*1.5);
      glVertex3f(-mScRadius/4, mScRadius*4, mScRadius*1.5);
      glVertex3f(-mScRadius/4,-mScRadius*4, mScRadius*1.5);
      glVertex3f( mScRadius/4,-mScRadius*4, mScRadius*1.5);

      glNormal3f( 0.0F, 0.0F,-1.0F);
      glVertex3f(-mScRadius/4,-mScRadius*4,-mScRadius*1.5);
      glVertex3f(-mScRadius/4, mScRadius*4,-mScRadius*1.5);
      glVertex3f( mScRadius/4, mScRadius*4,-mScRadius*1.5);
      glVertex3f( mScRadius/4,-mScRadius*4,-mScRadius*1.5);

      glNormal3f( 0.0F, 1.0F, 0.0F);
      glVertex3f( mScRadius/4, mScRadius*4, mScRadius*1.5);
      glVertex3f( mScRadius/4, mScRadius*4,-mScRadius*1.5);
      glVertex3f(-mScRadius/4, mScRadius*4,-mScRadius*1.5);
      glVertex3f(-mScRadius/4, mScRadius*4, mScRadius*1.5);

      glNormal3f( 0.0F,-1.0F, 0.0F);
      glVertex3f(-mScRadius/4,-mScRadius*4,-mScRadius*1.5);
      glVertex3f( mScRadius/4,-mScRadius*4,-mScRadius*1.5);
      glVertex3f( mScRadius/4,-mScRadius*4, mScRadius*1.5);
      glVertex3f(-mScRadius/4,-mScRadius*4, mScRadius*1.5);

      glNormal3f( 1.0F, 0.0F, 0.0F);
      glVertex3f( mScRadius/4, mScRadius*4, mScRadius*1.5);
      glVertex3f( mScRadius/4,-mScRadius*4, mScRadius*1.5);
      glVertex3f( mScRadius/4,-mScRadius*4,-mScRadius*1.5);
      glVertex3f( mScRadius/4, mScRadius*4,-mScRadius*1.5);

      glNormal3f(-1.0F, 0.0F, 0.0F);
      glVertex3f(-mScRadius/4,-mScRadius*4,-mScRadius*1.5);
      glVertex3f(-mScRadius/4,-mScRadius*4, mScRadius*1.5);
      glVertex3f(-mScRadius/4, mScRadius*4, mScRadius*1.5);
      glVertex3f(-mScRadius/4, mScRadius*4,-mScRadius*1.5);
      glEnd();
      glEndList();
   }
   else
   {
      glCallList( mGlList );
   }

   glMatrixMode(GL_MODELVIEW);
} // end DrawSpacecraft()


//------------------------------------------------------------------------------
//  void DrawSpacecraftTrajectory()
//------------------------------------------------------------------------------
/**
 * Draws spacecraft trajectory and spacecraft at the last point.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawSpacecraftTrajectory()
{
   glPushMatrix();
   glLoadIdentity();
   glBegin(GL_LINES);

   // assume same data points for all spacecrafts
   for (int sc=0; sc<mScCount; sc++)
   {
      // Draw spacecraft trajectory line based on points
      for (int i=1; i<mNumData; i++)
      {
         if (mTime[i] > mTime[i-1]) //loj: 11/03/04 changed from >=
         {
            *sIntColor = mScTrajColor[sc][i];
            glColor3ub(sGlColor->red, sGlColor->green, sGlColor->blue);
            
            glVertex3fv(mTempScPos[sc][i-1]);
            glVertex3fv(mTempScPos[sc][i]);
         }
      }
   }
   
   glEnd();
   glPopMatrix();


   //-------------------------------------------------------
   //draw spacecraft with texture
   //-------------------------------------------------------
   if (mDrawSpacecraft)
   {

      if (mNumData > 0) //loj: 6/22/04 changed 1 to 0
      {
         //loj: 7/2/04 moved inside of the loop for multiple spacecraft
         //glPushMatrix();
         //glLoadIdentity();

         for (int sc=0; sc<mScCount; sc++)
         {
            glPushMatrix();
            glLoadIdentity();
            
            // put spacecraft at final position
            glTranslatef(mTempScPos[sc][mNumData-1][0],
                         mTempScPos[sc][mNumData-1][1],
                         mTempScPos[sc][mNumData-1][2]);

            //MessageInterface::ShowMessage
            //   ("TrajPlotCanvas::DrawSpacecraftTrajectory() scColor=%d\n",
            //    mScTrajColor[sc][mNumData-1]);
            
            DrawSpacecraft(mScTrajColor[sc][mNumData-1]);
            glPopMatrix();
         }

         //loj: 7/2/04 moved inside of the loop for multiple spacecraft
         //DrawSpacecraft();
         //glPopMatrix();
      }
   }
} // end DrawSpacecraftTrajectory()


//------------------------------------------------------------------------------
//  void ChangeView(float viewX, float viewY, float viewZ)
//------------------------------------------------------------------------------
/**
 * Changes view by rotating the camera.
 *
 * @param <viewX> rotation angle of X component.
 * @param <viewY> rotation angle of Y component.
 * @param <viewZ> rotation angle of Z component.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::ChangeView(float viewX, float viewY, float viewZ)
{

   m_fCamRotationX = (int)(viewX) % 360 + 270;
   m_fCamRotationY = (int)(viewY) % 360;
   m_fCamRotationZ = (int)(viewZ) % 360;

   // don't let the rotation angles build up to some insane size
   if (m_fCamRotationY > 360)
      m_fCamRotationY -= 360;
   else if (m_fCamRotationY < 0)
      m_fCamRotationY += 360;

   // don't let the rotation angles build up to some insane size
   if (m_fCamRotationX > 450)
      m_fCamRotationX -= 360;
   else if (m_fCamRotationX < 90)
      m_fCamRotationX += 360;
   
   // don't let the rotation angles build up to some insane size
   if (m_fCamRotationZ > 360)
      m_fCamRotationZ -= 360;
   else if (m_fCamRotationZ < 0)
      m_fCamRotationZ += 360;
  
} // end ChangeView()


//------------------------------------------------------------------------------
//  void ChangeProjection(int width, int height, float axisLength)
//------------------------------------------------------------------------------
/**
 * Changes view projection by viewing area in pixel and axis length in
 * orghographic projection.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::ChangeProjection(int width, int height, float axisLength)
{    
   GLfloat fAspect = (GLfloat) height / (GLfloat) width;
        
   m_fViewLeft   = -axisLength/2;
   m_fViewRight  =  axisLength/2;
   m_fViewTop    =  axisLength/2;
   m_fViewBottom = -axisLength/2;
   m_fViewNear   = -axisLength/2;
   m_fViewFar    =  axisLength/2;
   
   // save the size we are setting the projection for later use
   if (width <= height)
   {
      m_fLeftPos = m_fViewLeft;
      m_fRightPos = m_fViewRight;
      m_fBottomPos = m_fViewBottom*fAspect;
      m_fTopPos = m_fViewTop*fAspect;
   }
   else
   {
      m_fLeftPos = m_fViewLeft / fAspect;
      m_fRightPos = m_fViewRight / fAspect;
      m_fBottomPos = m_fViewBottom;
      m_fTopPos = m_fViewTop;
   }
}


//------------------------------------------------------------------------------
// int ReadTextTrajectory(const wxString &filename)
//------------------------------------------------------------------------------
/**
 * Reads text trajectory file and initializes OpenGL.
 *
 * @param <filename> file name
 * @return number of data points.
 *
 * @note Assumes the trajectory file has time, x, y, z, vx, vy, vz.
 */
//------------------------------------------------------------------------------
int TrajPlotCanvas::ReadTextTrajectory(const wxString &filename)
{
   int numDataPoints = 0;
   mTextTrajFile =  new TextTrajectoryFile(std::string(filename.c_str()));
    
   if (mTextTrajFile->Open())
   {
      mTrajectoryData = mTextTrajFile->GetData();
        
      numDataPoints = mTrajectoryData.size();

      int sc = 0;
      for(int i=0; i<numDataPoints && i < MAX_DATA; i++)
      {
         mScTrajColor[sc][mNumData] = GmatColor::RED32;
         mTime[mNumData] = mTrajectoryData[i].time;
         mTempScPos[sc][mNumData][0] = mTrajectoryData[i].x;
         mTempScPos[sc][mNumData][1] = mTrajectoryData[i].y;
         mTempScPos[sc][mNumData][2] = mTrajectoryData[i].z;
         mTempEarthPos[mNumData][0] = 0.0;
         mTempEarthPos[mNumData][1] = 0.0;
         mTempEarthPos[mNumData][2] = 0.0;
         mNumData++;
      }

      mTextTrajFile->Close();
      wxLogStatus(MdiGlPlot::mdiParentGlFrame,
                  wxT("Number of data points: %d"), numDataPoints);
   }
   else
   {
      wxString info;
      info.Printf(_T("Cannot open trajectory file name: %s\n"),
                  filename.c_str());
        
      wxMessageDialog msgDialog(this, info, _T("ReadTextTrajectory File"));
      msgDialog.ShowModal();
      return numDataPoints;
   }
        
   // initialize GL
   if (!InitGL())
   {
      wxMessageDialog msgDialog(this, _T("InitGL() failed"),
                                _T("ReadTextTrajectory File"));
      msgDialog.ShowModal();
      return false;
   }
   //      else
   //      {
   //          wxMessageDialog msgDialog(this, _T("InitGL() successful"),
   //                                   _T("ReadTextTrajectory File"));
   //          msgDialog.ShowModal();
   //      }

   mInitialized = true;
    
   return numDataPoints;
    
} //end ReadTextTrajectory()

//------------------------------------------------------------------------------
// void UpdateSpacecraft(const Real &time, const RealArray &posX,
//                       const RealArray &posY, const RealArray &posZ,
//                       const UnsignedIntArray &color)
//------------------------------------------------------------------------------
/**
 * Updates spacecraft trajectory.
 *
 * @param <time> time
 * @param <posX> position x array
 * @param <posY> position y array
 * @param <posZ> position z array
 * @param <orbitColor> orbit color array
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::UpdateSpacecraft(const Real &time, const RealArray &posX,
                                      const RealArray &posY, const RealArray &posZ,
                                      const UnsignedIntArray &color)
{
   mScCount = posX.size();
   if (mScCount > MAX_SCS)
      mScCount = MAX_SCS;

   //--------------------------------------------------
   // update spacecraft position
   //--------------------------------------------------
   if (mNumData < MAX_DATA)
   {
      for (int i=0; i<mScCount; i++)
      {
         mTime[mNumData] = time;
         mTempScPos[i][mNumData][0] = posX[i];
         mTempScPos[i][mNumData][1] = posY[i];
         mTempScPos[i][mNumData][2] = posZ[i];
         mScTrajColor[i][mNumData]  = color[i];
         mTempEarthPos[mNumData][0] = 0.0;
         mTempEarthPos[mNumData][1] = 0.0;
         mTempEarthPos[mNumData][2] = 0.0;
      }
      
      //--------------------------------------------------
      // update planet position
      //--------------------------------------------------
      if (mNumData < MAX_DATA)
      {
         for (int body=0; body<MAX_BODIES; body++)
         {
            if (mBodyInUse[body])
            {
               CelestialBody *bodyPtr = mSolarSystem->GetBody(GmatPlot::BODY_NAME[body]);
               
               Rvector6 bodyState = bodyPtr->GetState(time);
               mBodyGciPos[body][mNumData][0] = bodyState[0];
               mBodyGciPos[body][mNumData][1] = bodyState[1];
               mBodyGciPos[body][mNumData][2] = bodyState[2];
               CopyVector3(mTempBodyPos[body][mNumData],
                           mBodyGciPos[body][mNumData]);

#if DEBUG_TRAJCANVAS_UPDATE
               MessageInterface::ShowMessage
                  ("TrajPlotCanvas::UpdateSpacecraft() %s pos = %f, %f, %f\n",
                   GmatPlot::BODY_NAME[body].c_str(), mTempBodyPos[body][mNumData][0],
                   mTempBodyPos[body][mNumData][1], mTempBodyPos[body][mNumData][2]);
#endif
            }
         }
      }
      
      mNumData++;
   }

   
   Refresh(false);
}

//---------------------------------------------------------------------------
// void AddBody(wxArrayString &bodyNames, UnsignedIntArray &bodyColors)
//---------------------------------------------------------------------------
void TrajPlotCanvas::AddBody(const wxArrayString &bodyNames,
                             const UnsignedIntArray &bodyColors)
{
   // clear bodies
   for (int i=0; i<MAX_BODIES; i++)
   {
      mBodyInUse[i] = false;
   }
   
   int bodyId;
   for (unsigned int i=0; i<bodyNames.GetCount(); i++)
   {
      bodyId = GetStdBodyId(std::string(bodyNames[i].c_str()));
      
      // body is not a standard body
      if (bodyId == UNKNOWN_BODY)
      {
         mOtherBodyCount++;
         bodyId = LAST_STD_BODY_ID + mOtherBodyCount;
         GmatPlot::BODY_NAME[bodyId] = bodyNames[i];
      }
      
      mBodyInUse[bodyId] = true;
      GmatPlot::BODY_COLOR[bodyId] = bodyColors[i];
         
#if DEBUG_TRAJCANVAS_BODY
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::AddBody() body=%s, bodyId=%d, color=%d\n",
          GmatPlot::BODY_NAME[bodyId].c_str(), bodyId, GmatPlot::BODY_COLOR[bodyId]);
#endif
   }
   
   LoadGLTextures();

}

//---------------------------------------------------------------------------
// void GotoStdBody(int bodyId)
//---------------------------------------------------------------------------
void TrajPlotCanvas::GotoStdBody(int bodyId)
{
   
#ifdef DEBUG_TRAJCANVAS_BODY
   MessageInterface::ShowMessage("TrajPlotCanvas::GotoStdBody() bodyId=%d\n",
                                 bodyId);
#endif

   if (!mBodyHasData[bodyId])
   {
      mBodyHasData[bodyId] = true;
      CelestialBody *bodyPtr = mSolarSystem->GetBody(GmatPlot::BODY_NAME[bodyId]);
      mBodyRadius[bodyId] = bodyPtr->GetEquatorialRadius();
      mBodyMaxZoomIn[bodyId] = mBodyRadius[bodyId] * RADIUS_ZOOM_RATIO;

      for (int i=0; i<mNumData; i++)
      {
         Rvector6 bodyState = bodyPtr->GetState(mTime[i]);
         mBodyGciPos[bodyId][i][0] = bodyState[0];
         mBodyGciPos[bodyId][i][1] = bodyState[1];
         mBodyGciPos[bodyId][i][2] = bodyState[2];
         CopyVector3(mTempBodyPos[bodyId][i], mBodyGciPos[bodyId][i]);
      }
   }
   
   mCenterViewBody = bodyId;
   mMaxZoomIn = mBodyMaxZoomIn[bodyId];

   if (bodyId == EARTH)
   {
      mAxisLength = DEFAULT_DIST;
   }
   else
   {
      Rvector3 pos(mTempBodyPos[bodyId][mNumData-1][0],
                   mTempBodyPos[bodyId][mNumData-1][1],
                   mTempBodyPos[bodyId][mNumData-1][2]);
      
      mAxisLength = pos.GetMagnitude();
   }
   
   SetProjection();
   DrawPicture();
   Refresh(false);
}

//---------------------------------------------------------------------------
// void GotoOtherBody(const wxString &body)
//---------------------------------------------------------------------------
void TrajPlotCanvas::GotoOtherBody(const wxString &body)
{
   
#ifdef DEBUG_TRAJCANVAS_BODY
   MessageInterface::ShowMessage("TrajPlotCanvas::GotoOtherBody() body=%s\n",
                                 body.c_str());
#endif
}

//---------------------------------------------------------------------------
// int GetStdBodyId(const std::string &name)
//---------------------------------------------------------------------------
int TrajPlotCanvas::GetStdBodyId(const std::string &name)
{
   for (int i=0; i<=LAST_STD_BODY_ID; i++)
      if (GmatPlot::BODY_NAME[i] == name)
         return i;

   MessageInterface::PopupMessage
      (Gmat::ERROR_, "TrajPlotCanvas::GetStdBodyId() body name: %s not found "
       "in the default list\n", name.c_str());

   return -1;
}

//---------------------------------------------------------------------------
//  void CopyVector3(float to[3], double from[3])
//---------------------------------------------------------------------------
void TrajPlotCanvas::CopyVector3(float to[3], double from[3])
{
   to[0] = from[0];
   to[1] = from[1];
   to[2] = from[2];
}


//---------------------------------------------------------------------------
//  void CopyVector3(float to[3], float from[3])
//---------------------------------------------------------------------------
void TrajPlotCanvas::CopyVector3(float to[3], float from[3])
{
   to[0] = from[0];
   to[1] = from[1];
   to[2] = from[2];
}


//---------------------------------------------------------------------------
//  void CopyVector3(double to[3], double from[3])
//---------------------------------------------------------------------------
void TrajPlotCanvas::CopyVector3(double to[3], double from[3])
{
   to[0] = from[0];
   to[1] = from[1];
   to[2] = from[2];
}


//---------------------------------------------------------------------------
//  void CopyVector3(double to[3], float from[3])
//---------------------------------------------------------------------------
void TrajPlotCanvas::CopyVector3(double to[3], float from[3])
{
   to[0] = from[0];
   to[1] = from[1];
   to[2] = from[2];
}


