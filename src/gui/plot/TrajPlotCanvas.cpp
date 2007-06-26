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
#include "GmatAppData.hpp"        // for GetGuiInterpreter()
#include "FileManager.hpp"        // for texture files
#include "ColorTypes.hpp"         // for namespace GmatColor::
#include "Rvector3.hpp"           // for Rvector3::GetMagnitude()
#include "AngleUtil.hpp"          // for ComputeAngleInDeg()
#include "MdiGlPlotData.hpp"
#include "MessageInterface.hpp"
#include "SubscriberException.hpp"

#include <string.h>               // for strlen()

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
//#  include <GL/glut.h>
#endif

// define SKIP_DEVIL if devil is not supported
//#define SKIP_DEVIL
#ifndef SKIP_DEVIL
// Include without IL/ so different versions of IL can be used without modifying
// the directory
// #  include <IL/il.h>
// #  include <IL/ilu.h>
// #  include <IL/ilut.h>
#  include <il.h>
#  include <ilu.h>
#  include <ilut.h>
#endif


// always define USE_TRACKBALL, it works better for rotation
#define USE_TRACKBALL
#ifdef USE_TRACKBALL
#include "AttitudeUtil.hpp"
using namespace FloatAttUtil;
#endif


// currently lighting is not working
//#define ENABLE_LIGHT_SOURCE
#define USE_MHA_TO_ROTATE_EARTH


// If Sleep in not defined (on unix boxes)
#ifndef Sleep 
#ifndef __WXMSW__
#include <unistd.h>
#define Sleep(t) usleep((t))
#endif
#endif


// if test Euler angle on mouse event
//#define COMPUTE_EULER_ANGLE
#ifdef COMPUTE_EULER_ANGLE
#include "AttitudeUtil.hpp"
//#define USE_MODELVIEW_MAT
//#define DEBUG_TRAJCANVAS_EULER 1
#endif

// For the newer (wx 2.7.x+) method, create a wxGLCanvas window using the
// constructor that does not create an implicit rendering context, create
// an explicit instance of a wxGLContext that is initialized with the
// wxGLCanvas yourself (highly recommended for future compatibility with
// wxWidgets and the flexibility of your own program!) then use either
// wxGLCanvas::SetCurrent with the instance of the wxGLContext or
// wxGLContext::SetCurrent with the instance of the wxGLCanvas to bind the
// OpenGL state that is represented by the rendering context to the canvas,
// and then call wxGLCanvas::SwapBuffers to swap the buffers of the OpenGL
// canvas and thus show your current output.
// This still not working so commented out. But there is a problem with
// showing object and status line in the wx 2.8.4 using implicit GLContext
//#define __USE_WX280_GL__

// skip over limit data
//#define SKIP_OVER_LIMIT_DATA

// debug
//#define DEBUG_TRAJCANVAS_INIT 1
//#define DEBUG_TRAJCANVAS_UPDATE 1
//#define DEBUG_TRAJCANVAS_UPDATE_OBJECT 2
//#define DEBUG_TRAJCANVAS_ACTION 1
//#define DEBUG_TRAJCANVAS_CONVERT 1
//#define DEBUG_TRAJCANVAS_DRAW 2
//#define DEBUG_TRAJCANVAS_ZOOM 1
//#define DEBUG_TRAJCANVAS_OBJECT 1
//#define DEBUG_TRAJCANVAS_TEXTURE 2
//#define DEBUG_TRAJCANVAS_PERSPECTIVE 1
//#define DEBUG_TRAJCANVAS_PROJ 2
//#define DEBUG_TRAJCANVAS_ANIMATION 1
//#define DEBUG_TRAJCANVAS_LONGITUDE 1
//#define DEBUG_SHOW_SKIP 1
//#define DEBUG_ROTATE 1

BEGIN_EVENT_TABLE(TrajPlotCanvas, wxGLCanvas)
   EVT_SIZE(TrajPlotCanvas::OnTrajSize)
   EVT_PAINT(TrajPlotCanvas::OnPaint)
   EVT_MOUSE_EVENTS(TrajPlotCanvas::OnMouse)
   EVT_KEY_DOWN(TrajPlotCanvas::OnKeyDown)
END_EVENT_TABLE()

using namespace GmatPlot;
using namespace GmatMathUtil;

//---------------------------------
// static data
//---------------------------------
const int TrajPlotCanvas::LAST_STD_BODY_ID = 10;
const int TrajPlotCanvas::MAX_COORD_SYS = 10;
const float TrajPlotCanvas::MAX_ZOOM_IN = 3700.0;
const float TrajPlotCanvas::RADIUS_ZOOM_RATIO = 2.2;
const float TrajPlotCanvas::DEFAULT_DIST = 30000.0;
const int TrajPlotCanvas::UNKNOWN_OBJ_ID = -999;

struct GlColorType
{
   Byte red;
   Byte green;
   Byte blue;
   Byte not_used;
};


// color
static int *sIntColor = new int;
static GlColorType *sGlColor = (GlColorType*)sIntColor;

//------------------------------------------------------------------------------
// TrajPlotCanvas(wxWindow *parent, wxWindowID id, 
//                const wxPoint& pos, const wxSize& size, const wxString &csName,
//                SolarSystem *solarSys, const wxString& name, long style)
//------------------------------------------------------------------------------
/**
 * Constructor.
 *
 * @param <parent>   parent window pointer
 * @param <id>       window id
 * @param <pos>      position (top, left) where the window to be placed within the
 *                   parent window
 * @param <size>     size of the window
 * @param <csName>   coordinate system name for data to be plotted in
 * @param <solarSys> solar system pointer to retrieve body information
 * @param <name>     title of window
 * @param <style>    style of window
 *
 */
//------------------------------------------------------------------------------
TrajPlotCanvas::TrajPlotCanvas(wxWindow *parent, wxWindowID id,
                               const wxPoint& pos, const wxSize& size,
                               const wxString &csName, SolarSystem *solarSys,
                               const wxString& name, long style)
   #ifdef __USE_WX280_GL__
   : wxGLCanvas(parent, id, 0, pos, size, style, name)
   #else
   : wxGLCanvas(parent, id, pos, size, style, name)
   #endif
{
   mGlInitialized = false;
   mPlotName = name;
   
   #if DEBUG_TRAJCANVAS_INIT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas() csName=%s, name=%s, size.X=%d, size.Y=%d\n",
       csName.c_str(), name.c_str(), size.GetWidth(), size.GetHeight());
   #endif
   
   #ifdef __USE_WX280_GL__
   // Note:
   // Use wxGLCanvas::m_glContext, otherwise resize will not work
   m_glContext = new wxGLContext(this);
   #endif
   
   // initalize data members
   theGuiInterpreter = GmatAppData::GetGuiInterpreter();
   theStatusBar = GmatAppData::GetMainFrame()->GetStatusBar();
   mTextTrajFile = NULL;
   mGlList = 0;
   mIsFirstRun = true;
   
   ResetPlotInfo();
   
   // projection
   mUsePerspectiveMode = false;
   
   // viewpoint
   mViewPointRefObjName = "UNKNOWN";
   mViewPointRefObj = NULL;
   mViewPointVectorObj = NULL;
   mViewDirectionObj = NULL;
   mViewPointRefVector.Set(0.0, 0.0, 0.0);
   mViewPointVector.Set(0.0, 0.0, 30000.0);
   mViewDirectionVector.Set(0.0, 0.0, -1.0);
   mVpLocVec.Set(0.0, 0.0, 30000.0);
   mViewScaleFactor = 1.0;
   mUseInitialViewPoint = false;
   mUseViewPointRefVector = true;
   mUseViewPointVector = true;
   mUseViewDirectionVector = true;
   mVpRefObjId = UNKNOWN_OBJ_ID;
   mVpVecObjId = UNKNOWN_OBJ_ID;
   mVdirObjId = UNKNOWN_OBJ_ID;
   
   // devault view
   mCanvasSize = size;
   mDefaultRotXAngle = 90.0;
   mDefaultRotYAngle = 0.0;
   mDefaultRotZAngle = 0.0;
   mDefaultViewDist = DEFAULT_DIST;
   
   // view model
   mUseGluLookAt = true;
   mUseSingleRotAngle = true;
   
   // performance
   // if mNumPointsToRedraw =  0 It redraws whole plot
   // if mNumPointsToRedraw = -1 It does not clear GL_COLOR_BUFFER
   mNumPointsToRedraw = 0;
   mRedrawLastPointsOnly = false;
   mUpdateFrequency = 50;
   
   mAxisLength = mCurrViewDist;
   
   mOriginName = "";
   mOriginId = 0;
   
   //original value
   mRotateAboutXaxis = true;   // 2-3-1
   mRotateAboutYaxis = false;  // 3-1-2
   mRotateAboutZaxis = false;  // 1-2-3
   
   //mRotateAboutXaxis = false;   // 2-3-1
   //mRotateAboutYaxis = false ;  // 3-1-2
   //mRotateAboutZaxis = true;    // 1-2-3
   
   mRotateXy = true;
   
   mZoomAmount = 300.0;
   
   // projection
   ChangeProjection(size.x, size.y, mAxisLength);
   
   mEarthRadius = 6378.14; //km
   mScRadius = 200;        //km: make big enough to see
   
   // light source
   mSunPresent = false;
   mEnableLightSource = true;
   
   // drawing options
   mDrawWireFrame = false;
   mDrawXyPlane = false;
   mDrawEcPlane = false;
   mDrawAxes = false;
   mDrawGrid = false;
   mDrawOrbitNormal = true;
   mXyPlaneColor = GmatColor::SKYBLUE;
   mEcPlaneColor = GmatColor::CHESTNUT;
   mSunLineColor = GmatColor::GOLDTAN;
   
   // animation
   mViewAnimation = false;
   mHasUserInterrupted = false;
   mUpdateInterval = 1;
   mFrameInc = 1;
   
   // message
   mShowMaxWarning = true;
   mOverCounter = 0;
   
   // solar system
   mSolarSystem = solarSys;
   //MessageInterface::ShowMessage
   //   ("==> TrajPlotCanvas::TrajPlotCanvas() mSolarSystem=%p\n", mSolarSystem);
   
   // objects
   mObjectDefaultRadius = 200; //km: make big enough to see
   mObjectRadius = NULL;
   mObjMaxZoomIn = NULL;
   mObjLastFrame = NULL;
   mDrawOrbitFlag = NULL;
   mObjectOrbitColor = NULL;
   mObjectGciPos = NULL;
   mObjectGciVel = NULL;
   mObjectIniPos = NULL;
   mObjectIniVel = NULL;
   mObjectTmpPos = NULL;
   mObjectTmpVel= NULL;
   
   // Zoom
   mMaxZoomIn = MAX_ZOOM_IN;
   
   // Spacecraft
   mScCount = 0;
   
   // Coordinate System
   mInternalCoordSystem = theGuiInterpreter->GetInternalCoordinateSystem();
   mInternalCoordSysName = wxString(mInternalCoordSystem->GetName().c_str());
   
   mViewCoordSysName = csName;
   mViewCoordSystem = (CoordinateSystem*)theGuiInterpreter->
      GetConfiguredObject(std::string(csName.c_str()));
   
   // CoordinateSystem conversion
   mIsInternalCoordSystem = true;
   mNeedSpacecraftConversion = false;
   mNeedOriginConversion = false;
   mNeedObjectConversion = false;
   
   if (!mViewCoordSysName.IsSameAs(mInternalCoordSysName))
      mNeedInitialConversion = true;
   else
      mNeedInitialConversion = false;
   
   #if DEBUG_TRAJCANVAS_INIT
   MessageInterface::ShowMessage
      ("   mInternalCoordSystem=%p, mViewCoordSystem=%p\n", mInternalCoordSystem,
       mViewCoordSystem);
   if (mInternalCoordSystem)
      MessageInterface::ShowMessage
         ("   mInternalCoordSystem=%s\n", mInternalCoordSystem->GetName().c_str());
   if (mViewCoordSystem)
      MessageInterface::ShowMessage
         ("   mViewCoordSystem=%s\n", mViewCoordSystem->GetName().c_str());
   MessageInterface::ShowMessage("TrajPlotCanvas() constructor exiting\n");
   #endif
   
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

   // Note:
   // deleting m_glContext is handled in wxGLCanvas
   
   ClearObjectArrays();
   
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
   
   // enable depth testing, so that objects further away from the
   // viewer aren't drawn over closer objects
   glEnable(GL_DEPTH_TEST);
   
   glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
   glDepthFunc(GL_LESS);
   //glDepthRange(0.0, 100.0); //loj: just tried - made no difference
   
   // speedups
   glEnable(GL_DITHER);
   
   // set polygons to be smoothly shaded (i.e. interpolate lighting equations
   // between points on polygons).
   glShadeModel(GL_SMOOTH);
   
   glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
   glHint(GL_POLYGON_SMOOTH_HINT, GL_FASTEST);
   
#ifndef SKIP_DEVIL
   
   // initalize devIL library
   ilInit();
   ilutInit();
   ilutRenderer(ILUT_OPENGL);
   
#endif
   
   if (!LoadGLTextures())
      return false;
   
   // pixel format
   if (!SetPixelFormatDescriptor())
   {
      throw SubscriberException("SetPixelFormatDescriptor failed\n");
   }
   
   // font
   SetDefaultGLFont();

   mShowMaxWarning = true;
   mViewAnimation = false;
   
   return true;
}


//------------------------------------------------------------------------------
// wxString GetGotoObjectName()
//------------------------------------------------------------------------------
wxString TrajPlotCanvas::GetGotoObjectName()
{
   return mObjectNames[mViewObjId];
}


//------------------------------------------------------------------------------
// wxGLContext* GetGLContext()
//------------------------------------------------------------------------------
/*
 * Return current GLContext pointer.
 */
//------------------------------------------------------------------------------
wxGLContext* TrajPlotCanvas::GetGLContext()
{
   return m_glContext;
}


//------------------------------------------------------------------------------
// void SetEndOfRun(bool flag = true)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetEndOfRun(bool flag)
{
   #if DEBUG_TRAJCANVAS_UPDATE
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::SetEndOfRun() TrajPlotCanvas::SetEndOfRun() flag=%d, "
       "mNumData=%d\n",  flag, mNumData);
   #endif
   
   mIsEndOfRun = flag;
   mIsEndOfData = flag;
   
   if (mNumData < 1)
   {
      Refresh(false);
      return;
   }
   
   if (mIsEndOfRun)
   {
      #if DEBUG_TRAJCANVAS_LONGITUDE
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::SetEndOfRun() mIsEndOfRun=%d, mNumData=%d\n",
          mIsEndOfRun, mNumData);
      #endif
      
      //-------------------------------------------------------
      // get first spacecraft id
      //-------------------------------------------------------
      int objId = UNKNOWN_OBJ_ID;
      for (int sc=0; sc<mScCount; sc++)
      {
         objId = GetObjectId(mScNameArray[sc].c_str());
         
         if (objId != UNKNOWN_OBJ_ID)
            break;
      }
      
      int index = objId * MAX_DATA * 3 + (mNumData-1) * 3;
      Real time = mTime[mNumData-1];
      Real x = mObjectGciPos[index+0];
      Real y = mObjectGciPos[index+1];;
      Real lst, longitude, mha;
      
      ComputeLongitudeLst(time, x, y, &mha, &longitude, &lst);
      mFinalMha = mha;
      mFinalLongitude = longitude;
      mFinalLst = lst;
      
      #if DEBUG_TRAJCANVAS_LONGITUDE
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::SetEndOfRun() mInitialLongitude=%f, time=%f, x=%f,\n   "
          "y=%f, mFinalMha=%f, mFinalLongitude=%f, mFinalLst=%f\n",
          mInitialLongitude, time, x, y, mha, longitude, lst);
      #endif

   }
}


//------------------------------------------------------------------------------
// void SetViewCoordSystem(const wxString &csName)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetViewCoordSystem(const wxString &csName)
{
   #if DEBUG_TRAJCANVAS_INIT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas()::SetViewCoordSysName() internalCS=%s, viewCS=%s\n",
       mInternalCoordSystem->GetName().c_str(), csName.c_str());
   #endif
   
   mViewCoordSysName = csName;
   
   mViewCoordSystem =
      //theGuiInterpreter->GetCoordinateSystem(std::string(csName.c_str()));
      (CoordinateSystem*)theGuiInterpreter->GetConfiguredObject(std::string(csName.c_str()));
   
   if (!mViewCoordSysName.IsSameAs(mInternalCoordSysName))
   {
      mIsInternalCoordSystem = false;
      mNeedInitialConversion = true;
      mNeedOriginConversion = true;
      mNeedObjectConversion = true;
   }
   else
   {
      mIsInternalCoordSystem = true;
      mNeedInitialConversion = false;
      mNeedOriginConversion = false;
      mNeedObjectConversion = false;
   }
   
}


//------------------------------------------------------------------------------
// void SetUsePerspectiveMode(bool perspMode)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetUsePerspectiveMode(bool perspMode)
{
   #if DEBUG_TRAJCANVAS_INIT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas()::SetUsePerspectiveMode() perspMode=%d\n", perspMode);
   #endif
   
   mUsePerspectiveMode = perspMode;
   
   if (mUsePerspectiveMode)
   {
      mfCamTransX = -mVpLocVec[0];
      mfCamTransY = -mVpLocVec[1];
      mfCamTransZ = -mVpLocVec[2];
      mUseGluLookAt = true; //loj: 12/7/05 try gluLookAt()
   }
   else
   {
      mfCamTransX = 0;
      mfCamTransY = 0;
      mfCamTransZ = 0;
      
      //loj: 12/27/07 set to use gluLookAt, it works for up direction
      //mUseGluLookAt = false;
      mUseGluLookAt = true;
   }
}


//------------------------------------------------------------------------------
// void SetObjectColors(const wxStringColorMap &objectColorMap)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetObjectColors(const wxStringColorMap &objectColorMap)
{
   mObjectColorMap = objectColorMap;
}


//------------------------------------------------------------------------------
// void SetShowObjects(const wxStringColorMap &showObjMap)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetShowObjects(const wxStringBoolMap &showObjMap)
{
   mShowObjectMap = showObjMap;
}


//------------------------------------------------------------------------------
// void SetShowOrbitNormals(const wxStringColorMap &showOrbNormMap)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetShowOrbitNormals(const wxStringBoolMap &showOrbNormMap)
{
   mShowOrbitNormalMap = showOrbNormMap;
}


//------------------------------------------------------------------------------
// void SetGLContext(wxGLContext *glContext)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetGLContext(wxGLContext *glContext)
{
   #ifdef __USE_WX280_GL__
   if (glContext == NULL)
      SetCurrent(*m_glContext);
   else
      SetCurrent(*glContext);
   #else
   SetCurrent();
   #endif
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
   
   // In wxWidgets-2.8.4, this shows previous plot
   #ifndef __USE_WX280_GL__
   SwapBuffers();
   #endif
}


//------------------------------------------------------------------------------
// void ResetPlotInfo()
//------------------------------------------------------------------------------
/**
 * Resets ploting infomation.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::ResetPlotInfo()
{
   mNumData = 0;
   mTotalPoints = 0;
   mBegIndex = 0;
   mOverCounter = 0;
   mIsEndOfData = false;
   mIsEndOfRun = false;
   
   // Initialize view
   if (mUseInitialViewPoint)
      SetDefaultView();
}


//------------------------------------------------------------------------------
// void RedrawPlot(bool viewAnimation)
//------------------------------------------------------------------------------
/**
 * Redraws plot.
 *
 * @param <viewAnimation> true if animation is viewed
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::RedrawPlot(bool viewAnimation)
{
   if (mAxisLength < mMaxZoomIn)
   {
      mAxisLength = mMaxZoomIn;
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::RedrawPlot() distance < max zoom in. distance set to %f\n",
          mAxisLength);
   }

   if (viewAnimation)
      ViewAnimation(mUpdateInterval, mFrameInc);
   else
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

   SetDefaultView();
   ChangeView(mCurrRotXAngle, mCurrRotYAngle, mCurrRotZAngle);
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
   #if DEBUG_TRAJCANVAS_ZOOM
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::ZoomIn() mAxisLength=%f, mMaxZoomIn=%f\n",
       mAxisLength, mMaxZoomIn);
   #endif

   Real realDist = (mAxisLength - mZoomAmount) / log(mAxisLength);

   if (mUsePerspectiveMode)
   {
      if (mAxisLength > mMaxZoomIn/mFovDeg*4)
      {
         mAxisLength = mAxisLength - realDist;
         
         if (mAxisLength < mObjectRadius[mOriginId]/2.0)
            mAxisLength = mObjectRadius[mOriginId]/2.0;
         
         ChangeProjection(mCanvasSize.x, mCanvasSize.y, mAxisLength);
      }
   }
   else
   {
      if (mAxisLength > mMaxZoomIn)
      {
         mAxisLength = mAxisLength - realDist;

         if (mAxisLength < mMaxZoomIn)
            mAxisLength = mMaxZoomIn;
   
         ChangeProjection(mCanvasSize.x, mCanvasSize.y, mAxisLength);
      }
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
   Real realDist = (mAxisLength + mZoomAmount) / log(mAxisLength);
   mAxisLength = mAxisLength + realDist;

   ChangeProjection(mCanvasSize.x, mCanvasSize.y, mAxisLength);
    
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
// void DrawXyPlane(bool flag)
//------------------------------------------------------------------------------
/**
 * Draws equatorial plane
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawXyPlane(bool flag)
{
   mDrawXyPlane = flag;
   Refresh(false);
}


//------------------------------------------------------------------------------
// void DrawEcPlane(bool flag)
//------------------------------------------------------------------------------
/**
 * Draws ecliptic plane
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawEcPlane(bool flag)
{
   mDrawEcPlane = flag;
   Refresh(false);
}


//------------------------------------------------------------------------------
// void OnDrawAxes(bool flag)
//------------------------------------------------------------------------------
/**
 * Draws axes.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::OnDrawAxes(bool flag)
{
   mDrawAxes = flag;
   Refresh(false);
}


//------------------------------------------------------------------------------
// void OnDrawGrid(bool flag)
//------------------------------------------------------------------------------
/**
 * Draws axes.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::OnDrawGrid(bool flag)
{
   mDrawGrid = flag;
   Refresh(false);
}


//------------------------------------------------------------------------------
// void DrawInOtherCoordSystem(const wxString &csName)
//------------------------------------------------------------------------------
/**
 * Draws objects in other coordinate system.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawInOtherCoordSystem(const wxString &csName)
{
   #if DEBUG_TRAJCANVAS_ACTION
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::DrawInOtherCoordSysName() viewCS=%s, newCS=%s\n",
       mViewCoordSysName.c_str(), csName.c_str());
   #endif
   
   // if current view CS name is different from the new CS name
   if (!mViewCoordSysName.IsSameAs(csName))
   {
      mViewCoordSysName = csName;
      
      mViewCoordSystem =
         //theGuiInterpreter->GetCoordinateSystem(std::string(csName.c_str()));
         (CoordinateSystem*)theGuiInterpreter->GetConfiguredObject(std::string(csName.c_str()));
      
      if (mViewCoordSystem->GetName() == mInternalCoordSystem->GetName())
         mIsInternalCoordSystem = true;
      else
         mIsInternalCoordSystem = false;
      
      wxString oldOriginName = mOriginName;
      mOriginName = mViewCoordSystem->GetOriginName().c_str();
      mOriginId = GetObjectId(mOriginName);
      
      mNeedSpacecraftConversion = true;
      mNeedOriginConversion = true;
      mNeedObjectConversion = true;

      UpdateRotateFlags();
      
      if (!mOriginName.IsSameAs(oldOriginName))
         GotoObject(mOriginName);
      
      ConvertObjectData();
      Refresh(false);
   }
   else
   {
      mNeedSpacecraftConversion = false;
      mNeedOriginConversion = false;
      mNeedObjectConversion = false;
   }
}


//---------------------------------------------------------------------------
// void GotoObject(const wxString &objName)
//---------------------------------------------------------------------------
void TrajPlotCanvas::GotoObject(const wxString &objName)
{
   int objId = GetObjectId(objName);
   
   mViewObjId = objId;
   mMaxZoomIn = mObjMaxZoomIn[objId];
   
   // if goto Object is center(0,0,0), zoom out to see the object,
   // otherwise, set to final position of the object
   if (objName == mViewObjName)
   {
      mAxisLength = mMaxZoomIn;
   }
   else
   {
      int index = objId * MAX_DATA * 3 + (mNumData-1) * 3;
      // compute mAxisLength
      Rvector3 pos(mObjectTmpPos[index+0], mObjectTmpPos[index+1],
                   mObjectTmpPos[index+2]);
      
      mAxisLength = pos.GetMagnitude();
      
      if (mAxisLength == 0.0)
         mAxisLength = mMaxZoomIn;
   }
   
   #ifdef DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::GotoObject() objName=%s, mViewObjId=%d, mMaxZoomIn=%f\n"
       "   mAxisLength=%f\n", objName.c_str(), mViewObjId, mMaxZoomIn, mAxisLength);
   #endif

   mIsEndOfData = true;
   mIsEndOfRun = true;

}


//---------------------------------------------------------------------------
// void GotoOtherBody(const wxString &body)
//---------------------------------------------------------------------------
void TrajPlotCanvas::GotoOtherBody(const wxString &body)
{
   #ifdef DEBUG_TRAJCANVAS_OBJECT
      MessageInterface::ShowMessage("TrajPlotCanvas::GotoOtherBody() body=%s\n",
                                    body.c_str());
   #endif
}


//---------------------------------------------------------------------------
// void ViewAnimation(int interval, int frameInc)
//---------------------------------------------------------------------------
void TrajPlotCanvas::ViewAnimation(int interval, int frameInc)
{
   #ifdef DEBUG_TRAJCANVAS_ANIMATION
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::ViewAnimation() interval=%d, frameInc=%d\n",
       interval, frameInc);
   #endif
   
   this->SetFocus(); // so that it can get key interrupt
   mViewAnimation = true;
   mUpdateInterval = interval;
   mFrameInc = frameInc;
   mHasUserInterrupted = false;
   
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   DrawFrame();
}


//------------------------------------------------------------------------------
// void SetGlObject(const StringArray &objNames,
//                  const UnsignedIntArray &objOrbitColors,
//                  const std::vector<SpacePoint*> &objArray)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetGlObject(const StringArray &objNames,
                                 const UnsignedIntArray &objOrbitColors,
                                 const std::vector<SpacePoint*> &objArray)
{
   #if DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::SetGlObject() objCount=%d, colorCount=%d.\n",
       objNames.size(), objOrbitColors.size());
   #endif
   
   mObjectArray = objArray;
   wxArrayString tempList;
   
   if (objNames.size() == objOrbitColors.size() &&
       objNames.size() == objArray.size())
   {
      for (UnsignedInt i=0; i<objNames.size(); i++)
      {
         tempList.Add(objNames[i].c_str());
      
         #if DEBUG_TRAJCANVAS_OBJECT > 1
         MessageInterface::ShowMessage
            ("TrajPlotCanvas::SetGlObject()  objNames[%d]=%s, objName=%s, "
             "addr=%d\n", i, objNames[i].c_str(),
             mObjectArray[i]->GetName().c_str(), mObjectArray[i]);
         #endif
      }
      
      AddObjectList(tempList, objOrbitColors);
   }
   else
   {
      MessageInterface::ShowMessage("TrajPlotCanvas::SetGlObject() object sizes "
                                    "are not the same. No ojbects added.\n");
   }
}


//------------------------------------------------------------------------------
// void SetGlCoordSystem(CoordinateSystem *viewCs, CoordinateSystem *viewUpCs)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetGlCoordSystem(CoordinateSystem *viewCs,
                                      CoordinateSystem *viewUpCs)
{
   mInitialCoordSystem = viewCs;
   mInitialCoordSysName = wxString(viewCs->GetName().c_str());
   
   mViewCoordSystem = mInitialCoordSystem;
   mViewCoordSysName = mInitialCoordSysName;
   
   mViewUpCoordSystem = viewUpCs;
   mViewUpCoordSysName = wxString(viewUpCs->GetName().c_str());
   
   // set view center object
   mOriginName = wxString(viewCs->GetOriginName().c_str());   
   mOriginId = GetObjectId(mOriginName);
   
   mViewObjName = mOriginName;
   mViewObjId = mOriginId;
   
   // if view coordinate system origin is spacecraft, make spacecraft radius smaller.
   // So that spapcecraft won't overlap each other.
   //@todo: need better way to scale spacecraft size.
   if (viewCs->GetOrigin()->IsOfType(Gmat::SPACECRAFT))
      mScRadius = 30;
   else if (viewCs->GetOrigin()->IsOfType(Gmat::CELESTIAL_BODY))
      mScRadius = mObjectRadius[mOriginId] * 0.03;
   
   // set center view object as origin of the CoordinateSystem if view direction
   // is not an object
   if (!mUseViewDirectionVector && mViewDirectionObj != NULL)
   {
      mViewObjName = wxString(mViewDirectionObj->GetName().c_str());
      //mViewObjId = GetObjectId(mViewObjName);
      //loj: 11/2/05 commented out because when solar system is deleted in the
      //Sandbox it crashes when rerunning the default mission
   }
   
   mMaxZoomIn = mObjMaxZoomIn[mOriginId];
   
   if (mUseInitialViewPoint)
   {
      mAxisLength = mMaxZoomIn;
   }
   
   UpdateRotateFlags();
   MakeValidCoordSysList();

   // add initial view coord. system if not added already
   if (mValidCSNames.Index(mInitialCoordSysName) == wxNOT_FOUND)
      mValidCSNames.Add(mInitialCoordSysName);

   #if DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage
      ("   viewCSName=%s, mViewCoordSystem=%d, originName=%s, "
       "mOriginId=%d\n", mViewCoordSysName.c_str(), mViewCoordSystem,
       mOriginName.c_str(),  mOriginId);
   MessageInterface::ShowMessage
      ("   mViewObjName=%s, mViewObjId=%d\n", mViewObjName.c_str(), mViewObjId);
   MessageInterface::ShowMessage
      ("   mViewUpCoordSysName=%s\n", mViewUpCoordSysName.c_str());
   #endif

} // end SetGlCoordSystem()


//------------------------------------------------------------------------------
// void SetGlViewOption(SpacePoint *vpRefObj, SpacePoint *vpVecObj,
//                      SpacePoint *vdObj, Real vsFactor,
//                      const Rvector3 &vpRefVec, const Rvector3 &vpVec,
//                      const Rvector3 &vdVec, bool usevpRefVec,
//                      bool usevpVec, bool usevdVec,
//                      bool useFixedFov, Real fov)
//------------------------------------------------------------------------------
/*
 * Sets OpenGL view options
 *
 * @param <vpRefObj> Viewpoint reference object pointer
 * @param <vpVecObj> Viewpoint vector object pointer
 * @param <vdObj>  View direction object pointer
 * @param <vsFactor> Viewpoint scale factor
 * @param <vpRefVec> 3 element vector for viewpoint ref. vector (use if usevpVec is true)
 * @param <vpVec> 3 element vector for viewpoint vector (use if usevpVec is true)
 * @param <vdVec> 3 element vector for view direction (use if usevdVec is true)
 * @param <usevpRefVec> true if use vector for viewpoint reference vector
 * @param <usevpVec> true if use vector for viewpoint vector
 * @param <usevdVec> true if use vector for view direction
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetGlViewOption(SpacePoint *vpRefObj, SpacePoint *vpVecObj,
                                     SpacePoint *vdObj, Real vsFactor,
                                     const Rvector3 &vpRefVec, const Rvector3 &vpVec,
                                     const Rvector3 &vdVec, const std::string &upAxis,
                                     bool usevpRefVec, bool usevpVec, bool usevdVec,
                                     bool useFixedFov, Real fov)
{
   mViewPointRefObj = vpRefObj;
   mViewPointVectorObj = vpVecObj;
   mViewDirectionObj = vdObj;
   mViewScaleFactor = vsFactor;
   mViewPointRefVector = vpRefVec;
   mViewPointVector = vpVec;
   mViewDirectionVector = vdVec;
   mViewUpAxisName = upAxis;
   mUseViewPointRefVector = usevpRefVec;
   mUseViewPointVector = usevpVec;
   mUseViewDirectionVector = usevdVec;
   mUseFixedFov = useFixedFov;
   mFixedFovAngle = fov;
   
   Rvector3 lvpRefVec(vpRefVec);
   Rvector3 lvpVec(vpVec);
   Rvector3 lvdVec(vdVec);
   
   #if DEBUG_TRAJCANVAS_PROJ
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::SetGlViewOption() mViewPointRefObj=%d, "
       "mViewPointVectorObj=%d\n   mViewDirectionObj=%d, mViewScaleFactor=%f   "
       "mViewPointRefVector=%s\n   mViewPointVector=%s, mViewDirectionVector=%s, "
       "mViewUpAxisName=%s\n   mUseViewPointRefVector=%d, mUseViewDirectionVector=%d, "
       "mUseFixedFov=%d, mFixedFovAngle=%f\n",  mViewPointRefObj, mViewPointVectorObj,
       mViewDirectionObj, mViewScaleFactor, lvpRefVec.ToString().c_str(),
       lvpVec.ToString().c_str(), lvdVec.ToString().c_str(), mViewUpAxisName.c_str(),
       mUseViewPointRefVector, mUseViewDirectionVector, mUseFixedFov, mFixedFovAngle);
   #endif
   
   // Set viewpoint ref. object id
   if (!mUseViewPointRefVector && mViewPointRefObj)
   {
      mViewPointRefObjName = mViewPointRefObj->GetName();
      
      mVpRefObjId = GetObjectId(mViewPointRefObj->GetName().c_str());
      
      if (mVpRefObjId == GmatPlot::UNKNOWN_BODY)
      {
         mUseViewPointRefVector = true;
         MessageInterface::ShowMessage
            ("*** Warning *** TrajPlotCanvas::SetGlViewOption() Cannot find "
             "mViewPointRefObj name=%s, so using vector=%s\n",
             mViewPointRefObj->GetName().c_str(),
             mViewPointRefVector.ToString().c_str());
      }
   }
   else
   {
      mViewPointRefObjName = "Earth";
      
      if (!mUseViewPointRefVector)
         MessageInterface::ShowMessage
            ("*** Warning *** TrajPlotCanvas::SetGlViewOption() "
             "ViewPointRefObject is NULL,"
             "so will use default Vector instead.\n");
   }
   
   // Set viewpoint vector object id
   if (!mUseViewPointVector && mViewPointVectorObj)
   {
      mVpVecObjId = GetObjectId(mViewPointVectorObj->GetName().c_str());
      
      if (mVpVecObjId == GmatPlot::UNKNOWN_BODY)
      {
         mUseViewPointVector = true;
         MessageInterface::ShowMessage
            ("*** Warning *** TrajPlotCanvas::SetGlViewOption() Cannot find "
             "mViewPointVectorObj name=%s, so using vector=%s\n",
             mViewPointVectorObj->GetName().c_str(),
             mViewPointVector.ToString().c_str());
      }
   }
   else
   {
      if (!mUseViewPointVector)
         MessageInterface::ShowMessage
            ("*** Warning *** TrajPlotCanvas::SetGlViewOption() "
             "ViewPointVectorObject is NULL, "
             "so will use default Vector instead.\n");
   }
   
   // Set view direction object id
   if (!mUseViewDirectionVector && mViewDirectionObj)
   {
      mVdirObjId = GetObjectId(mViewDirectionObj->GetName().c_str());
      
      if (mVdirObjId == GmatPlot::UNKNOWN_BODY)
      {
         mUseViewDirectionVector = true;
         MessageInterface::ShowMessage
            ("*** Warning *** TrajPlotCanvas::SetGlViewOption() Cannot find "
             "mViewDirectionObj name=%s, so using vector=%s\n",
             mViewDirectionObj->GetName().c_str(),
             mViewDirectionVector.ToString().c_str());
      }
   }
   else
   {
      if (!mUseViewDirectionVector)
         MessageInterface::ShowMessage
            ("*** Warning *** TrajPlotCanvas::SetGlViewOption() "
             "ViewDirectionObject is NULL,"
             "so will use default Vector instead.\n");
   }
   
   // Set view up direction
   if (mViewUpAxisName == "X")
      mUpState.Set(-1.0, 0.0, 0.0, 0.0, 0.0, 0.0);
   else if (mViewUpAxisName == "-X")
      mUpState.Set(1.0, 0.0, 0.0, 0.0, 0.0, 0.0);
   else if (mViewUpAxisName == "Y")
      mUpState.Set(0.0, -1.0, 0.0, 0.0, 0.0, 0.0);
   else if (mViewUpAxisName == "-Y")
      mUpState.Set(0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
   else if (mViewUpAxisName == "Z")
      mUpState.Set(0.0, 0.0, 1.0, 0.0, 0.0, 0.0);
   else if (mViewUpAxisName == "-Z")
      mUpState.Set(0.0, 0.0, -1.0, 0.0, 0.0, 0.0);
   
} //end SetGlViewOption()


//------------------------------------------------------------------------------
// void SetGlDrawOrbitFlag(const std::vector<bool> &drawArray)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetGlDrawOrbitFlag(const std::vector<bool> &drawArray)
{
   mDrawOrbitArray = drawArray;
   
   #if DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::SetGlDrawObjectFlag() mDrawOrbitArray.size()=%d, "
       "mObjectCount=%d\n", mDrawOrbitArray.size(), mObjectCount);
   
   bool draw;
   for (int i=0; i<mObjectCount; i++)
   {
      draw = mDrawOrbitArray[i] ? true : false;      
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::SetGlDrawObjectFlag() mDrawOrbitArray[%d]=%d\n",
          i, draw);
   }
   #endif
}


//------------------------------------------------------------------------------
// void SetGlShowObjectFlag(const std::vector<bool> &showArray)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetGlShowObjectFlag(const std::vector<bool> &showArray)
{
   mShowObjectArray = showArray;

   #if DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::SetGlDrawObjectFlag() mDrawOrbitArray.size()=%d, "
       "mObjectCount=%d\n", mShowObjectArray.size(), mObjectCount);
   #endif
   
   bool show;
   mSunPresent = false;
   
   for (int i=0; i<mObjectCount; i++)
   {
      show = mShowObjectArray[i] ? true : false;
      mShowObjectMap[mObjectNames[i]] = show;
      
      if (mObjectNames[i] == "Sun" && mShowObjectMap["Sun"])
         mSunPresent = true;
      
      #if DEBUG_TRAJCANVAS_OBJECT
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::SetGlShowObjectFlag() mShowObjectMap[%s]=%d\n",
          mObjectNames[i].c_str(), show);
      #endif
   }
   
   #if DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::SetGlDrawObjectFlag() mEnableLightSource=%d, mSunPresent=%d\n",
       mEnableLightSource, mSunPresent);
   #endif
   
   //loj: 11/4/05 Added light source
   #ifdef ENABLE_LIGHT_SOURCE
   if (mEnableLightSource && mSunPresent)
   {
      //----------------------------------------------------------------------
      // set OpenGL to recognize the counter clockwise defined side of a polygon
      // as its 'front' for lighting and culling purposes
      glFrontFace(GL_CCW);
      
      // enable face culling, so that polygons facing away (defines by front face)
      // from the viewer aren't drawn (for efficieny).
      glEnable(GL_CULL_FACE);

      // create a light:
      float lightColor[4]={1.0f, 1.0f, 1.0f, 1.0f};
      
      glLightfv(GL_LIGHT0, GL_AMBIENT_AND_DIFFUSE, lightColor);
      glLightfv(GL_LIGHT0, GL_SPECULAR, lightColor);
      
      // enable the light
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
      
      // tell OpenGL to use glColor() to get material properties for..
      glEnable(GL_COLOR_MATERIAL);
      
      // ..the front face's ambient and diffuse components
      glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
      //----------------------------------------------------------------------
   }
   #endif
   
}


//------------------------------------------------------------------------------
// void SetNumPointsToRedraw(Integer numPoints)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetNumPointsToRedraw(Integer numPoints)
{
   mNumPointsToRedraw = numPoints;
   mRedrawLastPointsOnly = false;

   // if mNumPointsToRedraw =  0 It redraws whole plot
   // if mNumPointsToRedraw = -1 It does not clear GL_COLOR_BUFFER
   if (mNumPointsToRedraw > 0)
      mRedrawLastPointsOnly = true;
}


//------------------------------------------------------------------------------
// void SetUpdateFrequency(Integer updFreq)
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetUpdateFrequency(Integer updFreq)
{
   mUpdateFrequency = updFreq;
}


//------------------------------------------------------------------------------
// void UpdatePlot(const StringArray &scNames, const Real &time,
//                 const RealArray &posX, const RealArray &posY,
//                 const RealArray &posZ, const RealArray &velX,
//                 const RealArray &velY, const RealArray &velZ,
//                 const UnsignedIntArray &scColors)
//------------------------------------------------------------------------------
/**
 * Updates spacecraft trajectory. Position and velocity should be in EarthMJ2000Eq
 * coordinate system
 *
 * @param <scNames> spacecraft name array
 * @param <time> time
 * @param <posX> position x array
 * @param <posY> position y array
 * @param <posZ> position z array
 * @param <velX> velocity x array
 * @param <velY> velocity y array
 * @param <velZ> velocity z array
 * @param <scColors> orbit color array
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::UpdatePlot(const StringArray &scNames, const Real &time,
                                const RealArray &posX, const RealArray &posY,
                                const RealArray &posZ, const RealArray &velX,
                                const RealArray &velY, const RealArray &velZ,
                                const UnsignedIntArray &scColors)
{   
   mScCount = scNames.size();
   
   if (mScCount > MAX_SCS)
      mScCount = MAX_SCS;
   
   #if DEBUG_TRAJCANVAS_UPDATE
   static int sNumDebugOutput = 100;
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::UpdatePlot() plot=%s, time=%f, mNumData=%d, mScCount=%d\n",
       GetName().c_str(), time, mNumData, mScCount);
   #endif
   
   mScNameArray = scNames;
   mTotalPoints++;
   
   if (mNumData < MAX_DATA)
   {
      mTime[mNumData] = time;
      Real mha, longitude, lst;
      ComputeLongitudeLst(mTime[mNumData], posX[0], posY[0], &mha, &longitude, &lst);
      
      #if DEBUG_TRAJCANVAS_LONGITUDE
      MessageInterface::ShowMessage
         ("   time=%f, mNumData=%d, mha=%f, longitude=%f, lst = %f\n",
          mTime[mNumData], mNumData, mha, longitude, lst);
      #endif
      
      if (mNumData == 0)
      {
         mInitialLongitude = longitude;
         mInitialMha = mha;
         #if DEBUG_TRAJCANVAS_LONGITUDE
         MessageInterface::ShowMessage
            ("TrajPlotCanvas::UpdatePlot() mInitialLongitude = %f, mInitialMha = %f\n",
             mInitialLongitude, mInitialMha);
         #endif
      }
            
      //-------------------------------------------------------
      // update spacecraft position
      //-------------------------------------------------------
      for (int sc=0; sc<mScCount; sc++)
      {
         int objId = GetObjectId(mScNameArray[sc].c_str());
         
         if (objId != UNKNOWN_OBJ_ID)
         {
            if (!mDrawOrbitArray[objId])
            {
               mDrawOrbitFlag[objId*MAX_DATA+mNumData] = false;
               continue;
            }
            
            mDrawOrbitFlag[objId*MAX_DATA+mNumData] = true;            
            mObjectOrbitColor[objId*MAX_DATA+mNumData] = scColors[sc];
            
            int index = objId*MAX_DATA*3+(mNumData*3);
            mObjectGciPos[index+0] = posX[sc];
            mObjectGciPos[index+1] = posY[sc];
            mObjectGciPos[index+2] = posZ[sc];
            
            mObjectGciVel[index+0] = velX[sc];
            mObjectGciVel[index+1] = velY[sc];
            mObjectGciVel[index+2] = velZ[sc];
            
            #if DEBUG_TRAJCANVAS_UPDATE
            if (mNumData < sNumDebugOutput)
            {
               MessageInterface::ShowMessage
                  ("   object=%s, objId=%d, color=%d\n", mObjectNames[sc].c_str(),
                   objId, mObjectOrbitColor[objId*MAX_DATA+mNumData]);
               MessageInterface::ShowMessage
                  ("   objId:%d gcipos = %f, %f, %f, color=%d\n", objId,
                   mObjectGciPos[index+0], mObjectGciPos[index+1],
                   mObjectGciPos[index+2]);
            }
            #endif
            
            if (mNeedInitialConversion)
            {
               Rvector6 inState, outState;
               
               // convert position and velocity
               inState.Set(posX[sc], posY[sc], posZ[sc],
                           velX[sc], velY[sc], velZ[sc]);
               
               mCoordConverter.Convert(time, inState, mInternalCoordSystem,
                                       outState, mViewCoordSystem);
               
               mObjectTmpPos[index+0] = outState[0];
               mObjectTmpPos[index+1] = outState[1];
               mObjectTmpPos[index+2] = outState[2];
               
               mObjectTmpVel[index+0] = outState[3];
               mObjectTmpVel[index+1] = outState[4];
               mObjectTmpVel[index+2] = outState[5];
               
               // copy to initial view coordinate system array
               CopyVector3(&mObjectIniPos[index], &mObjectTmpPos[index]);
               CopyVector3(&mObjectIniVel[index], &mObjectTmpVel[index]);          
            }
            else
            {
               CopyVector3(&mObjectTmpPos[index], &mObjectGciPos[index]);
               CopyVector3(&mObjectTmpVel[index], &mObjectGciVel[index]);            
            }
            
            #if DEBUG_TRAJCANVAS_UPDATE
            if (mNumData < sNumDebugOutput)
            {
               MessageInterface::ShowMessage
                  ("   objId:%d tmppos = %f, %f, %f\n", objId,
                   mObjectTmpPos[index+0], mObjectTmpPos[index+1],
                   mObjectTmpPos[index+2]);
            }
            #endif
            
            #if DEBUG_TRAJCANVAS_UPDATE > 1
            if (mNumData < sNumDebugOutput)
            {
               MessageInterface::ShowMessage
                  ("   objId:%d tmpvel = %f, %f, %f\n", objId,
                   mObjectTmpVel[index+0], mObjectTmpVel[index+1],
                   mObjectTmpVel[index+2]);
            }
            #endif
         }
      }
      
      //----------------------------------------------------
      // update object position
      //----------------------------------------------------
      for (int obj=0; obj<mObjectCount; obj++)
      {
         // if object pointer is not NULL
         if (mObjectArray[obj])
         {
            int objId = GetObjectId(mObjectNames[obj]);
            
            #if DEBUG_TRAJCANVAS_UPDATE_OBJECT
            MessageInterface::ShowMessage
               ("TrajPlotCanvas::UpdatePlot() object=%s, objId=%d\n",
                mObjectNames[obj].c_str(), objId);
            #endif
            
            // if object id found
            if (objId != UNKNOWN_OBJ_ID)
            {
               if (!mDrawOrbitArray[objId])
               {
                  mDrawOrbitFlag[objId*MAX_DATA+mNumData] = false;
                  continue;
               }
               
               mDrawOrbitFlag[objId*MAX_DATA+mNumData] = true;
               
               Rvector6 objState = mObjectArray[obj]->GetMJ2000State(time);
               int index = objId*MAX_DATA*3+(mNumData*3);
               mObjectGciPos[index+0] = objState[0];
               mObjectGciPos[index+1] = objState[1];
               mObjectGciPos[index+2] = objState[2];
               
               mObjectGciVel[index+0] = objState[3];
               mObjectGciVel[index+1] = objState[4];
               mObjectGciVel[index+2] = objState[5];
               
               #if DEBUG_TRAJCANVAS_UPDATE_OBJECT > 1
               MessageInterface::ShowMessage
                  ("TrajPlotCanvas::UpdatePlot() objState=%s\n",
                   objState.ToString().c_str());
               MessageInterface::ShowMessage
                  ("    %s gcipos = %f, %f, %f\n", mObjectNames[obj].c_str(),
                   mObjectGciPos[index+0],
                   mObjectGciPos[index+1],
                   mObjectGciPos[index+2]);
               #endif
               
               // convert objects to view CoordinateSystem
               if (mNeedInitialConversion)
               {
                  Rvector6 outState;
                  
                  mCoordConverter.Convert(time, objState, mInternalCoordSystem,
                                          outState, mViewCoordSystem);
                  
                  mObjectTmpPos[index+0] = outState[0];
                  mObjectTmpPos[index+1] = outState[1];
                  mObjectTmpPos[index+2] = outState[2];
                  
                  mObjectTmpVel[index+0] = outState[3];
                  mObjectTmpVel[index+1] = outState[4];
                  mObjectTmpVel[index+2] = outState[5];
                  
                  // copy to initial view coordinate system array
                  CopyVector3(&mObjectIniPos[index], &mObjectTmpPos[index]);
                  CopyVector3(&mObjectIniVel[index], &mObjectTmpVel[index]);          
               }
               else
               {
                  CopyVector3(&mObjectTmpPos[index], &mObjectGciPos[index]);
                  CopyVector3(&mObjectTmpVel[index], &mObjectGciVel[index]);
               }
               
               #if DEBUG_TRAJCANVAS_UPDATE_OBJECT > 1
               MessageInterface::ShowMessage
                  ("    %s tmppos = %f, %f, %f\n", mObjectNames[obj].c_str(),
                   mObjectTmpPos[index+0], mObjectTmpPos[index+1],
                   mObjectTmpPos[index+2]);
               #endif
               
            }
            else
            {
               #if DEBUG_TRAJCANVAS_UPDATE_OBJECT > 1
               MessageInterface::ShowMessage
                  ("TrajPlotCanvas::UpdatePlot() Cannot Add data. Invalid objId=%d\n",
                   objId);
               #endif
            }
         }
         else
         {
            #if DEBUG_TRAJCANVAS_UPDATE_OBJECT > 1
            MessageInterface::ShowMessage
               ("TrajPlotCanvas::UpdatePlot() Cannot add data. %s is NULL\n",
                mObjectNames[obj].c_str());
            #endif
         }
      }
      
      mNumData++;
   } // if (mNumData < MAX_DATA)
   else
   {
      mOverCounter++;
      mFinalTime = time;
      //glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      glClear(GL_DEPTH_BUFFER_BIT);
      DrawStatus("Final Frame#: ", mOverCounter+mNumData, "  Final Epoch: ",
                 mFinalTime, 0, 25,
                 "*** WARNING *** Data points exceeded the maximum");
      glFlush();
      SwapBuffers();
      
      if (mShowMaxWarning)
      {
         mShowMaxWarning = false;
         wxString msg;
         msg.Printf("*** WARNING *** The number of data points exceeded the "
                    "maximum of %d.\nPlease adjust data collect frequency to "
                    "see the whole plot.\n", MAX_DATA);
         //MessageInterface::PopupMessage(Gmat::INFO_, msg.c_str());
         MessageInterface::ShowMessage(msg.c_str());
      }
   }
   
}


//---------------------------------------------------------------------------
// void AddObjectList(wxArrayString &objNames, UnsignedIntArray &objColors,
//                    bool clearList=true)
//---------------------------------------------------------------------------
void TrajPlotCanvas::AddObjectList(const wxArrayString &objNames,
                                   const UnsignedIntArray &objColors,
                                   bool clearList)
{
   #if DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::AddObjectList() object count=%d, color count=%d\n",
       objNames.GetCount(), objColors.size());
   #endif
   
   // clear bodies
   if (clearList)
   {
      mObjectNames.Empty();
   }
   
   RgbColor rgb;
   
   mObjectCount = objNames.GetCount();
   ClearObjectArrays();
   
   if (!CreateObjectArrays())
      throw SubscriberException("There is not enough memory to allocate\n");
   
   for (int i=0; i<mObjectCount; i++)
   {
      // add object names
      mObjectNames.Add(objNames[i]);
      
      if (mObjectTextureIdMap.find(objNames[i]) == mObjectTextureIdMap.end())
      {
         #if DEBUG_TRAJCANVAS_OBJECT
         MessageInterface::ShowMessage
            ("TrajPlotCanvas::AddObjectList()  Bind new texture object=%s\n",
             objNames[i].c_str());
         #endif
         
         mObjectTextureIdMap[objNames[i]] = GmatPlot::UNINIT_TEXTURE;
      }
      
      // initialize show object
      mShowObjectMap[objNames[i]] = true;
      mShowOrbitNormalMap[objNames[i]] = false;
      
      // initialize object color
      rgb.Set(objColors[i]);
      mObjectColorMap[objNames[i]] = rgb;
      
      // set real object radius, if it is CelestialBody
      if (mObjectArray[i]->IsOfType(Gmat::CELESTIAL_BODY))
      {
         mObjectRadius[i] = ((CelestialBody*)(mObjectArray[i]))->GetEquatorialRadius();
         mObjMaxZoomIn[i] = mObjectRadius[i] * RADIUS_ZOOM_RATIO;
      }
      else
      {
         mObjectRadius[i] = mObjectDefaultRadius;
         mObjMaxZoomIn[i] = mObjectDefaultRadius * RADIUS_ZOOM_RATIO;
      }
      
      #if DEBUG_TRAJCANVAS_OBJECT > 1
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::AddObjectList() objNames[%d]=%s\n",
          i, objNames[i].c_str());
      #endif
   }
   
   // Always initialize GL before run, InitGL() is called in OnPaint()
   // if using 2.6.3 or later version
   // For 2.6.3 version initialize GL here
   #ifndef __USE_WX280_GL__
   InitGL();
   #endif
   
   ResetPlotInfo();
   ClearPlot();
   
} //AddObjectList()


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
      
      mObjectArray.push_back(NULL);
      wxArrayString tempList;
      tempList.Add("SC1");
      UnsignedIntArray objOrbitColors;
      objOrbitColors.push_back(GmatColor::RED32);
      AddObjectList(tempList, objOrbitColors);
      
      int sc = 0;
      int index = 0;
      
      for(int i=0; i<numDataPoints && i < MAX_DATA; i++)
      {
         index = sc * MAX_DATA * 3 + mNumData * 3;
         mTime[mNumData] = mTrajectoryData[i].time;
         mObjectOrbitColor[sc*MAX_DATA+mNumData] = GmatColor::RED32;
         mObjectTmpPos[index+0] = mTrajectoryData[i].x;
         mObjectTmpPos[index+1] = mTrajectoryData[i].y;
         mObjectTmpPos[index+2] = mTrajectoryData[i].z;
         mNumData++;
      }
      
      mTextTrajFile->Close();
      wxString text;
      text.Printf("Number of data points: %d", numDataPoints);
      theStatusBar->SetStatusText(text, 2);
      //wxLogStatus(GmatAppData::GetMainFrame(),
      //            wxT("Number of data points: %d"), numDataPoints);
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
   
   return numDataPoints;
    
} //end ReadTextTrajectory()


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
      #ifndef __USE_WX280_GL__
         if (!GetContext()) return;
      #endif
   #endif
   
   #ifdef __USE_WX280_GL__
   SetCurrent(*m_glContext);
   #else
   SetCurrent();
   #endif
   
   #ifdef __USE_WX280_GL__
   if (!mGlInitialized)
   {
      InitGL();
      mGlInitialized = true;
   }
   #endif
   
   if (mDrawWireFrame)
   {
      glPolygonMode(GL_FRONT, GL_LINE);
      glPolygonMode(GL_BACK, GL_LINE);
   }
   else
   {
      glPolygonMode(GL_FRONT, GL_FILL);
      glPolygonMode(GL_BACK, GL_FILL);
   }
   
   DrawPlot();
}


//------------------------------------------------------------------------------
// void OnTrajSize(wxSizeEvent& event)
//------------------------------------------------------------------------------
/**
 * Processes wxSizeEvent.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::OnTrajSize(wxSizeEvent& event)
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
      
      #ifdef __USE_WX280_GL__
      SetCurrent(*m_glContext);
      #else
      SetCurrent();
      #endif
      
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
     
   //MessageInterface::ShowMessage
   //   ("===> OnMouse() mUseInitialViewPoint=%d, mIsEndOfData=%d\n",
   //    mUseInitialViewPoint, mIsEndOfData);
   
   int flippedY;
   int width, height;
   int mouseX, mouseY;
   
   mViewAnimation = false;
   
   GetClientSize(&width, &height);
   ChangeProjection(width, height, mAxisLength);
   
   mouseX = event.GetX();
   mouseY = event.GetY();
   
   // First, flip the mouseY value so it is oriented right (bottom left is 0,0)
   flippedY = height - mouseY;
   
   GLfloat fEndX = mfLeftPos + ((GLfloat)mouseX /(GLfloat)width) *
      (mfRightPos - mfLeftPos);
   GLfloat fEndY = mfBottomPos + ((GLfloat)flippedY /(GLfloat)height)*
      (mfTopPos - mfBottomPos);
   
   if (mUseSingleRotAngle)
   {
      if (mIsEndOfRun)
         mUseSingleRotAngle = false;
      
      //loj: 9/29/05
      // This code is trying to compute Euler angle of last plot transformation
      // so that the plot can stay in the same orientation when user clicks it.
      // But it is not working yet.
      #ifdef COMPUTE_EULER_ANGLE
      if (mfCamSingleRotAngle != 0.0 || mfUpAngle != 0.0)
      {
         //compute Euler angles of viewpoint and up axis
         Rvector3 rotAngle = ComputeEulerAngles();
         
         mCurrRotXAngle = rotAngle[0];
         mCurrRotYAngle = rotAngle[1];
         mCurrRotZAngle = rotAngle[2];

         #if DEBUG_TRAJCANVAS_EULER
         MessageInterface::ShowMessage
            ("TrajPlotCanvas::OnMouse() mCurrRotXAngle=%f, %f, %f\n",
             mCurrRotXAngle, mCurrRotYAngle, mCurrRotZAngle);
         MessageInterface::ShowMessage
            ("TrajPlotCanvas::OnMouse() mfCamRotXYZAngle=%f, %f, %f\n",
             mfCamRotXAngle, mfCamRotYAngle, mfCamRotZAngle);
         #endif

         if (mUsePerspectiveMode)
         {
            // always look at from Z for rotation and zooming
            mfCamTransX = 0.0;
            mfCamTransY = 0.0;
            mfCamTransZ = -mVpLocVec.GetMagnitude();
         }
      }
      #endif
      
      
      //loj: 3/22/06
      // When USE_TRACKBALL: This code is trying to compute quaternion of last
      // plot transformation so that the plot can stay in the same orientation
      // when user clicks it after run completes.
      #ifdef USE_TRACKBALL
      if (!mUseSingleRotAngle)
      {
         // Get last modelview matrix and compute quat
         GLfloat mvMat[16];//, mvQ[4];
         glGetFloatv(GL_MODELVIEW_MATRIX, mvMat);
         FloatAttUtil::ToQuat(mQuat, mvMat);

         #if DEBUG_ROTATE
         MessageInterface::ShowMessage
            ("OnMouse() Final mvMat=\n%f, %f, %f, %f\n%f, %f, %f, %f\n"
             "%f, %f, %f, %f\n%f, %f, %f, %f\n",
             mvMat[0], mvMat[1], mvMat[2], mvMat[3], mvMat[4], mvMat[5],
             mvMat[6], mvMat[8], mvMat[8], mvMat[9], mvMat[10], mvMat[11],
             mvMat[12], mvMat[13], mvMat[14], mvMat[15]);
         MessageInterface::ShowMessage
            ("OnMouse=% f, % f, % f, % f\n", mQuat[0], mQuat[1], mQuat[2],
             mQuat[3]);
         #endif
         
      }
      #endif

   }
   
   //if mouse dragging
   if (event.Dragging())
   {
      //------------------------------
      // translating
      //------------------------------
      if (event.ShiftDown() && event.LeftIsDown())
      {
         // Do a X/Y Translate of the camera
         mfCamTransX += (fEndX - mfStartX);
         mfCamTransY += (fEndY - mfStartY);
         
         // repaint
         Refresh(false);
      }
      //------------------------------
      // rotating
      //------------------------------
      else if (event.LeftIsDown())
      {
         #ifdef USE_TRACKBALL
         
            // drag in progress, simulate trackball
            float spinQuat[4];
            
            ToQuat(spinQuat,
                      (2.0*mLastMouseX - width) / width,
                      (     height - 2.0*mLastMouseY) / height,
                      (     2.0*mouseX - width) / width,
                      (     height - 2.0*mouseY) / height);

            AddQuats(spinQuat, mQuat, mQuat);

            #if DEBUG_ROTATE
            MessageInterface::ShowMessage
               ("OnMouse() w=%d, h=%d, mLastMouseX=%d, mLastMouseY=%d, "
                "mouseX=%d, mouseY=%d\n", width, height, mLastMouseX,
                mLastMouseY, mouseX, mouseY);
            MessageInterface::ShowMessage
               ("OnMouse=% f, % f, % f, % f\n", mQuat[0], mQuat[1], mQuat[2],
                mQuat[3]);
            #endif
            
         #else
         
            #if DEBUG_ROTATE
            MessageInterface::ShowMessage
               ("   event.LeftIsDown() mCurrRotXYZAngle=%f, %f, %f\n",
                mCurrRotXAngle, mCurrRotYAngle, mCurrRotZAngle);
            #endif
            
            // if end-of-run compute new mfCamRotXYZAngle by calling ChangeView()
            if (mIsEndOfRun)
               ChangeView(mCurrRotXAngle, mCurrRotYAngle, mCurrRotZAngle);
            
            ComputeView(fEndX, fEndY);
            ChangeView(mCurrRotXAngle, mCurrRotYAngle, mCurrRotZAngle);

            //MessageInterface::ShowMessage
            //   ("===> after ChangeView() mfCamRotXYZAngle=%f, %f, %f\n",
            //    mfCamRotXAngle, mfCamRotYAngle, mfCamRotZAngle);
         
         #endif
         
         // repaint
         Refresh(false);
         
      }
      //------------------------------
      // zooming
      //------------------------------
      else if (event.RightIsDown())
      {            
         // if end-of-run compute new mfCamRotXYZAngle by calling ChangeView()
         if (mIsEndOfRun)
            ChangeView(mCurrRotXAngle, mCurrRotYAngle, mCurrRotZAngle);

         //VC++ error C2668: 'pow' : ambiguous call to overloaded function
         //'long double pow(long double,int)' 'float pow(float,int)' 'double pow(double,int);
         // Changed pow to GmatMathUtil::Pow();
         
         // find the length
         Real x2 = Pow(mouseX - mLastMouseX, 2);
         Real y2 = Pow(mouseY - mLastMouseY, 2);
         Real length = sqrt(x2 + y2);
         mZoomAmount = length * 100;

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
      }
   } // end if (event.Dragging())
   
   // save last position
   mLastMouseX = mouseX;
   mLastMouseY = mouseY;
   
   mfStartX = fEndX;
   mfStartY = fEndY;
   
   wxString mousePosStr;
   //mousePosStr.Printf("X = %g Y = %g", fEndX, fEndY);
   mousePosStr.Printf("X = %g Y = %g mouseX = %d, mouseY = %d",
                      fEndX, fEndY, mouseX, mouseY);
   //wxStatusBar *statusBar = GmatAppData::GetMainFrame()->GetStatusBar();
   theStatusBar->SetStatusText(mousePosStr, 2);
   
   //wxLogStatus(MdiGlPlot::mdiParentGlFrame,
   //            wxT("X = %d Y = %d lastX = %f lastY = %f Zoom amount = %f Distance = %f"),
   //            event.GetX(), event.GetY(), mfStartX, mfStartY, mZoomAmount, mAxisLength);
   event.Skip();
}


//------------------------------------------------------------------------------
// void OnKeyDown(wxKeyEvent &event)
//------------------------------------------------------------------------------
/**
 * Processes wxKeyEvent.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::OnKeyDown(wxKeyEvent &event)
{
   int keyDown = event.GetKeyCode();
   if (keyDown == WXK_ESCAPE)
      mHasUserInterrupted = true;
}


//------------------------------------------------------------------------------
// bool SetPixelFormatDescriptor()
//------------------------------------------------------------------------------
/**
 * Sets pixel format on Windows.
 */
//------------------------------------------------------------------------------
bool TrajPlotCanvas::SetPixelFormatDescriptor()
{
#ifdef __WXMSW__
   
   // On Windows, for OpenGL, you have to set the pixel format
   // once before doing your drawing stuff. This function
   // properly sets it up.
   
   HDC hdc = wglGetCurrentDC();
   
   PIXELFORMATDESCRIPTOR pfd =
   {
      sizeof(PIXELFORMATDESCRIPTOR),   // size of this pfd
      1,                     // version number
      PFD_DRAW_TO_WINDOW |   // support window
      PFD_SUPPORT_OPENGL |   // support OpenGL
      PFD_DOUBLEBUFFER,      // double buffered
      PFD_TYPE_RGBA,         // RGBA type
      24,                    // 24-bit color depth
      0, 0, 0, 0, 0, 0,      // color bits ignored
      0,                     // no alpha buffer
      0,                     // shift bit ignored
      0,                     // no accumulation buffer
      0, 0, 0, 0,            // accum bits ignored
      32,                    // 32-bit z-buffer
      0,                     // no stencil buffer
      0,                     // no auxiliary buffer
      PFD_MAIN_PLANE,        // main layer
      0,                     // reserved
      0, 0, 0                // layer masks ignored
   };
   
   // get the device context's best-available-match pixel format
   int pixelFormatId = ChoosePixelFormat(hdc, &pfd);
   
   #ifdef DEBUG_TRAJCANVAS_INIT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::SetPixelFormatDescriptor() pixelFormatId = %d \n",
       pixelFormatId);
   #endif
   
   if(pixelFormatId == 0)
   {
      MessageInterface::ShowMessage
         ("**** ERROR **** Failed to find a matching pixel format\n");
      return false;
   }
   
   // set the pixel format of the device context
   if (!SetPixelFormat(hdc, pixelFormatId, &pfd))
   {
      MessageInterface::ShowMessage
         ("**** ERROR **** Failed to set pixel format id %dn", pixelFormatId);
      return false;
   }
   
   return true;
   
#endif

   // Should we return true for non-Window system?
   //return false;
   return true;
}


//------------------------------------------------------------------------------
//  void SetDefaultGLFont()
//------------------------------------------------------------------------------
/**
 * Sets default GL font.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetDefaultGLFont()
{
#ifdef __WXMSW__
   // Set up font stuff for windows -
   // Make the Current font the device context's selected font
   //SelectObject(dc, Font->Handle);
   HDC hdc = wglGetCurrentDC();
   
   wglUseFontBitmaps(hdc, 0, 255, 1000);
   glListBase(1000); // base for displaying
#endif
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
   #if DEBUG_TRAJCANVAS_TEXTURE
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::LoadGLTextures() mObjectCount=%d\n", mObjectCount);
   #endif
   
   //--------------------------------------------------
   // load object texture if used
   //--------------------------------------------------
   for (int i=0; i<mObjectCount; i++)
   {
      if (mObjectArray[i]->IsOfType(Gmat::SPACECRAFT))
         continue;
      
      if (mObjectTextureIdMap[mObjectNames[i]] == GmatPlot::UNINIT_TEXTURE)
      {
         #if DEBUG_TRAJCANVAS_TEXTURE > 1
         MessageInterface::ShowMessage
            ("TrajPlotCanvas::LoadGLTextures() object=%s\n", mObjectNames[i].c_str());
         #endif
         
         mObjectTextureIdMap[mObjectNames[i]] = BindTexture(mObjectNames[i]);
      }
   }
   
   return true;
   
} //end LoadGLTextures()


//------------------------------------------------------------------------------
// GLuint BindTexture(const wxString &objName)
//------------------------------------------------------------------------------
/**
 * Loads textures and returns binding index.
 */
//------------------------------------------------------------------------------
GLuint TrajPlotCanvas::BindTexture(const wxString &objName)
{
   GLuint ret = GmatPlot::UNINIT_TEXTURE;

   //MessageInterface::ShowMessage("===> TrajPlotCanvas::BindTexture() ret = %d\n", ret);
   
   FileManager *fm = FileManager::Instance();
   std::string textureFile;  
   std::string name = std::string(objName.Upper().c_str());
   std::string filename = name + "_TEXTURE_FILE";
   
   try
   {
      textureFile = fm->GetFullPathname(filename);
      
      #ifndef SKIP_DEVIL
         ILboolean status = ilLoadImage((char*)textureFile.c_str());
         if (!status)
         {
            MessageInterface::ShowMessage
               ("*** WARNING *** TrajPlotCanvas::BindTexture() Unable to load "
                "texture file for %s\nfile name:%s\n", objName.c_str(),
                textureFile.c_str());
         }
         else
         {
            //ilutGLLoadImage((char*)textureFile.c_str());
            //ret = 1;
            ret = ilutGLBindTexImage();
         }
      #else
         
         glGenTextures(1, &ret);
         glBindTexture(GL_TEXTURE_2D, ret);

         // load image file
         if (!LoadImage((char*)textureFile.c_str()))
            ret = GmatPlot::UNINIT_TEXTURE;
         
      #endif
   }
   catch (BaseException &e)
   {
      MessageInterface::ShowMessage
         ("*** WARNING *** TrajPlotCanvas::BindTexture() Cannot bind texture "
          "image for %s.\n%s\n", objName.c_str(), e.GetFullMessage().c_str());
   }
   
   #if DEBUG_TRAJCANVAS_TEXTURE
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::BindTexture() objName=%s ret=%d\n", objName.c_str(),
       ret);
   #endif
   
   return ret;
} //end BindTexture()


//------------------------------------------------------------------------------
// void SetDefaultView()
//------------------------------------------------------------------------------
void TrajPlotCanvas::SetDefaultView()
{
   mCurrRotXAngle = mDefaultRotXAngle;
   mCurrRotYAngle = mDefaultRotYAngle;
   mCurrRotZAngle = mDefaultRotZAngle;
   mCurrViewDist = mDefaultViewDist;
   mAxisLength = mCurrViewDist;
   mfCamTransX = 0;
   mfCamTransY = 0;
   mfCamTransZ = 0;
   mfCamRotXAngle = 0;
   mfCamRotYAngle = 0;
   mfCamRotZAngle = 0;
   
   #ifdef USE_TRACKBALL
   ToQuat(mQuat, 0.0f, 0.0f, 0.0f, 0.0);
   #endif
   
   //mOriginId = GetObjectId("Earth");
   
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
   glMatrixMode(GL_PROJECTION); // first go to projection mode
   glLoadIdentity();
   SetupWorld();                // set it up
   glMatrixMode(GL_MODELVIEW);
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

   #if DEBUG_TRAJCANVAS_PROJ > 2
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::SetupWorld() mUsePerspectiveMode=%d, mUseSingleRotAngle=%d\n",
       mUsePerspectiveMode, mUseSingleRotAngle);
   #endif
   
   #if DEBUG_TRAJCANVAS_PROJ > 2
   if (mUseSingleRotAngle)
   {
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::SetupWorld() mfLeftPos=%f, mfRightPos=%f\n   mfBottomPos=%f, "
          "mfTopPos=%f\n   mfViewNear=%f, mfViewFar=%f\n", mfLeftPos, mfRightPos,
          mfBottomPos, mfTopPos, mfViewNear, mfViewFar);
   }
   #endif
   
   if (mUsePerspectiveMode)
   {
      
      // Setup how we view the world
      GLfloat aspect = (GLfloat)mCanvasSize.x / (GLfloat)mCanvasSize.y;

      //loj: 12/7/05 commented out, need only x and y size.
      //Real size = GmatMathUtil::Sqrt(mfRightPos * mfRightPos +
      //                               mfTopPos   * mfTopPos +
      //                               mfViewFar  * mfViewFar);
      
      Real size = GmatMathUtil::Sqrt(mfRightPos * mfRightPos +
                                     mfTopPos   * mfTopPos);
      
      Real dist = mVpLocVec.GetMagnitude();
      
      //loj: 12/7/05 mViewObjRadius = mObjectDefaultRadius*50;
      mViewObjRadius = mObjectDefaultRadius;
      
      if (mUseFixedFov && mUseSingleRotAngle)
      {
         mFovDeg = mFixedFovAngle;
      }
      else
      {
         if (!mUseViewDirectionVector && mViewDirectionObj != NULL)
         {
            // if showing view object (loj: 12/7/05 added)
            if (mShowObjectMap[mViewObjName])
            {
               //loj: 12/7/05 use mViewObjId
               //int objId = GetObjectId(mViewDirectionObj->GetName().c_str());
               //mViewObjRadius = mObjectRadius[objId];
               mViewObjRadius = mObjectRadius[mViewObjId];
            }
         }
         
         // compute fov angle
         //loj: 12/7/05 add mViewObjRadius to dist
         //mFovDeg = 2.0 * ATan(size/2.0, dist - mViewObjRadius) * DEG_PER_RAD;
         mFovDeg = 2.0 * ATan(size/2.0, dist + mViewObjRadius) * DEG_PER_RAD;
      }

      Real ratio = dist / mAxisLength;
      
      #if DEBUG_TRAJCANVAS_PERSPECTIVE
      if (mUseSingleRotAngle)
      {
         MessageInterface::ShowMessage
            ("   mAxisLength=%f, size=%f, dist=%f, mViewObjRadius=%f\n   "
             "mFovDeg=%f, ratio=%f, \n", mAxisLength, size, dist, mViewObjRadius,
             mFovDeg, ratio);
      }
      #endif

      //loj: 12/7/05 Added ratio to prevent near side clipping
      gluPerspective(mFovDeg, aspect, mAxisLength/(mFovDeg*15), mAxisLength * mFovDeg * ratio);

      //glFrustum(mfLeftPos, mfRightPos, mfViewBottom, mfTopPos, mAxisLength, mfViewFar*mAxisLength);
   }
   else
   {
      // Setup how we view the world
      glOrtho(mfLeftPos, mfRightPos, mfBottomPos, mfTopPos, mfViewNear, mfViewFar);
   }
   
   //-----------------------------------------------------------------
   // Note: mouse rotation is applied in TransformView as MODELVIEW mode
   //-----------------------------------------------------------------
   
   //camera moves opposite direction to center on object
   //this is the point of rotation
   
   int index = mViewObjId * MAX_DATA * 3 + (mNumData-1) * 3;
   glTranslatef(mObjectTmpPos[index+0], mObjectTmpPos[index+1],
                -mObjectTmpPos[index+2]);
   
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
   float fYAmnt = 360*(fEndX - mfStartX)/(mfRightPos - mfLeftPos);
   float fXAmnt = 360*(fEndY - mfStartY)/(mfBottomPos - mfTopPos);
   
   
   // always rotate the y axis
   mCurrRotYAngle = mfCamRotYAngle + fYAmnt;

   // Are we rotating the x or the z in this case?
   if (mRotateXy)
   {
      // x axis
      mCurrRotXAngle = mfCamRotXAngle + fXAmnt - 270;
      
      // z axis (loj: 9/28/05 Added)
      mCurrRotZAngle = mfCamRotZAngle + fXAmnt;
   }
   else
   {
      // z axis
      mCurrRotZAngle = mfCamRotZAngle + fXAmnt;
   }
}


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

   mfCamRotXAngle = (int)(viewX) % 360 + 270;
   mfCamRotYAngle = (int)(viewY) % 360;
   mfCamRotZAngle = (int)(viewZ) % 360;
   
   //MessageInterface::ShowMessage
   //   ("===> TrajPlotCanvas::ChangeView() mfCamRotXYZAngle = %f %f %f\n",
   //    mfCamRotXAngle, mfCamRotYAngle, mfCamRotZAngle);
   
   // don't let the rotation angles build up to some insane size
   if (mfCamRotYAngle > 360)
      mfCamRotYAngle -= 360;
   else if (mfCamRotYAngle < 0)
      mfCamRotYAngle += 360;

   // don't let the rotation angles build up to some insane size
   if (mfCamRotXAngle > 450)
      mfCamRotXAngle -= 360;
   else if (mfCamRotXAngle < 90)
      mfCamRotXAngle += 360;
   
   // don't let the rotation angles build up to some insane size
   if (mfCamRotZAngle > 360)
      mfCamRotZAngle -= 360;
   else if (mfCamRotZAngle < 0)
      mfCamRotZAngle += 360;
  
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
   
   mfViewLeft   = -axisLength/2;
   mfViewRight  =  axisLength/2;
   
   mfViewTop    =  axisLength/2;
   mfViewBottom = -axisLength/2;

   //texture maps upside down
   //mfViewTop    = -axisLength/2;
   //mfViewBottom =  axisLength/2;

   if (mUseGluLookAt)
   {
      //loj: 1/10/06 changed *2 to * 10000 to fix near/far clipping
      //mfViewNear = -axisLength * 10000.0;
      //mfViewFar  =  axisLength * 10000.0;
      mfViewNear = -axisLength * 100000.0;
      mfViewFar  =  axisLength * 100000.0;
   }
   else
   {
      mfViewNear = -axisLength/2;
      mfViewFar  =  axisLength/2;
   }
   
   // save the size we are setting the projection for later use
   if (width <= height)
   {
      mfLeftPos = mfViewLeft;
      mfRightPos = mfViewRight;
      mfBottomPos = mfViewBottom*fAspect;
      mfTopPos = mfViewTop*fAspect;
   }
   else
   {
      mfLeftPos = mfViewLeft / fAspect;
      mfRightPos = mfViewRight / fAspect;
      mfBottomPos = mfViewBottom;
      mfTopPos = mfViewTop;
   }
}


//------------------------------------------------------------------------------
//  void ComputeViewVectors(int frame)
//------------------------------------------------------------------------------
/**
 * Computes viewing vectors using viewing options.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::ComputeViewVectors(int frame)
{
   #if DEBUG_TRAJCANVAS_PROJ
   MessageInterface::ShowMessage("ComputeViewVectors() frame=%d, time=%f\n",
                                 frame, mTime[frame]);
   #endif

   mIsFirstRun = false;
   int index = 0;
   
   //-----------------------------------------------------------------
   // get viewpoint reference vector
   //-----------------------------------------------------------------
   mVpRefVec.Set(0.0, 0.0, 0.0);
   
   if (!mUseViewPointRefVector && mViewPointRefObj != NULL)
   {
      #if DEBUG_TRAJCANVAS_PROJ
      MessageInterface::ShowMessage
         ("ComputeViewVectors() mViewPointRefObj=%d, name=%s\n",
          mViewPointRefObj, mViewPointRefObj->GetName().c_str());
      #endif

      // if valid body id
      if (mVpRefObjId != UNKNOWN_OBJ_ID)
      {
         index = mVpRefObjId * MAX_DATA * 3 + frame * 3;
         // for efficiency, body data are computed in UpdatePlot() once.
         mVpRefVec.Set(mObjectTmpPos[index+0],
                       mObjectTmpPos[index+1],
                       mObjectTmpPos[index+2]);
      }
      else
      {
         MessageInterface::ShowMessage
            ("*** WARNING *** TrajPlotCanvas::ComputeViewVectors() Invalid "
             "mVpRefObjId=%d\n", mVpRefObjId);
      }
   }
   
   //-----------------------------------------------------------------
   // get viewpoint vector
   //-----------------------------------------------------------------
   mVpVec = mViewPointVector;
   
   if (!mUseViewPointVector && mViewPointVectorObj != NULL)
   {
      #if DEBUG_TRAJCANVAS_PROJ
      MessageInterface::ShowMessage
         ("ComputeViewVectors() mViewPointVectorObj=%d, name=%s, mVpVecObjId=%d\n",
          mViewPointVectorObj, mViewPointVectorObj->GetName().c_str(),
          mVpVecObjId);
      #endif
      
      // if valid body id
      if (mVpVecObjId != UNKNOWN_OBJ_ID)
      {
         if (mUseGluLookAt)
         {
            index = mVpVecObjId * MAX_DATA * 3 + frame * 3;
            
            // if look at from object, negate it so that we can see the object
            mVpVec.Set(-mObjectTmpPos[index+0],
                       -mObjectTmpPos[index+1],
                       -mObjectTmpPos[index+2]);

            // set z component to zero so that plane doesn't move up and down
            mVpVec[2] = 0.0;
            
         }
         else
         {
            index = mVpVecObjId * MAX_DATA * 3 + frame * 3;
            
            mVpVec.Set(mObjectTmpPos[index+0],
                       mObjectTmpPos[index+1],
                       mObjectTmpPos[index+2]);
         }
      }
      else
      {
         MessageInterface::ShowMessage
            ("*** WARNING *** TrajPlotCanvas::ComputeViewVectors() Invalid "
             "mVpVecObjId=%d\n", mVpVecObjId);
      }
   }
   
   //-----------------------------------------------------------------
   // get viewpoint location
   //-----------------------------------------------------------------
   mVpLocVec = mVpRefVec + (mViewScaleFactor * mVpVec);
   
   //-----------------------------------------------------------------
   // get view direction and view center vector
   //-----------------------------------------------------------------
   mVdVec = mViewDirectionVector;
   
   if (!mUseViewDirectionVector && mViewDirectionObj != NULL)
   {
      #if DEBUG_TRAJCANVAS_PROJ
      MessageInterface::ShowMessage
         ("ComputeViewVectors() mViewDirectionObj=%d, name=%s\n",
          mViewDirectionObj, mViewDirectionObj->GetName().c_str());
      #endif
      
      // if viewpoint ref object is same as view direction object
      // just look opposite side
      if (mViewDirectionObj->GetName() == mViewPointRefObjName)
      {
         mVdVec = -mVpLocVec;
      }
      else if (mVdirObjId != UNKNOWN_OBJ_ID)
      {
         index = mVdirObjId * MAX_DATA * 3 + frame * 3;
         
         // for efficiency, body data are computed in UpdatePlot() once.
         mVdVec.Set(mObjectTmpPos[index+0],
                    mObjectTmpPos[index+1],
                    mObjectTmpPos[index+2]);
         
         // check for 0.0 direction 
         if (mVdVec.GetMagnitude() == 0.0)
            mVdVec = mViewDirectionVector;
      }
      else
      {
         MessageInterface::ShowMessage
            ("*** WARNING *** TrajPlotCanvas::ComputeViewVectors() Invalid "
             "mVdirObjId=%d\n", mVdirObjId);
      }
   }

   // set view center vector for gluLookAt()
   mVcVec = mVdVec;
   
   #if DEBUG_TRAJCANVAS_PROJ
   MessageInterface::ShowMessage
      ("ComputeViewVectors() mVpRefVec=%s, mVpVec=%s\nmVpLocVec=%s, "
       " mVdVec=%s, mVcVec=%s\n", mVpRefVec.ToString().c_str(),
       mVpVec.ToString().c_str(), mVpLocVec.ToString().c_str(),
       mVdVec.ToString().c_str(), mVcVec.ToString().c_str());
   #endif
   
   
   //-----------------------------------------------------------------
   // set view center object
   //-----------------------------------------------------------------
      
   // compute axis length (this tells how far zoom out is)

   // Initially use mVpLocVec and later use changed value by mouse zoom-in/out.
   // This will use scale factor correctly for data points are less than
   // update frequency.
   if (mNumData <= mUpdateFrequency)
      mAxisLength = mVpLocVec.GetMagnitude();
   
   // if mAxisLength is too small, set to max zoom in value
   if (mAxisLength < mMaxZoomIn)
      mAxisLength = mMaxZoomIn;
   
   // compute camera rotation angle
   Real vdMag = mVdVec.GetMagnitude();
   
   mfCamSingleRotAngle = ACos(-(mVdVec[2]/vdMag)) * DEG_PER_RAD;
   
   // compute axis of rotation
   mfCamRotXAxis =  mVdVec[1];
   mfCamRotYAxis = -mVdVec[0];
   mfCamRotZAxis = 0.0;
   mUseSingleRotAngle = true;

   ComputeUpAngleAxis(frame);
   
   #if DEBUG_TRAJCANVAS_PROJ
   MessageInterface::ShowMessage
      ("==> ComputeViewVectors() mfCamTransXYZ=%f, %f, %f, mfCamSingleRotAngle=%f\n"
       "   mfCamRotXYZ=%f, %f, %f mAxisLength=%f\n", mfCamTransX, mfCamTransY,
       mfCamTransZ, mfCamSingleRotAngle, mfCamRotXAxis, mfCamRotYAxis,
       mfCamRotZAxis, mAxisLength);
   #endif
} // end ComputeViewVectors(int frame)


//------------------------------------------------------------------------------
// void ComputeUpAngleAxis(int frame)
//------------------------------------------------------------------------------
void TrajPlotCanvas::ComputeUpAngleAxis(int frame)
{
   // calculate view up direction
   
   Rvector6 upOutState;
   
   upOutState = mUpState;
   
   if (mViewUpCoordSystem->GetName() != mViewCoordSystem->GetName())
   {
      mCoordConverter.Convert(mTime[frame], mUpState, mViewUpCoordSystem,
                              upOutState, mViewCoordSystem);
   }
   
   mUpVec.Set(upOutState(0), upOutState(1), upOutState(2));

   // If view up and view direction is the same axis, change view direction,
   // so it can show up direction correctly even for gluLookAt.
   
   Rvector3 vdUnit = mVdVec.GetUnitVector();
   Real upDotView = mUpVec * vdUnit;
   //MessageInterface::ShowMessage("===> upDotView = %f\n", upDotView);
   
   if (Abs(upDotView) == 1.0)
   {
      //MessageInterface::ShowMessage("===> mUpVec and mVdVec are on the same axis.\n");
      
      if (Abs(mUpVec[0]) > 0.0)
         mVcVec = Cross(mUpVec, Rvector3(0.0, -1.0, 0.0));
      else if (Abs(mUpVec[1]) > 0.0)
         mVcVec = Cross(mUpVec, Rvector3(0.0, 0.0, -1.0));
      else
         mVcVec = Cross(mUpVec, Rvector3(-1.0, 0.0, 0.0));

      mVdVec = Cross(mVdVec, mVcVec);

      // if using gluLookAt, we don't want view point and view up direction line up
      if (mUseGluLookAt)
         mVpLocVec = -mVdVec;
   }
   
   // DJC added for "Up"   
   mfUpAngle = atan2(mVdVec[1],mVdVec[0]) * DEG_PER_RAD + 90.0;
   mfUpXAxis = mVdVec[0];
   mfUpYAxis = mVdVec[1];
   mfUpZAxis = mVdVec[2];
   
   #if DEBUG_TRAJCANVAS_PROJ
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::ComputeUpAngleAxis() mVpLocVec=%s, mVdVec=%s\n   "
       "mVcVec=%s, mUpVec=%s\n", mVpLocVec.ToString().c_str(),
       mVdVec.ToString().c_str(), mVcVec.ToString().c_str(),
       mUpVec.ToString().c_str());
   MessageInterface::ShowMessage
      ("   atan2(mVdVec[y],mVdVec[x])=%f, mfUpAngle=%f, mfUpXYZAxis=%f, %f, %f\n",
       atan2(mVdVec[1],mVdVec[0]), mfUpAngle, mfUpXAxis, mfUpYAxis, mfUpZAxis);
   #endif
} // end ComputeUpAngleAxis()


//------------------------------------------------------------------------------
// void TransformView(int frame)
//------------------------------------------------------------------------------
void TrajPlotCanvas::TransformView(int frame)
{
   if (frame < 0)
      return;

   #if DEBUG_TRAJPLOTCANVAS_DRAW
   MessageInterface::ShowMessage
      ("==> TrajPlotCanvas::TransformView() mUseSingleRotAngle=%d, "
       "mUseGluLookAt=%d, mIsEndOfData=%d, mIsEndOfRun=%d\n", mUseSingleRotAngle,
       mUseGluLookAt, mIsEndOfData, mIsEndOfRun);
   #endif
   
   glLoadIdentity();
   
   if (mUseSingleRotAngle)
   {
      //MessageInterface::ShowMessage("==> TransformView() call gluLookAt()\n");
      
      //glLoadIdentity();
      
      if (mUseGluLookAt)
      {
         //-----------------------------------------------------------
         // To fix Earth Z-axis flipping when looking at from SC
         //-----------------------------------------------------------         
         if (mViewUpAxisName == "X")
         {
            if (mVpLocVec[1] < 0)
               mUpVec.Set(1.0, 0.0, 0.0);
         }
         else if (mViewUpAxisName == "-X")
         {
            if (mVpLocVec[1] < 0)
               mUpVec.Set(-1.0, 0.0, 0.0);
         }
         else if (mViewUpAxisName == "Y")
         {
            if (mVpLocVec[0] < 0)
               mUpVec.Set(0.0, 1.0, 0.0);
         }
         else if (mViewUpAxisName == "-Y")
         {
            if (mVpLocVec[0] < 0)
               mUpVec.Set(0.0, -1.0, 0.0);
         }
         
         //MessageInterface::ShowMessage
         //   ("===> mVpLocVec=%s, mVcVec=%s, mUpVec=%s\n",
         //    mVpLocVec.ToString().c_str(), mVcVec.ToString().c_str(),
         //    mUpVec.ToString().c_str());
         
         //-------------------------------------------------
         // use gluLookAt()
         //-------------------------------------------------
         gluLookAt(mVpLocVec[0], mVpLocVec[1], mVpLocVec[2],
                   mVcVec[0], mVcVec[1], mVcVec[2],
                   mUpVec[0], mUpVec[1], mUpVec[2]);

      }
      else
      {
         glTranslatef(mfCamTransX, mfCamTransY, mfCamTransZ);
         glRotatef(mfCamSingleRotAngle,
                   mfCamRotXAxis, mfCamRotYAxis, mfCamRotZAxis);
         
         // DJC added for Up
         glRotatef(-mfUpAngle, mfUpXAxis, mfUpYAxis, -mfUpZAxis);
                           
      } //if (mUseGluLookAt)

   } //if (mUseSingleRotAngle)
   
   //-------------------------------------------------
   // add current user mouse rotation
   //-------------------------------------------------
   #ifdef USE_TRACKBALL      
      GLfloat m[4][4];
      BuildRotMatrix(m, mQuat);
      glMultMatrixf(&m[0][0]);      
   #else      
      ApplyEulerAngles();      
   #endif
         
} // end TransformView()


//------------------------------------------------------------------------------
//  void DrawFrame()
//------------------------------------------------------------------------------
/**
 * Draws whole picture.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawFrame()
{
   #if DEBUG_TRAJCANVAS_ANIMATION
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::DrawFrame() mNumData=%d, mUsenitialViewPoint=%d\n"
       "   mViewCoordSysName=%s, mInitialCoordSysName=%s\n", mNumData,
       mUseInitialViewPoint, mViewCoordSysName.c_str(), mInitialCoordSysName.c_str());
   #endif
   
   if (mUseInitialViewPoint)
   {
      
      #ifdef USE_TRACKBALL
         ToQuat(mQuat, 0.0f, 0.0f, 0.0f, 0.0);
      #endif
         
      SetDefaultView();
      
      if (!mViewCoordSysName.IsSameAs(mInitialCoordSysName))
      {         
         if (mInitialCoordSysName.IsSameAs(mInternalCoordSysName))
         {
            mViewCoordSystem = mInternalCoordSystem;
            mViewCoordSysName = mInternalCoordSysName;
            mIsInternalCoordSystem = true;
         }
         else
         {
            mViewCoordSystem = mInitialCoordSystem;
            mViewCoordSysName = mInitialCoordSysName;
            mIsInternalCoordSystem = false;
         }
      }
      
      UpdateRotateFlags();
      ConvertObjectData();
      
      // set view center object
      mOriginName = wxString(mViewCoordSystem->GetOriginName().c_str());
      mOriginId = GetObjectId(mOriginName);
      
      mViewObjName = mOriginName;
      GotoObject(mViewObjName);
   }
   
   int numberOfData = mNumData;
   mIsEndOfData = false;
   mIsEndOfRun = false;
   
   // refresh every 50 points (Allow user to set frame this increment?)
   for (int frame=1; frame<numberOfData; frame+=mFrameInc)
   {
      // wxYield() yields control to pending messages in the windowing system.
      
      // wxSafeYield() is similar to wxYield() except it disables the user
      // input to all program windows before calling wxYield and re-enables
      // it again afterwards.
      
      //wxSafeYield();
      wxYield(); //loj: 8/16/05 to allow mouse event
      
      if (mHasUserInterrupted)
         break;
      
      Sleep(mUpdateInterval);
      
      mNumData = frame;
      
      // Set projection here, because DrawPlot() is called in OnPaint()
      if (mUseInitialViewPoint)
         ComputeViewVectors(mNumData-1);
      
      ChangeProjection(mCanvasSize.x, mCanvasSize.y, mAxisLength);
      
      Refresh(false);
   }
   
   // final refresh, in case number of points is less than 50
   Refresh(false);
   
   mNumData = numberOfData;
   mIsEndOfData = true;
   mIsEndOfRun = true;
   
} // end DrawFrame()


//------------------------------------------------------------------------------
//  void DrawPlot()
//------------------------------------------------------------------------------
/**
 * Draws whole plot.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawPlot()
{
   #if DEBUG_TRAJCANVAS_DRAW
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::DrawPlot() mNumData=%d, mNeedOriginConversion=%d, "
       "mIsInternalCoordSystem=%d\n   mUseInitialViewPoint=%d, mAxisLength=%f\n",
       mNumData, mNeedOriginConversion, mIsInternalCoordSystem, mUseInitialViewPoint,
       mAxisLength);
   #endif
   
   if (mRedrawLastPointsOnly || mNumPointsToRedraw == 0)
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   else
      glClear(GL_DEPTH_BUFFER_BIT);
   
   DrawStatus("Frame#: ", mNumData-1, "  Epoch: ", mTime[mNumData-1], 0, 5);
   if (mOverCounter > 0)
      DrawStatus("Final Frame#: ", mOverCounter+mNumData, "  Final Epoch: ",
                 mFinalTime, 0, 25);
   
   #if DEBUG_TRAJCANVAS_DRAW
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::DrawPlot()mUseInitialViewPoint=%d, mIsEndOfData=%d, mIsEndOfRun=%d\n",
       mUseInitialViewPoint, mIsEndOfData, mIsEndOfRun);
   #endif
   
   // Plot is not refreshed when another panel is opened, so add glFlush()
   // and SwapBuffers() (loj: 4/5/06)
   if (mNumData < 1) // to avoid 0.0 time
   {
      glFlush();
      SwapBuffers();
      return;
   }
   
   // compute projection if using initial viewpoint and not end of run or
   // if not using initial viewpoint and not first run.
   // We need initial values for  gulLookAt()
   if ((mUseInitialViewPoint && !mIsEndOfRun) ||
       (!mUseInitialViewPoint && mIsFirstRun && mUseGluLookAt))
      ComputeViewVectors(mNumData-1);
   
   ChangeProjection(mCanvasSize.x, mCanvasSize.y, mAxisLength);
   
   // change back to view projection
   SetProjection();

   TransformView(mNumData-1);

   // tilt Origin rotation axis if needed
   if (mNeedOriginConversion)
   {
      glPushMatrix();
      TiltOriginZAxis();
   }
   
   // draw equatorial plane
   if (mDrawXyPlane)
      DrawEquatorialPlane(mXyPlaneColor);
   
   // draw axes
   if (mDrawAxes)
   {
      if (!mCanRotateAxes)
      {
         DrawAxes();
      }
   }
   
   // draw ecliptic plane
   if (mDrawEcPlane)
      DrawEclipticPlane(mEcPlaneColor);
   
   if (mNeedOriginConversion)
      glPopMatrix();
   
   // draw object orbit
   DrawObjectOrbit(mNumData-1);
   
   // draw Earth-Sun line
   if (mDrawSunLine)
      DrawSunLine(mNumData-1);
   
   glFlush();
   SwapBuffers();
   
} // end DrawPlot()


//------------------------------------------------------------------------------
// void DrawSphere(GLdouble radius, GLint slices, GLint stacks, GLenum style,
//                 GLenum orientation = GLU_OUTSIDE, GLenum normals = GL_NONE,
//                 GLenum textureCoords = GL_FALSE)
//------------------------------------------------------------------------------
/**
 * Draws sphere.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawSphere(GLdouble radius, GLint slices, GLint stacks,
                                GLenum style, GLenum orientation, GLenum normals,
                                GLenum textureCoords)
{
   GLUquadricObj* qobj = gluNewQuadric();
   gluQuadricDrawStyle(qobj, style);
   gluQuadricNormals(qobj, normals);
   gluQuadricTexture(qobj, textureCoords);
   gluSphere(qobj, radius, slices, stacks);
   gluQuadricOrientation(qobj, orientation);
   gluDeleteQuadric(qobj);
}


//------------------------------------------------------------------------------
//  void DrawObject(const wxString &objName, int frame)
//------------------------------------------------------------------------------
/**
 * Draws object sphere and maps texture image.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawObject(const wxString &objName, int frame)
{
   int objId = GetObjectId(objName);
   
   #if DEBUG_TRAJCANVAS_DRAW > 1
   MessageInterface::ShowMessage
         ("TrajPlotCanvas::DrawObject() drawing:%s, objId:%d, frame=%d\n",
          objName.c_str(), objId, frame);
   #endif
   
   //loj: 11/4/05 Added for light
   #ifdef ENABLE_LIGHT_SOURCE
   //-------------------------------------------------------
   // enable light source on option
   //-------------------------------------------------------
   if (mEnableLightSource && mSunPresent)
   {
      //Why whole Sun is not lit?
      //float lightPos[4] = {0.0f, 0.0f, 0.0f, 1.0f};
      float lightPos[4];
      int sunId = GetObjectId("Sun");
      int index = sunId * MAX_DATA * 3 + frame * 3;
      
      lightPos[0] = -mObjectTmpPos[index+0];
      lightPos[1] = -mObjectTmpPos[index+1];
      lightPos[2] = -mObjectTmpPos[index+2];
      // opposite direction with  sun earth line
      //lightPos[0] = mObjectTmpPos[index+0];
      //lightPos[1] = mObjectTmpPos[index+1];
      //lightPos[2] = mObjectTmpPos[index+2];
      lightPos[3] = 1.0;
      // If 4th value is zero, the light source is directional one, and
      // (x,y,z) values describes its direction.
      // If 4th value is nonzero, the light is positional, and the (x,y,z) values
      // specify the location of the light in homogeneous object coordinates.
      // By default, a positional light radiates in all directions.
      
      // reset the light position to reflect the transformations
      glLightfv(GL_LIGHT0, GL_POSITION, lightPos);
      
      // enable the lighting
      glEnable(GL_LIGHTING);
   }
   #endif
   
   #if DEBUG_TRAJCANVAS_DRAW > 1
   MessageInterface::ShowMessage
      ("   mObjectTextureIdMap[%s]=%d\n", objName.c_str(),
       mObjectTextureIdMap[objName]);
   #endif
   
   //-------------------------------------------------------
   // rotate Earth, need to rotate before texture mapping
   //-------------------------------------------------------
   if (objName == "Earth" && mCanRotateBody)
   {
      Real earthRotAngle = 0.0;
      Real initialLong = mInitialLongitude;
      Real offset = 40.0; // need this to line up earth texture with longitude
            
      //========== #ifdef
      #ifdef USE_MHA_TO_ROTATE_EARTH
      
      if (mSolarSystem)
      {         
         Real mha = 0.0;
         
         if (initialLong < 180.0)
            initialLong = -initialLong - offset;

         CelestialBody *earth = mSolarSystem->GetBody("Earth");
         if (earth)
            mha = earth->GetHourAngle(mTime[frame]);
         
         earthRotAngle = mha + initialLong + offset;
      
         #if DEBUG_TRAJCANVAS_DRAW > 1
         if (!mIsEndOfRun)
            MessageInterface::ShowMessage
               ("   mha=%f, earthRotAngle=%f\n", mha, earthRotAngle);
         #endif
         
      }
      
      //========== #else
      #else
      
      if (initialLong < 180.0)
         initialLong = -initialLong - offset;
      
      earthRotAngle = (Abs(mTime[frame] - mTime[0])) * 361.0 + mInitialMha +
         initialLong + offset;
      
      #endif
      //========== #endif

      earthRotAngle =
         AngleUtil::PutAngleInDegRange(earthRotAngle, 0.0, 360.0);
      
      #if DEBUG_TRAJCANVAS_DRAW > 1
      if (!mIsEndOfRun)
         MessageInterface::ShowMessage("   earthRotAngle=%f\n", earthRotAngle);
      #endif
      
      glRotatef(earthRotAngle, 0.0, 0.0, 1.0);
   }
   
   //-------------------------------------------------------
   // draw axes if it rotates with the body
   //-------------------------------------------------------
   if (mDrawAxes && objId == mOriginId && mCanRotateAxes)
   {
      DrawAxes();
   }
   
   //-------------------------------------------------------
   // draw object with texture on option
   //-------------------------------------------------------
   if (mObjectTextureIdMap[objName] != GmatPlot::UNINIT_TEXTURE)
   {
      //glColor4f(1.0, 1.0, 1.0, 1.0);
      glColor3f(1.0, 1.0, 1.0);
      
      glBindTexture(GL_TEXTURE_2D, mObjectTextureIdMap[objName]);
      glEnable(GL_TEXTURE_2D);
      
      if (objName == "Sun")
         DrawSphere(mObjectRadius[objId], 50, 50, GLU_FILL, GLU_INSIDE);
      else
         DrawSphere(mObjectRadius[objId], 50, 50, GLU_FILL);
      
      glDisable(GL_TEXTURE_2D);
      
      //----------------------------------------------------
      // draw grid on option
      //----------------------------------------------------
      if (mDrawGrid && objName == "Earth")
      {
         // This makes lines thicker
         //glEnable(GL_LINE_SMOOTH);
         //glLineWidth(1.5);
         
         // Just draw a wireframe sphere little bigger to show grid
         //glColor3f(0.20, 0.20, 0.50); // dark blue
         glColor3f(0.0, 0.0, 0.0);      // black
         //glColor3f(0.50, 0.10, 0.20); // maroon
         GLdouble radius = mObjectRadius[objId] + mObjectRadius[objId] * 0.03;
         DrawSphere(radius, 36, 18, GLU_LINE, GLU_OUTSIDE, GL_NONE, GL_FALSE);
      }
   }
   else
   {
      #if DEBUG_TRAJCANVAS_DRAW
      MessageInterface::ShowMessage
         ("*** WARNING *** TrajPlotCanvas::DrawObject() %s texture not found.\n",
          objName.c_str());
      #endif
      
      // Just draw a wireframe sphere if we get here
      glColor3f(0.20, 0.20, 0.50);
      DrawSphere(mObjectRadius[objId], 50, 50, GLU_LINE);      
      glDisable(GL_TEXTURE_2D);
   }
   
   #ifdef ENABLE_LIGHT_SOURCE
   if (mEnableLightSource && mSunPresent)
   {
      glDisable(GL_LIGHTING);
   }
   #endif
   
} // end DrawObject(const wxString &objName)


//------------------------------------------------------------------------------
//  void DrawObjectOrbit(int frame)
//------------------------------------------------------------------------------
/**
 * Draws object orbit and object at the frame number. The frame is the index
 * of the data buffer which starts at 0.
 *
 * @param  frame  Frame number to be used for drawing
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawObjectOrbit(int frame)
{
   int objId;
   wxString objName;
   
   #ifdef ENABLE_LIGHT_SOURCE
   if (mEnableLightSource && mSunPresent)
   {
      // we don't want the orbit paths lit
      glDisable(GL_LIGHTING);
   }
   #endif
   
   for (int obj=0; obj<mObjectCount; obj++)
   {
      
      objName = mObjectNames[obj];
      objId = GetObjectId(objName);
      mObjLastFrame[objId] = 0;
      
      #if DEBUG_TRAJCANVAS_DRAW
      MessageInterface::ShowMessage
         ("DrawObjectOrbit() obj=%d, objId=%d, objName=%s\n", obj, objId,
          objName.c_str());
      #endif
      
      // If not showing orbit or object, continue to next one
      if (!mDrawOrbitFlag[objId*MAX_DATA+frame] &&
          !mShowOrbitNormalMap[objName])
         continue;
      
      glPushMatrix();
      glBegin(GL_LINES);
      
      int beginFrame = 1;
      
      //---------------------------------------------------------
      // compute begin frame
      //---------------------------------------------------------
      if (mRedrawLastPointsOnly && !mIsEndOfRun)
      {
         beginFrame = frame - mNumPointsToRedraw;
         if (beginFrame < 1)
            beginFrame = 1;
      }
      else if (!mIsEndOfRun && mNumPointsToRedraw == -1)
      {
         beginFrame = frame - 2;
         if (beginFrame < 1)
            beginFrame = 1;
      }
      
      int index1 = 0;
      int index2 = 0;
      int index3 = 0;
      //---------------------------------------------------------
      // draw lines
      //---------------------------------------------------------
      for (int i = beginFrame; i <= frame; i++)
      {
         // Draw object orbit line based on points
         if ((mTime[i] > mTime[i-1]) ||
             (i>2 && mTime[i] < mTime[i-1]) && mTime[i-1] < mTime[i-2]) //for backprop
         {
            index1 = objId * MAX_DATA * 3 + (i-1) * 3;
            index2 = objId * MAX_DATA * 3 + i * 3;
            
            Rvector3 r1(mObjectTmpPos[index1+0], mObjectTmpPos[index1+1],
                        mObjectTmpPos[index1+2]);
            
            Rvector3 r2(mObjectTmpPos[index2+0], mObjectTmpPos[index2+1],
                        mObjectTmpPos[index2+2]);
            
            // if object position magnitude is 0, skip
            if (r1.GetMagnitude() == 0.0 || r2.GetMagnitude() == 0.0)
               continue;
            
            // if object position diff is over limit, skip (ScriptEx_TargetHohmann)
            #ifdef SKIP_OVER_LIMIT_DATA
            static Real sMaxDiffDist = 100000.0;
            // if difference is more than sMaxDiffDist skip
            if ((Abs(r2[0]- r1[0]) > sMaxDiffDist && (SignOf(r2[0]) != SignOf(r1[0]))) ||
                (Abs(r2[1]- r1[1]) > sMaxDiffDist && (SignOf(r2[1]) != SignOf(r1[1]))) ||
                (Abs(r2[2]- r1[2]) > sMaxDiffDist && (SignOf(r2[2]) != SignOf(r1[2]))))
            {
               #if DEBUG_SHOW_SKIP
               MessageInterface::ShowMessage
                  ("   plot=%s, i1=%d, i2=%d, time1=%f, time2=%f\n   r1=%s, r2=%s\n",
                   GetName().c_str(), i-1, i, mTime[i-1], mTime[i], r1.ToString().c_str(),
                   r2.ToString().c_str());
               #endif
               continue;
            }
            #endif
            
            // If drawing orbit lines
            if (mDrawOrbitFlag[objId*MAX_DATA+frame])
            {
               if (mObjectArray[obj]->IsOfType(Gmat::SPACECRAFT))
                  *sIntColor = mObjectOrbitColor[objId*MAX_DATA+i];
               else
                  *sIntColor = mObjectColorMap[objName].GetIntColor();
            
               glColor3ub(sGlColor->red, sGlColor->green, sGlColor->blue);
            
               glVertex3f((-mObjectTmpPos[index1+0]),
                          (-mObjectTmpPos[index1+1]),
                          ( mObjectTmpPos[index1+2]));
            
               glVertex3f((-mObjectTmpPos[index2+0]),
                          (-mObjectTmpPos[index2+1]),
                          ( mObjectTmpPos[index2+2]));
            }

            // save last valid frame to show object at final frame
            mObjLastFrame[objId] = i;
            
         }
      }
      
      glEnd();
      glPopMatrix();
      
      //---------------------------------------------------------
      // draw object orbit normal vector
      // (loj: 6/13/05 Fow now it only draws spacecraft orbit normal vector.)
      //---------------------------------------------------------
      if (mShowOrbitNormalMap[objName])
      {
         int numSkip = frame/12;
         UnsignedInt color;
         
         for (int i=1; i<=frame; i++)
         {
            if (numSkip <= 0 || i % numSkip != 0)
               continue;
            
            if ((mTime[i] > mTime[i-1]) ||
                (i>2 && mTime[i] < mTime[i-1]) && mTime[i-1] < mTime[i-2])
            {
               index1 = objId * MAX_DATA * 3 + (i-1) * 3;
               index2 = objId * MAX_DATA * 3 + i * 3;
               
               Rvector3 r1(mObjectTmpPos[index1+0],
                           mObjectTmpPos[index1+1],
                           mObjectTmpPos[index1+2]);
               
               Rvector3 r2(mObjectTmpPos[index2+0],
                           mObjectTmpPos[index2+1],
                           mObjectTmpPos[index2+2]);
               
               // if object position magnitude is 0, skip
               if (r1.GetMagnitude() == 0.0 || r2.GetMagnitude() == 0.0)
                  continue;
               
               glPushMatrix();
               
               // move to origin
               index3 = mOriginId * MAX_DATA * 3 + i * 3;
               
               glTranslatef(-mObjectTmpPos[index3+0],
                            -mObjectTmpPos[index3+1],
                             mObjectTmpPos[index3+2]);
                              
               if (mObjectArray[obj]->IsOfType(Gmat::SPACECRAFT))
                  color = mObjectOrbitColor[objId*MAX_DATA+i];
               else
                  color = mObjectColorMap[objName].GetIntColor();
               
               DrawObjectOrbitNormal(objId, i, color);
               glPopMatrix();
            }
         }
      }
      
      //---------------------------------------------------------
      //draw object with texture
      //---------------------------------------------------------
      
      if (frame > 0)
      {
         if (mShowObjectMap[objName])
         {
            index1 = objId * MAX_DATA * 3 + mObjLastFrame[objId] * 3;
            
            #if DEBUG_TRAJCANVAS_DRAW
            MessageInterface::ShowMessage
               ("   objName=%s, objId=%d, lastFrame=%d\n", objName.c_str(), objId,
                mObjLastFrame[objId]);
            MessageInterface::ShowMessage
               ("   mObjectTmpPos=%f, %f, %f\n",
                mObjectTmpPos[index1+0], mObjectTmpPos[index1+1],
                mObjectTmpPos[index1+2]);
            #endif
            
            glPushMatrix();
            
            // put object at final position
            glTranslatef(-mObjectTmpPos[index1+0],
                         -mObjectTmpPos[index1+1],
                          mObjectTmpPos[index1+2]);
            
            // first disable GL_TEXTURE_2D to show lines clearly
            // without this, lines are drawn dim (loj: 2007.06.11)
            glDisable(GL_TEXTURE_2D);
            
            if (mObjectArray[obj]->IsOfType(Gmat::SPACECRAFT))
               DrawSpacecraft(mObjectOrbitColor[objId*MAX_DATA+mObjLastFrame[objId]]);
            else
               DrawObject(objName, frame);
            
            glPopMatrix();
            
         }
      }
   }
   
   //MessageInterface::ShowMessage("===> DrawObjectOrbit() exiting\n");
   
} // end DrawObjectOrbit(int frame)


//------------------------------------------------------------------------------
//  void DrawObjectOrbitNormal(int obj, int objId, int frame, UnsignedInt color)
//------------------------------------------------------------------------------
/**
 * Draws object orbit normal vector.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawObjectOrbitNormal(int objId, int frame, UnsignedInt color)
{
   Real distance = (Real)mAxisLength/2.2;
   float endPos[3];
   
   int index = objId * MAX_DATA * 3 + frame * 3;
   Rvector3 r(mObjectTmpPos[index+0], mObjectTmpPos[index+1], mObjectTmpPos[index+2]);            
   Rvector3 v(mObjectTmpVel[index+0], mObjectTmpVel[index+1], mObjectTmpVel[index+2]);
   
   Rvector3 normV = Cross(r, v);
   normV.Normalize();
   
   //--------------------------------
   // draw normal vector line
   //--------------------------------
   
   // set color
   *sIntColor = color;
   glColor3ub(sGlColor->red, sGlColor->green, sGlColor->blue);
   
   glBegin(GL_LINES);
   
   // get orbit normal unit vector and multiply by distance
   // Add minus sign to x, y
   endPos[0] = -normV[0] * distance;
   endPos[1] = -normV[1] * distance;
   endPos[2] =  normV[2] * distance;
   
   glVertex3f(0.0, 0.0, 0.0);
   glVertex3f(endPos[0], endPos[1], endPos[2]);
   
   glEnd();
   
   // Show Orbit Normal direction text
   DrawStringAt(" +N", endPos[0], endPos[1], endPos[2]);
}


//------------------------------------------------------------------------------
//  void DrawSpacecraft(UnsignedInt scColor)
//------------------------------------------------------------------------------
/**
 * Draws spacecraft.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawSpacecraft(UnsignedInt scColor)
{
   // draw six faces of a long cube
   glBegin(GL_QUADS);
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

} // end DrawSpacecraft()


//------------------------------------------------------------------------------
//  void DrawEquatorialPlane(UnsignedInt color)
//------------------------------------------------------------------------------
/**
 * Draws equatorial plane circles.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawEquatorialPlane(UnsignedInt color)
{
   int i;
   float endPos[3];
   float distance;
   Real angle;
   
   static const Real RAD_PER_DEG =
      3.14159265358979323846264338327950288419716939937511 / 180.0;
   
   distance = (Real)mAxisLength;

   glPushMatrix();
   glBegin(GL_LINES);
   
   // set color
   *sIntColor = color;
   glColor3ub(sGlColor->red, sGlColor->green, sGlColor->blue);

   //-----------------------------------
   // draw lines
   //-----------------------------------
   for (i=0; i<360; i+=15)
   {
      angle = RAD_PER_DEG * ((Real)i);
      
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
   
   GLUquadricObj *qobj = gluNewQuadric();

   //==============================================================
   // Argosy code
   //==============================================================
   Real orthoDepth = distance;
   
   //orthoDepth = (half-size-of-image)*60/(half-FOV-degrees)
   if (mUsePerspectiveMode)
      orthoDepth = (mAxisLength*60) / (mFovDeg/2.0);

   Real ort = orthoDepth * 8;
   Real pwr = Floor(Log10(ort));
   Real size = Exp10(pwr)/100;
   Real imax = orthoDepth/size;
   
   //------------------------------------------
   // Draw MAJOR circles
   //------------------------------------------
   //SetCelestialColor (GlOptions.EquatorialPlaneColor);
   for (int i=1; i<=(int)imax; ++i)
      if (i%10==0)
         DrawCircle(qobj, i*size);
   
   //------------------------------------------
   // Draw MINOR circles
   //------------------------------------------
   //imax = minimum (imax, 100);
   imax = GmatMathUtil::Min(imax, 100);
   //ZColor color = GlOptions.EquatorialPlaneColor;
   Real factor = (size*100)/(ort);
   //color.Red *=factor;
   //color.Green *=factor;
   //color.Blue *=factor;
   //SetCelestialColor (color)

   GLubyte ubfactor = (GLubyte)(factor * 255);
   //MessageInterface::ShowMessage("===> ubfactor=%d, factor=%f\n", ubfactor, factor);

   // Why does alpha value have no effects?
   glColor4ub(sGlColor->red, sGlColor->green, sGlColor->blue, ubfactor);
   
   for (int i=1; i<=(int)imax; ++i)
      //if (i%10!=0 && (GlOptions.DrawDarkLines || factor > 0.5))
      //if (i%10!=0 && (factor > 0.5)) // why i%10!=0? not every 10th?
      if (i%10 == 0 || (factor > 0.5))
         DrawCircle(qobj, i*size);
   
   gluDeleteQuadric(qobj);
   
   glPopMatrix();
   
} // end DrawEquatorialPlane()


//------------------------------------------------------------------------------
//  void DrawEclipticPlane(UnsignedInt color)
//------------------------------------------------------------------------------
/**
 * Draws ecliptic plane circles.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawEclipticPlane(UnsignedInt color)
{
   // First rotate the grand coordinate system to obliquity of the ecliptic
   // (23.5) and draw equatorial plane
   
   glPushMatrix();
   glRotatef(23.5, -1, 0, 0);
   DrawEquatorialPlane(color);
   glPopMatrix();
} // end DrawEclipticPlane()


//------------------------------------------------------------------------------
//  void DrawSunLine(int frame)
//------------------------------------------------------------------------------
/**
 * Draws Origin to Sun lines.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::DrawSunLine(int frame)
{
   
   if (frame <= 0)
      return;
   
   int sunId = GetObjectId("Sun");
   
   // check for Sun
   if (sunId == UNKNOWN_OBJ_ID)
      return;
   
   Real originPos[3], sunPos[3];
   Real distance = (Real)mAxisLength;
   Real mag;
   
   //--------------------------------
   // draw sun line
   //--------------------------------
   
   // set color
   *sIntColor = mSunLineColor;
   glColor3ub(sGlColor->red, sGlColor->green, sGlColor->blue);
   
   glBegin(GL_LINES);
   int index = 0;
   
   // draw one line from origin to Sun
   index = mOriginId * MAX_DATA * 3 + frame * 3;
   originPos[0] = -mObjectTmpPos[index+0];
   originPos[1] = -mObjectTmpPos[index+1];
   originPos[2] =  mObjectTmpPos[index+2];
   
   index = sunId * MAX_DATA * 3 + frame * 3;
   sunPos[0] = -mObjectTmpPos[index+0];
   sunPos[1] = -mObjectTmpPos[index+1];
   sunPos[2] =  mObjectTmpPos[index+2];
   
   // show lines between Sun and Earth and to -Sun
   glVertex3f(originPos[0], originPos[1], originPos[2]);
   glVertex3f(sunPos[0], sunPos[1], sunPos[2]);
   glVertex3f(originPos[0], originPos[1], originPos[2]);
   glVertex3f(-sunPos[0], -sunPos[1], -sunPos[2]);
   
   glEnd();
   
   // Show Sun direction text
   glColor3f(1.0, 1.0, 0.0); // yellow
   
   // get sun unit vector and multiply by distance
   mag = sqrt(sunPos[0]*sunPos[0] + sunPos[1]*sunPos[1] + sunPos[2]*sunPos[2]);
   DrawStringAt(" +S", sunPos[0]/mag*distance/2.2,
                sunPos[1]/mag*distance/2.2,
                sunPos[2]/mag*distance/2.2);
   
} // end DrawSunLine()


//---------------------------------------------------------------------------
// void DrawAxes()
//---------------------------------------------------------------------------
void TrajPlotCanvas::DrawAxes()
{
   wxString axisLabel;
   GLfloat viewDist;

   glLineWidth(2.0); //loj: 11/2/05 Fix for L57
   
   //-----------------------------------
   // draw axes
   //-----------------------------------
   
   //viewDist = mCurrViewDist/2; //zooms in and out
   viewDist = mAxisLength/2.2; // stays the same
   glBegin(GL_LINES);
   
   glColor3f(0, 1, 0);   // x
   glVertex3f(-viewDist, 0, 0);
   glVertex3f( viewDist, 0, 0);
   
   glColor3f(0, 0, 1);   // y
   glVertex3f(0, -viewDist, 0);
   glVertex3f(0,  viewDist, 0);
   
   glColor3f(1, 1, 0);   // z
   glVertex3f(0, 0, -viewDist);
   glVertex3f(0, 0, viewDist);
   
   glEnd();
   
   //-----------------------------------
   // throw some text out...
   //-----------------------------------
   glColor3f(0, 1, 0);   // green
   axisLabel = "+X " + mViewCoordSysName;
   DrawStringAt(axisLabel, -viewDist, 0.0, 0.0);
   
   glColor3f(0, 0, 1);   // blue
   axisLabel = "+Y " + mViewCoordSysName;
   DrawStringAt(axisLabel, 0.0, -viewDist, 0.0);
   
   glColor3f(1, 1, 0);   // yellow
   axisLabel = "+Z " + mViewCoordSysName;
   DrawStringAt(axisLabel, 0.0, 0.0, viewDist);
   
   glLineWidth(1.0);
   
}


//---------------------------------------------------------------------------
// void DrawStatus(const wxString &label1, int frame, const wxString &label2,
//                 double time, int xpos = 0, int ypos = 0,
//                 const wxString &label3 = "")
//---------------------------------------------------------------------------
/*
 * Writes status
 */
//---------------------------------------------------------------------------
void TrajPlotCanvas::DrawStatus(const wxString &label1, int frame,
                                const wxString &label2, double time,
                                int xpos, int ypos, const wxString &label3)
{
   //----------------------------------------------------
   // draw current frame number and time
   //----------------------------------------------------
   //loj: 5/23/05 I want to use glWindowPos2f but it is available in version 1.4
   // then I'll not need to set GL_PROJECTION mode
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluOrtho2D(0.0, (GLfloat)mCanvasSize.x, 0.0, (GLfloat)mCanvasSize.y);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   wxString str;
   wxString text;
   str.Printf("%d", frame);
   text = label1 + str;
   str.Printf("%f", time);
   text = text + label2 + str;
   
   glColor3f(1, 1, 0); //yellow
   glRasterPos2i(xpos, ypos);
   glCallLists(strlen(text.c_str()), GL_BYTE, (GLubyte*)text.c_str());

   if (label3 != "")
   {
      glRasterPos2i(xpos, 50);
      glCallLists(strlen(label3.c_str()), GL_BYTE, (GLubyte*)label3.c_str());
   }
   
   //wxLogStatus(GmatAppData::GetMainFrame(), wxT("Frame#: %d, Time: %f"), frame,
   //            mTime[frame]);
   
}


//---------------------------------------------------------------------------
// void ApplyEulerAngles()
//---------------------------------------------------------------------------
void TrajPlotCanvas::ApplyEulerAngles()
{
   //MessageInterface::ShowMessage("==> TrajPlotCanvas::ApplyEulerAngles()\n");
   
   if (mRotateAboutXaxis)
   {
      glRotatef(mfCamRotYAngle, 0.0, 1.0, 0.0);
      glRotatef(mfCamRotZAngle, 0.0, 0.0, 1.0);
      glRotatef(mfCamRotXAngle, 1.0, 0.0, 0.0);
   }
   else if (mRotateAboutYaxis)
   {
      glRotatef(mfCamRotZAngle, 0.0, 0.0, 1.0);
      glRotatef(mfCamRotXAngle, 1.0, 0.0, 0.0);
      glRotatef(mfCamRotYAngle, 0.0, 1.0, 0.0);
   }
   else
   {
      glRotatef(mfCamRotXAngle, 1.0, 0.0, 0.0);
      glRotatef(mfCamRotYAngle, 0.0, 1.0, 0.0);
      glRotatef(mfCamRotZAngle, 0.0, 0.0, 1.0);
   }
}


//---------------------------------------------------------------------------
// void DrawStringAt(const wxString &str, GLfloat x, GLfloat y, GLfloat z)
//---------------------------------------------------------------------------
void TrajPlotCanvas::DrawStringAt(const wxString &str, GLfloat x, GLfloat y,
                                  GLfloat z)
{
   glRasterPos3f(x, y, z);
   glCallLists(strlen(str.c_str()), GL_BYTE, (GLubyte*)str.c_str());
}


//---------------------------------------------------------------------------
// void DrawCircle(GLUquadricObj *qobj, Real radius)
//---------------------------------------------------------------------------
void TrajPlotCanvas::DrawCircle(GLUquadricObj *qobj, Real radius)
{
   gluQuadricDrawStyle(qobj, GLU_LINE  );
   gluQuadricNormals  (qobj, GLU_SMOOTH);
   gluQuadricTexture  (qobj, GL_FALSE  );
   gluDisk(qobj, radius, radius, 50, 1);
}


//---------------------------------------------------------------------------
// int GetObjectId(const wxString &name)
//---------------------------------------------------------------------------
int TrajPlotCanvas::GetObjectId(const wxString &name)
{
   for (int i=0; i<mObjectCount; i++)
      if (mObjectNames[i] == name)
         return i;
   
   #if DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::GetObjectId() obj name: " + name +
       " not found in the object list\n");
   #endif
   
   return UNKNOWN_OBJ_ID;
}


//------------------------------------------------------------------------------
// void TrajPlotCanvas::ClearObjectArrays()
//------------------------------------------------------------------------------
void TrajPlotCanvas::ClearObjectArrays()
{
   #if DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage("TrajPlotCanvas::ClearObjectArrays() entered\n");
   #endif
   
   if (mObjectRadius)
      delete [] mObjectRadius;
   
   if (mObjMaxZoomIn)
      delete [] mObjMaxZoomIn;
   
   if (mObjLastFrame)
      delete [] mObjLastFrame;
   
   if (mDrawOrbitFlag)
      delete [] mDrawOrbitFlag;
   
   if (mObjectOrbitColor)
      delete [] mObjectOrbitColor;

   if (mObjectIniPos)
      delete [] mObjectIniPos;
   
   if (mObjectIniVel)
      delete [] mObjectIniVel;
   
   if (mObjectGciPos)
      delete [] mObjectGciPos;
   
   if (mObjectGciVel)
      delete [] mObjectGciVel;
   
   if (mObjectTmpPos)
      delete [] mObjectTmpPos;
   
   if (mObjectTmpVel)
      delete [] mObjectTmpVel;
   
   mObjectRadius = NULL;
   mObjMaxZoomIn = NULL;
   mObjLastFrame = NULL;
   mDrawOrbitFlag = NULL;
   mObjectOrbitColor = NULL;
   mObjectIniPos = NULL;
   mObjectIniVel = NULL;
   mObjectGciPos = NULL;
   mObjectGciVel = NULL;
   mObjectTmpPos = NULL;
   mObjectTmpVel= NULL;
   
   #if DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage("TrajPlotCanvas::ClearObjectArrays() exiting\n");
   #endif
}


//------------------------------------------------------------------------------
// bool TrajPlotCanvas::CreateObjectArrays()
//------------------------------------------------------------------------------
/*
 * Allocates arrays for objects.
 */
//------------------------------------------------------------------------------
bool TrajPlotCanvas::CreateObjectArrays()
{
   #if DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage
      ("CreateObjectArrays() allocating object arrays with %d\n", mObjectCount);
   #endif
   
   if ((mObjectRadius = new Real[mObjectCount]) == NULL)
      return false;
   
   for (int i=0; i<mObjectCount; i++)
      mObjectRadius[i] = 0.0;
   
   if ((mObjMaxZoomIn = new Real[mObjectCount]) == NULL)
      return false;
   
   for (int i=0; i<mObjectCount; i++)
      mObjMaxZoomIn[i] = 0.0;
   
   if ((mObjLastFrame = new int[mObjectCount]) == NULL)
      return false;
   
   if ((mDrawOrbitFlag = new bool[mObjectCount*MAX_DATA]) == NULL)
      return false;
   
   if ((mObjectOrbitColor = new UnsignedInt[mObjectCount*MAX_DATA]) == NULL)
      return false;
   
   if ((mObjectGciPos = new Real[mObjectCount*MAX_DATA*3]) == NULL)
      return false;
   
   if ((mObjectGciVel = new Real[mObjectCount*MAX_DATA*3]) == NULL)
      return false;
   
   if ((mObjectIniPos = new Real[mObjectCount*MAX_DATA*3]) == NULL)
      return false;
   
   if ((mObjectIniVel = new Real[mObjectCount*MAX_DATA*3]) == NULL)
      return false;
   
   if ((mObjectTmpPos = new Real[mObjectCount*MAX_DATA*3]) == NULL)
      return false;
   
   if ((mObjectTmpVel = new Real[mObjectCount*MAX_DATA*3]) == NULL)
      return false;
   
   #if DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage("TrajPlotCanvas::CreateObjectArrays() exiting\n");
   #endif
   
   return true;
}


//---------------------------------------------------------------------------
// bool TiltOriginZAxis()
//---------------------------------------------------------------------------
bool TrajPlotCanvas::TiltOriginZAxis()
{
   if (mNumData == 0)
      return false;
   
   if (mInternalCoordSystem == NULL || mViewCoordSystem == NULL)
      return false;

   std::string axisTypeName =
      mViewCoordSystem->GetRefObject(Gmat::AXIS_SYSTEM, "")->GetTypeName();
   
   #if DEBUG_TRAJCANVAS_DRAW
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::TiltOriginZAxis() AxisTypeName=%s\n", axisTypeName.c_str());
   #endif
   
   // rotate earth Z axis if view CS is EarthMJ2000Ec
   if (mViewCoordSystem->GetName() == "EarthMJ2000Ec")
   {
      Rvector6 inState, outState;
      
      inState.Set(0.0, 0.0, 1.0, 0.0, 0.0, 0.0);
      
      mCoordConverter.Convert(mTime[0], inState, mInternalCoordSystem,
                              outState, mViewCoordSystem);
      
      #if DEBUG_TRAJCANVAS_DRAW > 2
         MessageInterface::ShowMessage
            ("TrajPlotCanvas::TiltOriginZAxis() in=%g, %g, %g, out=%g, %g, %g\n",
             inState[0], inState[1], inState[2], outState[0], outState[1], outState[2]);
         Rvector3 vecA(inState[0], inState[1], inState[2]);
         Rvector3 vecB(outState[0], outState[1], outState[2]);
         Real angDeg = AngleUtil::ComputeAngleInDeg(vecA, vecB);
         MessageInterface::ShowMessage
            ("TrajPlotCanvas::TiltOriginZAxis() angDeg=%g\n", angDeg);
         //outState = 0, 0.397777, 0.917482
         //angDeg = 23.4393
      #endif
         
      // convert outState to rotation angle
      // How???
      
      // rotate Earth Z axis
      glRotatef(23.5, 1.0, 0.0, 0.0);
   }
   
   return true;
}


//---------------------------------------------------------------------------
// bool ConvertObjectData()
//---------------------------------------------------------------------------
bool TrajPlotCanvas::ConvertObjectData()
{
   if (mInternalCoordSystem == NULL || mViewCoordSystem == NULL)
      return false;
   
   Rvector6 inState, outState;
   
   #if DEBUG_TRAJCANVAS_CONVERT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::ConvertObjectData() internalCS=%s, viewCSName=%s, viewCS=%d\n",
       mInternalCoordSystem->GetName().c_str(), mViewCoordSystem->GetName().c_str(),
       mViewCoordSystem);
   #endif
   
   // do not convert if view CS is internal CS
   if (mIsInternalCoordSystem)
   {
      #if DEBUG_TRAJCANVAS_CONVERT
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::ConvertObjectData() No conversion is needed. "
          "Just copy MJ2000 pos\n");
      #endif
      for (int i=0; i<mObjectCount; i++)
      {
         int objId = GetObjectId(mObjectNames[i]);
         int index = 0;
         for (int i=0; i<mNumData; i++)
         {
            index = objId * MAX_DATA * 3 + i * 3;
            CopyVector3(&mObjectTmpPos[index], &mObjectGciPos[index]);
         }
      }
   }
   else if (mViewCoordSysName == mInitialCoordSysName)
   {
      #if DEBUG_TRAJCANVAS_CONVERT
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::ConvertObjectData() No conversion is needed. "
          "Just copy Initial View CS pos\n");
      #endif
      for (int i=0; i<mObjectCount; i++)
      {
         int objId = GetObjectId(mObjectNames[i]);
         int index = 0;
         
         for (int i=0; i<mNumData; i++)
         {
            index = objId * MAX_DATA * 3 + i * 3;
            CopyVector3(&mObjectTmpPos[index], &mObjectIniPos[index]);
         }
      }
   }
   else
   {
      for (int i=0; i<mObjectCount; i++)
      {
         int objId = GetObjectId(mObjectNames[i]);
         
         #if DEBUG_TRAJCANVAS_CONVERT
         MessageInterface::ShowMessage
            ("TrajPlotCanvas::ConvertObjectData() mObjectNames[%d]=%s\n", objId,
             mObjectNames[i].c_str());
         #endif
         
         for (int i=0; i<mNumData; i++)
            ConvertObject(objId, i);              
      }
   }
   
   return true;
} //ConvertObjectData()


//---------------------------------------------------------------------------
// void ConvertObject(int objId, int index)
//---------------------------------------------------------------------------
void TrajPlotCanvas::ConvertObject(int objId, int index)
{
   Rvector6 inState, outState;
   int start = objId*MAX_DATA*3+index*3;
   inState.Set(mObjectGciPos[start+0], mObjectGciPos[start+1],
               mObjectGciPos[start+2], mObjectGciVel[start+0],
               mObjectGciVel[start+1], mObjectGciVel[start+2]);
   
   mCoordConverter.Convert(mTime[index], inState, mInternalCoordSystem,
                           outState, mViewCoordSystem);
   
   mObjectTmpPos[index+0] = outState[0];
   mObjectTmpPos[index+1] = outState[1];
   mObjectTmpPos[index+2] = outState[2];
   
   mObjectTmpVel[index+0] = outState[3];
   mObjectTmpVel[index+1] = outState[4];
   mObjectTmpVel[index+2] = outState[5];
   
   #if DEBUG_TRAJCANVAS_CONVERT
   if (index < 10)
   {
      MessageInterface::ShowMessage
         ("   index=%d, inState=%16.9f, %16.9f, %16.9f\n"
          "           outState=%16.9f, %16.9f, %16.9f\n", index, inState[0],
          inState[1], inState[2], outState[0], outState[1], outState[2]);
   }
   #endif
}


//---------------------------------------------------------------------------
// void UpdateRotateFlags()
//---------------------------------------------------------------------------
void TrajPlotCanvas::UpdateRotateFlags()
{
   AxisSystem *axis =
      (AxisSystem*)mViewCoordSystem->GetRefObject(Gmat::AXIS_SYSTEM, "");

   if (axis->IsOfType("BodyFixedAxes") &&
       (mOriginName.IsSameAs(axis->GetStringParameter("Origin").c_str())))
   {
      mCanRotateBody = false;
      mCanRotateAxes = false;
   }
   else if (axis->IsOfType("InertialAxes"))
   {
      mCanRotateBody = true;
      mCanRotateAxes = false;
   }
   else
   {
      mCanRotateBody = false;
      mCanRotateAxes = false;
   }
   
   
   #if DEBUG_TRAJCANVAS_OBJECT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::UpdateRotateFlags() mCanRotateBody=%d, "
       "mCanRotateAxes=%d\n", mCanRotateBody, mCanRotateAxes);
   #endif
}


//------------------------------------------------------------------------------
// void MakeValidCoordSysList()
//------------------------------------------------------------------------------
/*
 * Make a list of coordinate system that can be converted from and to.
 */
//------------------------------------------------------------------------------
void TrajPlotCanvas::MakeValidCoordSysList()
{
   StringArray csList =
      theGuiInterpreter->GetListOfObjects(Gmat::COORDINATE_SYSTEM);
   CoordinateSystem *cs;
   SpacePoint *sp;
   wxString csName;
   wxString origin;
   wxString primary;
   wxString secondary;
   
   mValidCSNames.Empty();
   
   for (unsigned int i=0; i<csList.size(); i++)
   {
      origin = "None";
      primary = "None";
      secondary = "None";
      
      //cs = theGuiInterpreter->GetCoordinateSystem(csList[i]);
      cs = (CoordinateSystem*)theGuiInterpreter->GetConfiguredObject(csList[i]);
      csName = cs->GetName().c_str();
      
      //MessageInterface::ShowMessage("==> csName=%s\n", csName.c_str());
      
      sp = cs->GetOrigin();
      // cannot convert to CS with spacecraft as origin
      if (sp->IsOfType("Spacecraft"))
         continue;
      else
         origin = cs->GetOriginName().c_str();
      
      sp = cs->GetPrimaryObject();
      if (sp != NULL)
      {
         // cannot convert to CS with spacecraft as primary
         if (sp->IsOfType("Spacecraft"))
            continue;
         
         primary = sp->GetName().c_str();
      }
      
      sp = cs->GetSecondaryObject();
      if (sp != NULL)
      {
         // cannot convert to CS with spacecraft as secondary
         if (sp->IsOfType("Spacecraft"))
            continue;
         
         secondary = sp->GetName().c_str();
      }

      //MessageInterface::ShowMessage
      //   ("==> origin=%s, primary=%s, secondary=%s\n",
      //    origin.c_str(), primary.c_str(), secondary.c_str());

      if (origin == "None")
         continue;
      
      if (GetObjectId(origin) != UNKNOWN_OBJ_ID)
      {
         if (primary == "None" && secondary == "None")
         {
            mValidCSNames.Add(csName);
            continue;
         }

         if (primary != "None" && GetObjectId(primary) != UNKNOWN_OBJ_ID)
         {
            if (secondary == "None")
            {
               mValidCSNames.Add(csName);
               continue;
            }
            else if (GetObjectId(secondary) != UNKNOWN_OBJ_ID)
            {
               mValidCSNames.Add(csName);
               continue;               
            }
         }
      }
   }

   //for (unsigned int i=0; i<mValidCSNames.GetCount(); i++)
   //{
   //   MessageInterface::ShowMessage("==> validCSName=%s\n", mValidCSNames[i].c_str());
   //}
}


//---------------------------------------------------------------------------
// Rvector3 ComputeEulerAngles()
//---------------------------------------------------------------------------
Rvector3 TrajPlotCanvas::ComputeEulerAngles()
{
   Rvector3 modAngle;

   #ifndef COMPUTE_EULER_ANGLE
   return modAngle;

   
   #else
   
   bool error = false;
   Rvector3 eulerAngle;
   Rmatrix33 finalMat;
   
   #ifdef USE_MODELVIEW_MAT
   //if (mUseGluLookAt)
   {   
      // model view matrix
      static GLfloat sViewMat[16];
      
      glGetFloatv(GL_MODELVIEW_MATRIX, sViewMat);
      
      // OpenGL stores matrix in column major: ith column, jth row.
      
      Rmatrix33 mvmat(sViewMat[0], sViewMat[1], sViewMat[2],
                      sViewMat[4], sViewMat[5], sViewMat[6],
                      sViewMat[8], sViewMat[9], sViewMat[10]);
      
      finalMat = mvmat;
      
      #ifdef DEBUG_TRAJCANVAS_EULER
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::ComputeEulerAngles() sViewMat=\n"
          "   %f, %f, %f, %f\n   %f, %f, %f, %f\n"
          "   %f, %f, %f, %f\n   %f, %f, %f, %f\n",
          sViewMat[0], sViewMat[1], sViewMat[2], sViewMat[3],
          sViewMat[4], sViewMat[5], sViewMat[6], sViewMat[7],
          sViewMat[8], sViewMat[9], sViewMat[10], sViewMat[11],
          sViewMat[12], sViewMat[13], sViewMat[14], sViewMat[15]);
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::ComputeEulerAngles() mvmat=%s\n",
          mvmat.ToString().c_str());
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::ComputeEulerAngles() finalMat=%s\n",
          finalMat.ToString().c_str());
      #endif
   }
   #else
   //else
   {
   
      Rvector3 upAxis((Real)mfUpXAxis, (Real)mfUpYAxis, (Real)mfUpZAxis);
      Rvector3 rotAxis = Rvector3((Real)mfCamRotXAxis, (Real)mfCamRotYAxis,
                                  (Real)mfCamRotZAxis);

      #ifdef DEBUG_TRAJCANVAS_EULER
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::ComputeEulerAngles() mfUpAngle=%f, upAxis=%s\n",
          mfUpAngle, upAxis.ToString().c_str());
   
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::ComputeEulerAngles() mfCamSingleRotAngle=%f, rotAxis=%s\n",
          mfCamSingleRotAngle, rotAxis.ToString().c_str());
      #endif
      
      Rmatrix33 upMat, rotMat;
      
      try
      {
         // compute cosine matrix
         if (upAxis.GetMagnitude() > 0.0)
            upMat = GmatAttUtil::ToCosineMatrix(mfUpAngle * RAD_PER_DEG, upAxis);
         
         if (rotAxis.GetMagnitude() > 0.0)
            rotMat = GmatAttUtil::ToCosineMatrix(mfCamSingleRotAngle * RAD_PER_DEG, rotAxis);
         
         //finalMat = rotMat * upMat;
         finalMat = upMat * rotMat;
         
         #ifdef DEBUG_TRAJCANVAS_EULER
         MessageInterface::ShowMessage
            ("TrajPlotCanvas::ComputeEulerAngles() \n  rotMat=%s  upMat=%s  "
             "finalMat=%s\n", rotMat.ToString().c_str(),
             upMat.ToString().c_str(), finalMat.ToString().c_str());
         #endif
         
      }
      catch (BaseException &e)
      {
         error = true;
         MessageInterface::ShowMessage
            ("*** ERROR *** TrajPlotCanvas::ComputeEulerAngles() %s\n",
             e.GetFullMessage().c_str());
      }
   }
   #endif
   
   if (error)
      return modAngle;

   
   try
   {
      // convert finalMat to Euler angle
      if (mRotateAboutXaxis)
         eulerAngle = GmatAttUtil::ToEulerAngles(finalMat, 2, 3, 1);
      else if (mRotateAboutYaxis)
         eulerAngle = GmatAttUtil::ToEulerAngles(finalMat, 3, 1, 2);
      else
         eulerAngle = GmatAttUtil::ToEulerAngles(finalMat, 1, 2, 3);
            
      eulerAngle = eulerAngle * DEG_PER_RAD;
      
      #ifdef DEBUG_TRAJCANVAS_EULER
      MessageInterface::ShowMessage
         ("TrajPlotCanvas::ComputeEulerAngles() eulerAngle=%s\n",
          eulerAngle.ToString().c_str());
      #endif
      
   }
   catch (BaseException &e)
   {
      MessageInterface::ShowMessage
         ("*** ERROR *** TrajPlotCanvas::ComputeEulerAngles() %s\n",
          e.GetFullMessage().c_str());
   }
   
   modAngle = eulerAngle;
   
   //loj: 9/29/05
   // How can I compute modified rotation angle in general way?
   
   if (eulerAngle.GetMagnitude() == 0.0)
   {
      if (mRotateAboutXaxis) // 2-3-1 (Default)
      {
         modAngle[0] = -90 - 270;
         modAngle[1] = 90;
         modAngle[2] = 0;
      }
   }
   else
   {
      if (mRotateAboutXaxis) // 2-3-1 (Default)
      {
         if (eulerAngle(2) == 0.0)
         {
            if (mUseGluLookAt)
            {
               modAngle[0] = eulerAngle(0) + 90;
               modAngle[1] = eulerAngle(2);
               modAngle[2] = eulerAngle(1) + 180;
            }
            else
            {
               modAngle[0] = eulerAngle(0) - 270;
               modAngle[1] = eulerAngle(2);
               modAngle[2] = eulerAngle(1);
            }
         }
         else
         {
            if (mUseGluLookAt)
            {
               modAngle[0] = eulerAngle(0);
               modAngle[1] = eulerAngle(2) - 180;;
               modAngle[2] = eulerAngle(1);
            }
            else
            {
               modAngle[0] = eulerAngle(0);
               modAngle[1] = eulerAngle(2) - 90;
               modAngle[2] = eulerAngle(1);
            }
         }
      }
      else if (mRotateAboutZaxis) // 1-2-3
      {
         if (eulerAngle(2) == 0.0)
         {
            modAngle[0] = eulerAngle(0);
            modAngle[1] = eulerAngle(1) - 90;
            modAngle[2] = eulerAngle(2) + 90;
         }
         else
         {
            modAngle[0] = eulerAngle(0) - 180;
            modAngle[1] = eulerAngle(1);
            modAngle[2] = eulerAngle(2) - 90;
         }
      }
   }
   
   #ifdef DEBUG_TRAJCANVAS_EULER
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::ComputeEulerAngles() modAngle=%s\n",
       modAngle.ToString().c_str());
   #endif

   return modAngle;

   #endif
}


//---------------------------------------------------------------------------
// void ComputeLongitudeLst(Real time, Real x, Real y, Real *meanHourAngle,
//                          Real *longitude, Real *localSiderealTime)
//---------------------------------------------------------------------------
void TrajPlotCanvas::ComputeLongitudeLst(Real time, Real x, Real y,
                                         Real *meanHourAngle, Real *longitude,
                                         Real *localSiderealTime)
{
   Real mha = 0.0;
   Real lon = 0.0;
   Real lst = 0.0;
   *meanHourAngle = mha;
   *longitude = lon;
   *localSiderealTime = lst;

   if (mViewObjName != "Earth")
      return;
   
   // compute longitude of the first spacecraft
   if (mSolarSystem)
   {
      Real raRad = ATan(y, x);
      Real raDeg = RadToDeg(raRad, true);
      CelestialBody *earth = mSolarSystem->GetBody("Earth");
      if (earth)
         mha = earth->GetHourAngle(time);
      
      lon = raDeg - mha;
      lon = AngleUtil::PutAngleInDegRange(lon, 0.0, 360.0);
   }
   
   lst = mha + lon;
   lst = AngleUtil::PutAngleInDegRange(lst, 0.0, 360.0);
   *meanHourAngle = mha;
   *longitude = lon;
   *localSiderealTime = lst;
}


//---------------------------------------------------------------------------
//  void CopyVector3(Real to[3], Real from[3])
//---------------------------------------------------------------------------
void TrajPlotCanvas::CopyVector3(Real to[3], Real from[3])
{
   to[0] = from[0];
   to[1] = from[1];
   to[2] = from[2];
}


//---------------------------------------------------------------------------
// bool LoadImage(char *fileName)
//---------------------------------------------------------------------------
bool TrajPlotCanvas::LoadImage(char *fileName)
{
#ifndef SKIP_DEVIL
   return false;
   
#else
   #if DEBUG_TRAJCANVAS_INIT
   MessageInterface::ShowMessage
      ("TrajPlotCanvas::LoadImage() Not using DevIL. file=%s\n", fileName);
   #endif
   
   ::wxInitAllImageHandlers();
   wxImage image = wxImage(fileName);
   int width = image.GetWidth();
   int height = image.GetHeight();
   
   GLubyte *data = image.GetData();
   
   if (data == NULL)
      return false;
   
   #if DEBUG_TRAJCANVAS_INIT
   int size = width * height * 3;
   MessageInterface::ShowMessage
      ("   width=%d, height=%d, size=%d\n", width, height, size);
   #endif
   
   // Why is image upside down?
   // Get vertial mirror
   wxImage mirror = image.Mirror(false);
   GLubyte *data1 = mirror.GetData();
   
   //used for min and magnifying texture
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
   
   //pass image to opengl
   #ifndef __WXGTK__
      // This call crashes GMAT on Linux, so it is excluded here. 
      gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGB, width, height, GL_RGB,
                     GL_UNSIGNED_BYTE, data1);
   #endif
   
   return true;
#endif
}

