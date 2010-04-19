//$Id$
//------------------------------------------------------------------------------
//                             GuiPlotReceiver
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2003/12/18
//
/**
 * Implements GuiPlotReceiver class.
 * This class updates OpenGL canvas, XY plot window
 */
//------------------------------------------------------------------------------
#if !defined __CONSOLE_APP__

#include "gmatwxdefs.hpp"
#include "gmatwxrcs.hpp"
#include "GmatAppData.hpp"
#include "MdiGlPlotData.hpp"         // for 3D Visualization
#include "MdiChildViewFrame.hpp"     // for 3D Visualization
#include "MdiChildTrajFrame.hpp"     // for 3D Visualization
#include "MdiChild3DViewFrame.hpp"   // for 3D Visualization
#include "MdiTsPlotData.hpp"         // for XY plot
#include "MdiChildTsFrame.hpp"       // for XY plot

#endif

#include "GuiPlotReceiver.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_PLOTIF_GL 1
//#define DEBUG_PLOTIF_GL_CREATE 1
//#define DEBUG_PLOTIF_GL_DELETE 1
//#define DEBUG_PLOTIF_GL_UPDATE 1
//#define DEBUG_PLOTIF_XY 1
//#define DEBUG_PLOTIF_XY_UPDATE 1
//#define DEBUG_RENAME 1

//---------------------------------
//  static data
//---------------------------------
GuiPlotReceiver* GuiPlotReceiver::theGuiPlotReceiver = NULL;

//---------------------------------
//  public functions
//---------------------------------

//------------------------------------------------------------------------------
// GuiPlotReceiver* GuiPlotReceiver::Instance()
//------------------------------------------------------------------------------
/**
 * Method used to initialize and retrieve teh GuiPlotReceiver singleton
 *
 * @return The singleton pointer
 */
//------------------------------------------------------------------------------
GuiPlotReceiver* GuiPlotReceiver::Instance()
{
   if (theGuiPlotReceiver == NULL)
      theGuiPlotReceiver = new GuiPlotReceiver();

   return theGuiPlotReceiver;
}

//------------------------------------------------------------------------------
//  GuiPlotReceiver()
//------------------------------------------------------------------------------
/**
 * Constructor used to create the singleton
 */
//------------------------------------------------------------------------------
GuiPlotReceiver::GuiPlotReceiver()
{
}


//------------------------------------------------------------------------------
//  ~GuiPlotReceiver()
//------------------------------------------------------------------------------
/**
 * Destructor
 */
//------------------------------------------------------------------------------
GuiPlotReceiver::~GuiPlotReceiver()
{
}


//------------------------------------------------------------------------------
//  bool CreateGlPlotWindow(const std::string &plotName, ...)
//------------------------------------------------------------------------------
/*
 * Creates OpenGlPlot window
 *
 * @param <plotName> plot name
 * @param <oldName>  old plot name, this is needed for renaming plot
 * @param <drawEcPlane>  true if draw ecliptic plane
 * @param <drawXyPlane>  true if draw XY plane
 * @param <drawWirePlane>  true if draw wire frame
 * @param <drawAxes>  true if draw axes
 * @param <drawGrid>  true if draw grid
 * @param <drawSunLine>  true if draw earth sun lines
 * @param <overlapPlot>  true if overlap plot without clearing the plot
 * @param <usevpInfo>  true if use viewpoint info to draw plot
 * @param <usepm>  true if use perspective projection mode
 * @param <numPtsToRedraw>  number of points to redraw during the run
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::CreateGlPlotWindow(const std::string &plotName,
                                       const std::string &oldName,
                                       bool drawEcPlane, bool drawXyPlane,
                                       bool drawWireFrame, bool drawAxes,
                                       bool drawGrid, bool drawSunLine,
                                       bool overlapPlot, bool usevpInfo, bool usepm,
                                       Integer numPtsToRedraw)
{
   //-------------------------------------------------------
   // check if new MDI child frame is needed
   //-------------------------------------------------------
   bool createNewFrame = true;
   wxString currPlotName;
   MdiChildViewFrame *frame = NULL;

   #if DEBUG_PLOTIF_GL_CREATE
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::CreateGlPlotWindow() MdiGlPlot::numChildren=%d, "
       "plotName=%s\n   oldName=%s\n", MdiGlPlot::numChildren,
       plotName.c_str(), oldName.c_str());
  #endif

   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

      if (frame)
         currPlotName = frame->GetPlotName();
      else
         break;

      #if DEBUG_PLOTIF_GL_CREATE
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::CreateGlPlotWindow() currPlotName[%d]=%s, addr=%p\n",
          i, currPlotName.c_str(), frame);
      #endif

      if (currPlotName.IsSameAs(plotName.c_str()))
      {
         createNewFrame = false;
         break;
      }
      else if (currPlotName.IsSameAs(oldName.c_str()))
      {
         // change plot name
         frame->SetPlotName(wxString(plotName.c_str()));
         createNewFrame = false;
         break;
      }
   }

   //-------------------------------------------------------
   // create MDI child frame if not exist
   //-------------------------------------------------------
   if (createNewFrame)
   {
      #if DEBUG_PLOTIF_GL_CREATE
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::CreateGlPlotWindow() Creating MdiChildViewFrame "
          "%s\n", plotName.c_str());
      #endif

      Integer x,y, w, h;
      #ifdef __WXMAC__
         wxSize size = wxGetDisplaySize();
         Integer plotCount = MdiGlPlot::numChildren + MdiTsPlot::numChildren;
         w = (size.GetWidth() - 239) / 2;
         h = 350;
         Integer hLoc = plotCount % 2;
         Integer vLoc = (Integer)((plotCount) / 2);
         x = 238 + hLoc * w + 1;
         y = 20  + vLoc * (h+10);
      #else
         x = -1;
         y = -1;
         w = -1;
         h = -1;
      #endif

      if (currentView == GmatPlot::TRAJECTORY_PLOT)
      {
         #if DEBUG_PLOTIF_GL_CREATE
         MessageInterface::ShowMessage("   Creating MdiChildTrajFrame...\n");
         #endif
         frame = new MdiChildTrajFrame
            (GmatAppData::Instance()->GetMainFrame(),
             wxString(plotName.c_str()),
             wxString(plotName.c_str()),
             wxPoint(x, y), wxSize(w, h),
             wxDEFAULT_FRAME_STYLE);
      }
      else if (currentView == GmatPlot::ENHANCED_3D_VIEW)
      {
         #if DEBUG_PLOTIF_GL_CREATE
         MessageInterface::ShowMessage("   Creating MdiChild3DViewFrame...\n");
         #endif
         frame = new MdiChild3DViewFrame
            (GmatAppData::Instance()->GetMainFrame(),
             wxString(plotName.c_str()),
             wxString(plotName.c_str()),
             wxPoint(x, y), wxSize(w, h),
             wxDEFAULT_FRAME_STYLE);
      }
      else
      {
         MessageInterface::ShowMessage
            ("**** ERROR **** Unknown view type %d\n", currentView);
         return false;
      }
      
      if (frame)
         frame->Show();
      else
         return false;

      #if __WXMAC__
         frame->SetSize(w-1, h-1);
      #endif

      #if DEBUG_PLOTIF_GL_CREATE
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::CreateGlPlotWindow() frame->GetPlotName()=%s\n",
          frame->GetPlotName().c_str());
      #endif

      GmatAppData::Instance()->GetMainFrame()->Tile();

      ++MdiGlPlot::numChildren;
   }
   else
   {
      #if DEBUG_PLOTIF_GL_CREATE
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::CreateGlPlotWindow() PlotName:%s already exist.\n",
          plotName.c_str());
      #endif
   }

   #if DEBUG_PLOTIF_GL_CREATE
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::CreateGlPlotWindow() setting view options for %s\n",
       frame->GetPlotName().c_str());
   #endif

   frame->SetDrawXyPlane(drawXyPlane);
   frame->SetDrawEcPlane(drawEcPlane);
   frame->SetDrawWireFrame(drawWireFrame);
   frame->SetDrawAxes(drawAxes);
   frame->SetDrawGrid(drawGrid);
   frame->SetDrawSunLine(drawSunLine);

   frame->SetOverlapPlot(overlapPlot);
   frame->SetUseInitialViewDef(usevpInfo);
   frame->SetUsePerspectiveMode(usepm);
   frame->SetNumPointsToRedraw(numPtsToRedraw);

   #if DEBUG_PLOTIF_GL_CREATE
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::CreateGlPlotWindow() returning true, there are %d plots.\n",
       MdiGlPlot::numChildren);
   #endif

   return true;
} //CreateGlPlotWindow()


//------------------------------------------------------------------------------
// void SetGlSolarSystem(const std::string &plotName, SolarSystem *ss)
//------------------------------------------------------------------------------
void GuiPlotReceiver::SetGlSolarSystem(const std::string &plotName, SolarSystem *ss)
{
   #if DEBUG_PLOTIF_GL
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::SetGlSolarSystem() SolarSystem=%p\n", ss);
   #endif

   wxString owner = wxString(plotName.c_str());

   MdiChildViewFrame *frame = NULL;
   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(owner.c_str()))
      {
         frame->SetSolarSystem(ss);
      }
   }
}


//------------------------------------------------------------------------------
// void SetGlObject(const std::string &plotName,  ...
//------------------------------------------------------------------------------
void GuiPlotReceiver::SetGlObject(const std::string &plotName,
                                const StringArray &objNames,
                                const UnsignedIntArray &objOrbitColors,
                                const std::vector<SpacePoint*> &objArray)
{
   #if DEBUG_PLOTIF_GL
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::SetGlObject() plotName:%s\n", plotName.c_str());
   #endif

   wxString owner = wxString(plotName.c_str());

   MdiChildViewFrame *frame = NULL;
   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(owner.c_str()))
      {
         frame->SetGlObject(objNames, objOrbitColors, objArray);
      }
   }
}


//------------------------------------------------------------------------------
// static void SetGlCoordSystem(const std::string &plotName, ...
//------------------------------------------------------------------------------
void GuiPlotReceiver::SetGlCoordSystem(const std::string &plotName,
                                       CoordinateSystem *internalCs,
                                       CoordinateSystem *viewCs,
                                       CoordinateSystem *viewUpCs)
{
   #if DEBUG_PLOTIF_GL
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::SetGlCoordSystem() plotName:%s\n", plotName.c_str());
   #endif

   wxString owner = wxString(plotName.c_str());
   MdiChildViewFrame *frame = NULL;

   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(owner.c_str()))
      {
         frame->SetGlCoordSystem(internalCs, viewCs, viewUpCs);
      }
   }
}


//------------------------------------------------------------------------------
// void SetGlViewOption(const std::string &plotName, SpacePoint *vpRefObj, ...
//------------------------------------------------------------------------------
void GuiPlotReceiver::SetGlViewOption(const std::string &plotName,
                                    SpacePoint *vpRefObj, SpacePoint *vpVecObj,
                                    SpacePoint *vdObj, Real vsFactor,
                                    const Rvector3 &vpRefVec, const Rvector3 &vpVec,
                                    const Rvector3 &vdVec, const std::string &upAxis,
                                    bool usevpRefVec, bool usevpVec, bool usevdVec,
                                     bool useFixedFov, Real fov)
{
   #if DEBUG_PLOTIF_GL
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::SetGlViewOption() plotName:%s\n", plotName.c_str());
   #endif

   wxString owner = wxString(plotName.c_str());
   MdiChildViewFrame *frame = NULL;

   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(owner.c_str()))
      {
         #if DEBUG_PLOTIF_GL
         MessageInterface::ShowMessage
            ("GuiPlotReceiver::SetGlViewOption() vpRefObj=%d, vsFactor=%f\n",
             vpRefObj, vsFactor);
         #endif

         frame->SetGlViewOption(vpRefObj, vpVecObj, vdObj, vsFactor, vpRefVec,
                                vpVec, vdVec, upAxis, usevpRefVec,usevpVec,
                                usevdVec, useFixedFov, fov);
      }
   }
}


//------------------------------------------------------------------------------
// void SetGlDrawOrbitFlag(const std::string &plotName, ...
//------------------------------------------------------------------------------
void GuiPlotReceiver::SetGlDrawOrbitFlag(const std::string &plotName,
                                       const std::vector<bool> &drawArray)
{
   #if DEBUG_PLOTIF_GL
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::SetGlDrawOrbitFlag() plotName:%s\n", plotName.c_str());
   #endif

   wxString owner = wxString(plotName.c_str());
   MdiChildViewFrame *frame = NULL;

   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(owner.c_str()))
      {
         frame->SetGlDrawOrbitFlag(drawArray);
      }
   }
}


//------------------------------------------------------------------------------
// void SetGlShowObjectFlag(const std::string &plotName, ...
//------------------------------------------------------------------------------
void GuiPlotReceiver::SetGlShowObjectFlag(const std::string &plotName,
                                        const std::vector<bool> &showArray)
{
   #if DEBUG_PLOTIF_GL
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::SetGlShowObjectFlag() plotName:%s\n", plotName.c_str());
   #endif

   wxString owner = wxString(plotName.c_str());
   MdiChildViewFrame *frame = NULL;

   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(owner.c_str()))
      {
         frame->SetGlShowObjectFlag(showArray);
      }
   }
}


//------------------------------------------------------------------------------
// void SetGlUpdateFrequency(const std::string &plotName, Integer updFreq)
//------------------------------------------------------------------------------
void GuiPlotReceiver::SetGlUpdateFrequency(const std::string &plotName,
                                         Integer updFreq)
{
   wxString owner = wxString(plotName.c_str());
   MdiChildViewFrame *frame = NULL;

   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(owner.c_str()))
      {
         frame->SetGlUpdateFrequency(updFreq);
      }
   }
}


//------------------------------------------------------------------------------
//  bool IsThere(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Checks if OpenGlPlot exist.
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::IsThere(const std::string &plotName)
{
   if (GmatAppData::Instance()->GetMainFrame() != NULL)
   {
      wxString owner = wxString(plotName.c_str());

      MdiChildViewFrame *frame  = NULL;
      for (int i=0; i<MdiGlPlot::numChildren; i++)
      {
         frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            return true;
         }
      }
   }

   return false;
}


//------------------------------------------------------------------------------
//  bool DeleteGlPlot(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Deletes OpenGlPlot by plot name.
 *
 * @param <plotName> name of plot to be deleted
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::DeleteGlPlot(const std::string &plotName)
{
   GmatAppData *gmatAppData = GmatAppData::Instance();

   if (gmatAppData->GetMainFrame() != NULL)
   {
      #if DEBUG_PLOTIF_GL_DELETE
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::DeleteGlPlot() plotName=%s\n", plotName.c_str());
      #endif

      wxString owner = wxString(plotName.c_str());
      MdiChildViewFrame *frame = NULL;

      for (int i=0; i<MdiGlPlot::numChildren; i++)
      {
         frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            gmatAppData->GetMainFrame()->CloseChild(owner, GmatTree::OUTPUT_OPENGL_PLOT);
            gmatAppData->GetMainFrame()->Tile();
            break;
         }
      }
   }

   return true;
}


//------------------------------------------------------------------------------
//  bool RefreshGlPlot(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Refreshes OpenGlPlot.
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::RefreshGlPlot(const std::string &plotName)
{
   if (GmatAppData::Instance()->GetMainFrame() != NULL)
   {
      #if DEBUG_PLOTIF_GL
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::RefreshGlPlot() plotName=%s\n",plotName.c_str());
      #endif

      wxString owner = wxString(plotName.c_str());
      MdiChildViewFrame *frame = NULL;

      for (int i=0; i<MdiGlPlot::numChildren; i++)
      {
         frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            frame->RefreshPlot();
         }
      }
   }

   return true;
}


//------------------------------------------------------------------------------
// bool SetGlEndOfRun(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Sets end of run flag to OpenGlPlot.
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::SetGlEndOfRun(const std::string &plotName)
{
   if (GmatAppData::Instance()->GetMainFrame() != NULL)
   {
      #if DEBUG_PLOTIF_GL
         MessageInterface::ShowMessage
            ("GuiPlotReceiver::SetGlEndOfRun() plotName=%s\n",plotName.c_str());
      #endif
      wxString owner = wxString(plotName.c_str());

      MdiChildViewFrame *frame = NULL;
      for (int i=0; i<MdiGlPlot::numChildren; i++)
      {
         frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            frame->SetEndOfRun();
         }
      }
   }

   return true;
}


//------------------------------------------------------------------------------
//  bool UpdateGlPlot(const std::string &plotName, ...
//------------------------------------------------------------------------------
/*
 * Buffers data and updates OpenGL plow window if updateCanvas is true
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::UpdateGlPlot(const std::string &plotName,
                                 const std::string &oldName,
                                 const StringArray &scNames, const Real &time,
                                 const RealArray &posX, const RealArray &posY,
                                 const RealArray &posZ, const RealArray &velX,
                                 const RealArray &velY, const RealArray &velZ,
                                 const UnsignedIntArray &scColors, bool solving,
                                 Integer solverOption, bool updateCanvas)
{
   #if DEBUG_PLOTIF_GL_UPDATE
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::UpdateGlPlot() entered. time = %f, number of plots = %d\n",
       time, MdiGlPlot::numChildren);
   #endif

   bool updated = false;
   wxString owner = wxString(plotName.c_str());

   MdiChildViewFrame *frame = NULL;

   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

      #if DEBUG_PLOTIF_GL_UPDATE
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::UpdateGlPlot() frame[%d]->GetPlotName()=%s "
          "owner=%s\n", i, frame->GetPlotName().c_str(), owner.c_str());
      #endif
      
      if (frame)
      {
         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            #if DEBUG_PLOTIF_GL_UPDATE
            MessageInterface::ShowMessage
               ("GuiPlotReceiver::UpdateGlPlot() now updating '%s'...\n",
                frame->GetPlotName().c_str());
            #endif
            frame->UpdatePlot(scNames, time, posX, posY, posZ, velX, velY, velZ,
                              scColors, solving, solverOption, updateCanvas);
            
            updated = true;
         }
      }
   }

   return updated;
} // end UpdateGlPlot()


//------------------------------------------------------------------------------
// bool TakeGlAction(const std::string &plotName, const std::string &action)
//------------------------------------------------------------------------------
bool GuiPlotReceiver::TakeGlAction(const std::string &plotName,
                                 const std::string &action)
{
   #if DEBUG_PLOTIF_GL_CLEAR
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::ClearGlSolverData() entered\n");
   #endif

   bool retval = false;
   wxString owner = wxString(plotName.c_str());

   MdiChildViewFrame *frame = NULL;

   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildViewFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());

      #if DEBUG_PLOTIF_GL_CLEAR
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::ClearGlSolverData() frame[%d]->GetPlotName()=%s "
          "owner=%s\n", i, frame->GetPlotName().c_str(), owner.c_str());
      #endif

      if (frame)
      {
         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            frame->TakeAction(action);
            retval = true;
         }
      }
   }

   return retval;
}


//------------------------------------------------------------------------------
//  bool CreateTsPlotWindow(const std::string &plotName,
//       const std::string &oldName, const std::string &plotTitle,
//       const std::string &xAxisTitle, const std::string &yAxisTitle,
//       bool drawGrid = false)
//------------------------------------------------------------------------------
/*
 * Creates a TsPlot window.
 *
 * @param plotName Name of the plot
 * @param oldName Former name of the plot
 * @param plotTitle Title of the plot
 * @param xAxisTitle X-axis label for the plot
 * @param yAxisTitle Y-axis label for the plot
 * @param drawGrid Flag indicating if the grid lines should be drawn
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::CreateTsPlotWindow(const std::string &plotName,
                                       const std::string &oldName,
                                       const std::string &plotTitle,
                                       const std::string &xAxisTitle,
                                       const std::string &yAxisTitle,
                                       bool drawGrid)
{
   //-------------------------------------------------------
   // check if new MDI child frame needed
   //-------------------------------------------------------
   bool createNewFrame = true;
   wxString currPlotName;
   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());
      currPlotName = frame->GetPlotName();

      if (currPlotName.IsSameAs(plotName.c_str()))
      {
         createNewFrame = false;
         break;
      }
      else if (currPlotName.IsSameAs(oldName.c_str()))
      {
         #if DEBUG_RENAME
         MessageInterface::ShowMessage
            ("GuiPlotReceiver::CreateTsPlotWindow() currPlotName=%s, "
                  "oldName=%s\n", currPlotName.c_str(), oldName.c_str());
         #endif

         // change plot name
         frame->SetPlotName(wxString(plotName.c_str()));
         createNewFrame = false;
         break;
      }
   }

   //-------------------------------------------------------
   // create MDI child XY frame
   //-------------------------------------------------------
   if (createNewFrame)
   {
      #if DEBUG_PLOTIF_XY
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::CreateTsPlotWindow() Creating new "
          "MdiChildXyFrame\n   X Axis Title = %s  Y Axis Title = %s\n",
          xAxisTitle.c_str(), yAxisTitle.c_str());
      #endif

      Integer x,y, w, h;
      #ifdef __WXMAC__
         wxSize size = wxGetDisplaySize();
         Integer plotCount = MdiGlPlot::numChildren + MdiTsPlot::numChildren;
         w = (size.GetWidth() - 239) / 2;
         h = 350;
         Integer hLoc = plotCount % 2;
         Integer vLoc = (Integer)((plotCount) / 2);
         x = 238 + hLoc * w + 1;
         y = 20  + vLoc * (h+10);
      #else
         x = -1;
         y = -1;
         w = 500;
         h = 350;
      #endif


      // create a frame, containing a XY plot canvas
      frame =
         new MdiChildTsFrame(GmatAppData::Instance()->GetMainFrame(), true,
                             wxString(plotName.c_str()),
                             wxString(plotTitle.c_str()),
                             wxString(xAxisTitle.c_str()),
                             wxString(yAxisTitle.c_str()),
                             wxPoint(x, y), wxSize(w, h),
                             wxDEFAULT_FRAME_STYLE);

      frame->Show();

      GmatAppData::Instance()->GetMainFrame()->Tile();

      ++MdiTsPlot::numChildren;

      frame->RedrawCurve();
   }

   frame->SetShowGrid(drawGrid);
   frame->ResetZoom();

   #if DEBUG_PLOTIF_XY
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::CreateTsPlotWindow() leaving\n");
   #endif

   return true;
}

//------------------------------------------------------------------------------
//  bool DeleteTsPlot(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Deletes TsPlot by plot name.
 *
 * @param plotName name of plot to be deleted
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::DeleteTsPlot(const std::string &plotName)
{
   GmatAppData *gmatAppData = GmatAppData::Instance();

   if (gmatAppData->GetMainFrame() != NULL)
   {
      #if DEBUG_PLOTIF_XY
         MessageInterface::ShowMessage("GuiPlotReceiver::DeleteTsPlot()\n");
      #endif

      wxString owner = wxString(plotName.c_str());
      MdiChildTsFrame *frame = NULL;

      for (int i=0; i<MdiTsPlot::numChildren; i++)
      {
         frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            gmatAppData->GetMainFrame()->CloseChild(owner,
                  GmatTree::OUTPUT_XY_PLOT);
            gmatAppData->GetMainFrame()->Tile();
            break;
         }
      }
   }

   return true;
}

//------------------------------------------------------------------------------
// bool AddTsPlotCurve(const std::string &plotName, int curveIndex,
//                     int yOffset, Real yMin, Real yMax,
//                     const std::string &curveTitle,
//                     UnsignedInt penColor)
//------------------------------------------------------------------------------
/*
 * Adds a plot curve to XY plow window.
 *
 * @param plotName The name of the plot that receives the new curve
 * @param curveIndex The index for the curve
 * @param yOffset Offset used to shift the curve up or down; deprecated
 * @param yMin Minimum Y value for the curve; deprecated
 * @param yMax Maximum Y value for the curve; deprecated
 * @param curveTitle Label for the curve
 * @param penColor Default color for the curve
 *
 * @return true on success, false is no curve was added
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::AddTsPlotCurve(const std::string &plotName,
      int curveIndex, int yOffset, Real yMin, Real yMax,
      const std::string &curveTitle, UnsignedInt penColor)
{
   UnsignedInt localPenColor = penColor;
   if (penColor == 0)
      localPenColor = 0xFFFFFF;

   bool added = false;

   #if DEBUG_PLOTIF_XY
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::AddTsPlotCurve() entered."
       " plotName = " + plotName + " curveTitle = " +
       curveTitle + "\n");

   MessageInterface::ShowMessage
      ("GuiPlotReceiver::AddTsPlotCurve() numChildren = %d\n",
       MdiTsPlot::numChildren);
   #endif

   MdiChildTsFrame *frame = NULL;
   for (int i = 0; i < MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->AddPlotCurve(curveIndex, yOffset, yMin, yMax,
                             wxString(curveTitle.c_str()), localPenColor);
         added = true;
      }
   }

   return added;
}

//------------------------------------------------------------------------------
// bool DeleteAllTsPlotCurves(const std::string &plotName,
//                            const std::string &oldName)
//------------------------------------------------------------------------------
/*
 * Deletes all plot curves in XY plow window.
 *
 * @param plotName The name of the plot that receives the new curve
 * @param oldName The previous name of the plot that receives the new curve
 *
 * @return true on success, false if no action was taken
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::DeleteAllTsPlotCurves(const std::string &plotName,
                                          const std::string &oldName)
{
   #if DEBUG_PLOTIF_XY
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::DeleteAllPlotCurve() plotName = %s "
       "numChildren = %d\n", plotName.c_str(),
       MdiTsPlot::numChildren);
   #endif

   MdiChildTsFrame *frame = NULL;
   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());
      if (frame->GetPlotName().IsSameAs(plotName.c_str()) ||
          frame->GetPlotName().IsSameAs(oldName.c_str()))
      {
         frame->DeleteAllPlotCurves();
      }
   }

   return true;
}


//------------------------------------------------------------------------------
// bool DeleteTsPlotCurve(const std::string &plotName, int curveIndex)
//------------------------------------------------------------------------------
/*
 * Deletes a plot curve to XY plow window.
 *
 * @param plotName The name of the plot that receives the new curve
 * @param curveIndex Index of the curve that is to be deleted
 *
 * @return true on success, false if no curve was deleted
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::DeleteTsPlotCurve(const std::string &plotName,
      int curveIndex)
{
   #if DEBUG_PLOTIF_XY
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::DeleteTsPlotCurve() entered plotName = %s "
       "curveIndex = %d\n", plotName.c_str(), curveIndex);

   MessageInterface::ShowMessage
      ("GuiPlotReceiver::DeleteTsPlotCurve() numChildren = %d\n",
       MdiTsPlot::numChildren);
   #endif

   MdiChildTsFrame *frame = NULL;
   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->DeletePlotCurve(curveIndex);
      }
   }

   return true;
}


//------------------------------------------------------------------------------
// void ClearTsPlotData(const std::string &plotName))
//------------------------------------------------------------------------------
/**
 * Removes all data from the plot curves, leaving the curve containers in place
 * but empty.
 *
 * @param plotName The name of the plot that is being cleared
 */
//------------------------------------------------------------------------------
void GuiPlotReceiver::ClearTsPlotData(const std::string &plotName)
{
   #if DEBUG_PLOTIF_XY
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::ClearTsPlotData() numChildren = %d\n",
       MdiTsPlot::numChildren);
   #endif

   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->ClearPlotData();
      }
   }
}

//------------------------------------------------------------------------------
// void TsPlotPenUp(const std::string &plotName))
//------------------------------------------------------------------------------
/**
 * Tells a plot to stop drawing received data.  This method is idempotent.
 *
 * @param plotName The name of the plot that is being cleared
 */
//------------------------------------------------------------------------------
void GuiPlotReceiver::TsPlotPenUp(const std::string &plotName)
{
   #if DEBUG_PLOTIF_XY
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::TsPlotPenUp() numChildren = %d\n",
          MdiTsPlot::numChildren);
   #endif

   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->PenUp();
      }
   }
}

//------------------------------------------------------------------------------
// void TsPlotPenDown(const std::string &plotName))
//------------------------------------------------------------------------------
/**
 * Tells a plot to resume drawing received data.  This method is idempotent.
 *
 * @param plotName The name of the plot that is being cleared
 */
//------------------------------------------------------------------------------
void GuiPlotReceiver::TsPlotPenDown(const std::string &plotName)
{
   #if DEBUG_PLOTIF_XY
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::TsPlotPenDown() numChildren = %d\n",
          MdiTsPlot::numChildren);
   #endif

   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->PenDown();
      }
   }
}



//------------------------------------------------------------------------------
// void TsPlotMarkPoint(const std::string &plotName, Integer index,
//       Integer forCurve)
//------------------------------------------------------------------------------
/**
 * Marks a specific point on a specific curve of a TsPlot with an oversized X
 *
 * @param plotName The plot that contains the curve
 * @param index The index of the point that gets marked
 * @param curveNumber The index of the curve containing the point to mark.  Set
 *                    curveNumber to -1 to mark all curves.
 */
//------------------------------------------------------------------------------
void GuiPlotReceiver::TsPlotMarkPoint(const std::string &plotName,
      Integer index, Integer forCurve)
{
   #if DEBUG_PLOTIF_XY
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::TsPlotPenDown() numChildren = %d\n",
          MdiTsPlot::numChildren);
   #endif

   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->MarkPoint(index, forCurve);
      }
   }
}


//------------------------------------------------------------------------------
// void TsPlotChangeColor(const std::string &plotName, Integer index,
//       UnsignedInt newColor, Integer forCurve)
//------------------------------------------------------------------------------
/**
 * Changes the color of a curve partway into a run
 *
 * @param plotName The plot that contains the curve
 * @param index The index of the first point that gets the new color
 * @param newColor The new color
 * @param forCurve The index of the curve that is changing color.
 */
//------------------------------------------------------------------------------
void GuiPlotReceiver::TsPlotChangeColor(const std::string &plotName,
      Integer index, UnsignedInt newColor, Integer forCurve)
{
   #if DEBUG_PLOTIF_XY
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::TsPlotChangeColor() numChildren = %d\n",
          MdiTsPlot::numChildren);
   #endif

   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->ChangeColor(index, newColor, forCurve);
      }
   }
}


//------------------------------------------------------------------------------
// void TsPlotChangeMarker(const std::string &plotName, Integer index,
//       Integer newMarker, Integer forCurve)
//------------------------------------------------------------------------------
/**
 * Changes the marker used on a curve, starting at a specified point
 *
 * @param plotName The plot that contains the curve
 * @param index The index of the first point that gets the new color
 * @param newMarker The new marker
 * @param forCurve The index of the curve that is changing color.
 */
//------------------------------------------------------------------------------
void GuiPlotReceiver::TsPlotChangeMarker(const std::string &plotName,
      Integer index, Integer newMarker, Integer forCurve)
{
   #if DEBUG_PLOTIF_XY
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::TsPlotPenDown() numChildren = %d\n",
          MdiTsPlot::numChildren);
   #endif

   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->ChangeMarker(index, newMarker, forCurve);
      }
   }
}


//------------------------------------------------------------------------------
// void TsPlotRescale(const std::string &plotName)
//------------------------------------------------------------------------------
/**
 * Sends a rescale message to the plot
 *
 * @param plotName The plot that is to be rescaled
 */
//------------------------------------------------------------------------------
void GuiPlotReceiver::TsPlotRescale(const std::string &plotName)
{
   #if DEBUG_PLOTIF_XY
      MessageInterface::ShowMessage
         ("GuiPlotReceiver::TsPlotRescale() numChildren = %d\n",
          MdiTsPlot::numChildren);
   #endif

   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->Rescale();
      }
   }
}


//------------------------------------------------------------------------------
// void TsPlotCurveSettings(const std::string &plotName, bool useLines,
//       Integer lineWidth, Integer lineStyle, bool useMarkers,
//       Integer markerSize, Integer marker, bool useHiLow, Integer forCurve)
//------------------------------------------------------------------------------
/**
 * Sets the default settings for a curve
 *
 * @param plotName The name of the plot that contains the curve
 * @param useLines Flag that is set if the curve should have lines connecting
 *                 the curve points
 * @param lineWidth The width, in pixels, of all drawn lines
 * @param lineStyle The style of the lines
 * @param useMarkers Flag used to toggle on markers at each point on the curve
 * @param markerSize The size of the marker
 * @param marker The marker to be used
 * @param useHiLow Flag used to turn error bars on
 * @param forCurve The index of the curve receiving the settings
 */
//------------------------------------------------------------------------------
void GuiPlotReceiver::TsPlotCurveSettings(const std::string &plotName,
      bool useLines, Integer lineWidth, Integer lineStyle, bool useMarkers,
      Integer markerSize, Integer marker, bool useHiLow, Integer forCurve)
{
   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->CurveSettings(useLines, lineWidth, lineStyle, useMarkers,
               markerSize, marker, useHiLow, forCurve);
      }
   }
}

//------------------------------------------------------------------------------
// void SetTsPlotTitle(const std::string &plotName,
//       const std::string &plotTitle)
//------------------------------------------------------------------------------
/**
 * Sets the title for a plot
 *
 * @param plotName The name of the plot
 * @param plotTitle The new title for the plot
 */
//------------------------------------------------------------------------------
void GuiPlotReceiver::SetTsPlotTitle(const std::string &plotName,
                                   const std::string &plotTitle)
{
   #if DEBUG_PLOTIF_XY
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::SetTsPlotTitle() plotName = %s "
       "plotTitle = %s\n", plotName.c_str(), plotTitle.c_str());
   #endif

   MdiChildTsFrame *frame = NULL;
   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         #if DEBUG_PLOTIF_XY
            MessageInterface::ShowMessage
               ("GuiPlotReceiver::SetTsPlotTitle() calling "
                " frame->SetPlotTitle() \n");
         #endif

         frame->SetPlotTitle(wxString(plotTitle.c_str()));
      }
   }
}


//------------------------------------------------------------------------------
// void GuiPlotReceiver::ShowTsPlotLegend(const std::string &plotName)
//------------------------------------------------------------------------------
/**
 * Turns on display of the plot legend
 *
 * @param plotName The name of the plot
 *
 * @note This method is not yet implemented
 */
//------------------------------------------------------------------------------
void GuiPlotReceiver::ShowTsPlotLegend(const std::string &plotName)
{
//   MdiChildXyFrame *frame = NULL;
//   for (int i=0; i<MdiTsPlot::numChildren; i++)
//   {
//      frame = (MdiChildXyFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());
//
//      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
//      {
//         #if DEBUG_PLOTIF_XY
//            MessageInterface::ShowMessage
//               ("GuiPlotReceiver::ShowTsPlotLegend() calling  frame->ShowPlotLegend() \n");
//         #endif
//
//         frame->ShowPlotLegend();
//      }
//   }
}


//------------------------------------------------------------------------------
// bool RefreshTsPlot(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Refreshes the XY plot.
 *
 * @param plotName name of xy plot
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::RefreshTsPlot(const std::string &plotName)
{
   if (GmatAppData::Instance()->GetMainFrame() != NULL)
   {
      #if DEBUG_PLOTIF_XY_UPDATE
         MessageInterface::ShowMessage
            ("GuiPlotReceiver::RefreshTsPlot() plotName=%s, numChildren=%d\n",
             plotName.c_str(), MdiTsPlot::numChildren);
      #endif

      wxString owner = wxString(plotName.c_str());

      MdiChildTsFrame *frame = NULL;

      for (int i=0; i<MdiTsPlot::numChildren; i++)
      {
         frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());
         if (frame)
         {
            if (frame->GetPlotName().IsSameAs(owner.c_str()))
            {
               frame->RedrawCurve();
               #if __WXMAC__
                  frame->Refresh(true,NULL);
               #endif
            }
         }
      }
   }

   return true;
}


//------------------------------------------------------------------------------
// bool UpdateTsPlot(const std::string &plotName, const std::string &oldName,
//                   const Real &xval, const Rvector &yvals,
//                   const std::string &plotTitle,
//                   const std::string &xAxisTitle,
//                   const std::string &yAxisTitle, bool updateCanvas,
//                   bool drawGrid)
//------------------------------------------------------------------------------
/*
 * Updates a TsPlot window.
 *
 * @param plotName name of xy plot
 * @param oldName Former name of the plot, or an empty string
 * @param xval x value
 * @param yvals y values, should be in the order of curve added
 * @param plotTitle The plot's Title
 * @param xAxisTitle The plot's X axis title
 * @param yAxisTitle The plot's Y axis title
 * @param updateCanvas Flag indicating if the canvas should update immediately
 * @param drawGrid flag indicating if the grid should be drawn
 *
 * @return true if an update occurred, false otherwise
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::UpdateTsPlot(const std::string &plotName,
                                 const std::string &oldName,
                                 const Real &xval, const Rvector &yvals,
                                 const std::string &plotTitle,
                                 const std::string &xAxisTitle,
                                 const std::string &yAxisTitle,
                                 bool updateCanvas, bool drawGrid)
{
   bool updated = false;
   wxString owner = wxString(plotName.c_str());

   #if DEBUG_PLOTIF_XY_UPDATE
   MessageInterface::ShowMessage
      ("GuiPlotReceiver::UpdateTsPlot() numChildren = %d\n",
       MdiTsPlot::numChildren);
   #endif

   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame)
      {
         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            int numCurves = frame->GetCurveCount();
            #if DEBUG_PLOTIF_XY_UPDATE
               MessageInterface::ShowMessage
               ("GuiPlotReceiver::UpdateTsPlot() numCurves = %d\n", numCurves);
            #endif

            for (int j=0; j<numCurves; j++)
            {
               #if DEBUG_PLOTIF_XY_UPDATE
                  MessageInterface::ShowMessage
                  ("GuiPlotReceiver::UpdateTsPlot() yvals[%d] = %f\n", j, yvals(j));
               #endif

               frame->AddDataPoints(j, xval, yvals(j));
            }

            if (updateCanvas)
               frame->RedrawCurve();

            updated = true;
         }
      }
   }

   return updated;
}

//------------------------------------------------------------------------------
// bool UpdateTsPlotData(const std::string &plotName, const Real &xval,
//       const Rvector &yvals, const Rvector *yhis, const Rvector *ylows)
//------------------------------------------------------------------------------
/**
 * Updates the data on a plot, passing in a set of y values for a given x, and
 * optionally the data used to draw error bars
 *
 * @param plotName The name of the plot receiving the data
 * @param xval The X value associated with the points
 * @param yvals The Y values associated with the points; these are assigned to
 *              the curves indexed in the order contained in the array
 * @param yhis +sigma error data for the error bars; NULL if not used
 * @param ylows -sigma error for the error bars; if NULL, the low error is
 *              assumed to have the same magnitude as the high error
 *
 * @return true if the data was processed, false if not
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::UpdateTsPlotData(const std::string &plotName,
      const Real &xval, const Rvector &yvals, const Rvector *yhis,
      const Rvector *ylows)
{
   bool updated = false;

   wxString owner = wxString(plotName.c_str());
   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame)
      {
         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            int numCurves = frame->GetCurveCount();
            #if DEBUG_PLOTIF_XY_UPDATE
               MessageInterface::ShowMessage
               ("GuiPlotReceiver::UpdateTsPlot() numCurves = %d\n", numCurves);
            #endif

            for (int j=0; j<numCurves; j++)
            {
               #if DEBUG_PLOTIF_XY_UPDATE
                  MessageInterface::ShowMessage
                  ("GuiPlotReceiver::UpdateTsPlot() yvals[%d] = %f\n", j, yvals(j));
               #endif

               if (yhis != NULL)
               {
                  if (ylows != NULL)
                     frame->AddDataPoints(j, xval, yvals(j), (*yhis)(j),
                           (*ylows)(j));
                  else
                     frame->AddDataPoints(j, xval, yvals(j), (*yhis)(j));
               }
               else
                  frame->AddDataPoints(j, xval, yvals(j));
            }

            if (frame->IsActive())
            {
               frame->RedrawCurve();
            }
            updated = true;
         }
      }
   }

   return updated;
}


//------------------------------------------------------------------------------
// bool UpdateTsPlotCurve(const std::string &plotName, const Integer whichCurve,
//       const Real xval, const Real yval, const Real yhi, const Real ylow)
//------------------------------------------------------------------------------
/**
 * Adds a point to the plot data for a specific curve on a plot
 *
 * @param plotName The name of the plot receiving the data
 * @param whichCurve Index of the curve receiving the data
 * @param xval The X value associated with the point
 * @param yval The Y value associated with the points
 * @param yhi +sigma error data for the point's error bar; only used if hi > 0.0
 * @param ylow -sigma error for the point's error bar; if <= 0.0, the low error
 *            is assumed to have the same magnitude as the high error
 *
 * @return true if the data was processed, false if not
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::UpdateTsPlotCurve(const std::string &plotName,
                      const Integer whichCurve, const Real xval,
                      const Real yval, const Real yhi, const Real ylow)
{
   bool updated = false;

   if (whichCurve >= 0)
   {
      wxString owner = wxString(plotName.c_str());
      MdiChildTsFrame *frame = NULL;

      for (int i=0; i<MdiTsPlot::numChildren; i++)
      {
         frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

         if (frame)
         {
            if (frame->GetPlotName().IsSameAs(owner.c_str()))
            {
               int numCurves = frame->GetCurveCount();
               #if DEBUG_PLOTIF_XY_UPDATE
                  MessageInterface::ShowMessage
                        ("GuiPlotReceiver::UpdateTsPlotCurve() numCurves = %d\n",
                         numCurves);
               #endif

               if (whichCurve < numCurves)
               {
                  #if DEBUG_PLOTIF_XY_UPDATE
                     MessageInterface::ShowMessage
                     ("GuiPlotReceiver::UpdateTsPlot() yvals[%d] = %f\n", j, yvals(j));
                  #endif

                  frame->AddDataPoints(whichCurve, xval, yval, yhi, ylow);
               }
               if (frame->IsActive())
               {
                  frame->RedrawCurve();
               }
               updated = true;
            }
         }
      }
   }

   return updated;
}


//------------------------------------------------------------------------------
// bool DeactivateTsPlot(const std::string &plotName)
//------------------------------------------------------------------------------
/**
 * Disables redrawing for a plot.  This method is used when a plot is receiving
 * a large amount of data all at once, so that the update performance doesn't
 * degrade.
 *
 * @param plotName The name of the plot receiving the data
 *
 * @return true is a plot received the message, false if not
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::DeactivateTsPlot(const std::string &plotName)
{
   bool deactivated = false;

   wxString owner = wxString(plotName.c_str());
   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame)
      {
         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            frame->IsActive(false);
            deactivated = true;
         }
      }
   }

   return deactivated;
}


//------------------------------------------------------------------------------
// bool ActivateTsPlot(const std::string &plotName)
//------------------------------------------------------------------------------
/**
 * Enables redrawing for a plot, and forces an immediate update.  This method is
 * used to redraw a plot after it has been disabled and  received a large amount
 * of data all at once.
 *
 * @param plotName The name of the plot receiving the data
 *
 * @return true is a plot received the message, false if not
 */
//------------------------------------------------------------------------------
bool GuiPlotReceiver::ActivateTsPlot(const std::string &plotName)
{
   bool activated = false;

   wxString owner = wxString(plotName.c_str());
   MdiChildTsFrame *frame = NULL;

   for (int i=0; i<MdiTsPlot::numChildren; i++)
   {
      frame = (MdiChildTsFrame*)(MdiTsPlot::mdiChildren.Item(i)->GetData());

      if (frame)
      {
         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            frame->IsActive(true);
            frame->RedrawCurve();
            activated = true;
         }
      }
   }

   return activated;
}
