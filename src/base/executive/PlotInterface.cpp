//$Header$
//------------------------------------------------------------------------------
//                             PlotInterface
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
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
 * Implements PlotInterface class.
 * This class updates OpenGL canvas, XY plot window
 */
//------------------------------------------------------------------------------
#if !defined __CONSOLE_APP__

#include "gmatwxdefs.hpp"
#include "gmatwxrcs.hpp"
#include "GmatAppData.hpp"
#include <wx/settings.h>    // for wxSYS_SCREEN_Y
// for OpenGL plot
#include "MdiGlPlotData.hpp"
#include "MdiChildTrajFrame.hpp"
// for XY plot
#include "MdiXyPlotData.hpp"
#include "MdiChildXyFrame.hpp"

#endif

#include <iostream>                // for cout, endl
#include "PlotInterface.hpp"       // for PlotInterface functions
#include "Rvector.hpp"
#include "MessageInterface.hpp"    // for ShowMessage()

//#define DEBUG_PLOTIF_GL 1
//#define DEBUG_PLOTIF_GL_UPDATE 1
//#define DEBUG_PLOTIF_XY 1
//#define DEBUG_PLOTIF_XY_UPDATE 1
//#define DEBUG_RENAME 1

//---------------------------------
//  static data
//---------------------------------

//---------------------------------
//  public functions
//---------------------------------

//------------------------------------------------------------------------------
//  PlotInterface()
//------------------------------------------------------------------------------
PlotInterface::PlotInterface()
{
}


//------------------------------------------------------------------------------
//  ~PlotInterface()
//------------------------------------------------------------------------------
PlotInterface::~PlotInterface()
{
}


//------------------------------------------------------------------------------
//  bool CreateGlPlotWindow(const std::string &plotName, const std::string &oldName,
//                          const std::string &csName, SolarSystem *ssPtr,
//                          bool drawEcPlane, bool drawEqPlane, bool drawWireFrame,
//                          bool overlapPlot, bool usevpInfo, bool usepm)
//------------------------------------------------------------------------------
/*
 * Creates OpenGlPlot window
 *
 * @param <plotName> plot name
 * @param <oldName>  old plot name, this is needed for renaming plot
 * @param <csName>  coordinate system name
 * @param <ssPtr>  solar system pionter
 * @param <drawEcPlane>  true if draw ecliptic plane
 * @param <drawEqPlane>  true if draw equatorial plane
 * @param <drawWirePlane>  true if draw wire frame
 * @param <overlapPlot>  true if overlap plot without clearing the plot
 * @param <usevpInfo>  true if use viewpoint info to draw plot
 * @param <usepm>  true if use perspective projection mode
 */
//------------------------------------------------------------------------------
bool PlotInterface::CreateGlPlotWindow(const std::string &plotName,
                                       const std::string &oldName,
                                       const std::string &csName,
                                       SolarSystem *ssPtr,
                                       bool drawEcPlane, bool drawEqPlane,
                                       bool drawWireFrame, bool overlapPlot,
                                       bool usevpInfo, bool usepm)
{    
#if defined __CONSOLE_APP__
   return true;
#else

   //-------------------------------------------------------
   // check if new MDI child frame is needed
   //-------------------------------------------------------
   bool createNewFrame = true;
   wxString currPlotName;
   MdiChildTrajFrame *currPlotFrame = NULL;
   
   #if DEBUG_PLOTIF_GL
   MessageInterface::ShowMessage
      ("PlotInterface::CreateGlPlotWindow() MdiGlPlot::numChildren=%d, plotName=%s\n",
       MdiGlPlot::numChildren, plotName.c_str());
   #endif
     
   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      //currPlotFrame = (MdiChildTrajFrame*)MdiGlPlot::mdiChildren[i];
      currPlotFrame = (MdiChildTrajFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());
      currPlotName = currPlotFrame->GetPlotName();
      
      #if DEBUG_PLOTIF_GL
      MessageInterface::ShowMessage
         ("PlotInterface::CreateGlPlotWindow() currPlotName[%d]=%s, addr=%d\n",
          i, currPlotName.c_str(), currPlotFrame);
      #endif
      
      if (currPlotName.IsSameAs(plotName.c_str()))
      {
         createNewFrame = false;
         break;
      }
      else if (currPlotName.IsSameAs(oldName.c_str()))
      {
         // change plot name
//          ((MdiChildTrajFrame*)MdiGlPlot::mdiChildren[i])->
//             SetPlotName(wxString(plotName.c_str()));
         currPlotFrame->SetPlotName(wxString(plotName.c_str()));
         createNewFrame = false;
         break;
      }
   }
   
   //-------------------------------------------------------
   // create MDI child frame if not exist
   //-------------------------------------------------------
   if (createNewFrame)
   {
      #if DEBUG_PLOTIF_GL
      MessageInterface::ShowMessage
         ("PlotInterface::CreateGlPlotWindow() Creating MdiChildTrajFrame "
          "%s\n", plotName.c_str());
      #endif
      
      //GmatAppData::GetMainFrame()->trajMainSubframe =
      currPlotFrame =
         new MdiChildTrajFrame(GmatAppData::GetMainFrame(), true,
                               wxString(plotName.c_str()),
                               wxString(plotName.c_str()),
                               wxPoint(-1, -1), wxSize(-1, -1), //wxSize(500, 350), //wxSize(-1, -1),
                               wxDEFAULT_FRAME_STYLE, wxString(csName.c_str()),
                               ssPtr);
      currPlotFrame->Show();
      
      #if DEBUG_PLOTIF_GL
      MessageInterface::ShowMessage
         ("PlotInterface::CreateGlPlotWindow() frame->GetPlotName()=%s\n",
          currPlotFrame->GetPlotName().c_str());
      //GmatAppData::GetMainFrame()->trajMainSubframe->GetPlotName().c_str());
      #endif
      
      GmatAppData::GetMainFrame()->Tile(); //loj: 5/17/05 uncommented
      
      ++MdiGlPlot::numChildren;

      
      //----------------------------------------------------
      // initialize GL
      //----------------------------------------------------
      //if (!GmatAppData::GetMainFrame()->trajMainSubframe->mCanvas->InitGL())
//       if (!currPlotFrame->mCanvas->InitGL())
//       {
//          wxMessageDialog msgDialog(GmatAppData::GetMainFrame(),
//                                    _T("InitGL() failed"), _T("CreateGlPlotWindow"));
//          msgDialog.ShowModal();
//          return false;
//       }
   }
   else
   {
      #if DEBUG_PLOTIF_GL
      MessageInterface::ShowMessage
         ("PlotInterface::CreateGlPlotWindow() PlotName:%s already exist.\n",
          plotName.c_str());
      #endif
   }
      
   #if DEBUG_PLOTIF_GL
   MessageInterface::ShowMessage
      ("PlotInterface::CreateGlPlotWindow() setting view options for %s\n",
       currPlotFrame->GetPlotName().c_str());
   #endif
   
   currPlotFrame->SetDrawEqPlane(drawEqPlane);
   currPlotFrame->SetDrawEcPlane(drawEcPlane);
   currPlotFrame->SetDrawWireFrame(drawWireFrame);
   currPlotFrame->SetOverlapPlot(overlapPlot);
   currPlotFrame->SetUseViewPointInfo(usevpInfo);
   currPlotFrame->SetUsePerspectiveMode(usepm);
   currPlotFrame->SetViewCoordSystem(wxString(csName.c_str()));

   #if DEBUG_PLOTIF_GL
   MessageInterface::ShowMessage
      ("PlotInterface::CreateGlPlotWindow() returning true\n");
   #endif
   
   return true;
#endif
} //CreateGlPlotWindow()


//------------------------------------------------------------------------------
// void SetGlObject(const std::string &plotName,  ...
//------------------------------------------------------------------------------
void PlotInterface::SetGlObject(const std::string &plotName,
                                const StringArray &objNames,
                                const UnsignedIntArray &objOrbitColors,
                                const std::vector<SpacePoint*> objArray)
{
#if defined __CONSOLE_APP__
   return;
#else
   
   #if DEBUG_PLOTIF_GL
   MessageInterface::ShowMessage
      ("PlotInterface::SetGlObject() plotName:%s\n", plotName.c_str());
   #endif
   
   wxString owner = wxString(plotName.c_str());

   MdiChildTrajFrame *frame = NULL;
   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildTrajFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());
      
      if (frame->GetPlotName().IsSameAs(owner.c_str()))
      {            
         frame->SetGlObject(objNames, objOrbitColors, objArray);
      }
   }
   
#endif
}


//------------------------------------------------------------------------------
// static void SetGlCoordSystem(const std::string &plotName, ...
//------------------------------------------------------------------------------
void PlotInterface::SetGlCoordSystem(const std::string &plotName,
                                     CoordinateSystem *viewCs,
                                     CoordinateSystem *viewUpCs)
{
#if defined __CONSOLE_APP__
   return;
#else

   #if DEBUG_PLOTIF_GL
   MessageInterface::ShowMessage
      ("PlotInterface::SetGlCoordSystem() plotName:%s\n", plotName.c_str());
   #endif
   
   wxString owner = wxString(plotName.c_str());
   MdiChildTrajFrame *frame = NULL;
   
   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildTrajFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());
      
      if (frame->GetPlotName().IsSameAs(owner.c_str()))
      {
         frame->SetGlCoordSystem(viewCs, viewUpCs);
      }
   }
#endif
   
}


//------------------------------------------------------------------------------
// void SetGlViewOption(const std::string &plotName, SpacePoint *vpRefObj, ...
//------------------------------------------------------------------------------
void PlotInterface::SetGlViewOption(const std::string &plotName,
                                    SpacePoint *vpRefObj, SpacePoint *vpVecObj,
                                    SpacePoint *vdObj, Real vsFactor,
                                    const Rvector3 &vpRefVec, const Rvector3 &vpVec,
                                    const Rvector3 &vdVec, const std::string &upAxis,
                                    bool usevpRefVec, bool usevpVec, bool usevdVec,
                                     bool useFixedFov, Real fov)
{
#if defined __CONSOLE_APP__
   return;
#else
   
   #if DEBUG_PLOTIF_GL
   MessageInterface::ShowMessage
      ("PlotInterface::SetGlViewOption() plotName:%s\n", plotName.c_str());
   #endif
   
   wxString owner = wxString(plotName.c_str());
   MdiChildTrajFrame *frame = NULL;
   
   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildTrajFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());
      
      if (frame->GetPlotName().IsSameAs(owner.c_str()))
      {
         #if DEBUG_PLOTIF_GL
         MessageInterface::ShowMessage
            ("PlotInterface::SetGlViewOption() vpRefObj=%d, vsFactor=%f\n",
             vpRefObj, vsFactor);
         #endif
         
         frame->SetGlViewOption(vpRefObj, vpVecObj, vdObj, vsFactor, vpRefVec,
                                vpVec, vdVec, upAxis, usevpRefVec,usevpVec,
                                usevdVec, useFixedFov, fov);
      }
   }
#endif
}


//------------------------------------------------------------------------------
//  bool IsThere(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Checks if OpenGlPlot exist.
 */
//------------------------------------------------------------------------------
bool PlotInterface::IsThere(const std::string &plotName)
{    
#if defined __CONSOLE_APP__
   return true;
#else

   if (GmatAppData::GetMainFrame() != NULL)
   {
      wxString owner = wxString(plotName.c_str());
      
      MdiChildTrajFrame *frame  = NULL;
      for (int i=0; i<MdiGlPlot::numChildren; i++)
      {
         frame = (MdiChildTrajFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());
            
         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            return true;
         }
      }
   }

   return false;
#endif
}


//------------------------------------------------------------------------------
//  bool DeleteGlPlot()
//------------------------------------------------------------------------------
/*
 * Deletes OpenGlPlot
 */
//------------------------------------------------------------------------------
bool PlotInterface::DeleteGlPlot()
{    
#if defined __CONSOLE_APP__
   return true;
#else
   
   if (GmatAppData::GetMainFrame() != NULL)
   {
      #if DEBUG_PLOTIF_GL
      MessageInterface::ShowMessage("PlotInterface::DeleteGlPlot() "
                                    "Deleting MdiGlPlot::mdiChildren[]\n");
      #endif
      
      MdiChildTrajFrame *frame;
      for (int i=0; i<MdiGlPlot::numChildren; i++)
      {
         frame = (MdiChildTrajFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());
         frame->DeletePlot();
      }
   }
   
   return true;
#endif
}


//------------------------------------------------------------------------------
//  delete RefreshGlPlot(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Refreshes OpenGlPlot.
 */
//------------------------------------------------------------------------------
bool PlotInterface::RefreshGlPlot(const std::string &plotName)
{    
#if defined __CONSOLE_APP__
   return true;
#else

   if (GmatAppData::GetMainFrame() != NULL)
   {
      #if DEBUG_PLOTIF_GL
         MessageInterface::ShowMessage
            ("PlotInterface::RefreshGlPlot() plotName=%s\n",plotName.c_str());
      #endif
      wxString owner = wxString(plotName.c_str());

      MdiChildTrajFrame *frame = NULL;
      for (int i=0; i<MdiGlPlot::numChildren; i++)
      {
         frame = (MdiChildTrajFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());
            
         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            frame->RefreshPlot();
         }
      }
   }
   
   return true;
#endif
}


//------------------------------------------------------------------------------
//  bool UpdateGlPlot(const std::string &plotName, ...
//------------------------------------------------------------------------------
/*
 * Buffers data and updates OpenGL plow window if updateCanvas is true
 */
//------------------------------------------------------------------------------
bool PlotInterface::UpdateGlPlot(const std::string &plotName,
                                 const std::string &oldName,
                                 const std::string &csName,
                                 const StringArray &scNames,
                                 const Real &time, const RealArray &posX,
                                 const RealArray &posY, const RealArray &posZ,
                                 const UnsignedIntArray &scColors,
                                 bool updateCanvas)
{   
#if defined __CONSOLE_APP__
   return true;
#else

   #if DEBUG_PLOTIF_GL_UPDATE
   MessageInterface::ShowMessage
      ("PlotInterface::UpdateGlPlot() entered. time = %f\n", time);
   #endif
   
   bool updated = false;
   wxString owner = wxString(plotName.c_str());

   //loj: 4/21/05 This code not needed. new OpenGL window is created from OpenGlPlot
   //    if (GmatAppData::GetMainFrame() == NULL)
   //    {
   //       if (!CreateGlPlotWindow(plotName, oldName, csName))
   //          return false;
   //    }
   
   MdiChildTrajFrame *frame = NULL;
   
   for (int i=0; i<MdiGlPlot::numChildren; i++)
   {
      frame = (MdiChildTrajFrame*)(MdiGlPlot::mdiChildren.Item(i)->GetData());
      
      #if DEBUG_PLOTIF_GL_UPDATE
      MessageInterface::ShowMessage
         ("PlotInterface::UpdateGlPlot() frame[%d]->GetPlotName()=%s "
          "owner=%s\n", i, frame->GetPlotName().c_str(), owner.c_str());
      #endif
      
      if (frame->GetPlotName().IsSameAs(owner.c_str()))
      {
         
         //MessageInterface::ShowMessage
         //   ("PlotInterface::UpdateGlPlot() now updating GL plot...\n");
         
         frame->UpdatePlot(scNames, time, posX, posY, posZ, scColors,
                           updateCanvas);

         updated = true;
      }
   }

   return updated;
   
#endif
} // end UpdateGlPlot()


//------------------------------------------------------------------------------
//  bool CreateXyPlotWindow(const std::string &plotName,
//                          const std::string &oldName,
//                          const std::string &plotTitle,
//                          const std::string &xAxisTitle,
//                          const std::string &yAxisTitle,
//                          bool drawGrid = false)
//------------------------------------------------------------------------------
/*
 * Creates XyPlot window.
 *
 * @param <plotName> name of plot
 */
//------------------------------------------------------------------------------
bool PlotInterface::CreateXyPlotWindow(const std::string &plotName,
                                       const std::string &oldName,
                                       const std::string &plotTitle,
                                       const std::string &xAxisTitle,
                                       const std::string &yAxisTitle,
                                       bool drawGrid)
{    
#if defined __CONSOLE_APP__
   return true;
#else

   //-------------------------------------------------------
   // check if new MDI child frame needed
   //-------------------------------------------------------
   bool createNewFrame = true;
   wxString currPlotName;
   MdiChildXyFrame *currPlotFrame = NULL;
   
   for (int i=0; i<MdiXyPlot::numChildren; i++)
   {
      currPlotFrame = (MdiChildXyFrame*)(MdiXyPlot::mdiChildren.Item(i)->GetData());
      currPlotName = currPlotFrame->GetPlotName();

      if (currPlotName.IsSameAs(plotName.c_str()))
      {
         createNewFrame = false;
         break;
      }
      else if (currPlotName.IsSameAs(oldName.c_str()))
      {
         #if DEBUG_RENAME
         MessageInterface::ShowMessage
            ("PlotInterface::CreateXyPlotWindow() currPlotName=%s, oldName=%s\n",
             currPlotName.c_str(), oldName.c_str());
         #endif
         
         // change plot name
//          ((MdiChildXyFrame*)MdiXyPlot::mdiChildren[i])-> //loj: 5/19/05
//             SetPlotName(wxString(plotName.c_str()));
         currPlotFrame->SetPlotName(wxString(plotName.c_str()));
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
         ("PlotInterface::CreateXyPlotWindow() Creating new "
          "MdiChildXyFrame\n X Axis Title = %s  Y Axis Title = %s\n",
          xAxisTitle.c_str(), yAxisTitle.c_str());
      #endif
      
      // create a frame, containing a XY plot canvas
      currPlotFrame =
         new MdiChildXyFrame(GmatAppData::GetMainFrame(), true,
                             wxString(plotName.c_str()),
                             wxString(plotTitle.c_str()),
                             wxString(xAxisTitle.c_str()),
                             wxString(yAxisTitle.c_str()),
                             wxPoint(-1, -1), wxSize(500, 350),
                             wxDEFAULT_FRAME_STYLE);
                             
      currPlotFrame->Show();
      
      GmatAppData::GetMainFrame()->Tile(); //loj: 5/18/05 uncommented

      ++MdiXyPlot::numChildren;

//       GmatAppData::GetMainFrame()->xyMainSubframe->RedrawCurve();
      currPlotFrame->RedrawCurve();
   }

//    GmatAppData::GetMainFrame()->xyMainSubframe->SetShowGrid(drawGrid);
//    GmatAppData::GetMainFrame()->xyMainSubframe->ResetZoom();
   currPlotFrame->SetShowGrid(drawGrid);
   currPlotFrame->ResetZoom();
   
   return true;
#endif
}

//------------------------------------------------------------------------------
//  bool DeleteXyPlot(bool hideFrame)
//------------------------------------------------------------------------------
/*
 * Deletes XyPlot.
 *
 */
//------------------------------------------------------------------------------
bool PlotInterface::DeleteXyPlot(bool hideFrame)
{    
#if defined __CONSOLE_APP__
   return true;
#else
   
   if (GmatAppData::GetMainFrame() != NULL)
   {
      #if DEBUG_PLOTIF_XY
      MessageInterface::ShowMessage("PlotInterface::DeleteXyPlot()\n");
      #endif

      MdiChildXyFrame *frame = NULL;
      for (int i=0; i<MdiXyPlot::numChildren; i++)
      {
         frame = (MdiChildXyFrame*)(MdiXyPlot::mdiChildren.Item(i)->GetData());
         frame->DeletePlot();
      }
   }
   
   return true;
#endif
}

//------------------------------------------------------------------------------
// bool AddXyPlotCurve(const std::string &plotName, int curveIndex,
//                     int yOffset, Real yMin, Real yMax,
//                     const std::string &curveTitle,
//                     UnsignedInt penColor)
//------------------------------------------------------------------------------
/*
 * Adds a plot curve to XY plow window.
 */
//------------------------------------------------------------------------------
bool PlotInterface::AddXyPlotCurve(const std::string &plotName, int curveIndex,
                                   int yOffset, Real yMin, Real yMax,
                                   const std::string &curveTitle,
                                   UnsignedInt penColor)
{
#if defined __CONSOLE_APP__
   return true;
#else

   bool added = false;
   
   #if DEBUG_PLOTIF_XY
   MessageInterface::ShowMessage
      ("PlotInterface::AddXyPlotCurve() entered."
       " plotName = " + plotName + " curveTitle = " + 
       curveTitle + "\n");
   
   MessageInterface::ShowMessage
      ("PlotInterface::AddXyPlotCurve() numChildren = %d\n",
       MdiXyPlot::numChildren);
   #endif
   
   MdiChildXyFrame *frame = NULL;
   for (int i=0; i<MdiXyPlot::numChildren; i++)
   {
      frame = (MdiChildXyFrame*)(MdiXyPlot::mdiChildren.Item(i)->GetData());
        
      //if (frame->GetPlotName() == wxString(plotName.c_str()))
      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->AddPlotCurve(curveIndex, yOffset, yMin, yMax,
                             wxString(curveTitle.c_str()), penColor);
         added = true;
      }
   }

   return added;
#endif
}

//------------------------------------------------------------------------------
// bool DeleteAllXyPlotCurves(const std::string &plotName,
//                            const std::string &oldName)
//------------------------------------------------------------------------------
/*
 * Deletes all plot curves in XY plow window.
 */
//------------------------------------------------------------------------------
bool PlotInterface::DeleteAllXyPlotCurves(const std::string &plotName,
                                          const std::string &oldName)
{
#if defined __CONSOLE_APP__
   return true;
#else
   
   #if DEBUG_PLOTIF_XY
   MessageInterface::ShowMessage
      ("PlotInterface::DeleteAllPlotCurve() plotName = %s "
       "numChildren = %d\n", plotName.c_str(),
       MdiXyPlot::numChildren);
   #endif

   MdiChildXyFrame *frame = NULL;
   for (int i=0; i<MdiXyPlot::numChildren; i++)
   {
      frame = (MdiChildXyFrame*)(MdiXyPlot::mdiChildren.Item(i)->GetData());
      if (frame->GetPlotName().IsSameAs(plotName.c_str()) ||
          frame->GetPlotName().IsSameAs(oldName.c_str()))
      {
         frame->DeleteAllPlotCurves();
      }
   }

   return true;
#endif
}


//------------------------------------------------------------------------------
// bool DeleteXyPlotCurve(const std::string &plotName, int curveIndex)
//------------------------------------------------------------------------------
/*
 * Deletes a plot curve to XY plow window.
 */
//------------------------------------------------------------------------------
bool PlotInterface::DeleteXyPlotCurve(const std::string &plotName, int curveIndex)
{
#if defined __CONSOLE_APP__
   return true;
#else
   
   #if DEBUG_PLOTIF_XY
      MessageInterface::ShowMessage
         ("PlotInterface::DeleteXyPlotCurve() entered plotName = %s "
          "curveIndex = %d\n", plotName.c_str(), curveIndex);
    
      MessageInterface::ShowMessage
         ("PlotInterface::DeleteXyPlotCurve() numChildren = %d\n",
          MdiXyPlot::numChildren);
   #endif

      MdiChildXyFrame *frame = NULL;
   for (int i=0; i<MdiXyPlot::numChildren; i++)
   {
      frame = (MdiChildXyFrame*)(MdiXyPlot::mdiChildren.Item(i)->GetData());
      
      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->DeletePlotCurve(curveIndex);
      }
   }

   return true;
#endif
}


//------------------------------------------------------------------------------
// void ClearXyPlotData(const std::string &plotName))
//------------------------------------------------------------------------------
void PlotInterface::ClearXyPlotData(const std::string &plotName)
{
#if defined __CONSOLE_APP__
   return;
#else

   MdiChildXyFrame *frame = NULL;
   for (int i=0; i<MdiXyPlot::numChildren; i++)
   {
      frame = (MdiChildXyFrame*)(MdiXyPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         frame->ClearPlotData();
      }
   }
#endif
}

//------------------------------------------------------------------------------
// void SetXyPlotTitle(const std::string &plotName, const std::string &plotTitle)
//------------------------------------------------------------------------------
void PlotInterface::SetXyPlotTitle(const std::string &plotName,
                                   const std::string &plotTitle)
{
#if defined __CONSOLE_APP__
   return;
#else
   
   #if DEBUG_PLOTIF_XY
   MessageInterface::ShowMessage
      ("PlotInterface::SetXyPlotTitle() plotName = %s "
       "plotTitle = %s\n", plotName.c_str(), plotTitle.c_str());
   #endif
   
   MdiChildXyFrame *frame = NULL;
   for (int i=0; i<MdiXyPlot::numChildren; i++)
   {
      frame = (MdiChildXyFrame*)(MdiXyPlot::mdiChildren.Item(i)->GetData());
      
      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         #if DEBUG_PLOTIF_XY
            MessageInterface::ShowMessage
               ("PlotInterface::SetXyPlotTitle() calling "
                " frame->SetPlotTitle() \n");
         #endif
            
         frame->SetPlotTitle(wxString(plotTitle.c_str()));
      }
   }
#endif
}


//------------------------------------------------------------------------------
// void ShowXyPlotLegend(const std::string &plotName)
//------------------------------------------------------------------------------
void PlotInterface::ShowXyPlotLegend(const std::string &plotName)
{
#if defined __CONSOLE_APP__
   return;
#else
   
   MdiChildXyFrame *frame = NULL;
   for (int i=0; i<MdiXyPlot::numChildren; i++)
   {
      frame = (MdiChildXyFrame*)(MdiXyPlot::mdiChildren.Item(i)->GetData());

      if (frame->GetPlotName().IsSameAs(plotName.c_str()))
      {
         #if DEBUG_PLOTIF_XY
            MessageInterface::ShowMessage
               ("PlotInterface::ShowXyPlotLegend() calling  frame->ShowPlotLegend() \n");
         #endif
            
         frame->ShowPlotLegend();
      }
   }
#endif
}


//------------------------------------------------------------------------------
// bool RefreshXyPlot(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Refreshes XY plot curve.
 *
 * @param <plotName> name of xy plot
 */
//------------------------------------------------------------------------------
bool PlotInterface::RefreshXyPlot(const std::string &plotName)
{
#if defined __CONSOLE_APP__
   return true;
#else

   if (GmatAppData::GetMainFrame() != NULL)
   {        
      #if DEBUG_PLOTIF_XY_UPDATE
         MessageInterface::ShowMessage
            ("PlotInterface::RefreshXyPlot() plotName=%s, numChildren=%d\n",
             plotName.c_str(), MdiXyPlot::numChildren);
      #endif
      
      wxString owner = wxString(plotName.c_str());

      MdiChildXyFrame *frame = NULL;
      for (int i=0; i<MdiXyPlot::numChildren; i++)
      {
         frame = (MdiChildXyFrame*)(MdiXyPlot::mdiChildren.Item(i)->GetData());
         
         if (frame->GetPlotName().IsSameAs(owner.c_str()))
         {
            frame->RedrawCurve();
            //loj: 8/17/04 commented out so that MainFrame gets user input focus
            // during the run
            //frame->SetFocus();
         }
      }
   }
   
   return true;
#endif
}


//------------------------------------------------------------------------------
// bool UpdateXyPlot(const std::string &plotName, const std::string &oldName,
//                   const Real &xval, const Rvector &yvals,
//                   const std::string &plotTitle,
//                   const std::string &xAxisTitle,
//                   const std::string &yAxisTitle, bool updateCanvas,
//                   bool drawGrid)
//------------------------------------------------------------------------------
/*
 * Updates XY plot curve.
 *
 * @param <plotName> name of xy plot
 * @param <xval> x value
 * @param <yvals> y values, should be in the order of curve added
 */
//------------------------------------------------------------------------------
bool PlotInterface::UpdateXyPlot(const std::string &plotName,
                                 const std::string &oldName,
                                 const Real &xval, const Rvector &yvals,
                                 const std::string &plotTitle,
                                 const std::string &xAxisTitle,
                                 const std::string &yAxisTitle,
                                 bool updateCanvas, bool drawGrid)
{
#if defined __CONSOLE_APP__
   return true;
#else

   bool updated = false;
   wxString owner = wxString(plotName.c_str());

   //loj: 5/18/05 This code is not needed.
//    if (GmatAppData::GetMainFrame() == NULL)
//    {        
//       CreateXyPlotWindow(plotName, oldName, plotTitle, xAxisTitle, yAxisTitle,
//                          drawGrid);
//       #if DEBUG_PLOTIF_XY_UPDATE
//          MessageInterface::ShowMessage
//             ("PlotInterface::UpdateXyPlot()" + plotName + " " +
//              plotTitle + " " + xAxisTitle + " " + yAxisTitle + "\n");
//       #endif
//    }
        
   #if DEBUG_PLOTIF_XY_UPDATE
   MessageInterface::ShowMessage
      ("PlotInterface::UpdateXyPlot() numChildren = %d\n",
       MdiXyPlot::numChildren);
   #endif
   
   MdiChildXyFrame *frame = NULL;
   for (int i=0; i<MdiXyPlot::numChildren; i++)
   {
      frame = (MdiChildXyFrame*)(MdiXyPlot::mdiChildren.Item(i)->GetData());
      
      if (frame->GetPlotName().IsSameAs(owner.c_str()))
      {
         int numCurves = frame->GetCurveCount();
            
         #if DEBUG_PLOTIF_XY_UPDATE
            MessageInterface::ShowMessage
               ("PlotInterface::UpdateXyPlot() numCurves = %d\n", numCurves);
         #endif
            
         for (int j=0; j<numCurves; j++)
         {
            #if DEBUG_PLOTIF_XY_UPDATE
               MessageInterface::ShowMessage
                  ("PlotInterface::UpdateXyPlot() yvals[%d] = %f\n", j, yvals(j));
            #endif
               
            frame->AddDataPoints(j, xval, yvals(j));
         }

         if (updateCanvas)
            frame->RedrawCurve();
            
         updated = true;
      }
   }
   
   return updated;
#endif
}
