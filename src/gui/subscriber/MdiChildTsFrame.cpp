//$Id$
//------------------------------------------------------------------------------
//                              MdiChildTsFrame
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// ** Legal **
//
// Author: Linda Jun
// Created: 2004/01/20
/**
 * Implements MdiChildXyFrame class for xy plot.
 */
//------------------------------------------------------------------------------
#include "gmatwxrcs.hpp"
#include "MdiChildTsFrame.hpp"

#include "MdiTsPlotData.hpp"
#include "TsPlotCurve.hpp"
#include "TsPlotXYCanvas.hpp"
#include "GmatAppData.hpp"
#include "RealUtilities.hpp" // for Abs(), Min(), Max()
#include <fstream>           // for ifstream (plot input file)

#include "wx/image.h"
#include "wx/listctrl.h"
#include "wx/sizer.h"
#include "wx/log.h"
#include "wx/intl.h"
#include "wx/gdicmn.h"       // for color

#include "RgbColor.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_MDI_TS_FRAME
//#define DEBUG_RENAME

BEGIN_EVENT_TABLE(MdiChildTsFrame, GmatMdiChildFrame)
   EVT_MENU(GmatPlot::MDI_TS_OPEN_PLOT_FILE, MdiChildTsFrame::OnOpenXyPlotFile)
   EVT_MENU(GmatPlot::MDI_TS_CHILD_QUIT, MdiChildTsFrame::OnQuit)
   EVT_MENU(GmatPlot::MDI_TS_CHANGE_TITLE, MdiChildTsFrame::OnChangeTitle)
   EVT_MENU(GmatPlot::MDI_TS_CLEAR_PLOT, MdiChildTsFrame::OnClearPlot)
   EVT_MENU(GmatPlot::MDI_TS_SHOW_DEFAULT_VIEW, MdiChildTsFrame::OnShowDefaultView)
   EVT_MENU(GmatPlot::MDI_TS_DRAW_GRID, MdiChildTsFrame::OnDrawGrid)
   EVT_MENU(GmatPlot::MDI_TS_DRAW_DOTTED_LINE, MdiChildTsFrame::OnDrawDottedLine)
   EVT_MENU(GmatPlot::MDI_TS_HELP_VIEW, MdiChildTsFrame::OnHelpView)

//   EVT_PLOT_CLICKED(-1, MdiChildTsFrame::OnPlotClick)
   EVT_ACTIVATE(MdiChildTsFrame::OnActivate)
   EVT_SIZE(MdiChildTsFrame::OnSize)
   EVT_MOVE(MdiChildTsFrame::OnMove)
   EVT_CLOSE(MdiChildTsFrame::OnPlotClose)
END_EVENT_TABLE()

//------------------------------------------------------------------------------
// MdiChildTsFrame(wxMDIParentFrame *parent, bool isMainFrame,
//                 const wxString &plotName, const wxString& plotTitle,
//                 const wxString& xAxisTitle, const wxString& yAxisTitle,
//                 const wxPoint& pos, const wxSize& size, const long style)
//------------------------------------------------------------------------------
MdiChildTsFrame::MdiChildTsFrame(wxMDIParentFrame *parent, bool isMainFrame,
                                 const wxString &plotName, const wxString& plotTitle,
                                 const wxString& xAxisTitle, const wxString& yAxisTitle,
                                 const wxPoint& pos, const wxSize& size, const long style)
   : GmatMdiChildFrame(parent, plotName, plotTitle, GmatTree::OUTPUT_XY_PLOT, -1,
                       pos, size, style | wxNO_FULL_REPAINT_ON_RESIZE)
{
   mXyPlot = (TsPlotCanvas *) NULL;
   mIsMainFrame = isMainFrame;
   mPlotName = plotName;
   mPlotTitle = plotTitle;
   mXAxisTitle = xAxisTitle;
   mYAxisTitle = yAxisTitle;

   for (int i=0; i<MAX_NUM_CURVE; i++)
      mHasFirstXSet[i] = false;
   
   #ifdef DEBUG_MDI_TS_FRAME
   MessageInterface::ShowMessage
      ("MdiChildTsFrame::MdiChildTsFrame()\n   X Axis Title = '%s'\n"
       "   Y Axis Title = '%s'\n   isMainFrame = %d\n", xAxisTitle.c_str(),
       yAxisTitle.c_str(), isMainFrame);
   #endif
   
   MdiTsPlot::mdiChildren.Append(this);
    
   // Give it an icon
#ifdef __WXMSW__
   SetIcon(wxIcon(_T("chrt_icn")));
#else
   SetIcon(wxIcon( mondrian_xpm ));
#endif

   // Create XyPlotFrame
   int width, height;
   GetClientSize(&width, &height);
   
   #ifdef DEBUG_MDI_TS_FRAME
   MessageInterface::ShowMessage("   Creating TsPlotCanvas\n");
   #endif
   
   TsPlotCanvas *frame =
      new TsPlotXYCanvas(this, -1, wxPoint(0, 0), wxSize(width, height),
                       wxTAB_TRAVERSAL,//wxPLOT_DEFAULT,
                       plotTitle);
   
   mXyPlot = frame;
   
   wxBoxSizer *topSizer = new wxBoxSizer( wxVERTICAL );
   
   topSizer->Add(mXyPlot, 1, wxALIGN_CENTER |wxEXPAND);
   
   SetAutoLayout( TRUE ); //loj: this is called implicitly by SetSizer()
   SetSizer( topSizer );
   
   // this should work for MDI frames as well as for normal ones
   SetSizeHints(100, 100);
   GmatAppData::Instance()->GetMainFrame()->theMdiChildren->Append(this);
   
   isActive = true;


   #ifdef DEBUG_MDI_TS_FRAME
   MessageInterface::ShowMessage("MdiChildTsFrame::MdiChildTsFrame() leaving\n");
   #endif
}


//------------------------------------------------------------------------------
// ~MdiChildTsFrame()
//------------------------------------------------------------------------------
MdiChildTsFrame::~MdiChildTsFrame()
{
   #ifdef DEBUG_MDI_TS_FRAME
   MessageInterface::ShowMessage
      ("~MdiChildTsFrame() mPlotName=%s\n", mPlotName.c_str());
   #endif
   
   MdiTsPlot::mdiChildren.DeleteObject(this);
   MdiTsPlot::numChildren--;
   
   #ifdef DEBUG_MDI_TS_FRAME
   MessageInterface::ShowMessage("~MdiChildTsFrame() exiting\n");
   #endif
}


//------------------------------------------------------------------------------   
// int ReadXyPlotFile(const wxString &filename)
//------------------------------------------------------------------------------   
int MdiChildTsFrame::ReadXyPlotFile(const wxString &filename)
{
   std::ifstream inStream;  // input data stream
   double tempData[7]; //loj: time, x, y, z, vx, vy, vz for build 2
   int numData = 0;
   double startTime;
   
   if (filename != "")
   {       
      inStream.open(filename.c_str());
      if (inStream.is_open())
      {
         TsPlotCurve *xCurve = new TsPlotCurve(0, -40000.0, 40000.0, "Position X");
         TsPlotCurve *yCurve = new TsPlotCurve(0, -40000.0, 40000.0, "Position Y");
         TsPlotCurve *zCurve = new TsPlotCurve(0, -40000.0, 40000.0, "Position Z");

         // read 1st line to get start time
         for (int i=0; i<7; i++)
            inStream >> tempData[i];

         startTime = tempData[0];

         // set time, X, Y, Z
         xCurve->AddData(0.0, tempData[1]); // time, X
         yCurve->AddData(0.0, tempData[2]); // time, Y
         zCurve->AddData(0.0, tempData[3]); // time, Z
         numData++;

         while(!inStream.eof())
         {
            // read time, X, Y, Z, Vx, Vy, Vz
            for (int i=0; i<7; i++)
               inStream >> tempData[i];

            // set time, X, Y, Z
            xCurve->AddData(tempData[0]-startTime, tempData[1]); // time, X
            yCurve->AddData(tempData[0]-startTime, tempData[2]); // time, Y
            zCurve->AddData(tempData[0]-startTime, tempData[3]); // time, Z

            numData++;
         }
      }

      Update();
   }

   return numData;
}

//------------------------------------------------------------------------------
// bool DeletePlot()
//------------------------------------------------------------------------------
bool MdiChildTsFrame::DeletePlot()
{
   MessageInterface::ShowMessage("MdiChildTsFrame::DeletePlot()\n");
   Close(TRUE);

   return true;
}

//------------------------------------------------------------------------------
// void SetPlotTitle(const wxString &title)
//------------------------------------------------------------------------------
void MdiChildTsFrame::SetPlotTitle(const wxString &title)
{
   #ifdef DEBUG_MDI_TS_FRAME
   MessageInterface::ShowMessage("MdiChildTsFrame::SetPlotTitle() title = %s\n",
                                 title.c_str());
   #endif
   
   mPlotTitle = title;
   
   if (mXyPlot)
      mXyPlot->SetLabel(title.c_str(), TsPlotCanvas::PLOT_TITLE);
}

//------------------------------------------------------------------------------
// void ShowPlotLegend()
//------------------------------------------------------------------------------
void MdiChildTsFrame::ShowPlotLegend()
{
   #ifdef DEBUG_MDI_TS_FRAME
      MessageInterface::ShowMessage("MdiChildTsFrame::ShowLegend() entered\n");
   #endif
      
   if (mXyPlot)
      mXyPlot->ShowLegend();
}


//------------------------------------------------------------------------------   
// void AddPlotCurve(int curveIndex, int yOffset, double yMin, double yMax,
//                   const wxString &curveTitle, UnsignedInt penColor)
//------------------------------------------------------------------------------   
void MdiChildTsFrame::AddPlotCurve(int curveIndex, int yOffset, double yMin,
                                   double yMax, const wxString &curveTitle,
                                   UnsignedInt penColor)
{
   #ifdef DEBUG_MDI_TS_FRAME
      MessageInterface::ShowMessage
         ("MdiChildTsFrame::AddPlotCurve() yMin = %f, yMax = %f\n", yMin, yMax);
   #endif
   
   if (mXyPlot != NULL)
   {
      mHasFirstXSet[curveIndex] = false;
      
      // Create XyPlotCurve
      TsPlotCurve *curve = new TsPlotCurve(yOffset, yMin, yMax, curveTitle);
      
      #ifdef DEBUG_MDI_TS_FRAME
         MessageInterface::ShowMessage(
            "MdiChildTsFrame::AddPlotCurve() curve title = %s\n",
            curveTitle.c_str());
      #endif
         
      mXyPlot->AddData(curve, penColor);
      mXyPlot->SetDataName(curveTitle.c_str());
      
      #ifdef DEBUG_MDI_TS_FRAME
            MessageInterface::ShowMessage
               ("MdiChildTsFrame::AddPlotCurve() curve count = %d added\n",
                mXyPlot->GetCurveCount());
      #endif
   }
   else
   {
      MessageInterface::ShowMessage("MdiChildTsFrame::AddPlotCurve() mXyPlot is NULL... \n");
   }
}

//------------------------------------------------------------------------------   
// void DeleteAllPlotCurves()
//------------------------------------------------------------------------------   
void MdiChildTsFrame::DeleteAllPlotCurves()
{
   if (mXyPlot != NULL)
   {
      #ifdef DEBUG_MDI_TS_FRAME
      MessageInterface::ShowMessage
         ("MdiChildTsFrame::DeleteAllPlotCurve() curve count=%d \n",
          mXyPlot->GetCurveCount());
      #endif
      
      while (mXyPlot->GetCurveCount() > 0)
         DeletePlotCurve(0);
   }
   else
   {
      MessageInterface::ShowMessage("MdiChildTsFrame::DeletePlotCurve() mXyPlot is NULL... \n");
   }
}

//------------------------------------------------------------------------------   
// void DeletePlotCurve(int curveIndex)
//------------------------------------------------------------------------------   
void MdiChildTsFrame::DeletePlotCurve(int curveIndex)
{
   #ifdef DEBUG_MDI_TS_FRAME
   MessageInterface::ShowMessage
      ("MdiChildTsFrame::DeletePlotCurve() curveIndex = %d\n", curveIndex);
   #endif
   
   if (mXyPlot != NULL)
   {
      TsPlotCurve* curve = mXyPlot->GetPlotCurve(curveIndex);
      if (curve != NULL)
         mXyPlot->DeletePlotCurve(curveIndex);
      mHasFirstXSet[curveIndex] = false;
   }
   else
   {
      MessageInterface::ShowMessage("MdiChildTsFrame::DeletePlotCurve() mXyPlot is NULL... \n");
   }
}

//------------------------------------------------------------------------------
// void AddDataPoints(int curveIndex, double xData, double yData)
//------------------------------------------------------------------------------
/*
 * Updates XY plot curve.
 *
 * @param <curveIndex> curve number
 * @param <xData> x value to be updated
 * @param <yData> y value to be updated
 */
//------------------------------------------------------------------------------
void MdiChildTsFrame::AddDataPoints(int curveIndex, double xData, double yData)
{
   #ifdef DEBUG_POINT_ADD
      MessageInterface::ShowMessage(
         "MdiChildTsFrame::AddDataPoints(%d, %lf, %lf )\n", curveIndex, xData, 
         yData);
   #endif
   
   if (mXyPlot)
   {
      TsPlotCurve *curve = mXyPlot->GetPlotCurve(curveIndex);
      if (curve)
         curve->AddData(xData, yData);
   } 
}

//------------------------------------------------------------------------------
// void ClearPlotData()
//------------------------------------------------------------------------------
/*
 * Clears XY plot data.
 */
//------------------------------------------------------------------------------
void MdiChildTsFrame::ClearPlotData()
{
   if (mXyPlot)
   {
      mXyPlot->ClearAllCurveData();
   }
}


//------------------------------------------------------------------------------
// void PenUp()
//------------------------------------------------------------------------------
/*
 * Temporarily stops drawing to the plot.
 */
//------------------------------------------------------------------------------
void MdiChildTsFrame::PenUp()
{
   if (mXyPlot)
   {
      mXyPlot->PenUp();
   }
}


//------------------------------------------------------------------------------
// void PenDown()
//------------------------------------------------------------------------------
/*
 * Resumes drawing to the plot.
 */
//------------------------------------------------------------------------------
void MdiChildTsFrame::PenDown()
{
   if (mXyPlot)
   {
      mXyPlot->PenDown();
   }
}


void MdiChildTsFrame::Rescale()
{
   if (mXyPlot)
   {
      mXyPlot->Rescale();
   }
}


bool MdiChildTsFrame::IsActive()
{
   return isActive;
}

void MdiChildTsFrame::IsActive(bool yesno)
{
   isActive = yesno;
}

void MdiChildTsFrame::CurveSettings(const std::string &plotName, bool useLines,
      Integer lineWidth, bool useMarkers, Integer markerSize, Integer forCurve)
{
   if (mXyPlot)
   {
      Integer count = mXyPlot->GetCurveCount();
      if (forCurve == -1)
      {
         for (Integer i = 0; i < count; ++i)
         {
            TsPlotCurve *curve = mXyPlot->GetPlotCurve(i);
            curve->UseLine(useLines);
            curve->SetWidth(lineWidth);
            curve->UseMarker(useMarkers);
            curve->SetMarkerSize(markerSize);
         }
      }
      else
         if (forCurve < count)
         {
            TsPlotCurve *curve = mXyPlot->GetPlotCurve(forCurve);
            curve->UseLine(useLines);
            curve->SetWidth(lineWidth);
            curve->UseMarker(useMarkers);
            curve->SetMarkerSize(markerSize);
         }
   }
}

//------------------------------------------------------------------------------
// void RedrawCurve()
//------------------------------------------------------------------------------
/*
 * Redraws XY plot curve.
 */
//------------------------------------------------------------------------------
void MdiChildTsFrame::RedrawCurve()
{    
   if (mXyPlot)
   {
      Update(); // need Update to show plot as it runs
      
      mXyPlot->DataUpdate();
      wxPaintEvent pvt;
      mXyPlot->OnPaint(pvt);
      mXyPlot->Update();
      
      // On linux, this call gives the GUI a time slice to update the plot
      #ifdef __WXGTK__
         ::wxYield();
      #endif
   }
}

//------------------------------------------------------------------------------
// void SetPlotName(const wxString &name)
//------------------------------------------------------------------------------
void MdiChildTsFrame::SetPlotName(const wxString &name)
{
   #ifdef DEBUG_RENAME
      MessageInterface::ShowMessage("MdiChildTsFrame::SetPlotName() name=%s\n",
                                    name.c_str());
   #endif
   mPlotName = name;
   SetTitle(name);
}

//------------------------------------------------------------------------------
// void SetShowGrid(bool show)
//------------------------------------------------------------------------------
/*
 * Sets show grid menu option
 */
//------------------------------------------------------------------------------
void MdiChildTsFrame::SetShowGrid(bool show)
{
   if (mXyPlot)
   {
      theMenuBar->Check(GmatPlot::MDI_TS_DRAW_GRID, show);
      mXyPlot->ShowGrid(show);
   }
}

//------------------------------------------------------------------------------
// void ResetZoom()
//------------------------------------------------------------------------------
/*
 * Resets plot zoom value
 */
//------------------------------------------------------------------------------
void MdiChildTsFrame::ResetZoom()
{
   if (mXyPlot)
   {
//      mXyPlot->ResetZoom();
   }
}

//------------------------------------------------------------------------------
// void OnQuit(wxCommandEvent& WXUNUSED(event))
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnQuit(wxCommandEvent& WXUNUSED(event))
{
   Close(TRUE);
}

//------------------------------------------------------------------------------
// void OnChangeTitle(wxCommandEvent& WXUNUSED(event))
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnChangeTitle(wxCommandEvent& WXUNUSED(event))
{
   if (mXyPlot)
   {
      //static wxString s_title = _T("Plot Frame");
      wxString oldTitle = _T("Fred"); // mXyPlot->GetPlotTitle();

      wxString newTitle = wxGetTextFromUser(_T("Enter the new title"),
                                         _T(""),
                                         oldTitle,
                                         GetParent()->GetParent());
      if ( !newTitle )
         return;
      
      mXyPlot->SetLabel(newTitle.c_str(), TsPlotCanvas::PLOT_TITLE);
   }
}

//------------------------------------------------------------------------------
// void OnClearPlot(wxCommandEvent& WXUNUSED(event))
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnClearPlot(wxCommandEvent& WXUNUSED(event))
{
//   if (mXyPlot)
//      mXyPlot->ClearPlot();
}

//------------------------------------------------------------------------------
// void OnShowDefaultView(wxCommandEvent& WXUNUSED(event))
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnShowDefaultView(wxCommandEvent& event)
{
//   if (mXyPlot)
//      mXyPlot->ShowDefaultView();
}

//------------------------------------------------------------------------------
// void OnDrawGrid(wxCommandEvent& event)
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnDrawGrid(wxCommandEvent& event)
{
   if (mXyPlot)
   {
      mXyPlot->ShowGrid(event.IsChecked());
   }
}

//------------------------------------------------------------------------------
// void OnDrawDottedLine(wxCommandEvent& WXUNUSED(event))
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnDrawDottedLine(wxCommandEvent& event)
{
//   if (mXyPlot)
//      mXyPlot->DrawDottedLine(event.IsChecked());
}

//------------------------------------------------------------------------------
// void OnHelpView(wxCommandEvent& WXUNUSED(event))
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnHelpView(wxCommandEvent& event)
{
}

////------------------------------------------------------------------------------
//// void OnPlotClick(wxPlotEvent &event)
////------------------------------------------------------------------------------
//void MdiChildTsFrame::OnPlotClick(wxPlotEvent &event)
//{
//   if (mXyPlot)
//   {
//      XyPlotCurve *curve = (XyPlotCurve*)event.GetCurve();
//      double x = (event.GetPosition() * mXyPlot->GetUnitsPerValue()) +
//         curve->GetFirstX();
//      double y = event.GetCurve()->GetY( event.GetPosition() );
//        
//      //MessageInterface::ShowMessage("MdiChildTsFrame::OnPlotClick() xpos = %d  "
//      //                              "firstx = %f  unitspervalue = %g\n", event.GetPosition(),
//      //                              curve->GetFirstX(), mXyPlot->GetUnitsPerValue());
//        
//      wxString info;
//      //info.Printf("%s: %5.3f  %s: %f\n", GetXAxisTitle().c_str(), x,
//      //            curve->GetCurveTitle().c_str(), y);
//      info.Printf("%s: %5.3f  %s: %g\n", GetXAxisTitle().c_str(), x, //loj: 12/10/04 changed to %g
//                  curve->GetCurveTitle().c_str(), y);
//        
//      //loj: 2/26/04 changed to wxLogStatus
//      wxLogStatus(GmatAppData::Instance()->GetMainFrame(), info);
//        
//      //mLogTextCtrl->AppendText(info);
//        
//      //wxLogMessage(GetXAxisTitle() + ": %5.3f, " +
//      //             curve->GetCurveTitle() + ": %f", x, y);
//   }
//}

//------------------------------------------------------------------------------
// void OnActivate(wxActivateEvent& event)
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnActivate(wxActivateEvent& event)
{
   if ( event.GetActive() && mXyPlot )
   {
      mXyPlot->SetFocus();
   }
   
   GmatMdiChildFrame::OnActivate(event);
}

//------------------------------------------------------------------------------
// void OnMove(wxMoveEvent& event)
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnMove(wxMoveEvent& event)
{
   // VZ: here everything is totally wrong under MSW, the positions are
   //     different and both wrong (pos2 is off by 2 pixels for me which seems
   //     to be the width of the MDI canvas border)
    
   //      wxPoint pos1 = event.GetPosition(),
   //              pos2 = GetPosition();
   //      wxLogStatus(MdiXyPlot::MdiChildTsFrame,
   //                  wxT("position from event: (%d, %d), from frame (%d, %d)"),
   //                  pos1.x, pos1.y, pos2.x, pos2.y);

   event.Skip();
}

//------------------------------------------------------------------------------
// void OnSize(wxSizeEvent& event)
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnSize(wxSizeEvent& event)
{
   // VZ: under MSW the size event carries the client size (quite
   //     unexpectedly) *except* for the very first one which has the full
   //     size... what should it really be? TODO: check under wxGTK
    
   //      wxSize size1 = event.GetSize(),
   //             size2 = GetSize(),
   //             size3 = GetClientSize();
   //      wxLogStatus(MdiXyPlot::MdiChildTsFrame,
   //                  wxT("size from event: %dx%d, from frame %dx%d, client %dx%d"),
   //                  size1.x, size1.y, size2.x, size2.y, size3.x, size3.y);

   event.Skip();
}


//------------------------------------------------------------------------------
// void OnPlotClose(wxCloseEvent& event)
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnPlotClose(wxCloseEvent& event)
{
   #ifdef DEBUG_PLOT_CLOSE
   MessageInterface::ShowMessage
      ("MdiChildTsFrame::OnPlotClose() mPlotName='%s' entered\n", mPlotName.c_str());
   #endif
   
   // Add any check before closing
   
   // remove from list of frames but do not delete
   if (GmatAppData::Instance()->GetMainFrame()->RemoveChild(GetName(), mItemType, false))
   {
      event.Skip();
   }
   else
   {
      event.Veto();
      MessageInterface::PopupMessage
         (Gmat::ERROR_, "**** Internal error occurred, Please close from the ToolBar");
   }
   
   #ifdef DEBUG_PLOT_CLOSE
   MessageInterface::ShowMessage
      ("MdiChildTsFrame::OnPlotClose() mPlotName='%s' exiting\n", mPlotName.c_str());
   #endif
}


//------------------------------------------------------------------------------
// void OnClose(wxCloseEvent& event)
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnClose(wxCloseEvent& event)
{
   // Add any check before closing
   
   #ifdef DEBUG_PLOT_CLOSE
   MessageInterface::ShowMessage
      ("MdiChildTsFrame::OnClose() mPlotName='%s' entered\n", mPlotName.c_str());
   #endif
   
   GmatMdiChildFrame::OnClose(event);
   event.Skip();
   
   #ifdef DEBUG_PLOT_CLOSE
   MessageInterface::ShowMessage
      ("MdiChildTsFrame::OnClose() mPlotName='%s' exiting\n", mPlotName.c_str());
   #endif
}


//---------------------------------
// protected methods
//---------------------------------

//------------------------------------------------------------------------------
// void AdjustYScale()
//------------------------------------------------------------------------------
/*
 * Automaticlly adjusts y scale to y minimum and maximum value
 */
//------------------------------------------------------------------------------
void MdiChildTsFrame::AdjustYScale()
{
   using namespace GmatMathUtil;
   
//   double ymin = GetYMin();
//   double ymax = GetYMax();
//   double yMaxScale = Max(Abs(ymin), Abs(ymax));
//   double yMargin = yMaxScale * 0.1;
   
// #ifdef DEBUG_MDI_TS_FRAME
//    MessageInterface::ShowMessage
//       ("MdiChildTsFrame::AdjustYScale() ymin=%f ymax=%f yMaxScale=%f yMargin=%f\n",
//        ymin, ymax, yMaxScale, yMargin);
// #endif

//   for (unsigned int i=0; i<mXyPlot->GetCount(); i++)
//   {
//      mXyPlot->GetAt(i)->SetStartY(-yMaxScale - yMargin);
//      mXyPlot->GetAt(i)->SetEndY(yMaxScale + yMargin);
//   }
}

//------------------------------------------------------------------------------
// double GetYMin()
//------------------------------------------------------------------------------
/*
 * Returns minimum y value of all curves
 */
//------------------------------------------------------------------------------
double MdiChildTsFrame::GetYMin()
{
   double minVal = -123456789.0; //loj: return some other value?
   
   if (mXyPlot)
   {
      std::vector<double> yMinVals;
      std::vector<double>::iterator pos;
   
      #ifdef DEBUG_MDI_TS_FRAME
         MessageInterface::ShowMessage
            ("MdiChildTsFrame::GetYMin() yMinVals.size()=%d\n",
             yMinVals.size());;
      #endif

      if (yMinVals.size() == 1)
      {
         minVal = yMinVals[0];
      }
      else if (yMinVals.size() >= 2)
      {
         pos = min_element(yMinVals.begin(), yMinVals.end());
         minVal = *pos;
      }
   }

   return minVal;
}

//------------------------------------------------------------------------------
// double GetYMax()
//------------------------------------------------------------------------------
/*
 * Returns minimum y value of all curves
 */
//------------------------------------------------------------------------------
double MdiChildTsFrame::GetYMax()
{
   double maxVal = 123456789.0; //loj: return some other value?
   
   if (mXyPlot)
   {
      std::vector<double> yMaxVals;
      std::vector<double>::iterator pos;
      
      #ifdef DEBUG_MDI_TS_FRAME
         MessageInterface::ShowMessage
            ("MdiChildTsFrame::GetYMax() yMaxVals.size()=%d\n",
            yMaxVals.size());;
      #endif
      
      if (yMaxVals.size() == 1)
      {
         maxVal = yMaxVals[0];
      }
      else if (yMaxVals.size() >= 2)
      {
         pos = max_element(yMaxVals.begin(), yMaxVals.end());
         maxVal = *pos;
      }
   }

   return maxVal;
}

//------------------------------------------------------------------------------
// void OnOpenXyPlotFile(wxCommandEvent& WXUNUSED(event) )
//------------------------------------------------------------------------------
void MdiChildTsFrame::OnOpenXyPlotFile(wxCommandEvent& WXUNUSED(event) )
{
   wxFileDialog fileDialog(this, _T("Open Text XY Plot File"),
                           _T(""), _T(""), _T("text XY Plot file (*.txt)|*.txt"));
   
   fileDialog.SetDirectory(wxGetCwd());
   GmatAppData *gmatAppData = GmatAppData::Instance();
   
   if (fileDialog.ShowModal() == wxID_OK)
   {
      wxString xyPlotFileName = fileDialog.GetPath();
      
      ++MdiTsPlot::numChildren;
      gmatAppData->GetMainFrame()->tsSubframe->SetPlotName("XYPlotFile" +
                                                           MdiTsPlot::numChildren);
      gmatAppData->GetMainFrame()->tsSubframe->SetTitle(xyPlotFileName);
      
      //-----------------------------------
      // Read text XY Plot file
      //-----------------------------------
      int dataPoints =
         gmatAppData->GetMainFrame()->tsSubframe->ReadXyPlotFile(xyPlotFileName);
      
      if (dataPoints > 0)
      {
         gmatAppData->GetMainFrame()->tsSubframe->Show(TRUE);
         wxLogStatus(gmatAppData->GetMainFrame(),
                     wxT("Number of lines read: %d"), dataPoints);
      }
   }
}
