//$Id$
//------------------------------------------------------------------------------
//                              TsPlotCurve
//------------------------------------------------------------------------------
//
// Author: Darrel Conway, Thinking Systems, Inc.
// Created: 2005/05/20
//
// This code is Copyright (c) 2005, Thinking Systems, Inc.
// Licensed under the Academic Free License version 3.0.
/**
 * Implements the TsPlotCurves used on TsPlotCanvas's.
 */
//------------------------------------------------------------------------------
 

#include "TsPlotCurve.hpp"
//#include "LinearInterpolator.hpp"
#include "MessageInterface.hpp"


// #define DEBUG_PENUP_PENDOWN
// #define TEST_POINT_22  // Tests curve change features starting at point 22

//------------------------------------------------------------------------------
// TsPlotCurve(int offsetY, double startY, double endY, double defaultY)
//------------------------------------------------------------------------------
TsPlotCurve::TsPlotCurve(int offsetY, double startY, double endY,
                         const wxString &curveTitle) :
   minX              (1e99),
   maxX              (-1e99),
   minY              (1e99),
   maxY              (-1e99),
   rangeChanged      (false),
   domainChanged     (false),
   useLines          (true),
   useMarkers        (false),
   currentMarkerStyle(unsetMarker),
   markerSize        (3),
   lineWidth         (1),
   lineStyle         (wxSOLID),
   penIsDown         (true),
   lastPointPlotted  (0)
{
//   mInterp = new LinearInterpolator();
   ordinate.clear();
   abscissa.clear();
}


//------------------------------------------------------------------------------
// ~TsPlotCurve()
//------------------------------------------------------------------------------
TsPlotCurve::~TsPlotCurve()
{
   ordinate.clear();
   abscissa.clear();

//   delete mInterp;
}


//------------------------------------------------------------------------------
// void AddData(double x, double y)
//------------------------------------------------------------------------------
void TsPlotCurve::AddData(double x, double y)
{
   #ifdef DEBUG_PENUP_PENDOWN
      static Integer counter = 0;
      if (abscissa.size() == 0) 
         counter = 0;
         
      if (abscissa.size() == 80 && counter == 0)
      {
         counter = 1;
         MessageInterface::ShowMessage("Pen up\n");
         PenUp();
      }
      if (counter == 1) 
      {
         ++counter;
         MessageInterface::ShowMessage("Pen down\n");
         PenDown();
      }
   #endif
   
   if (penIsDown)
   {
      if (abscissa.size() == 0) 
      {
         #ifdef DEBUG_FIRST_POINT
            MessageInterface::ShowMessage("Adding initial data: [%lf, %lf]\n", 
               x, y);
         #endif
         
         minX = maxX = x;
         minY = maxY = y;
         rangeChanged = true;
         domainChanged = true;
      }
         
      abscissa.push_back(x);
      ordinate.push_back(y);
      
   
      if (x < minX)
      {
         minX = x;
         domainChanged = true;
      }
      if (x > maxX)
      {
         maxX = x;
         domainChanged = true;
      }
      if (y < minY)
      {
         minY = y;
         rangeChanged = true;
      }
      if (y > maxY)
      {
         maxY = y;
         rangeChanged = true;
      }
   
      #if DEBUG_XY_PLOT_CURVE_ADD
         MessageInterface::ShowMessage
            ("TsPlotCurve::AddData() size = %d, x = %lf, y = %lf\n",
             abscissa.size(), abscissa.back(), ordinate.back());
      #endif
   }
}

////------------------------------------------------------------------------------
//// void SetInterpolator(Interpolator *interp)
////------------------------------------------------------------------------------
//void TsPlotCurve::SetInterpolator(Interpolator *interp)
//{
//   if (mInterp == NULL)
//      mInterp = interp;
//}


//------------------------------------------------------------------------------
// double GetStartX()
//------------------------------------------------------------------------------
double TsPlotCurve::GetMinX()
{
      return minX;
}

//------------------------------------------------------------------------------
// double GetEndX()
//------------------------------------------------------------------------------
double TsPlotCurve::GetMaxX()
{
      return maxX;
}


//------------------------------------------------------------------------------
// virtual double GetY(wxInt32 x)
//------------------------------------------------------------------------------
double TsPlotCurve::GetY(double x)
{
   double yVal = 0.0;
   unsigned int i;
    
   for (i=0; i < abscissa.size(); i++)
   {
      if (abscissa[i] == x)
      {
         yVal = ordinate[i];
         break;
      }
      else if (abscissa[i] > x)
      {
         if (i == 0)
            ++i;
         yVal = ordinate[i-1];
         // interpolate
//         mInterp->Clear();
//         mInterp->AddPoint(mXdata[i-1], &mYdata[i-1]);
//         mInterp->AddPoint(mXdata[i], &mYdata[i]);
//         mInterp->Interpolate(x, &yVal);
         break;
      }
   }
    
   #if DEBUG_XY_PLOT_CURVE_GET
      MessageInterface::ShowMessage
         ("TsPlotCurve::GetY() size = %d, x = %lf, y = %lf\n",
          abscissa.size(), x, yVal);
   #endif
   
   return yVal;
}


//------------------------------------------------------------------------------
// virtual void Clear()
//------------------------------------------------------------------------------
void TsPlotCurve::Clear()
{
   abscissa.clear();
   ordinate.clear();
   penUpIndex.clear();
   colorIndex.clear();
   markerIndex.clear();

   highlightIndex.clear();


   wxColour def = linecolor[0];
   linecolor.clear();
   linecolor.push_back(def);
   colorIndex.push_back(0);

   #ifdef TEST_POINT_22
      // Go cyan from point 22 to 32
      SetColour(*wxCYAN, 22);
      SetColour(def, 32);
      // Mark these points as a check
      HighlightPoint(22);
      HighlightPoint(32);
   #endif

   currentMarkerStyle = markerStyles[0];
   markerStyles.clear();
   markerStyles.push_back((MarkerType)currentMarkerStyle);
   markerIndex.push_back(0);

   #ifdef TEST_POINT_22
      // Make an x from 22 - 27
      markerStyles.push_back(xMarker);
      markerIndex.push_back(22);
      markerStyles.push_back((MarkerType)currentMarkerStyle);
      markerIndex.push_back(27);
   #endif
}


//------------------------------------------------------------------------------
// virtual double GetYMin()
//------------------------------------------------------------------------------
double TsPlotCurve::GetMinY()
{
   return minY;
}


//------------------------------------------------------------------------------
// virtual double GetYMax()
//------------------------------------------------------------------------------
double TsPlotCurve::GetMaxY()
{
#if DEBUG_XY_PLOT_CURVE_MAX
   MessageInterface::ShowMessage
      ("TsPlotCurve::GetYMax() size = %d, max=%f\n",
       ordinate.size(), maxY);
#endif
   
   return maxY;
}


//------------------------------------------------------------------------------
// void PenUp()
//------------------------------------------------------------------------------
void TsPlotCurve::PenUp()
{
   penUpIndex.push_back((int)abscissa.size() - 1);
   penIsDown = false;
}

//------------------------------------------------------------------------------
// void PenDown()
//------------------------------------------------------------------------------
void TsPlotCurve::PenDown()
{
   penIsDown = true;
}


//------------------------------------------------------------------------------
// const std::vector<int>* GetPenUpLocations()
//------------------------------------------------------------------------------
const std::vector<int>* TsPlotCurve::GetPenUpLocations()
{
   return &penUpIndex;
}

const std::vector<int>* TsPlotCurve::GetColorChangeLocations()
{
   return &colorIndex;
}

const std::vector<int>* TsPlotCurve::GetMarkerChangeLocations()
{
   return &markerIndex;
}

const std::vector<int>* TsPlotCurve::GetHighlightPoints()
{
   return &highlightIndex;
}

void TsPlotCurve::Rescale()
{
   if (abscissa.size() > 0)
   {
      minX = maxX = abscissa[0];
      minY = maxY = ordinate[0];

      for (unsigned int i=1; i < abscissa.size(); i++)
      {
         if (abscissa[i] < minX)
            minX = abscissa[i];
         if (abscissa[i] > maxX)
            maxX = abscissa[i];
         if (ordinate[i] < minY)
            minY = ordinate[i];
         if (ordinate[i] > maxY)
            maxY = ordinate[i];
      }
   }
}


void TsPlotCurve::SetColour(wxColour rgb, int where)
{
   if (where == -1)
      where = abscissa.size();
   if ((where != 0) || (linecolor.size() == 0))
   {
      linecolor.push_back(rgb);
      colorIndex.push_back(where);
   }
   else
   {
      linecolor[0] = rgb;
   }
}


wxColour TsPlotCurve::GetColour(int whichOne)
{
   if (((int)(linecolor.size()) > whichOne) && (whichOne >= 0))
   {
      wxColour thisOne = linecolor[whichOne];
   }
//   else
//   {
//      MessageInterface::ShowMessage("Getting color %d unavailable on %p\n",
//            whichOne, this);
//      MessageInterface::ShowMessage("   linecolor size = %d\n", linecolor.size());
//   }

   if (linecolor.size() == 0)
      return *wxRED;

   if (whichOne == 0)
      return linecolor[0];

   if (whichOne < (int)linecolor.size())
      return linecolor[whichOne];

   return linecolor[0];
}


void TsPlotCurve::SetWidth(int w)
{
   lineWidth = w;
}

int TsPlotCurve::GetWidth()
{
   return lineWidth;
}


/**
 * Line styles defined in wxWidgets
 *    wxSOLID  Solid style.
 *    wxTRANSPARENT  No pen is used.
 *    wxDOT    Dotted style.
 *    wxLONG_DASH    Long dashed style.
 *    wxSHORT_DASH   Short dashed style.
 *    wxDOT_DASH  Dot and dash style.
 *    wxSTIPPLE   Use the stipple bitmap.
 *    wxUSER_DASH    Use the user dashes: see wxPen::SetDashes.
 */
void TsPlotCurve::SetStyle(int ls)
{
   lineStyle = ls;
}


int TsPlotCurve::GetStyle()
{
   return lineStyle;
}

void TsPlotCurve::HighlightPoint(int index)
{
   if (index == -1)
      index = abscissa.size();
   highlightIndex.push_back(index);
}


bool TsPlotCurve::UseLine()
{
   return useLines;
}

bool TsPlotCurve::UseLine(bool tf)
{
   return useLines = tf;
}


bool TsPlotCurve::UseMarker()
{
   return useMarkers;
}


bool TsPlotCurve::UseMarker(bool tf)
{
   return useMarkers = tf;
}


int TsPlotCurve::GetMarker(int whichOne)
{
   if (markerStyles.size() == 0)
      return unsetMarker;

   if (whichOne == 0)
      return markerStyles[0];

   if (whichOne < (int)markerStyles.size())
      currentMarkerStyle = markerStyles[whichOne];
   return currentMarkerStyle;
}

void TsPlotCurve::SetMarker(MarkerType newType, int where)
{
   if (where > 0)
   {
      markerIndex.push_back(where);
      markerStyles.push_back(newType);
   }
   else
   {
      if (markerIndex.size() == 0)
      {
         markerIndex.push_back(0);
         markerStyles.push_back(newType);
      }
      else
      {
         markerIndex[0]  = 0;
         markerStyles[0] = newType;
      }
   }
}

int TsPlotCurve::GetMarkerSize()
{
   return markerSize;
}

void TsPlotCurve::SetMarkerSize(int newSize)
{
   if (newSize > 0)
      markerSize = newSize;

   if (newSize > 12)
      markerSize = 12;
}
