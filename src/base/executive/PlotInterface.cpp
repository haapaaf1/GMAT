//$Id$
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

#include "PlotInterface.hpp"
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
PlotReceiver *PlotInterface::thePlotReceiver = NULL;

//---------------------------------
//  public functions
//---------------------------------


//------------------------------------------------------------------------------
// void SetPlotReceiver(PlotReceiver *pr)
//------------------------------------------------------------------------------
void PlotInterface::SetPlotReceiver(PlotReceiver *pr)
{
   thePlotReceiver = pr;
}


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
bool PlotInterface::CreateGlPlotWindow(const std::string &plotName,
                                       const std::string &oldName,
                                       bool drawEcPlane, bool drawXyPlane,
                                       bool drawWireFrame, bool drawAxes,
                                       bool drawGrid, bool drawSunLine,
                                       bool overlapPlot, bool usevpInfo, bool usepm,
                                       Integer numPtsToRedraw)
{
   #if DEBUG_PLOTIF_GL_CREATE
   MessageInterface::ShowMessage
      ("PI::CreateGlPlotWindow() %s entered, thePlotReceiver=<%p>\n", plotName.c_str(),
       thePlotReceiver);
   #endif
   
   if (thePlotReceiver != NULL)
      return thePlotReceiver->CreateGlPlotWindow(plotName, oldName, drawEcPlane, 
                   drawXyPlane, drawWireFrame, drawAxes, drawGrid, drawSunLine,
                   overlapPlot, usevpInfo, usepm, numPtsToRedraw);
   return false;
}


//------------------------------------------------------------------------------
// void SetGlSolarSystem(const std::string &plotName, SolarSystem *ss)
//------------------------------------------------------------------------------
void PlotInterface::SetGlSolarSystem(const std::string &plotName, SolarSystem *ss)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->SetGlSolarSystem(plotName, ss);
}


//------------------------------------------------------------------------------
// void SetGlObject(const std::string &plotName,  ...
//------------------------------------------------------------------------------
void PlotInterface::SetGlObject(const std::string &plotName,
                                const StringArray &objNames,
                                const UnsignedIntArray &objOrbitColors,
                                const std::vector<SpacePoint*> &objArray)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->SetGlObject(
            plotName, objNames, objOrbitColors, objArray);
}


//------------------------------------------------------------------------------
// static void SetGlCoordSystem(const std::string &plotName, ...
//------------------------------------------------------------------------------
void PlotInterface::SetGlCoordSystem(const std::string &plotName,
                                     CoordinateSystem *viewCs,
                                     CoordinateSystem *viewUpCs)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->SetGlCoordSystem(plotName, viewCs, viewUpCs);
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
   if (thePlotReceiver != NULL)
      thePlotReceiver->SetGlViewOption(plotName, vpRefObj, vpVecObj, vdObj, 
            vsFactor, vpRefVec, vpVec, vdVec, upAxis, usevpRefVec, usevpVec, 
            usevdVec, useFixedFov, fov);
}


//------------------------------------------------------------------------------
// void SetGlDrawOrbitFlag(const std::string &plotName, ...
//------------------------------------------------------------------------------
void PlotInterface::SetGlDrawOrbitFlag(const std::string &plotName,
                                       const std::vector<bool> &drawArray)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->SetGlDrawOrbitFlag(plotName, drawArray);
}


//------------------------------------------------------------------------------
// void SetGlShowObjectFlag(const std::string &plotName, ...
//------------------------------------------------------------------------------
void PlotInterface::SetGlShowObjectFlag(const std::string &plotName,
                                        const std::vector<bool> &showArray)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->SetGlShowObjectFlag(plotName, showArray);
}


//------------------------------------------------------------------------------
// void SetGlUpdateFrequency(const std::string &plotName, Integer updFreq)
//------------------------------------------------------------------------------
void PlotInterface::SetGlUpdateFrequency(const std::string &plotName,
                                         Integer updFreq)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->SetGlUpdateFrequency(plotName, updFreq);
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
   if (thePlotReceiver != NULL)
      return thePlotReceiver->IsThere(plotName);
   
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
bool PlotInterface::DeleteGlPlot(const std::string &plotName)
{    
   if (thePlotReceiver != NULL)
      return thePlotReceiver->DeleteGlPlot(plotName);
   
   return false;
}


//------------------------------------------------------------------------------
//  bool RefreshGlPlot(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Refreshes OpenGlPlot.
 */
//------------------------------------------------------------------------------
bool PlotInterface::RefreshGlPlot(const std::string &plotName)
{    
   if (thePlotReceiver != NULL)
      return thePlotReceiver->RefreshGlPlot(plotName);
   
   return false;
}


//------------------------------------------------------------------------------
// bool SetGlEndOfRun(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Sets end of run flag to OpenGlPlot.
 */
//------------------------------------------------------------------------------
bool PlotInterface::SetGlEndOfRun(const std::string &plotName)
{    
   if (thePlotReceiver != NULL)
      return thePlotReceiver->SetGlEndOfRun(plotName);
   
   return false;
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
                                 const StringArray &scNames, const Real &time,
                                 const RealArray &posX, const RealArray &posY,
                                 const RealArray &posZ, const RealArray &velX,
                                 const RealArray &velY, const RealArray &velZ,
                                 const UnsignedIntArray &scColors, bool solving,
                                 Integer solverOption, bool updateCanvas)
{
   #if DEBUG_PLOTIF_GL_UPDATE
   MessageInterface::ShowMessage
      ("PI::UpdateGlPlot() '%s' entered, thePlotReceiver=<%p>\n", plotName.c_str(),
       thePlotReceiver);
   #endif
   
   if (thePlotReceiver != NULL)
      return thePlotReceiver->UpdateGlPlot(plotName, oldName, scNames, time,
            posX, posY, posZ, velX, velY, velZ, scColors, solving, solverOption, 
            updateCanvas);

   return false;
} // end UpdateGlPlot()


//------------------------------------------------------------------------------
// bool TakeGlAction(const std::string &plotName, const std::string &action)
//------------------------------------------------------------------------------
bool PlotInterface::TakeGlAction(const std::string &plotName,
                                 const std::string &action)
{
   if (thePlotReceiver != NULL)
      return thePlotReceiver->TakeGlAction(plotName, action);
   
   return false;
}


//------------------------------------------------------------------------------
//  bool CreateTsPlotWindow(const std::string &plotName,
//                          const std::string &oldName,
//                          const std::string &plotTitle,
//                          const std::string &xAxisTitle,
//                          const std::string &yAxisTitle,
//                          bool drawGrid = false)
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
 * @return true on success, false is no plot was created
 */
//------------------------------------------------------------------------------
bool PlotInterface::CreateTsPlotWindow(const std::string &plotName,
                                       const std::string &oldName,
                                       const std::string &plotTitle,
                                       const std::string &xAxisTitle,
                                       const std::string &yAxisTitle,
                                       bool drawGrid)
{    
   if (thePlotReceiver != NULL)
      return thePlotReceiver->CreateTsPlotWindow(plotName, oldName, plotTitle, 
            xAxisTitle, yAxisTitle, drawGrid);
   
   return false;
}


//------------------------------------------------------------------------------
//  bool DeleteTsPlot(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Deletes a TsPlot by plot name.
 *
 * @param plotName name of plot to be deleted
 *
 * @return true on success, false is no plot was deleted
 */
//------------------------------------------------------------------------------
bool PlotInterface::DeleteTsPlot(const std::string &plotName)
{    
   if (thePlotReceiver != NULL)
      return thePlotReceiver->DeleteTsPlot(plotName);
   
   return false;
}


//------------------------------------------------------------------------------
// bool AddTsPlotCurve(const std::string &plotName, int curveIndex,
//                     int yOffset, Real yMin, Real yMax,
//                     const std::string &curveTitle,
//                     UnsignedInt penColor)
//------------------------------------------------------------------------------
/*
 * Adds a plot curve to an XY plow window.
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
bool PlotInterface::AddTsPlotCurve(const std::string &plotName, int curveIndex,
                                   int yOffset, Real yMin, Real yMax,
                                   const std::string &curveTitle,
                                   UnsignedInt penColor)
{
   if (thePlotReceiver != NULL)
      return thePlotReceiver->AddTsPlotCurve(plotName, curveIndex, yOffset, 
            yMin, yMax, curveTitle, penColor);
   
   return false;
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
bool PlotInterface::DeleteAllTsPlotCurves(const std::string &plotName,
                                          const std::string &oldName)
{
   if (thePlotReceiver != NULL)
      return thePlotReceiver->DeleteAllTsPlotCurves(plotName, oldName);
      
   return false;
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
bool PlotInterface::DeleteTsPlotCurve(const std::string &plotName, int curveIndex)
{
   if (thePlotReceiver != NULL)
      return thePlotReceiver->DeleteTsPlotCurve(plotName, curveIndex);
      
   return false;
}


//------------------------------------------------------------------------------
// void ClearTsPlotData(const std::string &plotName)
//------------------------------------------------------------------------------
/**
 * Removes all data from the plot curves, leaving the curve containers in place
 * but empty.
 *
 * @param plotName The name of the plot that is being cleared
 */
//------------------------------------------------------------------------------
void PlotInterface::ClearTsPlotData(const std::string &plotName)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->ClearTsPlotData(plotName);
}


//------------------------------------------------------------------------------
// void TsPlotPenUp(const std::string &plotName)
//------------------------------------------------------------------------------
/**
 * Tells a plot to stop drawing received data.  This method is idempotent.
 *
 * @param plotName The name of the plot that is being cleared
 */
//------------------------------------------------------------------------------
void PlotInterface::TsPlotPenUp(const std::string &plotName)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->TsPlotPenUp(plotName);
}

//------------------------------------------------------------------------------
// void TsPlotPenDown(const std::string &plotName)
//------------------------------------------------------------------------------
/**
 * Tells a plot to resume drawing received data.  This method is idempotent.
 *
 * @param plotName The name of the plot that is being cleared
 */
//------------------------------------------------------------------------------
void PlotInterface::TsPlotPenDown(const std::string &plotName)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->TsPlotPenDown(plotName);
}


//------------------------------------------------------------------------------
// void TsPlotMarkPoint(const std::string &plotName, Integer index,
//       Integer curveNumber)
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
void PlotInterface::TsPlotMarkPoint(const std::string &plotName, Integer index,
      Integer curveNumber)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->TsPlotMarkPoint(plotName, index, curveNumber);
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
void PlotInterface::TsPlotRescale(const std::string &plotName)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->TsPlotRescale(plotName);
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
void PlotInterface::TsPlotCurveSettings(const std::string &plotName,
      bool useLines, Integer lineWidth, Integer lineStyle, bool useMarkers,
      Integer markerSize, Integer marker, bool useHiLow, Integer forCurve)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->TsPlotCurveSettings(plotName, useLines, lineWidth,
            lineStyle, useMarkers, markerSize, marker, useHiLow, forCurve);
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
void PlotInterface::SetTsPlotTitle(const std::string &plotName,
                                   const std::string &plotTitle)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->SetTsPlotTitle(plotName, plotTitle);
}


//------------------------------------------------------------------------------
// void ShowTsPlotLegend(const std::string &plotName)
//------------------------------------------------------------------------------
/**
 * Turns on display of the plot legend
 *
 * @param plotName The name of the plot
 */
//------------------------------------------------------------------------------
void PlotInterface::ShowTsPlotLegend(const std::string &plotName)
{
   if (thePlotReceiver != NULL)
      thePlotReceiver->ShowTsPlotLegend(plotName);
}


//------------------------------------------------------------------------------
// bool RefreshTsPlot(const std::string &plotName)
//------------------------------------------------------------------------------
/*
 * Refreshes all plot curves on a plot
 *
 * @param plotName The name of the plot
 *
 * @return true on success, false if nothing was refreshed
 */
//------------------------------------------------------------------------------
bool PlotInterface::RefreshTsPlot(const std::string &plotName)
{
   if (thePlotReceiver != NULL)
      return thePlotReceiver->RefreshTsPlot(plotName);
      
   return false;
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
 * Updates an XY plot window.
 *
 * @param plotName name of the xy plot
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
bool PlotInterface::UpdateTsPlot(const std::string &plotName,
                                 const std::string &oldName,
                                 const Real &xval, const Rvector &yvals,
                                 const std::string &plotTitle,
                                 const std::string &xAxisTitle,
                                 const std::string &yAxisTitle,
                                 bool updateCanvas, bool drawGrid)
{
   if (thePlotReceiver != NULL)
      return thePlotReceiver->UpdateTsPlot(plotName, oldName, xval, yvals,
            plotTitle, xAxisTitle, yAxisTitle, updateCanvas, drawGrid);
      
   return false;
}


//------------------------------------------------------------------------------
// bool UpdateTsPlotData(const std::string &plotName, const Real &xval,
//       const Rvector &yvals, const Rvector &hiError, const Rvector &lowError)
//------------------------------------------------------------------------------
/**
 * Updates the data on a plot, passing in a set of y values for a given x, and
 * optionally the data used to draw error bars
 *
 * @param plotName The name of the plot receiving the data
 * @param xval The X value associated with the points
 * @param yvals The Y values associated with the points; these are assigned to
 *              the curves indexed in the order contained in the array
 * @param hiError +sigma error data for the error bars
 * @param lowError -sigma error for the error bars; if no data is contained in
 *                 this array, the low error is assumed to have the same
 *                 magnitude as the high error
 *
 * @return true if the data was processed, false if not
 */
//------------------------------------------------------------------------------
bool PlotInterface::UpdateTsPlotData(const std::string &plotName,
                             const Real &xval, const Rvector &yvals,
                             const Rvector &hiError, const Rvector &lowError)
{
   if (thePlotReceiver != NULL)
      return thePlotReceiver->UpdateTsPlotData(plotName, xval, yvals, &hiError,
            &lowError);

   return false;
}

//------------------------------------------------------------------------------
// bool UpdateTsPlotCurve(const std::string &plotName, Integer whichCurve,
//       const Real &xval, const Real &yval, const Real hi, const Real low)
//------------------------------------------------------------------------------
/**
 * Adds a point to the plot data for a specific curve on a plot
 *
 * @param plotName The name of the plot receiving the data
 * @param whichCurve Index of the curve receiving the data
 * @param xval The X value associated with the point
 * @param yval The Y value associated with the points
 * @param hi +sigma error data for the point's error bar; only used if hi > 0.0
 * @param low -sigma error for the point's error bar; if <= 0.0, the low error
 *            is assumed to have the same magnitude as the high error
 *
 * @return true if the data was processed, false if not
 */
//------------------------------------------------------------------------------
bool PlotInterface::UpdateTsPlotCurve(const std::string &plotName,
                             Integer whichCurve, const Real &xval,
                             const Real &yval, const Real hi, const Real low)
{
   if (thePlotReceiver != NULL)
      return thePlotReceiver->UpdateTsPlotCurve(plotName, whichCurve, xval,
            yval, hi, low);

   return false;
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
bool PlotInterface::DeactivateTsPlot(const std::string &plotName)
{
   if (thePlotReceiver != NULL)
      return thePlotReceiver->DeactivateTsPlot(plotName);

   return false;
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
bool PlotInterface::ActivateTsPlot(const std::string &plotName)
{
   if (thePlotReceiver != NULL)
      return thePlotReceiver->ActivateTsPlot(plotName);

   return false;
}
