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
 * Declares PlotInterface class.
 */
//------------------------------------------------------------------------------
#ifndef GuiPlotInterface_hpp
#define GuiPlotInterface_hpp

#include "gmatdefs.hpp"
#include "Rvector.hpp"
#include "SolarSystem.hpp"
#include "CoordinateSystem.hpp"
#include "PlotInterface.hpp"

class GuiPlotInterface : PlotInterface
{

public:
   // for OpenGL Plot
   bool CreateGlPlotWindow(const std::string &plotName,
                                  const std::string &oldName,
                                  bool drawEcPlane, bool drawEqPlane,
                                  bool drawWireFrame, bool drawAxes, bool drawGrid,
                                  bool drawESLines, bool overlapPlot,
                                  bool usevpInfo, bool usepm,
                                  Integer numPtsToRedraw);
   
   void SetGlSolarSystem(const std::string &plotName, SolarSystem *ss);
   
   void SetGlObject(const std::string &plotName,
                           const StringArray &objNames,
                           const UnsignedIntArray &objOrbitColors,
                           const std::vector<SpacePoint*> &objArray);
   
   void SetGlCoordSystem(const std::string &plotName,
                                CoordinateSystem *viewCs,
                                CoordinateSystem *viewUpCs);
   
   void SetGlViewOption(const std::string &plotName,
                               SpacePoint *vpRefObj, SpacePoint *vpVecObj,
                               SpacePoint *vdObj, Real vsFactor,
                               const Rvector3 &vpRefVec, const Rvector3 &vpVec,
                               const Rvector3 &vdVec, const std::string &upAxis,
                               bool usevpRefVec, bool usevpVec, bool usevdVec,
                               bool useFixedFov, Real fov);
   
   void SetGlDrawOrbitFlag(const std::string &plotName,
                                  const std::vector<bool> &drawArray);
   
   void SetGlShowObjectFlag(const std::string &plotName,
                                   const std::vector<bool> &showArray);
   
   void SetGlUpdateFrequency(const std::string &plotName, Integer updFreq);
   
   bool IsThere(const std::string &plotName);
   
   bool DeleteGlPlot(const std::string &plotName);
   bool RefreshGlPlot(const std::string &plotName);
   bool SetGlEndOfRun(const std::string &plotName);
   
   bool UpdateGlPlot(const std::string &plotName,
                            const std::string &oldName,
                            const StringArray &scNames, const Real &time,
                            const RealArray &posX, const RealArray &posY,
                            const RealArray &posZ, const RealArray &velX,
                            const RealArray &velY, const RealArray &velZ,
                            const UnsignedIntArray &scColors, bool solving,
                            Integer solverOption, bool updateCanvas);
   
   bool TakeGlAction(const std::string &plotName,
                            const std::string &action);
   
   // for XY plot
   bool CreateTsPlotWindow(const std::string &plotName,
                                  const std::string &oldName,
                                  const std::string &plotTitle,
                                  const std::string &xAxisTitle,
                                  const std::string &yAxisTitle,
                                  bool drawGrid = false);
   bool DeleteTsPlot(const std::string &plotName);
   bool AddTsPlotCurve(const std::string &plotName, int curveIndex,
                              int yOffset, Real yMin, Real yMax,
                              const std::string &curveTitle,
                              UnsignedInt penColor);
   bool DeleteAllTsPlotCurves(const std::string &plotName,
                                     const std::string &oldName);
   bool DeleteTsPlotCurve(const std::string &plotName, int curveIndex);
   void ClearTsPlotData(const std::string &plotName);
   void TsPlotPenUp(const std::string &plotName);
   void TsPlotPenDown(const std::string &plotName);
   
   void SetTsPlotTitle(const std::string &plotName,
                              const std::string &plotTitle);
   void ShowTsPlotLegend(const std::string &plotName);
   bool RefreshTsPlot(const std::string &plotName);
   bool UpdateTsPlot(const std::string &plotName,
                            const std::string &oldName,
                            const Real &xval, const Rvector &yvals,
                            const std::string &plotTitle,
                            const std::string &xAxisTitle,
                            const std::string &yAxisTitle,
                            bool updateCanvas, bool drawGrid);
   

   
private:

   GuiPlotInterface();
   ~GuiPlotInterface();

};

#endif
