//$Id$
//------------------------------------------------------------------------------
//                             PlotReceiver
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel Conway, based on code written by Linda Jun
// Created: 2008/06/16
//
/**
 * Declares PlotReceiver class.
 */
//------------------------------------------------------------------------------


#include "PlotReceiver.hpp"

//------------------------------------------------------------------------------
// void SetViewType(GmatPlot::ViewType view)
//------------------------------------------------------------------------------
void PlotReceiver::SetViewType(GmatPlot::ViewType view)
{
   currentView = view;
}


//------------------------------------------------------------------------------
// ViewType GetViewType()
//------------------------------------------------------------------------------
GmatPlot::ViewType PlotReceiver::GetViewType()
{
   return currentView;
}


//------------------------------------------------------------------------------
// PlotReceiver()
//------------------------------------------------------------------------------
/**
 * Default constructor.
 */
//------------------------------------------------------------------------------
PlotReceiver::PlotReceiver()
{
   currentView = GmatPlot::TRAJECTORY_PLOT;
}


//------------------------------------------------------------------------------
// ~PlotReceiver()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
PlotReceiver::~PlotReceiver()
{
}
