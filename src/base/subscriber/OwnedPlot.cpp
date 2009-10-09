//$Id$
//------------------------------------------------------------------------------
//                                  OwnedPlot
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel Conway, based on TsPlot code by Linda Jun
// Created: 2009/09/28
//
/**
 * Implements XyPlot class controlled by Sandbox elements rather than
 * Subscribers.
 */
//------------------------------------------------------------------------------

#include "OwnedPlot.hpp"
#include "PlotInterface.hpp"     // for XY plot
#include "SubscriberException.hpp"
#include "MessageInterface.hpp"  // for ShowMessage()

//#define DEBUG_OwnedPlot_INIT 1
//#define DEBUG_OwnedPlot_PARAM 1
//#define DEBUG_OwnedPlot_OBJECT 1
//#define DEBUG_OwnedPlot_UPDATE 2
//#define DEBUG_ACTION_REMOVE 1
//#define DEBUG_RENAME 1

//---------------------------------
// static data
//---------------------------------

const std::string
OwnedPlot::PARAMETER_TEXT[OwnedPlotParamCount - GmatBaseParamCount] =
{
   "Add",
   "PlotTitle",
   "XAxisTitle",
   "YAxisTitle",
   "Grid",
   "DataCollectFrequency",
   "UpdatePlotFrequency",
   "ShowPlot",
   "ShowLegend",
   "DefaultColor",
   "UseLines",
   "LineWidth",
   "LineStyle",
   "UseMarkers",
   "MarkerSize",
   "Marker",
   "UseHiLow"
}; 


const Gmat::ParameterType
OwnedPlot::PARAMETER_TYPE[OwnedPlotParamCount - GmatBaseParamCount] =
{
   Gmat::OBJECTARRAY_TYPE, // "Add",
   Gmat::STRING_TYPE,      // "PlotTitle",
   Gmat::STRING_TYPE,      // "XAxisTitle",
   Gmat::STRING_TYPE,      // "YAxisTitle",
   Gmat::ON_OFF_TYPE,      // "Grid",
   Gmat::INTEGER_TYPE,     // "DataCollectFrequency",
   Gmat::INTEGER_TYPE,     // "UpdatePlotFrequency",
   Gmat::BOOLEAN_TYPE,     // "ShowPlot",
   Gmat::BOOLEAN_TYPE,     // "ShowLegend",
   Gmat::INTEGER_TYPE,     // "DefaultColor",  <-- May need to be unsigned
   Gmat::BOOLEAN_TYPE,     // "UseLines",
   Gmat::INTEGER_TYPE,     // "LineWidth",
   Gmat::INTEGER_TYPE,     // "LineStyle",
   Gmat::BOOLEAN_TYPE,     // "UseMarkers",
   Gmat::INTEGER_TYPE,     // "MarkerSize",
   Gmat::INTEGER_TYPE,     // "Marker",        <-- Maybe make a string
   Gmat::BOOLEAN_TYPE,     // "UseHiLow"
};

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// OwnedPlot(const std::string &name, Parameter *xParam,
//        Parameter *firstYParam, const std::string &plotTitle,
//        const std::string &xAxisTitle, const std::string &yAxisTitle,
//        bool drawGrid)
//------------------------------------------------------------------------------
OwnedPlot::OwnedPlot(const std::string &name, const std::string &plotTitle,
               const std::string &xAxisTitle, const std::string &yAxisTitle) :
   GmatBase(Gmat::XY_PLOT, "OwnedPlot", name),
   mOldName                (name),
   mPlotTitle              (plotTitle),
   mXAxisTitle             (xAxisTitle),
   mYAxisTitle             (yAxisTitle),
   mDrawGrid               ("On"),
   mIsOwnedPlotWindowSet   (false),
   mDataCollectFrequency   (1),
   mUpdatePlotFrequency    (1),
   defaultColor            (0xFF0000),
   markerSize              (3),
   markerStyle             (-1),
   lineWidth               (1),
   lineStyle               (100),      // wxSOLID = 100, in wx/defs.h
   useLines                (true),
   useMarkers              (false),
   useHiLow                (false),
   active                  (true),
   showLegend              (true),
   isEndOfReceive          (false),
   isEndOfRun              (false),
   isInitialized           (false),
   mSolverIterations       ("All"),
   runstate                (Gmat::RUNNING)
{
   // GmatBase data
   objectTypes.push_back(Gmat::XY_PLOT);
   objectTypeNames.push_back("XYPlot");
   parameterCount = OwnedPlotParamCount;
}


//------------------------------------------------------------------------------
// OwnedPlot(const OwnedPlot &orig)
//------------------------------------------------------------------------------
OwnedPlot::OwnedPlot(const OwnedPlot &orig) :
   GmatBase(orig),
   mOldName                (orig.mOldName),
   mPlotTitle              (orig.mPlotTitle),
   mXAxisTitle             (orig.mXAxisTitle),
   mYAxisTitle             (orig.mYAxisTitle),
   mDrawGrid               (orig.mDrawGrid),
   mIsOwnedPlotWindowSet   (orig.mIsOwnedPlotWindowSet),
   mDataCollectFrequency   (orig.mDataCollectFrequency),
   mUpdatePlotFrequency    (orig.mUpdatePlotFrequency),
   defaultColor            (orig.defaultColor),
   markerSize              (orig.markerSize),
   markerStyle             (orig.markerStyle),
   lineWidth               (orig.lineWidth),
   lineStyle               (orig.lineStyle),
   useLines                (orig.useLines),
   useMarkers              (orig.useMarkers),
   useHiLow                (orig.useHiLow),
   active                  (true),
   showLegend              (orig.showLegend),
   isEndOfReceive          (false),
   isEndOfRun              (false),
   isInitialized           (false),
   mSolverIterations       ("All"),
   runstate                (Gmat::RUNNING)
{
   curveNames        = orig.curveNames;
   curveDataIDs      = orig.curveDataIDs;
   supportedData     = orig.supportedData;
   supportedObjects  = orig.supportedObjects;
}


//------------------------------------------------------------------------------
// OwnedPlot& operator=(const OwnedPlot& orig)
//------------------------------------------------------------------------------
/**
 * The assignment operator
 */
//------------------------------------------------------------------------------
OwnedPlot& OwnedPlot::operator=(const OwnedPlot& orig)
{
   if (this == &orig)
      return *this;
   
   GmatBase::operator=(orig);
   
   mOldName                = orig.mOldName;
   mPlotTitle              = orig.mPlotTitle;
   mXAxisTitle             = orig.mXAxisTitle;
   mYAxisTitle             = orig.mYAxisTitle;
   mDrawGrid               = orig.mDrawGrid;
   mIsOwnedPlotWindowSet   = orig.mIsOwnedPlotWindowSet;
   mDataCollectFrequency   = orig.mDataCollectFrequency;
   mUpdatePlotFrequency    = orig.mUpdatePlotFrequency;
   defaultColor            = orig.defaultColor;
   markerSize              = orig.markerSize;
   markerStyle             = orig.markerStyle;
   lineWidth               = orig.lineWidth;
   lineStyle               = orig.lineStyle;
   useLines                = orig.useLines;
   useMarkers              = orig.useMarkers;
   useHiLow                = orig.useHiLow;
   active                  = true;
   showLegend              = orig.showLegend;
   isEndOfReceive          = false;
   isEndOfRun              = false;
   isInitialized           = false;
   mSolverIterations       = "All";
   runstate                = Gmat::RUNNING;
   
   curveNames              = orig.curveNames;
   curveDataIDs            = orig.curveDataIDs;
   supportedData           = orig.supportedData;
   supportedObjects        = orig.supportedObjects;

   return *this;
}


//------------------------------------------------------------------------------
// ~OwnedPlot(void)
//------------------------------------------------------------------------------
OwnedPlot::~OwnedPlot()
{
}


//------------------------------------------------------------------------------
// virtual bool Initialize()
//------------------------------------------------------------------------------
bool OwnedPlot::Initialize()
{
   #if DEBUG_OwnedPlot_INIT
      MessageInterface::ShowMessage
            ("OwnedPlot::Initialize() active=%d, mNumYParams=%d\n", active,
             mNumYParams);
   #endif
   
   GmatBase::Initialize();
   isEndOfReceive = false;
   isEndOfRun = false;
   
   bool status = false;
   DeletePlotCurves();
   
   if (active)
   {
      // build plot title
      BuildPlotTitle();
      
      // Create OwnedPlotWindow, if not exist
      #if DEBUG_OwnedPlot_INIT
         MessageInterface::ShowMessage
               ("OwnedPlot::Initialize() calling CreateOwnedPlotWindow()\n");
      #endif
      
      PlotInterface::CreateTsPlotWindow(instanceName, mOldName, mPlotTitle,
            mXAxisTitle, mYAxisTitle, (mDrawGrid == "On"));
      
      PlotInterface::SetTsPlotTitle(instanceName, mPlotTitle);
      mIsOwnedPlotWindowSet = true;
      
      // add to Y params to OwnedPlotWindow
      //loj: temp code
      int yOffset = 0; //loj: I don't know how this is used
      Real yMin = -40000.0; //loj: should parameter provide minimum value?
      Real yMax =  40000.0; //loj: should parameter provide maximum value?
      
      #if DEBUG_OwnedPlot_INIT
         MessageInterface::ShowMessage
               ("OwnedPlot::Initialize() Get curveTitle and penColor\n");
      #endif
      
      for (UnsignedInt i = 0; i < curveNames.size(); ++i)
      {
         #if DEBUG_OwnedPlot_INIT
            MessageInterface::ShowMessage("OwnedPlot::Initialize() "
                  "curveTitle = %s\n", curveNames[i].c_str());
         #endif
         
         PlotInterface::AddTsPlotCurve(instanceName, i, yOffset, yMin, yMax,
               curveNames[i], curveColor[i]);
         PlotInterface::TsPlotCurveSettings(instanceName, curveUseLines[i],
               curveLineWidth[i], curveLineStyle[i], curveUseMarkers[i],
               curveMarkerSize[i], curveMarker[i], curveUseHiLow[i], i);
      }
      
      PlotInterface::ShowTsPlotLegend(instanceName);
      status = true;
      
      #if DEBUG_OwnedPlot_INIT
         MessageInterface::ShowMessage("OwnedPlot::Initialize() calling "
               "ClearOwnedPlotData()\n");
      #endif
      
      PlotInterface::ClearTsPlotData(instanceName);
      PlotInterface::TsPlotCurveSettings(instanceName, useLines, lineWidth,
            useMarkers, markerSize);
   }
   else
   {
      #if DEBUG_OwnedPlot_INIT
      MessageInterface::ShowMessage("OwnedPlot::Initialize() DeleteOwnedPlot()\n");
      #endif
      
      status =  PlotInterface::DeleteTsPlot(instanceName);
   }
   
   #if DEBUG_OwnedPlot_INIT
      MessageInterface::ShowMessage("OwnedPlot::Initialize() leaving "
            "status=%d\n", status);
   #endif
   
   return status;
}

//---------------------------------
// methods inherited from GmatBase
//---------------------------------

//------------------------------------------------------------------------------
//  GmatBase* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the OwnedPlot.
 *
 * @return clone of the OwnedPlot.
 *
 */
//------------------------------------------------------------------------------
GmatBase* OwnedPlot::Clone() const
{
   return (new OwnedPlot(*this));
}


//---------------------------------------------------------------------------
// void Copy(const GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Sets this object to match another one.
 * 
 * @param orig The original that is being copied.
 */
//---------------------------------------------------------------------------
void OwnedPlot::Copy(const GmatBase* orig)
{
   operator=(*((OwnedPlot *)(orig)));
}


//------------------------------------------------------------------------------
// bool SetName(const std::string &who, const std;:string &oldName = "")
//------------------------------------------------------------------------------
/**
 * Set the name for this instance.
 *
 * @see GmatBase
 *
 */
//------------------------------------------------------------------------------
bool OwnedPlot::SetName(const std::string &who, const std::string &oldName)
{
   #if DEBUG_RENAME
      MessageInterface::ShowMessage("OwnedPlot::SetName() newName=%s\n",
            who.c_str());
   #endif
   
   if (oldName == "")
      mOldName = instanceName;
   else
      mOldName = oldName;
   
   return GmatBase::SetName(who);
}


//------------------------------------------------------------------------------
// virtual bool TakeAction(const std::string &action,  
//                         const std::string &actionData = "");
//------------------------------------------------------------------------------
/**
 * This method performs action.
 *
 * @param <action> action to perform
 * @param <actionData> action data associated with action
 * @return true if action successfully performed
 *
 */
//------------------------------------------------------------------------------
bool OwnedPlot::TakeAction(const std::string &action,
                        const std::string &actionData)
{
   #if DEBUG_ACTION_REMOVE
      MessageInterface::ShowMessage("OwnedPlot::TakeAction() action=%s, "
            "actionData=%s\n", action.c_str(), actionData.c_str());
   #endif
   
   if (action == "Clear")
   {
      return ClearYParameters();
   }
   else if (action == "Remove")
   {
      return RemoveYParameter(actionData);
   }
   else if (action == "ClearData")
   {
      return ResetYParameters();
   }
   else if (action == "PenUp")
   {
      return PenUp();
   }
   else if (action == "PenDown")
   {
      return PenDown();
   }
   else if (action == "Rescale")
   {
      return RescaleData();
   }
   
   return false;
}


//---------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//---------------------------------------------------------------------------
bool OwnedPlot::RenameRefObject(const Gmat::ObjectType type,
                             const std::string &oldName,
                             const std::string &newName)
{
   #if DEBUG_RENAME
      MessageInterface::ShowMessage
         ("OwnedPlot::RenameRefObject() type=%s, oldName=%s, newName=%s\n",
          GetObjectTypeString(type).c_str(), oldName.c_str(), newName.c_str());
   #endif
   
   if (type != Gmat::PARAMETER && type != Gmat::COORDINATE_SYSTEM &&
       type != Gmat::SPACECRAFT)
      return true;
   
   if (type == Gmat::PARAMETER)
   {
      // Y parameters
      for (UnsignedInt i = 0; i < curveNames.size(); ++i)
         if (curveNames[i] == oldName)
            curveNames[i] = newName;
   }
   
   return true;
}


//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
std::string OwnedPlot::GetParameterText(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < OwnedPlotParamCount)
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   else
      return GmatBase::GetParameterText(id);
    
}

//------------------------------------------------------------------------------
// Integer GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
Integer OwnedPlot::GetParameterID(const std::string &str) const
{
   for (int i=GmatBaseParamCount; i<OwnedPlotParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }
   
   return GmatBase::GetParameterID(str);
}


//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
Gmat::ParameterType OwnedPlot::GetParameterType(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < OwnedPlotParamCount)
      return PARAMETER_TYPE[id - GmatBaseParamCount];
   else
      return GmatBase::GetParameterType(id);
}


//------------------------------------------------------------------------------
// std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
std::string OwnedPlot::GetParameterTypeString(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < OwnedPlotParamCount)
      return GmatBase::PARAM_TYPE_STRING[GetParameterType(id -
            GmatBaseParamCount)];
   else
      return GmatBase::GetParameterTypeString(id);
}

//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool OwnedPlot::IsParameterReadOnly(const Integer id) const
{
   if (
       (id == PLOT_TITLE)             ||
       (id == X_AXIS_TITLE)           ||
       (id == Y_AXIS_TITLE)           ||
       (id == DATA_COLLECT_FREQUENCY) ||
       (id == UPDATE_PLOT_FREQUENCY)  ||
       (id == USE_LINES)              ||
       (id == LINE_WIDTH)             ||
       (id == USE_MARKERS)            ||
       (id == MARKER_SIZE)
      )
      return true;
   
   return GmatBase::IsParameterReadOnly(id);
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer OwnedPlot::GetIntegerParameter(const Integer id) const
{
   switch (id)
   {
   case DATA_COLLECT_FREQUENCY:
      return mDataCollectFrequency;

   case UPDATE_PLOT_FREQUENCY:
      return mUpdatePlotFrequency;

   case DEFAULT_COLOR:
      return defaultColor;

   case LINE_WIDTH:
      return lineWidth;

   case LINE_STYLE:
      return lineStyle;

   case MARKER_SIZE:
      return markerSize;

   case MARKER_STYLE:
      return markerStyle;

   default:
      return GmatBase::GetIntegerParameter(id);
   }
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerParameter(const std::string &label) const
//------------------------------------------------------------------------------
Integer OwnedPlot::GetIntegerParameter(const std::string &label) const
{
   return GetIntegerParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// virtual Integer SetIntegerParameter(const Integer id, const Integer value)
//------------------------------------------------------------------------------
Integer OwnedPlot::SetIntegerParameter(const Integer id, const Integer value)
{
   switch (id)
   {
      case DATA_COLLECT_FREQUENCY:
         mDataCollectFrequency = value;
         return value;

      case UPDATE_PLOT_FREQUENCY:
         mUpdatePlotFrequency = value;
         return value;

      case DEFAULT_COLOR:
         defaultColor = value;
         return defaultColor;

      case LINE_WIDTH:
         lineWidth = value;
         return value;

      case LINE_STYLE:
         lineStyle = value;
         return lineStyle;

      case MARKER_SIZE:
         markerSize = value;
         return value;

      case MARKER_STYLE:
         markerStyle = value;
         return markerStyle;

      default:
         return GmatBase::SetIntegerParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// virtual Integer SetIntegerParameter(const std::string &label,
//                                     const Integer value)
//------------------------------------------------------------------------------
Integer OwnedPlot::SetIntegerParameter(const std::string &label,
                                    const Integer value)
{
   return SetIntegerParameter(GetParameterID(label), value);
}


//---------------------------------------------------------------------------
//  std::string GetOnOffParameter(const Integer id) const
//---------------------------------------------------------------------------
std::string OwnedPlot::GetOnOffParameter(const Integer id) const
{
   switch (id)
   {
      case DRAW_GRID:
         return mDrawGrid;
      default:
         return GmatBase::GetOnOffParameter(id);
   }
}


//------------------------------------------------------------------------------
// std::string OwnedPlot::GetOnOffParameter(const std::string &label) const
//------------------------------------------------------------------------------
std::string OwnedPlot::GetOnOffParameter(const std::string &label) const
{
   return GetOnOffParameter(GetParameterID(label));
}


//---------------------------------------------------------------------------
//  bool SetOnOffParameter(const Integer id, const std::string &value)
//---------------------------------------------------------------------------
bool OwnedPlot::SetOnOffParameter(const Integer id, const std::string &value)
{
   switch (id)
   {
      case DRAW_GRID:
         mDrawGrid = value;
         return true;

      default:
         return GmatBase::SetOnOffParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// bool SetOnOffParameter(const std::string &label, const std::string &value)
//------------------------------------------------------------------------------
bool OwnedPlot::SetOnOffParameter(const std::string &label,
      const std::string &value)
{
   return SetOnOffParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string OwnedPlot::GetStringParameter(const Integer id) const
{
   switch (id)
   {
      case PLOT_TITLE:
         return mPlotTitle;

      case X_AXIS_TITLE:
         return mXAxisTitle;

      case Y_AXIS_TITLE:
         return mYAxisTitle;

      default:
         return GmatBase::GetStringParameter(id);
   }
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label) const
//------------------------------------------------------------------------------
std::string OwnedPlot::GetStringParameter(const std::string &label) const
{
   #if DEBUG_XY_PARAM
      MessageInterface::ShowMessage("OwnedPlot::GetStringParameter() "
            "label = %s\n", label.c_str());
   #endif
   
   return GetStringParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
bool OwnedPlot::SetStringParameter(const Integer id, const std::string &value)
{
   #if DEBUG_OwnedPlot_PARAM
      MessageInterface::ShowMessage("OwnedPlot::SetStringParameter() id = %d, "
                                    "value = %s \n", id, value.c_str());
   #endif
   
   switch (id)
   {
      case ADD:
         if (find(curveNames.begin(), curveNames.end(), value) ==
               curveNames.end())
         {
            curveNames.push_back(value);
            curveColor.push_back(defaultColor);
            curveLineWidth.push_back(lineWidth);
            curveLineStyle.push_back(lineStyle);
            if (markerStyle == -1)
               curveMarker.push_back(curveNames.size() % 10);
            else
               curveMarker.push_back(markerStyle);
            curveMarkerSize.push_back(markerSize);
            curveUseLines.push_back(useLines);
            curveUseMarkers.push_back(useMarkers);
            curveUseHiLow.push_back(useHiLow);
         }
         return true;

      case PLOT_TITLE:
         mPlotTitle = value;
         return true;

      case X_AXIS_TITLE:
         mXAxisTitle = value;
         return true;

      case Y_AXIS_TITLE:
         mYAxisTitle = value;
         return true;

      default:
         return GmatBase::SetStringParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label,
//                         const std::string &value)
//------------------------------------------------------------------------------
bool OwnedPlot::SetStringParameter(const std::string &label,
                                const std::string &value)
{
   #if DEBUG_OwnedPlot_PARAM
      MessageInterface::ShowMessage("OwnedPlot::SetStringParameter() "
            "label = %s, value = %s \n", label.c_str(), value.c_str());
   #endif
   
   return SetStringParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// virtual bool SetStringParameter(const Integer id, const std::string &value,
//                                 const Integer index)
//------------------------------------------------------------------------------
bool OwnedPlot::SetStringParameter(const Integer id, const std::string &value,
                                const Integer index)
{
   switch (id)
   {
      case ADD:
         if ((index < 0) || (index > (Integer)curveNames.size()))
            return false;
         if (index < (Integer)curveNames.size())
         {
            curveNames[index] = value;
         }
         else
         {
            curveNames.push_back(value);
            curveColor.push_back(defaultColor);
            curveLineWidth.push_back(lineWidth);
            curveLineStyle.push_back(lineStyle);
            if (markerStyle == -1)
               curveMarker.push_back(curveNames.size() % 10);
            else
               curveMarker.push_back(markerStyle);
            curveMarkerSize.push_back(markerSize);
            curveUseLines.push_back(useLines);
            curveUseMarkers.push_back(useMarkers);
            curveUseHiLow.push_back(useHiLow);
         }
         return true;

      default:
         return GmatBase::SetStringParameter(id, value, index);
   }
}


//------------------------------------------------------------------------------
// virtual bool SetStringParameter(const std::string &label,
//                                 const std::string &value,
//                                 const Integer index)
//------------------------------------------------------------------------------
bool OwnedPlot::SetStringParameter(const std::string &label,
                                const std::string &value,
                                const Integer index)
{
   #if DEBUG_OwnedPlot_PARAM
      MessageInterface::ShowMessage
         ("OwnedPlot::SetStringParameter() label=%s, value=%s, index=%d \n",
          label.c_str(), value.c_str(), index);
   #endif
   
   return SetStringParameter(GetParameterID(label), value, index);
}


//------------------------------------------------------------------------------
// const StringArray& GetStringArrayParameter(const Integer id) const
//------------------------------------------------------------------------------
const StringArray& OwnedPlot::GetStringArrayParameter(const Integer id) const
{
   switch (id)
   {
      case ADD:
         return curveNames;
      default:
         return GmatBase::GetStringArrayParameter(id);
   }
}


//------------------------------------------------------------------------------
// StringArray& GetStringArrayParameter(const std::string &label) const
//------------------------------------------------------------------------------
const StringArray& OwnedPlot::GetStringArrayParameter(
      const std::string &label) const
{
   return GetStringArrayParameter(GetParameterID(label));
}


bool OwnedPlot::GetBooleanParameter(const Integer id) const
{
   if (id == SHOW_PLOT)
      return active;

   if (id == SHOW_LEGEND)
      return showLegend;

   if (id == USE_LINES)
      return useLines;

   if (id == USE_MARKERS)
      return useMarkers;

   if (id == USE_HI_LOW)
      return useHiLow;

   return GmatBase::GetBooleanParameter(id);
}

bool OwnedPlot::GetBooleanParameter(const std::string &label) const
{
   return GetBooleanParameter(GetParameterID(label));
}

bool OwnedPlot::SetBooleanParameter(const std::string &label, const bool value)
{
   return SetBooleanParameter(GetParameterID(label), value);
}

bool OwnedPlot::SetBooleanParameter(const Integer id, const bool value)
{
   if (id == SHOW_PLOT)
   {
      active = value;
      return active;
   }

   if (id == SHOW_LEGEND)
   {
      showLegend = value;
      return showLegend;
   }

   if (id == USE_LINES)
   {
      useLines = value;
      if (useLines == false)
         useMarkers = true;
      return useLines;
   }

   if (id == USE_MARKERS)
   {
      useMarkers = value;
      // Always have to have either markers or lines
      if (useMarkers == false)
         useLines = true;
      return useMarkers;
   }

   if (id == USE_HI_LOW)
   {
      useHiLow = value;
      return useHiLow;
   }

   return GmatBase::SetBooleanParameter(id, value);
}


bool OwnedPlot::Activate()
{
   PlotInterface::ActivateTsPlot(instanceName);
   return true;
}


bool OwnedPlot::Deactivate()
{
   PlotInterface::DeactivateTsPlot(instanceName);
   return true;
}


void OwnedPlot::SetData(std::vector<RealArray*> &dataBlast)
{
   #if DEBUG_OwnedPlot_UPDATE > 1
      MessageInterface::ShowMessage
         ("OwnedPlot::SetData() entered. isEndOfReceive=%d, active=%d, "
               "runState=%d\n", isEndOfReceive, active, runstate);
   #endif

   RealArray *xData = dataBlast[0];

   #if DEBUG_OwnedPlot_UPDATE > 1
      RealArray *yData = dataBlast[1];

      MessageInterface::ShowMessage("   Sample points: 0->[%le %le] "
            "5->[%le %le]\n", (*xData)[0], (*yData)[0], (*xData)[5],
            (*yData)[5]);
   #endif

   Real xval;
   Rvector yvals = Rvector(curveNames.size());

   for (UnsignedInt i = 0; i < xData->size(); ++i)
   {
      xval = (*xData)[i];
      for (UnsignedInt j = 0; j < curveNames.size(); ++j)
         yvals[j] = (*(dataBlast[j+1]))[i];

      PlotInterface::UpdateTsPlotData(instanceName, xval, yvals);
   }
}


void OwnedPlot::SetCurveData(const Integer forCurve, RealArray *xData,
           RealArray *yData)
{
   #if DEBUG_OwnedPlot_UPDATE > 1
      MessageInterface::ShowMessage
         ("OwnedPlot::SetData() entered. isEndOfReceive=%d, active=%d, "
          "runState=%d\n", isEndOfReceive, active, runstate);
   #endif

   for (UnsignedInt i = 0; i < xData->size(); ++i)
   {
      PlotInterface::UpdateTsPlotCurve(instanceName, forCurve, (*xData)[i],
            (*yData)[i]);
   }
}


bool OwnedPlot::MarkPoint(Integer whichOne, Integer forCurve)
{
   bool retval = false;

   return retval;
}

Integer OwnedPlot::SetUsedDataID(Integer id, Integer forCurve)
{
   if (forCurve < -1)
      return -1;

   Integer curveId = -1;

   if (forCurve == -1)
   {
      curveId = (int)supportedData.size();
      supportedData.push_back(id);
   }
   else
   {
      if (forCurve < (int)supportedData.size())
      {
         supportedData[forCurve] = id;
         curveId = forCurve;
      }
      else if (forCurve == (int)supportedData.size())
      {
         curveId = (int)supportedData.size();
         supportedData.push_back(id);
      }
   }

   return curveId;
}

void OwnedPlot::SetUsedObjectID(Integer id)
{
   bool alreadyThere = false;

   for (UnsignedInt i = 0; i < supportedObjects.size(); ++i)
      if (supportedObjects[i] == id)
      {
         alreadyThere = true;
         break;
      }

   if (!alreadyThere)
      supportedObjects.push_back(id);
}

Integer OwnedPlot::UsesData(Integer id)
{
   Integer retval = -1;

   for (UnsignedInt i = 0; i < supportedData.size(); ++i)
      if (supportedData[i] == id)
      {
         retval = i;
         break;
      }

   return retval;
}

Integer OwnedPlot::UsesObject(Integer id)
{
   Integer retval = -1;

   for (UnsignedInt i = 0; i < supportedObjects.size(); ++i)
      if (supportedObjects[i] == id)
      {
         retval = i;
         break;
      }

   return retval;
}

//---------------------------------
// protected methods
//---------------------------------

//------------------------------------------------------------------------------
// void BuildPlotTitle()
//------------------------------------------------------------------------------
void OwnedPlot::BuildPlotTitle()
{
   if (mXAxisTitle == "")
      mXAxisTitle = "Epoch";
   if (mYAxisTitle == "")
      mYAxisTitle = "Residual";
   if (mPlotTitle == "")
      mPlotTitle  = "Residual data";
}

//------------------------------------------------------------------------------
// bool ClearYParameters()
//------------------------------------------------------------------------------
bool OwnedPlot::ClearYParameters()
{
   DeletePlotCurves();
   curveNames.clear();
   // mPlotTitle = "";
   // mXAxisTitle = "";
   // mYAxisTitle = "";
   mIsOwnedPlotWindowSet = false;
   return true;
}

//------------------------------------------------------------------------------
// bool RemoveYParameter(const std::string &name)
//------------------------------------------------------------------------------
/*
 * Removes curve from the curve list
 *
 * @param <name> parameter name to be removed from the Y parameter list
 *
 * @return true if parameter was removed from the Y parameter list, false
 *         otherwise
 *
 */
//------------------------------------------------------------------------------
bool OwnedPlot::RemoveYParameter(const std::string &name)
{
   #if DEBUG_ACTION_REMOVE
      MessageInterface::ShowMessage
         ("OwnedPlot::RemoveYParameter() name=%s\n--- Before remove:\n",
               name.c_str());
      for (int i=0; i<mNumYParams; i++)
      {
         MessageInterface::ShowMessage("mYParamNames[%d]=%s\n", i,
                                       mYParamNames[i].c_str());
      }
   #endif

   StringArray::iterator pos1;

   for (pos1 = curveNames.begin(); pos1 != curveNames.end(); ++pos1)
   {
      if (*pos1 == name)
      {
         curveNames.erase(pos1);

         #if DEBUG_ACTION_REMOVE
            MessageInterface::ShowMessage("---After remove\n");
            for (UnsignedInt i = 0; i < curveNames.size(); ++i)
            {
               MessageInterface::ShowMessage("curveNames[%d] = %s\n", i,
                     curveNames[i].c_str());
            }
         #endif

         return true;
      }
   }

   //------------------------------------------
   // loj: 9/29/04
   // Need to remove from PlotCurves also
   //------------------------------------------

   #if DEBUG_ACTION_REMOVE
      MessageInterface::ShowMessage("OwnedPlot::RemoveYParameter() name = %s "
            "not found\n", name.c_str());
   #endif

   return false;
}

//------------------------------------------------------------------------------
// bool ResetYParameters()
//------------------------------------------------------------------------------
bool OwnedPlot::ResetYParameters()
{
   PlotInterface::ClearTsPlotData(instanceName);
   return true;
}

//------------------------------------------------------------------------------
// bool PenUp()
//------------------------------------------------------------------------------
bool OwnedPlot::PenUp()
{
   PlotInterface::TsPlotPenUp(instanceName);
   return true;
}

//------------------------------------------------------------------------------
// bool PenDown()
//------------------------------------------------------------------------------
bool OwnedPlot::PenDown()
{
   PlotInterface::TsPlotPenDown(instanceName);
   return true;
}


bool OwnedPlot::RescaleData()
{
   PlotInterface::TsPlotRescale(instanceName);
   return true;
}


//------------------------------------------------------------------------------
// void DeletePlotCurves()
//------------------------------------------------------------------------------
void OwnedPlot::DeletePlotCurves()
{
   // delete exiting curves
   PlotInterface::DeleteAllTsPlotCurves(instanceName, mOldName);
}


