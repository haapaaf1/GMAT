//$Id$
//------------------------------------------------------------------------------
//                                  XyPlot
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2004/01/22
//
/**
 * Implements XyPlot class.
 */
//------------------------------------------------------------------------------

#include "XyPlot.hpp"
#include "PlotInterface.hpp"     // for XY plot
#include "SubscriberException.hpp"
#include "MessageInterface.hpp"  // for ShowMessage()

//#define DEBUG_TSPLOT_INIT 1
//#define DEBUG_TSPLOT_PARAM 1
//#define DEBUG_TSPLOT_OBJECT 1 
//#define DEBUG_TSPLOT_UPDATE 2
//#define DEBUG_ACTION_REMOVE 1
//#define DEBUG_RENAME 1

//---------------------------------
// static data
//---------------------------------

const std::string
XyPlot::PARAMETER_TEXT[XyPlotParamCount - SubscriberParamCount] =
{
   "XVariable",
   "YVariables",
   "PlotTitle",
   "XAxisTitle",
   "YAxisTitle",
   "ShowGrid",
   "DataCollectFrequency",
   "UpdatePlotFrequency",
   "ShowPlot",
   "UseLines",
   "LineWidth",
   "UseMarkers",
   "MarkerSize",
   "Drawing",
   "IndVar",
   "Add",
   "Grid",
}; 

const Gmat::ParameterType
XyPlot::PARAMETER_TYPE[XyPlotParamCount - SubscriberParamCount] =
{
   Gmat::OBJECT_TYPE,      // "IndVar","XVariable"
   Gmat::OBJECTARRAY_TYPE, // "Add","YVariables"
   Gmat::STRING_TYPE,      // "PlotTitle",
   Gmat::STRING_TYPE,      // "XAxisTitle",
   Gmat::STRING_TYPE,      // "YAxisTitle",
   Gmat::BOOLEAN_TYPE,     // "ShowGrid"
   Gmat::INTEGER_TYPE,     // "DataCollectFrequency",
   Gmat::INTEGER_TYPE,     // "UpdatePlotFrequency",
   Gmat::BOOLEAN_TYPE,     // "ShowPlot",
   Gmat::BOOLEAN_TYPE,     // "UseLines",
   Gmat::INTEGER_TYPE,     // "LineWidth",
   Gmat::BOOLEAN_TYPE,     // "UseMarkers",
   Gmat::INTEGER_TYPE,     // "MarkerSize",
   Gmat::BOOLEAN_TYPE,     // "Drawing"
   Gmat::OBJECT_TYPE,      // "IndVar","XVariable"
   Gmat::OBJECTARRAY_TYPE, // "Add","YVariables"
   Gmat::ON_OFF_TYPE,      // "Grid","ShowGrid"
};

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// XyPlot(const std::string &name, Parameter *xParam,
//        Parameter *firstYParam, const std::string &plotTitle,
//        const std::string &xAxisTitle, const std::string &yAxisTitle,
//        bool drawGrid)
//------------------------------------------------------------------------------
XyPlot::XyPlot(const std::string &name, Parameter *xParam,
               Parameter *firstYParam, const std::string &plotTitle,
               const std::string &xAxisTitle, const std::string &yAxisTitle) :
   Subscriber("XYPlot", name)
{
   // GmatBase data
   objectTypes.push_back(Gmat::XY_PLOT);
   objectTypeNames.push_back("XYPlot");
   parameterCount = XyPlotParamCount;
   
   mDrawGrid = true;
   mNumYParams = 0;
   
   mXParamName = "";
   mNumXParams = 0;
   
   mXParam = xParam;
   if (firstYParam != NULL)
      AddYParameter(firstYParam->GetName(), mNumYParams);
   
   mOldName = instanceName;
   mPlotTitle = plotTitle;
   mXAxisTitle = xAxisTitle;
   mYAxisTitle = yAxisTitle;
   
   mIsXyPlotWindowSet = false;
   mDataCollectFrequency = 1;
   mUpdatePlotFrequency = 10;
   mNumDataPoints = 0;           // Found by Bob Wiegand w/ Valgrind

   useLines = true;
   lineWidth = 1;
   useMarkers = false;
   markerSize = 3;
   drawing = true;
   breakCount = 0;
}


//------------------------------------------------------------------------------
// XyPlot(const XyPlot &orig)
//------------------------------------------------------------------------------
XyPlot::XyPlot(const XyPlot &orig) :
   Subscriber(orig)
{
   mXParam = orig.mXParam;
   mYParams = orig.mYParams;
   
   mNumXParams = orig.mNumXParams;
   mNumYParams = orig.mNumYParams;
   
   mXParamName = orig.mXParamName;
   mYParamNames = orig.mYParamNames;
   
   mAllParamNames = orig.mAllParamNames;
   
   mOldName = orig.mOldName;
   mPlotTitle = orig.mPlotTitle;
   mXAxisTitle = orig.mXAxisTitle;
   mYAxisTitle = orig.mYAxisTitle;
   mDrawGrid = orig.mDrawGrid;
   mIsXyPlotWindowSet = orig.mIsXyPlotWindowSet;
   
   mDataCollectFrequency = orig.mDataCollectFrequency;
   mUpdatePlotFrequency = orig.mUpdatePlotFrequency;
   
   mNumDataPoints = orig.mNumDataPoints;
   mNumCollected = orig.mNumCollected;

   useLines   = orig.useLines;
   lineWidth  = orig.lineWidth;
   useMarkers = orig.useMarkers;
   markerSize = orig.markerSize;
   drawing    = orig.drawing;
   breakCount = orig.breakCount;
}


//------------------------------------------------------------------------------
// XyPlot& operator=(const XyPlot& orig)
//------------------------------------------------------------------------------
/**
 * The assignment operator
 */
//------------------------------------------------------------------------------
XyPlot& XyPlot::operator=(const XyPlot& orig)
{
   if (this == &orig)
      return *this;
   
   Subscriber::operator=(orig);
   
   mXParam = orig.mXParam;
   mYParams = orig.mYParams;
   
   mNumXParams = orig.mNumXParams;
   mNumYParams = orig.mNumYParams;
   
   mXParamName = orig.mXParamName;
   mYParamNames = orig.mYParamNames;
   
   mAllParamNames = orig.mAllParamNames;
   
   mOldName = orig.mOldName;
   mPlotTitle = orig.mPlotTitle;
   mXAxisTitle = orig.mXAxisTitle;
   mYAxisTitle = orig.mYAxisTitle;
   mDrawGrid = orig.mDrawGrid;
   mIsXyPlotWindowSet = orig.mIsXyPlotWindowSet;
   
   mDataCollectFrequency = orig.mDataCollectFrequency;
   mUpdatePlotFrequency = orig.mUpdatePlotFrequency;
   
   mNumDataPoints = orig.mNumDataPoints;
   mNumCollected = orig.mNumCollected;
   
   useMarkers = orig.useMarkers;
   drawing = orig.drawing;
   breakCount = orig.breakCount;

   return *this;
}


//------------------------------------------------------------------------------
// ~XyPlot(void)
//------------------------------------------------------------------------------
XyPlot::~XyPlot()
{
}

//------------------------------------------------------------------------------
// bool SetXParameter(const std::string &paramName)
//------------------------------------------------------------------------------
bool XyPlot::SetXParameter(const std::string &paramName)
{
   if (paramName != "")
   {
      mXParamName = paramName;
      mNumXParams = 1; //loj: only 1 X parameter for now
      return true;
   }
   
   return false;
}


//------------------------------------------------------------------------------
// bool AddYParameter(const std::string &paramName, Integer index)
//------------------------------------------------------------------------------
bool XyPlot::AddYParameter(const std::string &paramName, Integer index)
{
   #if DEBUG_TSPLOT_PARAM
   MessageInterface::ShowMessage("XyPlot::AddYParameter() name = %s\n",
                                 paramName.c_str());
   #endif
   
   if (paramName != "" && index == mNumYParams)
   {
      // if paramName not found, add (based on loj fix to XYPlot)
      if (find(mYParamNames.begin(), mYParamNames.end(), paramName) ==
          mYParamNames.end())
      {
         mYParamNames.push_back(paramName);
         mNumYParams = mYParamNames.size();
         mYParams.push_back(NULL);
         return true;
      }
   }

   return false;
}

//----------------------------------
// methods inherited from Subscriber
//----------------------------------

//------------------------------------------------------------------------------
// virtual bool Initialize()
//------------------------------------------------------------------------------
bool XyPlot::Initialize()
{
   if (GmatGlobal::Instance()->GetRunMode() == GmatGlobal::TESTING_NO_PLOTS)
      return true;
   
   #if DEBUG_TSPLOT_INIT
   MessageInterface::ShowMessage
      ("XyPlot::Initialize() active=%d, mNumYParams=%d\n", active, mNumYParams);
   #endif
   
   // Check if there are parameters selected for XyPlot
   if (active)
   {
      if (mNumXParams == 0 || mNumYParams == 0)
      {
         active = false;
         MessageInterface::PopupMessage
            (Gmat::WARNING_,
             "*** WARNING *** The XYPlot named \"%s\" will not be shown.\n"
             "No parameters were selected for X Axis or Y Axis.\n",
             GetName().c_str());
         return false;
      }
      
      if (mXParam == NULL || mYParams[0] == NULL)
      {
         active = false;
         MessageInterface::PopupMessage
            (Gmat::WARNING_,
             "*** WARNING *** The XYPlot named \"%s\" will not be shown.\n"
             "The first parameter selected for X Axis or Y Axis is NULL\n",
             GetName().c_str());
         return false;
      }
   }
   
   Subscriber::Initialize();
   
   bool status = false;
   DeletePlotCurves();
   
   if (active)
   {
      // build plot title
      BuildPlotTitle();
      
      // Create XyPlotWindow, if not exist
      #if DEBUG_TSPLOT_INIT
      MessageInterface::ShowMessage
         ("XyPlot::Initialize() calling CreateXyPlotWindow()\n");
      #endif
      
      PlotInterface::CreateXyPlotWindow(instanceName, mOldName, mPlotTitle,
                                        mXAxisTitle, mYAxisTitle, mDrawGrid);
      
      PlotInterface::SetXyPlotTitle(instanceName, mPlotTitle);
      mIsXyPlotWindowSet = true;
      
      // add to Y params to XyPlotWindow
      //loj: temp code
      int yOffset = 0; //loj: I don't know how this is used
      Real yMin = -40000.0; //loj: should parameter provide minimum value?
      Real yMax =  40000.0; //loj: should parameter provide maximum value?
      
      #if DEBUG_TSPLOT_INIT
      MessageInterface::ShowMessage
         ("XyPlot::Initialize() Get curveTitle and penColor\n");
      #endif
      
      for (int i=0; i<mNumYParams; i++)
      {
         std::string curveTitle = mYParams[i]->GetName();
         UnsignedInt penColor = mYParams[i]->GetUnsignedIntParameter("Color");
            
         #if DEBUG_TSPLOT_INIT
         MessageInterface::ShowMessage("XyPlot::Initialize() curveTitle = %s\n",
                                       curveTitle.c_str());
         #endif
         
         PlotInterface::AddXyPlotCurve(instanceName, i, yOffset, yMin, yMax,
                                       curveTitle, penColor);
      }
      
      PlotInterface::ShowXyPlotLegend(instanceName);
      status = true;
      
      #if DEBUG_TSPLOT_INIT
      MessageInterface::ShowMessage("XyPlot::Initialize() calling ClearXyPlotData()\n");
      #endif
      
      PlotInterface::ClearXyPlotData(instanceName);
      PlotInterface::XyPlotCurveSettings(instanceName, useLines, lineWidth, 100,
            useMarkers, markerSize, -1);
   }
   else
   {
      #if DEBUG_TSPLOT_INIT
      MessageInterface::ShowMessage("XyPlot::Initialize() DeleteXyPlot()\n");
      #endif
      
      status =  PlotInterface::DeleteXyPlot(instanceName);
   }
   
   #if DEBUG_TSPLOT_INIT
   MessageInterface::ShowMessage("XyPlot::Initialize() leaving stauts=%d\n", status);
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
 * This method returns a clone of the XyPlot.
 *
 * @return clone of the XyPlot.
 *
 */
//------------------------------------------------------------------------------
GmatBase* XyPlot::Clone() const
{
   return (new XyPlot(*this));
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
void XyPlot::Copy(const GmatBase* orig)
{
   operator=(*((XyPlot *)(orig)));
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
bool XyPlot::SetName(const std::string &who, const std::string &oldName)
{
   #if DEBUG_RENAME
   MessageInterface::ShowMessage("XyPlot::SetName() newName=%s\n", who.c_str());
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
bool XyPlot::TakeAction(const std::string &action,
                        const std::string &actionData)
{
   #if DEBUG_ACTION_REMOVE
   MessageInterface::ShowMessage("XyPlot::TakeAction() action=%s, actionData=%s\n",
                                 action.c_str(), actionData.c_str());
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
   else if (action == "MarkPoint")
   {
      return MarkPoint();
   }
   else if (action == "MarkBreak")
   {
      return MarkBreak();
   }
   else if (action == "ClearFromBreak")
   {
      return ClearFromBreak();
   }
   else if (action == "Darken")
   {
      std::stringstream data;
      data << actionData;
      Integer factor;
      data >> factor;
      return Darken(factor);
   }
   else if (action == "Lighten")
   {
      std::stringstream data;
      data << actionData;
      Integer factor;
      data >> factor;
      return Lighten(factor);
   }
   // Add color change and marker change here(?)
   
   return false;
}


//---------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//---------------------------------------------------------------------------
bool XyPlot::RenameRefObject(const Gmat::ObjectType type,
                             const std::string &oldName,
                             const std::string &newName)
{
   #if DEBUG_RENAME
   MessageInterface::ShowMessage
      ("XyPlot::RenameRefObject() type=%s, oldName=%s, newName=%s\n",
       GetObjectTypeString(type).c_str(), oldName.c_str(), newName.c_str());
   #endif
   
   if (type != Gmat::PARAMETER && type != Gmat::COORDINATE_SYSTEM &&
       type != Gmat::SPACECRAFT)
      return true;
   
   if (type == Gmat::PARAMETER)
   {
      // X parameter
      if (mXParamName == oldName)
         mXParamName = newName;
   
      // Y parameters
      for (unsigned int i=0; i<mYParamNames.size(); i++)
      {
         if (mYParamNames[i] == oldName)
            mYParamNames[i] = newName;
      }
   }
   else
   {
      std::string::size_type pos = mXParamName.find(oldName);
      
      if (pos != mXParamName.npos)
         mXParamName.replace(pos, oldName.size(), newName);
      
      for (unsigned int i=0; i<mYParamNames.size(); i++)
      {
         pos = mYParamNames[i].find(oldName);
         
         if (pos != mYParamNames[i].npos)
            mYParamNames[i].replace(pos, oldName.size(), newName);
      }
   }
   
   return true;
}


//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
std::string XyPlot::GetParameterText(const Integer id) const
{
   if (id >= SubscriberParamCount && id < XyPlotParamCount)
      return PARAMETER_TEXT[id - SubscriberParamCount];
   else
      return Subscriber::GetParameterText(id);
    
}

//------------------------------------------------------------------------------
// Integer GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
Integer XyPlot::GetParameterID(const std::string &str) const
{
   for (int i=SubscriberParamCount; i<XyPlotParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - SubscriberParamCount])
      {
         switch (i)
            case IND_VAR:
            case ADD:
            case DRAW_GRID:
               WriteDeprecatedMessage(i);
         return i;
      }
   }
   // deprecated labels
   
   return Subscriber::GetParameterID(str);
}


//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
Gmat::ParameterType XyPlot::GetParameterType(const Integer id) const
{
   if (id >= SubscriberParamCount && id < XyPlotParamCount)
      return PARAMETER_TYPE[id - SubscriberParamCount];
   else
      return Subscriber::GetParameterType(id);
}


//------------------------------------------------------------------------------
// std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
std::string XyPlot::GetParameterTypeString(const Integer id) const
{
   if (id >= SubscriberParamCount && id < XyPlotParamCount)
      return GmatBase::PARAM_TYPE_STRING[GetParameterType(id - SubscriberParamCount)];
   else
      return Subscriber::GetParameterTypeString(id);
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
bool XyPlot::IsParameterReadOnly(const Integer id) const
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
       (id == MARKER_SIZE)            ||
       (id == DRAWING)                ||
       (id == IND_VAR)                ||
       (id == ADD)                    ||
       (id == DRAW_GRID)            
      )
      return true;
   
   return Subscriber::IsParameterReadOnly(id);
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer XyPlot::GetIntegerParameter(const Integer id) const
{
   switch (id)
   {
   case DATA_COLLECT_FREQUENCY:
      return mDataCollectFrequency;
   case UPDATE_PLOT_FREQUENCY:
      return mUpdatePlotFrequency;
   case LINE_WIDTH:
      return lineWidth;
   case MARKER_SIZE:
      return markerSize;
   default:
      return Subscriber::GetIntegerParameter(id);
   }
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerParameter(const std::string &label) const
//------------------------------------------------------------------------------
Integer XyPlot::GetIntegerParameter(const std::string &label) const
{
   return GetIntegerParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// virtual Integer SetIntegerParameter(const Integer id, const Integer value)
//------------------------------------------------------------------------------
Integer XyPlot::SetIntegerParameter(const Integer id, const Integer value)
{
   switch (id)
   {
   case DATA_COLLECT_FREQUENCY:
      mDataCollectFrequency = value;
      return value;
   case UPDATE_PLOT_FREQUENCY:
      mUpdatePlotFrequency = value;
      return value;
   case LINE_WIDTH:
      lineWidth = value;
      return value;
   case MARKER_SIZE:
      markerSize = value;
      return value;
   default:
      return Subscriber::SetIntegerParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// virtual Integer SetIntegerParameter(const std::string &label,
//                                     const Integer value)
//------------------------------------------------------------------------------
Integer XyPlot::SetIntegerParameter(const std::string &label,
                                    const Integer value)
{
   return SetIntegerParameter(GetParameterID(label), value);
}


//---------------------------------------------------------------------------
//  std::string GetOnOffParameter(const Integer id) const
//---------------------------------------------------------------------------
std::string XyPlot::GetOnOffParameter(const Integer id) const
{
   switch (id)
   {
   case DRAW_GRID:
      WriteDeprecatedMessage(id);
      if (mDrawGrid)
         return "On";
      else
         return "Off";
   default:
      return Subscriber::GetOnOffParameter(id);
   }
}


//------------------------------------------------------------------------------
// std::string XyPlot::GetOnOffParameter(const std::string &label) const
//------------------------------------------------------------------------------
std::string XyPlot::GetOnOffParameter(const std::string &label) const
{
   return GetOnOffParameter(GetParameterID(label));
}


//---------------------------------------------------------------------------
//  bool SetOnOffParameter(const Integer id, const std::string &value)
//---------------------------------------------------------------------------
bool XyPlot::SetOnOffParameter(const Integer id, const std::string &value)
{
   switch (id)
   {
   case DRAW_GRID:
      WriteDeprecatedMessage(id);
      mDrawGrid = value == "On";
      return true;
   default:
      return Subscriber::SetOnOffParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// bool SetOnOffParameter(const std::string &label, const std::string &value)
//------------------------------------------------------------------------------
bool XyPlot::SetOnOffParameter(const std::string &label, const std::string &value)
{
   return SetOnOffParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string XyPlot::GetStringParameter(const Integer id) const
{
   switch (id)
   {
   case IND_VAR:
      WriteDeprecatedMessage(id);
   case XVARIABLE:
      return mXParamName;
   case PLOT_TITLE:
      return mPlotTitle;
   case X_AXIS_TITLE:
      return mXAxisTitle;
   case Y_AXIS_TITLE:
      return mYAxisTitle;
   default:
      return Subscriber::GetStringParameter(id);
   }
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label) const
//------------------------------------------------------------------------------
std::string XyPlot::GetStringParameter(const std::string &label) const
{
   #if DEBUG_XY_PARAM
   MessageInterface::ShowMessage("XyPlot::GetStringParameter() label = %s\n",
                                 label.c_str());
   #endif
   
   return GetStringParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
bool XyPlot::SetStringParameter(const Integer id, const std::string &value)
{
   #if DEBUG_TSPLOT_PARAM
   MessageInterface::ShowMessage("XyPlot::SetStringParameter() id = %d, "
                                 "value = %s \n", id, value.c_str());
   #endif
   
   switch (id)
   {
   case IND_VAR:
      WriteDeprecatedMessage(id);
   case XVARIABLE:
      return SetXParameter(value);
   case ADD:
      WriteDeprecatedMessage(id);
   case YVARIABLES:
      return AddYParameter(value, mNumYParams);
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
      return Subscriber::SetStringParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label,
//                         const std::string &value)
//------------------------------------------------------------------------------
bool XyPlot::SetStringParameter(const std::string &label,
                                const std::string &value)
{
   #if DEBUG_TSPLOT_PARAM
   MessageInterface::ShowMessage("XyPlot::SetStringParameter() label = %s, "
                                 "value = %s \n", label.c_str(), value.c_str());
   #endif
   
   return SetStringParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// virtual bool SetStringParameter(const Integer id, const std::string &value,
//                                 const Integer index)
//------------------------------------------------------------------------------
bool XyPlot::SetStringParameter(const Integer id, const std::string &value,
                                const Integer index)
{
   switch (id)
   {
   case ADD:
      WriteDeprecatedMessage(id);
   case YVARIABLES:
      return AddYParameter(value, index);
   default:
      return Subscriber::SetStringParameter(id, value, index);
   }
}


//------------------------------------------------------------------------------
// virtual bool SetStringParameter(const std::string &label,
//                                 const std::string &value,
//                                 const Integer index)
//------------------------------------------------------------------------------
bool XyPlot::SetStringParameter(const std::string &label,
                                const std::string &value,
                                const Integer index)
{
   #if DEBUG_TSPLOT_PARAM
   MessageInterface::ShowMessage
      ("XyPlot::SetStringParameter() label=%s, value=%s, index=%d \n",
       label.c_str(), value.c_str(), index);
   #endif
   
   return SetStringParameter(GetParameterID(label), value, index);
}


//------------------------------------------------------------------------------
// const StringArray& GetStringArrayParameter(const Integer id) const
//------------------------------------------------------------------------------
const StringArray& XyPlot::GetStringArrayParameter(const Integer id) const
{
   switch (id)
   {
   case ADD:
      WriteDeprecatedMessage(id);
   case YVARIABLES:
      return mYParamNames;
   default:
      return Subscriber::GetStringArrayParameter(id);
   }
}


//------------------------------------------------------------------------------
// StringArray& GetStringArrayParameter(const std::string &label) const
//------------------------------------------------------------------------------
const StringArray& XyPlot::GetStringArrayParameter(const std::string &label) const
{
   return GetStringArrayParameter(GetParameterID(label));
}


bool XyPlot::GetBooleanParameter(const Integer id) const
{
   if (id == SHOW_PLOT)
      return active;
   if (id == USE_MARKERS)
      return useMarkers;
   if (id == USE_LINES)
      return useLines;
   if (id == SHOW_GRID)
      return mDrawGrid;
   if (id == DRAWING)
      return drawing;
   return Subscriber::GetBooleanParameter(id);
}

bool XyPlot::GetBooleanParameter(const std::string &label) const
{
   return GetBooleanParameter(GetParameterID(label));
}

bool XyPlot::SetBooleanParameter(const std::string &label, const bool value)
{
   return SetBooleanParameter(GetParameterID(label), value);
}

bool XyPlot::SetBooleanParameter(const Integer id, const bool value)
{
   if (id == SHOW_PLOT)
   {
      active = value;
      return active;
   }
   if (id == USE_MARKERS)
   {
      useMarkers = value;
      // Always have to have either markers or lines
      if (useMarkers == false)
         useLines = true;
      return useMarkers;
   }
   if (id == USE_LINES)
   {
      useLines = value;
      if (useLines == false)
         useMarkers = true;
      return useLines;
   }
   if (id == SHOW_GRID)
   {
      mDrawGrid = value;
      return true;
   }
   return Subscriber::SetBooleanParameter(id, value);
}

//------------------------------------------------------------------------------
// virtual GmatBase* GetRefObject(const Gmat::ObjectType type,
//                                const std::string &name)
//------------------------------------------------------------------------------
GmatBase* XyPlot::GetRefObject(const Gmat::ObjectType type,
                               const std::string &name)
{
   // if name is X parameter
   if (name == mXParamName)
   {
      return mXParam;
   }
   else
   {
      // name is Y parameter
      for (int i=0; i<mNumYParams; i++)
      {
         if (mYParamNames[i] == name)
            return mYParams[i];
      }
   }
   
   throw GmatBaseException("XyPlot::GetRefObject() the object name: " + name +
                           "not found\n");
}


//------------------------------------------------------------------------------
// virtual bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
//                           const std::string &name = "")
//------------------------------------------------------------------------------
bool XyPlot::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                          const std::string &name)
{
   if (type == Gmat::PARAMETER)
   {
      // X parameter
      if (name == mXParamName)
      {
         mXParam = (Parameter*)obj;
         
         if (!mXParam->IsPlottable())
               throw SubscriberException
                  ("The X parameter: " + name + " of " + instanceName +
                   " is not plottable\n");
         
         #if DEBUG_TSPLOT_OBJECT
         MessageInterface::ShowMessage
            ("XyPlot::SetRefObject() mXParam:%s successfully set\n",
             obj->GetName().c_str());
         #endif
      }
      
      // Y parameters
      for (int i=0; i<mNumYParams; i++)
      {
         if (mYParamNames[i] == name)
         {
            mYParams[i] = (Parameter*)obj;
            
            if (!mYParams[i]->IsPlottable())
            {
               throw SubscriberException
                  ("The Y parameter: " + name + " of " + instanceName +
                   " is not plottable\n");
            }
            
            #if DEBUG_TSPLOT_OBJECT
            MessageInterface::ShowMessage
               ("XyPlot::SetRefObject() mYParams[%s] successfully set\n",
                obj->GetName().c_str());
            #endif
            
            return true;
         }
      }
   }
   
   return false;
}


//------------------------------------------------------------------------------
// virtual bool HasRefObjectTypeArray()
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
bool XyPlot::HasRefObjectTypeArray()
{
   return true;
}


//------------------------------------------------------------------------------
// const ObjectTypeArray& GetRefObjectTypeArray()
//------------------------------------------------------------------------------
/**
 * Retrieves the list of ref object types used by this class.
 *
 * @return the list of object types.
 * 
 */
//------------------------------------------------------------------------------
const ObjectTypeArray& XyPlot::GetRefObjectTypeArray()
{
   refObjectTypes.clear();
   refObjectTypes.push_back(Gmat::PARAMETER);
   return refObjectTypes;
}


//------------------------------------------------------------------------------
// virtual const StringArray& GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
const StringArray& XyPlot::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   mAllParamNames.clear();
   
   switch (type)
   {
   case Gmat::UNKNOWN_OBJECT:
   case Gmat::PARAMETER:
      // add x parameter
      if (mXParamName != "")
         mAllParamNames.push_back(mXParamName);
      
      // add y parameters
      for (int i=0; i<mNumYParams; i++)
         if (mYParamNames[i] != "")
            mAllParamNames.push_back(mYParamNames[i]);
      break;
   default:
      break;
   }
   
   return mAllParamNames;
}

//---------------------------------
// protected methods
//---------------------------------

//------------------------------------------------------------------------------
// void BuildPlotTitle()
//------------------------------------------------------------------------------
void XyPlot::BuildPlotTitle()
{
   //set X and Y axis title
   //if (mXAxisTitle == "")
   //{
      if (mXParam)
         mXAxisTitle = mXParam->GetName();
      else {
         mXAxisTitle = "No X parameters";
         mYAxisTitle = "empty";
         mPlotTitle  = "Plot not fully initialized";
         return;
      }
      //}
      
   #if DEBUG_TSPLOT_INIT
   MessageInterface::ShowMessage("XyPlot::BuildPlotTitle() mXAxisTitle = %s\n",
                                 mXAxisTitle.c_str());
   #endif
   
   mYAxisTitle = "";
   for (int i= 0; i<mNumYParams-1; i++)
   {
      mYAxisTitle += (mYParams[i]->GetName() + ", ");
   }
   mYAxisTitle += mYParams[mNumYParams-1]->GetName();
   
   #if DEBUG_TSPLOT_INIT
   MessageInterface::ShowMessage("XyPlot::BuildPlotTitle() mYAxisTitle = %s\n",
                                 mYAxisTitle.c_str());
   #endif
   
   mPlotTitle = "(" + mXAxisTitle + ")" + " vs " + "(" + mYAxisTitle + ")";
   
   #if DEBUG_TSPLOT_INIT
   MessageInterface::ShowMessage("XyPlot::BuildPlotTitle() mPlotTitle = %s\n",
                                 mPlotTitle.c_str());
   #endif
}

//------------------------------------------------------------------------------
// bool ClearYParameters()
//------------------------------------------------------------------------------
bool XyPlot::ClearYParameters()
{
   DeletePlotCurves();
   mYParams.clear();
   mYParamNames.clear();
   mNumYParams = 0;
   mPlotTitle = "";
   mXAxisTitle = "";
   mYAxisTitle = "";
   mIsXyPlotWindowSet = false;
   return true;
}

//------------------------------------------------------------------------------
// bool RemoveYParameter(const std::string &name)
//------------------------------------------------------------------------------
/*
 * Removes parameter from the Y parameter list
 *
 * @param <name> parameter name to be removed from the Y parameter list
 *
 * @return true if parameter was removed from the Y parameter list, false otherwise
 *
 */
//------------------------------------------------------------------------------
bool XyPlot::RemoveYParameter(const std::string &name)
{
   #if DEBUG_ACTION_REMOVE
   MessageInterface::ShowMessage
      ("XyPlot::RemoveYParameter() name=%s\n--- Before remove:\n", name.c_str());
   for (int i=0; i<mNumYParams; i++)
   {
      MessageInterface::ShowMessage("mYParamNames[%d]=%s\n", i,
                                    mYParamNames[i].c_str());
   }
   #endif
   
   StringArray::iterator pos1;
   std::vector<Parameter*>::iterator pos2 = mYParams.begin();

   for (pos1 = mYParamNames.begin(); pos1 != mYParamNames.end(); pos1++)
   {
      if (*pos1 == name)
      {
         mYParamNames.erase(pos1);
         mYParams.erase(pos2);
         mNumYParams = mYParamNames.size();
         
         #if DEBUG_ACTION_REMOVE
         MessageInterface::ShowMessage("---After remove\n");
         for (int i=0; i<mNumYParams; i++)
         {
            MessageInterface::ShowMessage("mYParamNames[%d]=%s\n", i,
                                          mYParamNames[i].c_str());
         }
         #endif
         
         return true;
      }
      else
      {
         advance(pos2, 1);
      }
   }
   
   //------------------------------------------
   // loj: 9/29/04
   // Need to remove from PlotCurves also
   //------------------------------------------
   
   #if DEBUG_ACTION_REMOVE
   MessageInterface::ShowMessage("XyPlot::RemoveYParameter() name=%s not found\n");
   #endif
   
   return false;
}

//------------------------------------------------------------------------------
// bool ResetYParameters()
//------------------------------------------------------------------------------
bool XyPlot::ResetYParameters()
{
   PlotInterface::ClearXyPlotData(instanceName);
   return true;
}

//------------------------------------------------------------------------------
// bool PenUp()
//------------------------------------------------------------------------------
bool XyPlot::PenUp()
{
   PlotInterface::XyPlotPenUp(instanceName);
   drawing = false;
   return true;
}

//------------------------------------------------------------------------------
// bool PenDown()
//------------------------------------------------------------------------------
bool XyPlot::PenDown()
{
   PlotInterface::XyPlotPenDown(instanceName);
   drawing = true;
   return true;
}

//------------------------------------------------------------------------------
// bool MarkPoint()
//------------------------------------------------------------------------------
/**
 * Places an X marker at the current point on all curves in a plot
 * @return true on success
 */
//------------------------------------------------------------------------------
bool XyPlot::MarkPoint()
{
   PlotInterface::XyPlotMarkPoint(instanceName);
   return true;
}

//------------------------------------------------------------------------------
// bool Darken(Integer factor)
//------------------------------------------------------------------------------
/**
 * Darkens the curves on a plot starting at the current position
 *
 * @param factor The darkening factor
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool XyPlot::Darken(Integer factor)
{
   PlotInterface::XyPlotDarken(instanceName, factor);
   return true;
}

//------------------------------------------------------------------------------
// bool Lighten(Integer factor)
//------------------------------------------------------------------------------
/**
 * Lightens the curves on a plot starting at the current position
 *
 * @param factor The lightening factor
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool XyPlot::Lighten(Integer factor)
{
   PlotInterface::XyPlotLighten(instanceName, factor);
   return true;
}


//------------------------------------------------------------------------------
// bool MarkBreak()
//------------------------------------------------------------------------------
/**
 * Sets a break point on all active curves
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool XyPlot::MarkBreak()
{
   if (mSolverIterOption == SI_CURRENT)
      PlotInterface::XyPlotMarkBreak(instanceName);
   return true;
}

//------------------------------------------------------------------------------
// bool ClearFromBreak()
//------------------------------------------------------------------------------
/**
 * Deletes all data after the most recent break point 
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool XyPlot::ClearFromBreak()
{
   if (mSolverIterOption == SI_CURRENT)
      PlotInterface::XyPlotClearFromBreak(instanceName);
   return true;
}


//------------------------------------------------------------------------------
// void DeletePlotCurves()
//------------------------------------------------------------------------------
void XyPlot::DeletePlotCurves()
{
   // delete exiting curves
   PlotInterface::DeleteAllXyPlotCurves(instanceName, mOldName);
}


// methods inherited from Subscriber
//------------------------------------------------------------------------------
// bool Distribute(int len)
//------------------------------------------------------------------------------
bool XyPlot::Distribute(int len)
{
   //loj: How do I convert data to Real data?
   return false;
}

//------------------------------------------------------------------------------
// bool Distribute(const Real * dat, Integer len)
//------------------------------------------------------------------------------
bool XyPlot::Distribute(const Real * dat, Integer len)
{
   if (GmatGlobal::Instance()->GetRunMode() == GmatGlobal::TESTING_NO_PLOTS)
      return true;
   
   #if DEBUG_TSPLOT_UPDATE > 1
   MessageInterface::ShowMessage
      ("XyPlot::Distribute() entered. isEndOfReceive=%d, active=%d, runState=%d\n",
       isEndOfReceive, active, runstate);
   #endif
   
   if (isEndOfReceive)
   {
      // if targetting and draw target is None, just return
      if (mSolverIterations == "None" &&
          ((runstate == Gmat::TARGETING) || (runstate == Gmat::OPTIMIZING) ||
                (runstate == Gmat::SOLVING)))
         return true;
      
      if (active)
         return PlotInterface::RefreshXyPlot(instanceName);
   }
   
   // if targeting and draw target is None, just return
   if (mSolverIterations == "None" &&
       ((runstate == Gmat::TARGETING) || (runstate == Gmat::OPTIMIZING) ||
             (runstate == Gmat::SOLVING)))
      return true;
   
   if (len > 0)
   {
      if (mXParam != NULL && mNumYParams > 0)
      {
         // get x param
         Real xval = mXParam->EvaluateReal();

         #if DEBUG_TSPLOT_UPDATE
         MessageInterface::ShowMessage("XyPlot::Distribute() xval = %f\n", xval);
         #endif
         
         // get y params
         Rvector yvals = Rvector(mNumYParams);
         
         // put yvals in the order of parameters added
         for (int i=0; i<mNumYParams; i++)
         {
            if (mYParams[i] == NULL)
            {
               MessageInterface::PopupMessage
                  (Gmat::WARNING_,
                   "*** WARNING *** The XYPlot named \"%s\" will not be shown.\n"
                   "The parameter selected for Y Axis is NULL\n",
                   GetName().c_str());
               return true;
            }
            
            yvals[i] = mYParams[i]->EvaluateReal();
            
            #if DEBUG_TSPLOT_UPDATE
            MessageInterface::ShowMessage
               ("XyPlot::Distribute() yvals[%d] = %f\n", i, yvals[i]);
            #endif
         }
         
         // update xy plot
         // X value must start from 0
         if (mIsXyPlotWindowSet)
         {
            mNumDataPoints++;
            
            if ((mNumDataPoints % mDataCollectFrequency) == 0)
            {
               mNumDataPoints = 0;
               mNumCollected++;
               bool update = (mNumCollected % mUpdatePlotFrequency) == 0;
               
               #if DEBUG_TSPLOT_UPDATE > 1
               MessageInterface::ShowMessage
                  ("XyPlot::Distribute() calling PlotInterface::UpdateXyPlot()\n");
               #endif
               
               return PlotInterface::UpdateXyPlot(instanceName, mOldName, xval,
                     yvals, mPlotTitle, mXAxisTitle, mYAxisTitle,
                     mSolverIterOption, update, mDrawGrid);
               if (update)
                  mNumCollected = 0;
            }
         }
      }
   }
   
   //loj: always return true otherwise next subscriber will not call ReceiveData()
   //     in Publisher::Publish()
   return true;
}

//------------------------------------------------------------------------------
// void WriteDeprecatedMessage(Integer id) const
//------------------------------------------------------------------------------
void XyPlot::WriteDeprecatedMessage(Integer id) const
{
   // Write only one message per session
   static bool writeXVariable = true;
   static bool writeYVariables = true;
   static bool writeShowGrid = true;
   
   switch (id)
   {
   case IND_VAR:
      if (writeXVariable)
      {
         MessageInterface::ShowMessage
            (deprecatedMessageFormat.c_str(), "IndVar", GetName().c_str(),
             "XVariable");
         writeXVariable = false;
      }
      break;
   case ADD:
      if (writeYVariables)
      {
         MessageInterface::ShowMessage
            (deprecatedMessageFormat.c_str(), "Add", GetName().c_str(),
             "YVariables");
         writeYVariables = false;
      }
      break;
   case DRAW_GRID:
      if (writeShowGrid)
      {
         MessageInterface::ShowMessage
            (deprecatedMessageFormat.c_str(), "Grid", GetName().c_str(),
             "ShowGrid");
         writeShowGrid = false;
      }
      break;
   default:
      break;
   }
}

