//$Header$
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
#include "Moderator.hpp"         // for GetParameter()
#include "MessageInterface.hpp"  // for ShowMessage()

//#define DEBUG_XYPLOT_INIT 1
//#define DEBUG_XYPLOT_PARAM 1
//#define DEBUG_XYPLOT_UPDATE 1
//#define DEBUG_ACTION_REMOVE 1

//---------------------------------
// static data
//---------------------------------

const std::string
XyPlot::PARAMETER_TEXT[XyPlotParamCount - SubscriberParamCount] =
{
   "IndVar",
   "Add",
   "PlotTitle",
   "XAxisTitle",
   "YAxisTitle",
   "Grid",
   "TargetStatus",
   "DataCollectFrequency",
   "UpdatePlotFrequency",
}; 

const Gmat::ParameterType
XyPlot::PARAMETER_TYPE[XyPlotParamCount - SubscriberParamCount] =
{
   Gmat::STRING_TYPE,
   Gmat::STRINGARRAY_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::INTEGER_TYPE,
   Gmat::INTEGER_TYPE,
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
               const std::string &xAxisTitle, const std::string &yAxisTitle,
               bool drawGrid) :
   Subscriber("XYPlot", name) //loj: 10/28/04 Changed from XyPlot
{
   // GmatBase data
   parameterCount = XyPlotParamCount;
    
   mDrawGrid = drawGrid;
   mDrawTarget = false;
   mNumYParams = 0;

   mXParamName = "";
   mNumXParams = 0;
    
   mXParam = xParam;
   if (firstYParam != NULL)
      AddYParameter(firstYParam->GetName(), mNumYParams);

   mPlotTitle = plotTitle;
   mXAxisTitle = xAxisTitle;
   mYAxisTitle = yAxisTitle;
   
   mIsXyPlotWindowSet = false;
   mDataCollectFrequency = 1;
   mUpdatePlotFrequency = 10;
}

//------------------------------------------------------------------------------
// XyPlot(const XyPlot &copy)
//------------------------------------------------------------------------------
XyPlot::XyPlot(const XyPlot &copy) :
   Subscriber(copy)
{
   mXParam = copy.mXParam;
   mYParams = copy.mYParams; //loj: 6/4/04 remove this later
   mYParamMap = copy.mYParamMap;
   
   mNumXParams = copy.mNumXParams;
   mNumYParams = copy.mNumYParams;

   mXParamName = copy.mXParamName;
   mYParamNames = copy.mYParamNames;
    
   mPlotTitle = copy.mPlotTitle;
   mXAxisTitle = copy.mXAxisTitle;
   mYAxisTitle = copy.mYAxisTitle;
   mDrawGrid = copy.mDrawGrid;
   mDrawTarget = copy.mDrawTarget;
   mIsXyPlotWindowSet = copy.mIsXyPlotWindowSet;
    
   mDataCollectFrequency = copy.mDataCollectFrequency;
   mUpdatePlotFrequency = copy.mUpdatePlotFrequency;
    
   mNumDataPoints = copy.mNumDataPoints;
   mNumCollected = copy.mNumCollected;
}

//------------------------------------------------------------------------------
// ~XyPlot(void)
//------------------------------------------------------------------------------
XyPlot::~XyPlot(void)
{
}

//------------------------------------------------------------------------------
// bool SetXParameter(const std::string &paramName)
//------------------------------------------------------------------------------
bool XyPlot::SetXParameter(const std::string &paramName)
{
   //@todo Do not use Moderator
   
   bool status = false;
   Moderator *theModerator = Moderator::Instance();

   if (paramName != "")
   {
      mXParamName = paramName;
      mXParam = NULL;
      
      // get parameter pointer
      Parameter *param = theModerator->GetParameter(paramName);
      if (param != NULL)
      {
         mXParam = param;
         mNumXParams = 1; //loj: only 1 X parameter for now
      }
      status = true;
   }

   return status;
}

//------------------------------------------------------------------------------
// bool AddYParameter(const std::string &paramName, Integer index)
//------------------------------------------------------------------------------
bool XyPlot::AddYParameter(const std::string &paramName, Integer index)
{
   bool status = false;
   Moderator *theModerator = Moderator::Instance();
    
   if (paramName != "" && index == mNumYParams)
   {
      mYParamNames.push_back(paramName);
      mYParamMap[paramName] = NULL;
      mNumYParams = mYParamNames.size();

      // get parameter pointer
      Parameter *param = theModerator->GetParameter(paramName);
      if (param != NULL)
      {
#if DEBUG_XYPLOT_PARAM
         MessageInterface::ShowMessage("XyPlot::AddYParameter() name = %s\n",
                                       param->GetName().c_str());
#endif
         mYParamMap[paramName] = param;
         mYParams.push_back(param);
      }
      status = true;
   }

   return status;
}

//----------------------------------
// methods inherited from Subscriber
//----------------------------------

//------------------------------------------------------------------------------
// virtual bool Initialize()
//------------------------------------------------------------------------------
bool XyPlot::Initialize()
{
   Subscriber::Initialize();

   //-----------------------------------
   //@todo
   // need to set Parameter pointers
   //-----------------------------------
   // implement this later

#if DEBUG_XYPLOT_INIT
   MessageInterface::ShowMessage("XyPlot::Initialize() active=%d\n", active);
#endif
   
   bool status = false;
   DeletePlotCurves();
   
   if (active)
   {
      // build plot title
      BuildPlotTitle();
      
      // Create XyPlotWindow, if not exist
#if DEBUG_XYPLOT_INIT
      MessageInterface::ShowMessage("XyPlot::Initialize() calling CreateXyPlotWindow()\n");
#endif
      PlotInterface::CreateXyPlotWindow(instanceName, mPlotTitle,
                                        mXAxisTitle, mYAxisTitle, mDrawGrid);
      
      PlotInterface::SetXyPlotTitle(instanceName, mPlotTitle);
      mIsXyPlotWindowSet = true; //loj: 5/12/04 Do I need this flag?
      
      // add to Y params to XyPlotWindow
      //loj: temp code
      int yOffset = 0; //loj: I don't know how this is used
      Real yMin = -40000.0; //loj: should parameter provide minimum value?
      Real yMax =  40000.0; //loj: should parameter provide maximum value?

      for (int i=0; i<mNumYParams; i++)
      {
         std::string curveTitle = mYParams[i]->GetName();
         UnsignedInt penColor = mYParams[i]->GetUnsignedIntParameter("Color");
         
#if DEBUG_XYPLOT_INIT
         MessageInterface::ShowMessage("XyPlot::Initialize() curveTitle = %s\n",
                                       curveTitle.c_str());
#endif
         PlotInterface::AddXyPlotCurve(instanceName, i, yOffset, yMin, yMax,
                                       curveTitle, penColor);
      }

      PlotInterface::ShowXyPlotLegend(instanceName); //loj: 7/14/04 added
      status = true;
        
#if DEBUG_XYPLOT_INIT
      MessageInterface::ShowMessage("XyPlot::Initialize() calling ClearXyPlotData()\n");
#endif
      PlotInterface::ClearXyPlotData(instanceName);
   }
   else
   {
      if (mIsXyPlotWindowSet)
      {        
         mIsXyPlotWindowSet = false;
         status = PlotInterface::DeleteXyPlot(true);
      }
      else
      {
         status = true;
      }
   }

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
GmatBase* XyPlot::Clone(void) const
{
   return (new XyPlot(*this));
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
   
   return false;
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
         return i;
   }
   
   return Subscriber::GetParameterID(str);
}

//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
Gmat::ParameterType XyPlot::GetParameterType(const Integer id) const
{
   if (id >= 0 && id < XyPlotParamCount)
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


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string XyPlot::GetStringParameter(const Integer id) const
{
   switch (id)
   {
   case IND_VAR:
      return mXParamName;
   case PLOT_TITLE:
      return mPlotTitle;
   case X_AXIS_TITLE:
      return mXAxisTitle;
   case Y_AXIS_TITLE:
      return mYAxisTitle;
   case DRAW_GRID:
      if (mDrawGrid)
         return "On";
      else
         return "Off";
   case TARGET_STATUS:
      if (mDrawTarget)
         return "On";
      else
         return "Off";
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
#if DEBUG_XYPLOT_PARAM
   MessageInterface::ShowMessage("XyPlot::SetStringParameter() id = %d, "
                                 "value = %s \n", id, value.c_str());
#endif
   switch (id)
   {
   case IND_VAR:
      return SetXParameter(value);
   case ADD:
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
   case DRAW_GRID:
      if (value == "On" || value == "Off")
      {
         mDrawGrid = (value == "On");
         return true;
      }
      else
      {
         return false;
      }
   case TARGET_STATUS:
      if (value == "On" || value == "Off")
      {
         mDrawTarget = (value == "On");
         return true;
      }
      else
      {
         return false;
      }
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
#if DEBUG_XYPLOT_PARAM
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
#if DEBUG_XYPLOT_PARAM
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

//---------------------------------
// protected methods
//---------------------------------

//------------------------------------------------------------------------------
// void BuildPlotTitle()
//------------------------------------------------------------------------------
void XyPlot::BuildPlotTitle()
{
   //set X and Y axis title
   if (mXAxisTitle == "")
      mXAxisTitle = mXParam->GetName();

#if DEBUG_XYPLOT_INIT
   MessageInterface::ShowMessage("XyPlot::BuildPlotTitle() mXAxisTitle = %s\n",
                                 mXAxisTitle.c_str());
#endif
   
   if (mYAxisTitle == "")
   {
      mYAxisTitle = "";
      for (int i= 0; i<mNumYParams-1; i++)
      {
         mYAxisTitle += (mYParams[i]->GetName() + ", ");
      }
      mYAxisTitle += mYParams[mNumYParams-1]->GetName();
   }
            
#if DEBUG_XYPLOT_INIT
   MessageInterface::ShowMessage("XyPlot::BuildPlotTitle() mYAxisTitle = %s\n",
                                 mYAxisTitle.c_str());
#endif
    
   if (mPlotTitle == "")
   {
      mPlotTitle = "(" + mXAxisTitle + ")" + " vs " + "(" + mYAxisTitle + ")";
   }
    
#if DEBUG_XYPLOT_INIT
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
   
   bool canRemove = false;
   StringArray::iterator pos1 =
      find(mYParamNames.begin(), mYParamNames.end(), name);
   std::vector<Parameter*>::iterator pos2 =
      (std::vector<Parameter*>::iterator)(mYParams.begin());
   
   if (pos1 != mYParamNames.end())
   {
      while (pos2 != (std::vector<Parameter*>::iterator)(mYParams.end()))
      {
         if ((*pos2)->GetName() != name)
         {
            ++pos2;
         }
         else
         {
            canRemove = true;
            break;
         }
      }
      
      if (canRemove)
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
         //------------------------------------------
         // loj: 9/29/04
         // Need to remove from PlotCurves also
         //------------------------------------------
         return true;
      }
   }
   
#if DEBUG_ACTION_REMOVE
   MessageInterface::ShowMessage("XyPlot::RemoveYParameter() name=%s not found\n");
#endif
   return false;
}

//------------------------------------------------------------------------------
// void DeletePlotCurves()
//------------------------------------------------------------------------------
void XyPlot::DeletePlotCurves()
{
   // delete exiting curves
   PlotInterface::DeleteAllXyPlotCurves(instanceName);
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
   //loj: 6/22/04 added if (isEndOfReceive)
   if (isEndOfReceive)
   {
      return PlotInterface::RefreshXyPlot(instanceName);
   }

   //loj: 8/6/04 added
   // if targetting and draw target is off, just return
   if (!mDrawTarget && (Publisher::Instance()->GetRunState() == Gmat::TARGETING))
      return true;
   
   if (len > 0)
   {
      if (mXParam != NULL && mNumYParams > 0)
      {
         // get x param
         Real xval = mXParam->EvaluateReal();

#if DEBUG_XYPLOT_UPDATE
         MessageInterface::ShowMessage("XyPlot::Distribute() xval = %f\n", xval);
#endif
         //xval = dat[0]; // loj: temp code to test XY plot dat[0] is time
         //MessageInterface::ShowMessage("XyPlot::Distribute() xval = %f\n", xval);
            
         // get y params
         Rvector yvals = Rvector(mNumYParams);

         // put yvals in the order of parameters added
         for (int i=0; i<mNumYParams; i++)
         {
            yvals[i] = mYParams[i]->EvaluateReal();
            
#if DEBUG_XYPLOT_UPDATE
            MessageInterface::ShowMessage
               ("XyPlot::Distribute() yvals[%d] = %f\n", i, yvals[i]);
#endif
            //yvals[i] = dat[1]; //loj: temp code to test XY plot dat[1] is pos X
            //MessageInterface::ShowMessage("XyPlot::Distribute() yvals = %f\n", yvals[i]);
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

               //MessageInterface::ShowMessage
               //   ("XyPlot::Distribute() calling PlotInterface::UpdateXyPlot()\n");
               
               return PlotInterface::UpdateXyPlot(instanceName, xval, yvals,
                                                  mPlotTitle, mXAxisTitle, mYAxisTitle,
                                                  update, mDrawGrid);
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

