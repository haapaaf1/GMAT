//$Header$
//------------------------------------------------------------------------------
//                                  OpenGlPlot
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2003/12/16
//
/**
 * Implements OpenGlPlot class.
 */
//------------------------------------------------------------------------------

#include "OpenGlPlot.hpp"
#include "PlotInterface.hpp"     // for UpdateGlSpacecraft()
#include "ColorTypes.hpp"        // for namespace GmatColor::
#include "Publisher.hpp"         // for Instance()
#include "MessageInterface.hpp"  // for ShowMessage()
#include <algorithm>             // for find(), distance()

#define DEBUG_OPENGL_INIT 0
#define DEBUG_OPENGL_PARAM 0
#define DEBUG_OPENGL_UPDATE 0
#define TEST_MULTI_SC 0

//---------------------------------
// static data
//---------------------------------
const std::string
OpenGlPlot::PARAMETER_TEXT[OpenGlPlotParamCount - SubscriberParamCount] =
{
   "Add",
   "SpacecraftList",
   "ClearSpacecraftList",
   "OrbitColor",
   "TargetColor",
   "Axis",
   "EquatorialPlane",
   "WireFrame",
   "TargetStatus",
   "DataCollectFrequency",
   "UpdatePlotFrequency"
}; 

const Gmat::ParameterType
OpenGlPlot::PARAMETER_TYPE[OpenGlPlotParamCount - SubscriberParamCount] =
{
   Gmat::STRING_TYPE,
   Gmat::STRINGARRAY_TYPE,
   Gmat::BOOLEAN_TYPE,
   Gmat::UNSIGNED_INT_TYPE,
   Gmat::UNSIGNED_INT_TYPE,
   Gmat::BOOLEAN_TYPE,
   Gmat::BOOLEAN_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::INTEGER_TYPE,
   Gmat::INTEGER_TYPE
};

//loj: 7/30/04 added
const UnsignedInt
OpenGlPlot::DEFAULT_ORBIT_COLOR[MAX_SC_COLOR] =
{
   GmatColor::RED32,     GmatColor::YELLOW32,  GmatColor::LIME32,
   GmatColor::AQUA32,    GmatColor::BLUE32,    GmatColor::FUCHSIA32,
   GmatColor::PINK32,    GmatColor::ORANGE32,  GmatColor::L_BLUE32,
   GmatColor::BEIGE32,   GmatColor::SILVER32,  GmatColor::GREEN32,
   GmatColor::L_BROWN32, GmatColor::PURPLE32,  GmatColor::NAVY32
};

//------------------------------------------------------------------------------
// OpenGlPlot(const std::string &name)
//------------------------------------------------------------------------------
OpenGlPlot::OpenGlPlot(const std::string &name) :
   Subscriber("OpenGlPlot", name)
{
   // GmatBase data
   parameterCount = OpenGlPlotParamCount;

   mDrawAxis = false;
   mDrawEquatorialPlane = true;
   mDrawWireFrame = false;
   mDrawTarget = false;
   mDataCollectFrequency = 1;
   mUpdatePlotFrequency = 10;
   mNumData = 0;
   mNumCollected = 0;
   mScNameArray.clear();
   mScXArray.clear();
   mScYArray.clear();
   mScZArray.clear();
   mOrbitColorArray.clear();
   mTargetColorArray.clear();
   mScCount = 0;
}

//------------------------------------------------------------------------------
// OpenGlPlot(const OpenGlPlot &ogl)
//------------------------------------------------------------------------------
OpenGlPlot::OpenGlPlot(const OpenGlPlot &ogl) :
   Subscriber(ogl)
{
   mDrawAxis = ogl.mDrawAxis;
   mDrawEquatorialPlane = ogl.mDrawEquatorialPlane;
   mDrawWireFrame = ogl.mDrawWireFrame;
   mDrawTarget = ogl.mDrawTarget;
   mDataCollectFrequency = ogl.mDataCollectFrequency;
   mUpdatePlotFrequency = ogl.mUpdatePlotFrequency;
   mScCount = ogl.mScCount;
   mScNameArray = ogl.mScNameArray;
   mScXArray = ogl.mScXArray;
   mScYArray = ogl.mScYArray;
   mScZArray = ogl.mScZArray;
   mOrbitColorArray = ogl.mOrbitColorArray;
   mTargetColorArray = ogl.mTargetColorArray;
   mOrbitColorMap = ogl.mOrbitColorMap;
   mTargetColorMap = ogl.mTargetColorMap;
   mNumData = 0;
   mNumCollected = 0;
}

//------------------------------------------------------------------------------
// ~OpenGlPlot(void)
//------------------------------------------------------------------------------
OpenGlPlot::~OpenGlPlot(void)
{
}

//----------------------------------
// inherited methods from Subscriber
//----------------------------------

//------------------------------------------------------------------------------
// virtual bool Initialize()
//------------------------------------------------------------------------------
bool OpenGlPlot::Initialize()
{
   Subscriber::Initialize();
   
#if DEBUG_OPENGL_INIT
   MessageInterface::ShowMessage("OpenGlPlot::Initialize() entered mScCount = %d\n",
                                 mScCount);
#endif
   if (mScCount > 0)
   {
      if (active)
      {
#if DEBUG_OPENGL_INIT
         MessageInterface::ShowMessage("OpenGlPlot::Initialize() CreateGlPlotWindow()\n");
#endif
         return PlotInterface::CreateGlPlotWindow(instanceName, mDrawWireFrame);
      }
      else
      {
#if DEBUG_OPENGL_INIT
         MessageInterface::ShowMessage("OpenGlPlot::Initialize() DeleteGlPlot()\n");
#endif
         return PlotInterface::DeleteGlPlot();
      }
   }
   else
   {
      active = false;
      MessageInterface::PopupMessage
         (Gmat::WARNING_, "OpenGlPlot is turned off. No spacecraft "
          "has been added to OpenGlPlot\n"); //loj: 6/25/04 added \n
      return false;
   }
}

//------------------------------------------------------------------------------
//  GmatBase* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the OpenGlPlot.
 *
 * @return clone of the OpenGlPlot.
 *
 */
//------------------------------------------------------------------------------
GmatBase* OpenGlPlot::Clone(void) const
{
   return (new OpenGlPlot(*this));
}

//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
std::string OpenGlPlot::GetParameterText(const Integer id) const
{
   if (id >= SubscriberParamCount && id < OpenGlPlotParamCount)
      return PARAMETER_TEXT[id - SubscriberParamCount];
   else
      return Subscriber::GetParameterText(id);
    
}

//------------------------------------------------------------------------------
// Integer GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
Integer OpenGlPlot::GetParameterID(const std::string &str) const
{
   for (int i=SubscriberParamCount; i<OpenGlPlotParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - SubscriberParamCount])
         return i;
   }
   
   return Subscriber::GetParameterID(str);
}

//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
Gmat::ParameterType OpenGlPlot::GetParameterType(const Integer id) const
{
   if (id >= SubscriberParamCount && id < OpenGlPlotParamCount)
      return PARAMETER_TYPE[id - SubscriberParamCount];
   else
      return Subscriber::GetParameterType(id);
}

//------------------------------------------------------------------------------
// std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
std::string OpenGlPlot::GetParameterTypeString(const Integer id) const
{
   if (id >= SubscriberParamCount && id < OpenGlPlotParamCount)
      return GmatBase::PARAM_TYPE_STRING[GetParameterType(id - SubscriberParamCount)];
   else
      return Subscriber::GetParameterTypeString(id);
}

//------------------------------------------------------------------------------
// bool GetBooleanParameter(const Integer id) const
//------------------------------------------------------------------------------
bool OpenGlPlot::GetBooleanParameter(const Integer id) const
{
   switch (id)
   {
   case DRAW_AXIS:
      return mDrawAxis;
   case DRAW_EQUATORIAL_PLANE:
      return mDrawEquatorialPlane;
   default:
      return Subscriber::GetBooleanParameter(id);
   }
}

//------------------------------------------------------------------------------
// bool SetBooleanParameter(const bool id, const bool value)
//------------------------------------------------------------------------------
bool OpenGlPlot::SetBooleanParameter(const Integer id, const bool value)
{
   switch (id)
   {
   case DRAW_AXIS:
      mDrawAxis = value;
      return mDrawAxis;
   case DRAW_EQUATORIAL_PLANE:
      mDrawEquatorialPlane = value;
      return mDrawEquatorialPlane;
   case CLEAR_SPACECRAFT_LIST:
      ClearSpacecraftList();
      return true;
   default:
      return Subscriber::SetBooleanParameter(id, value);
   }
}

//------------------------------------------------------------------------------
// UnsignedInt GetUnsignedIntParameter(const Integer id,
//                                     const std::string &item)
//------------------------------------------------------------------------------
UnsignedInt OpenGlPlot::GetUnsignedIntParameter(const Integer id,
                                                const std::string &item)
{   
   switch (id)
   {
   case ORBIT_COLOR:
      if (mOrbitColorMap.find(item) != mOrbitColorMap.end())
         return mOrbitColorMap[item];
      else
         return GmatBase::UNSIGNED_INT_PARAMETER_UNDEFINED;
   case TARGET_COLOR:
      if (mTargetColorMap.find(item) != mTargetColorMap.end())
         return mTargetColorMap[item];
      else
         return GmatBase::UNSIGNED_INT_PARAMETER_UNDEFINED;
   default:
      return Subscriber::GetUnsignedIntParameter(id);
   }
}

//------------------------------------------------------------------------------
// UnsignedInt GetUnsignedIntParameter(const std::string &label,
//                                     const std::string &item)
//------------------------------------------------------------------------------
UnsignedInt OpenGlPlot::GetUnsignedIntParameter(const std::string &label,
                                                const std::string &item)
{
   return GetUnsignedIntParameter(GetParameterID(label), item);
}

//------------------------------------------------------------------------------
// UnsignedInt SetUnsignedIntParameter(const Integer id,
//                                     const std::string &item,
//                                     const UnsignedInt value)
//------------------------------------------------------------------------------
UnsignedInt OpenGlPlot::SetUnsignedIntParameter(const Integer id,
                                                const std::string &item,
                                                const UnsignedInt value)
{
#if DEBUG_OPENGL_PARAM
   MessageInterface::ShowMessage
      ("OpenGlPlot::SetUnsignedIntParameter()"
       "id=%d, item=%s, value=%d\n", id, item.c_str(), value);
#endif
   
   switch (id)
   {
   case ORBIT_COLOR:
      if (mOrbitColorMap.find(item) != mOrbitColorMap.end())
      {
         mOrbitColorMap[item] = value;
         return value;
      }
      return GmatBase::UNSIGNED_INT_PARAMETER_UNDEFINED;
   case TARGET_COLOR:
      if (mTargetColorMap.find(item) != mTargetColorMap.end())
      {
         mTargetColorMap[item] = value;
         return value;
      }
      return GmatBase::UNSIGNED_INT_PARAMETER_UNDEFINED;
   default:
      return Subscriber::SetUnsignedIntParameter(id, value);
   }
}

//------------------------------------------------------------------------------
// UnsignedInt SetUnsignedIntParameter(const std::string &label,
//                                     const std::string &item,
//                                     const UnsignedInt value)
//------------------------------------------------------------------------------
UnsignedInt OpenGlPlot::SetUnsignedIntParameter(const std::string &label,
                                                const std::string &item,
                                                const UnsignedInt value)
{
   return SetUnsignedIntParameter(GetParameterID(label), item, value);
}

//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string OpenGlPlot::GetStringParameter(const Integer id) const
{
   switch (id)
   {
   case WIRE_FRAME:
      if (mDrawWireFrame)
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
std::string OpenGlPlot::GetStringParameter(const std::string &label) const
{
#if DEBUG_OPENGL_PARAM
   MessageInterface::ShowMessage
      ("OpenGlPlot::GetStringParameter() label = %s\n",
       label.c_str());
#endif
   return GetStringParameter(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
bool OpenGlPlot::SetStringParameter(const Integer id, const std::string &value)
{
#if DEBUG_OPENGL_PARAM
   MessageInterface::ShowMessage
      ("OpenGlPlot::SetStringParameter() id = %d, value = %s \n",
       id, value.c_str());
#endif
   
   switch (id)
   {
   case ADD:
      return AddSpacecraft(value);
   case WIRE_FRAME:
      if (value == "On" || value == "Off")
      {
         mDrawWireFrame = (value == "On");
#if DEBUG_OPENGL_PARAM
         MessageInterface::ShowMessage
            ("OpenGlPlot::SetStringParameter() mDrawWireFrame=%d\n", mDrawWireFrame);
#endif
         return true;
      }
   case TARGET_STATUS:
      if (value == "On" || value == "Off")
      {
         mDrawTarget = (value == "On");
#if DEBUG_OPENGL_PARAM
         MessageInterface::ShowMessage
            ("OpenGlPlot::SetStringParameter() mDrawTarget=%d\n", mDrawTarget);
#endif
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
bool OpenGlPlot::SetStringParameter(const std::string &label,
                                    const std::string &value)
{
#if DEBUG_OPENGL_PARAM
   MessageInterface::ShowMessage("OpenGlPlot::SetStringParameter() label = %s, "
                                 "value = %s \n", label.c_str(), value.c_str());
#endif
   
   return SetStringParameter(GetParameterID(label), value);
}

//------------------------------------------------------------------------------
// const StringArray& GetStringArrayParameter(const Integer id) const
//------------------------------------------------------------------------------
const StringArray& OpenGlPlot::GetStringArrayParameter(const Integer id) const
{
   switch (id)
   {
   case SPACECRAFT_LIST:
      return mScNameArray;
   default:
      return Subscriber::GetStringArrayParameter(id);
   }
}

//------------------------------------------------------------------------------
// StringArray& GetStringArrayParameter(const std::string &label) const
//------------------------------------------------------------------------------
const StringArray& OpenGlPlot::GetStringArrayParameter(const std::string &label) const
{
   return GetStringArrayParameter(GetParameterID(label));
}

//---------------------------------
// protected methods
//---------------------------------

//------------------------------------------------------------------------------
// bool AddSpacecraft(const std::string &name)
//------------------------------------------------------------------------------
bool OpenGlPlot::AddSpacecraft(const std::string &name)
{
   bool status = false;
    
   if (name != "")
   {
      mScNameArray.push_back(name);
      mScXArray.push_back(0.0);
      mScYArray.push_back(0.0);
      mScZArray.push_back(0.0);
      mOrbitColorArray.push_back(0);
      mTargetColorArray.push_back(0);
      mScCount = mScNameArray.size();

      if (mScCount < MAX_SC_COLOR)
      {
         mOrbitColorMap[name] = DEFAULT_ORBIT_COLOR[mScCount-1];
         mTargetColorMap[name] = GmatColor::ORANGE32;
      }
      else
      {
         mOrbitColorMap[name] = GmatColor::RED32;
         mTargetColorMap[name] = GmatColor::ORANGE32;
      }
      
      status = true;
   }

#if DEBUG_OPENGL_PARAM
   MessageInterface::ShowMessage
      ("OpenGlPlot::AddSpacecraft() mScCount=%d name=%s\n",
       mScCount, name.c_str());
#endif
   
   return status;
}

//------------------------------------------------------------------------------
// void ClearSpacecraftList()
//------------------------------------------------------------------------------
void OpenGlPlot::ClearSpacecraftList()
{
   mScNameArray.clear();
   mScXArray.clear();
   mScYArray.clear();
   mScZArray.clear();
   mOrbitColorMap.clear();
   mTargetColorMap.clear();
   mScCount = 0;
}

//------------------------------------------------------------------------------
// Integer FindIndexOfElement(StringArray &labelArray, const std::string &label)
//------------------------------------------------------------------------------
Integer OpenGlPlot::FindIndexOfElement(StringArray &labelArray,
                                       const std::string &label)
{
   std::vector<std::string>::iterator pos;
   pos = find(labelArray.begin(), labelArray.end(),  label);
   return distance(labelArray.begin(), pos);
}

//--------------------------------------
// methods inherited from Subscriber
//--------------------------------------

//------------------------------------------------------------------------------
// bool Distribute(int len)
//------------------------------------------------------------------------------
bool OpenGlPlot::Distribute(int len)
{
   //loj: How do I convert data to Real data?
   return false;
}

//------------------------------------------------------------------------------
// bool Distribute(const Real *dat, Integer len)
//------------------------------------------------------------------------------
bool OpenGlPlot::Distribute(const Real *dat, Integer len)
{
   //loj: 6/22/04 added if (isEndOfReceive)
   if (isEndOfReceive)
   {
      return PlotInterface::RefreshGlPlot(instanceName);
   }

   Publisher *thePublisher = Publisher::Instance();

   // if targetting and draw target is off, just return
   if (!mDrawTarget && (thePublisher->GetRunState() == Gmat::TARGETING))
      return true;
   
   if (mScCount > 0)
   {
      if (len > 0)
      {
         mNumData++;

         if ((mNumData % mDataCollectFrequency) == 0)
         {
            mNumData = 0;
            mNumCollected++;
            bool update = (mNumCollected % mUpdatePlotFrequency) == 0;

            //loj: 7/30/04 -- try new PublishedDataMap
            StringArray labelArray =
               thePublisher->GetStringArrayParameter("PublishedDataMap");

#if DEBUG_OPENGL_UPDATE
            MessageInterface::ShowMessage("OpenGlPlot::Distribute() labelArray=\n");
            for (int j=0; j<(int)labelArray.size(); j++)
            {
               MessageInterface::ShowMessage
                  ("%s ", labelArray[j].c_str());
            }
            MessageInterface::ShowMessage("\n");
#endif
            
            Integer idX, idY, idZ;
            for (int i=0; i<mScCount; i++)
            {
               idX = FindIndexOfElement(labelArray, mScNameArray[i]+".X");
               idY = FindIndexOfElement(labelArray, mScNameArray[i]+".Y");
               idZ = FindIndexOfElement(labelArray, mScNameArray[i]+".Z");
               mScXArray[i] = dat[idX];
               mScYArray[i] = dat[idY];
               mScZArray[i] = dat[idZ];
               mOrbitColorArray[i] = mOrbitColorMap[mScNameArray[i]];
               mTargetColorArray[i] = mTargetColorMap[mScNameArray[i]];
               
#if DEBUG_OPENGL_UPDATE
               MessageInterface::ShowMessage
                  ("OpenGlPlot::Distribute() scNo=%d x=%f y=%f z=%f\n",
                   i, mScXArray[i], mScYArray[i], mScZArray[i]);
#endif
            }

            //loj: 8/5/04 added
            // If targeting, use targeting color
            if (thePublisher->GetRunState() == Gmat::TARGETING)
            {
               //loj: 7/13/04 used new method taking arrays
               PlotInterface::
                  UpdateGlSpacecraft(instanceName,
                                     dat[0], mScXArray, mScYArray, mScZArray,
                                     mTargetColorArray, update, mDrawWireFrame);
            }
            else
            {
               PlotInterface::
                  UpdateGlSpacecraft(instanceName,
                                     dat[0], mScXArray, mScYArray, mScZArray,
                                     mOrbitColorArray, update, mDrawWireFrame);
            }
            
            if (update)
               mNumCollected = 0;
         }
      }
   }

   //loj: always return true otherwise next subscriber will not call ReceiveData()
   //     in Publisher::Publish()
   return true;
}

