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
#include "MessageInterface.hpp"  // for ShowMessage()

//---------------------------------
// static data
//---------------------------------
const std::string
OpenGlPlot::PARAMETER_TEXT[OpenGlPlotParamCount] =
{
   "Axis",
   "DrawEquatorialPlane",
   "WireFrame",
   "DataCollectFrequency",
   "UpdatePlotFrequency"
}; 

const Gmat::ParameterType
OpenGlPlot::PARAMETER_TYPE[OpenGlPlotParamCount] =
{
   Gmat::BOOLEAN_TYPE,
   Gmat::BOOLEAN_TYPE,
   Gmat::STRING_TYPE,
   Gmat::INTEGER_TYPE,
   Gmat::INTEGER_TYPE
};

//------------------------------------------------------------------------------
// OpenGlPlot(const std::string &name)
//------------------------------------------------------------------------------
OpenGlPlot::OpenGlPlot(const std::string &name)
   : Subscriber      ("OpenGlPlot", name)
{
   // GmatBase data
   parameterCount = OpenGlPlotParamCount;

   mDrawAxis = false;
   mDrawEquatorialPlane = true;
   mDrawWireFrame = false;
   mDataCollectFrequency = 1;
   mUpdatePlotFrequency = 10;
   mNumData = 0;
   mNumCollected = 0;
}

//------------------------------------------------------------------------------
// ~OpenGlPlot(void)
//------------------------------------------------------------------------------
OpenGlPlot::~OpenGlPlot(void)
{
}

//loj: 3/8/04 added
//------------------------------------------------------------------------------
// virtual bool Initialize()
//------------------------------------------------------------------------------
bool OpenGlPlot::Initialize()
{
   //MessageInterface::ShowMessage("OpenGlPlot::Initialize() entered\n");
    
   if (active)
   {
      //MessageInterface::ShowMessage("OpenGlPlot::Initialize() CreateGlPlotWindow()\n");
      return PlotInterface::CreateGlPlotWindow();
   }
   else
   {
      //MessageInterface::ShowMessage("OpenGlPlot::Initialize() DeleteGlPlot()\n");
      return PlotInterface::DeleteGlPlot();
   }
}

//------------------------------------------------------------------------------
// bool Distribute(int len)
//------------------------------------------------------------------------------
bool OpenGlPlot::Distribute(int len)
{
   //loj: How do I convert data to Real data?
   return false;
}

//------------------------------------------------------------------------------
// bool Distribute(const Real * dat, Integer len)
//------------------------------------------------------------------------------
bool OpenGlPlot::Distribute(const Real * dat, Integer len)
{
   if (len > 0)
   {
      mNumData++;
    
      if ((mNumData % mDataCollectFrequency) == 0)
      {
         mNumCollected++;
         bool update = (mNumCollected % mUpdatePlotFrequency) == 0;

         //loj: assumes data in time, x, y, z order
         return PlotInterface::UpdateGlSpacecraft(dat[0], dat[1], dat[2], dat[3],
                                                  update, mDrawWireFrame);
      }
   }
    
   //loj: always return true otherwise next subscriber will not call ReceiveData()
   //     in Publisher::Publish()
   return true;
}

//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
std::string OpenGlPlot::GetParameterText(const Integer id) const
{
   if (id >= DRAW_AXIS && id < OpenGlPlotParamCount)
      return PARAMETER_TEXT[id];
   else
      return Subscriber::GetParameterText(id);
    
}

//------------------------------------------------------------------------------
// Integer GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
Integer OpenGlPlot::GetParameterID(const std::string &str) const
{
   for (int i=0; i<OpenGlPlotParamCount-1; i++)
   {
      if (str == PARAMETER_TEXT[i])
         return i;
   }
   
   return Subscriber::GetParameterID(str);
}

//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
Gmat::ParameterType OpenGlPlot::GetParameterType(const Integer id) const
{
   if (id >= DRAW_AXIS && id < OpenGlPlotParamCount)
      return PARAMETER_TYPE[id];
   else
      return Subscriber::GetParameterType(id);
}

//------------------------------------------------------------------------------
// std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
std::string OpenGlPlot::GetParameterTypeString(const Integer id) const
{
   if (id >= DRAW_AXIS && id <= OpenGlPlotParamCount)
      return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
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
   default:
      return Subscriber::SetBooleanParameter(id, value);
   }
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
   default:
      return Subscriber::GetStringParameter(id);
   }
}

//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label) const
//------------------------------------------------------------------------------
std::string OpenGlPlot::GetStringParameter(const std::string &label) const
{
   //MessageInterface::ShowMessage("OpenGlPlot::GetStringParameter() label = %s\n",
   //                              label.c_str());

   return GetStringParameter(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
bool OpenGlPlot::SetStringParameter(const Integer id, const std::string &value)
{
   //MessageInterface::ShowMessage("OpenGlPlot::SetStringParameter() id = %d, "
   //                              "value = %s \n", id, value.c_str());
   
   switch (id)
   {
   case WIRE_FRAME:
      if (value == "On" || value == "Off")
      {
         mDrawWireFrame = (value == "On");
         MessageInterface::ShowMessage
            ("OpenGlPlot::SetStringParameter() mDrawWireFrame=%d\n", mDrawWireFrame);
             
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
   //MessageInterface::ShowMessage("OpenGlPlot::SetStringParameter() label = %s, "
   //                              "value = %s \n", label.c_str(), value.c_str());

   return SetStringParameter(GetParameterID(label), value);
}
