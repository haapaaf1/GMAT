//$Id$
//------------------------------------------------------------------------------
//                                  GroundTrackPlot
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number X-XXXXX-X
//
// Author: Linda Jun
// Created: 2011/05/31
//
/**
 * Implements GroundTrackPlot class.
 */
//------------------------------------------------------------------------------

#include "GroundTrackPlot.hpp"
#include "PlotInterface.hpp"       // for UpdateGlPlot()
#include "SubscriberException.hpp" // for SubscriberException()
#include "MessageInterface.hpp"    // for ShowMessage()


//#define DBGLVL_PARAM_STRING 2


//---------------------------------
// static data
//---------------------------------

/// Available show foot print options
StringArray GroundTrackPlot::footPrintOptions;

const std::string
GroundTrackPlot::FOOT_PRINT_OPTION_TEXT[FootPrintOptionCount] =
{
   "All", "None",
};


const std::string
GroundTrackPlot::PARAMETER_TEXT[GroundTrackPlotParamCount - OrbitPlotParamCount] =
{
   "CentralBody",
   "TextureMap",
   "ShowFootPrints",
}; 


const Gmat::ParameterType
GroundTrackPlot::PARAMETER_TYPE[GroundTrackPlotParamCount - OrbitPlotParamCount] =
{
   Gmat::OBJECT_TYPE,         // "CentralBody"
   Gmat::STRING_TYPE,         // "TextureMap"
   Gmat::ENUMERATION_TYPE,    // "ShowFootPrints"
};


//------------------------------------------------------------------------------
// GroundTrackPlot(const std::string &type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * The default constructor
 */
//------------------------------------------------------------------------------
GroundTrackPlot::GroundTrackPlot(const std::string &name)
   : OrbitPlot("GroundTrackPlot", name)
{
   // GmatBase data
   parameterCount = GroundTrackPlotParamCount;
   objectTypeNames.push_back("GroundTrackPlot");
   
   centralBody = NULL;
   centralBodyName = "Earth";
   footPrints = "None";
   
   footPrintOptions.clear();
   for (UnsignedInt i = 0; i < FootPrintOptionCount; i++)
      footPrintOptions.push_back(FOOT_PRINT_OPTION_TEXT[i]);
}


//------------------------------------------------------------------------------
// GroundTrackPlot(const GroundTrackPlot &plot)
//------------------------------------------------------------------------------
/**
 * The copy consturctor
 */
//------------------------------------------------------------------------------
GroundTrackPlot::GroundTrackPlot(const GroundTrackPlot &plot)
   : OrbitPlot(plot)
{
   centralBodyName    = plot.centralBodyName;
   footPrints         = plot.footPrints;
   textureMapFileName = plot.textureMapFileName;
}


//------------------------------------------------------------------------------
// GroundTrackPlot& operator=(const GroundTrackPlot &plot)
//------------------------------------------------------------------------------
/**
 * The assignment operator
 */
//------------------------------------------------------------------------------
GroundTrackPlot& GroundTrackPlot::operator=(const GroundTrackPlot& plot)
{
   if (this == &plot)
      return *this;
   
   OrbitPlot::operator=(plot);
   
   centralBodyName    = plot.centralBodyName;
   footPrints         = plot.footPrints;
   textureMapFileName = plot.textureMapFileName;
   
   return *this;
}


//------------------------------------------------------------------------------
// ~GroundTrackPlot()
//------------------------------------------------------------------------------
/**
 * Destructor
 *
 * @note This destructor does not delete 3DView window, but clears data.
 *       3DView window is deleted when it is closed by the user or GMAT
 *       shuts down.
 */
//------------------------------------------------------------------------------
GroundTrackPlot::~GroundTrackPlot()
{
}


//----------------------------------
// inherited methods from Subscriber
//----------------------------------

//---------------------------------
// inherited methods from GmatBase
//---------------------------------

//------------------------------------------------------------------------------
//  bool Validate()
//------------------------------------------------------------------------------
/**
 * Performs any pre-run validation that the object needs.
 *
 * @return true unless validation fails.
 */
//------------------------------------------------------------------------------
bool GroundTrackPlot::Validate()
{
   return true;
}


//------------------------------------------------------------------------------
// virtual bool Initialize()
//------------------------------------------------------------------------------
bool GroundTrackPlot::Initialize()
{
   if (GmatGlobal::Instance()->GetRunMode() == GmatGlobal::TESTING_NO_PLOTS)
      return true;

   // show future work message
   MessageInterface::ShowMessage("***** Ground Track Plot is in progress.\n");
   return false;
   
   bool retval = OrbitPlot::Initialize();
   
   #if DBGLVL_INIT
   MessageInterface::ShowMessage
      ("GroundTrackPlot::Initialize() this=<%p>'%s', active=%d, isInitialized=%d, "
       "isEndOfReceive=%d, mAllSpCount=%d\n", this, GetName().c_str(), active,
       isInitialized, isEndOfReceive, mAllSpCount);
   #endif
   
   
   #if DBGLVL_INIT
   MessageInterface::ShowMessage("GroundTrackPlot::Initialize() returning %d\n", retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the GroundTrackPlot.
 *
 * @return clone of the GroundTrackPlot.
 *
 */
//------------------------------------------------------------------------------
GmatBase* GroundTrackPlot::Clone() const
{
   return (new GroundTrackPlot(*this));
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
void GroundTrackPlot::Copy(const GmatBase* orig)
{
   operator=(*((GroundTrackPlot *)(orig)));
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
bool GroundTrackPlot::TakeAction(const std::string &action,
                           const std::string &actionData)
{
   return OrbitPlot::TakeAction(action, actionData);
}


//---------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//---------------------------------------------------------------------------
bool GroundTrackPlot::RenameRefObject(const Gmat::ObjectType type,
                                      const std::string &oldName,
                                      const std::string &newName)
{
   #if DBGLVL_RENAME
   MessageInterface::ShowMessage
      ("GroundTrackPlot::RenameRefObject() type=%s, oldName=%s, newName=%s\n",
       GetObjectTypeString(type).c_str(), oldName.c_str(), newName.c_str());
   #endif
   
   if (type == Gmat::CELESTIAL_BODY)
   {
      // for central body name
      if (centralBodyName == oldName)
         centralBodyName = newName;
   }
   
   return true;
}


//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
std::string GroundTrackPlot::GetParameterText(const Integer id) const
{
   if (id >= OrbitPlotParamCount && id < GroundTrackPlotParamCount)
      return PARAMETER_TEXT[id - OrbitPlotParamCount];
   else
      return OrbitPlot::GetParameterText(id);
    
}


//------------------------------------------------------------------------------
// Integer GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
Integer GroundTrackPlot::GetParameterID(const std::string &str) const
{
   for (int i=OrbitPlotParamCount; i<GroundTrackPlotParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - OrbitPlotParamCount])
         return i;
   }
   
   return OrbitPlot::GetParameterID(str);
}


//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
Gmat::ParameterType GroundTrackPlot::GetParameterType(const Integer id) const
{
   if (id >= OrbitPlotParamCount && id < GroundTrackPlotParamCount)
      return PARAMETER_TYPE[id - OrbitPlotParamCount];
   else
      return OrbitPlot::GetParameterType(id);
}


//------------------------------------------------------------------------------
// std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
std::string GroundTrackPlot::GetParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
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
bool GroundTrackPlot::IsParameterReadOnly(const Integer id) const
{
   if (id == COORD_SYSTEM || id == DRAW_OBJECT || id == ORBIT_COLOR ||
       id == TARGET_COLOR)
      return true;
   
   return OrbitPlot::IsParameterReadOnly(id);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string GroundTrackPlot::GetStringParameter(const Integer id) const
{
   #if DBGLVL_PARAM_STRING
   MessageInterface::ShowMessage
      ("GroundTrackPlot::GetStringParameter() id = %d\n", id);
   #endif
   
   switch (id)
   {
   case CENTRAL_BODY:
      return centralBodyName;
   case TEXTURE_MAP:
      return textureMapFileName;
   case SHOW_FOOT_PRINTS:
      return footPrints;
   default:
      return OrbitPlot::GetStringParameter(id);
   }
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label) const
//------------------------------------------------------------------------------
std::string GroundTrackPlot::GetStringParameter(const std::string &label) const
{
   #if DBGLVL_PARAM_STRING
   MessageInterface::ShowMessage
      ("GroundTrackPlot::GetStringParameter() label = %s\n", label.c_str());
   #endif
   
   return GetStringParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
bool GroundTrackPlot::SetStringParameter(const Integer id, const std::string &value)
{
   #if DBGLVL_PARAM_STRING
   MessageInterface::ShowMessage
      ("GroundTrackPlot::SetStringParameter() this=<%p>'%s', id=%d<%s>, value='%s'\n",
       this, instanceName.c_str(), id, GetParameterText(id).c_str(), value.c_str());
   #endif
   
   switch (id)
   {
   case CENTRAL_BODY:
      centralBodyName = value;
      return true;
   case TEXTURE_MAP:
      textureMapFileName = value;
      return true;
   case SHOW_FOOT_PRINTS:
      {
         bool itemFound = false;
         int index = -1;
         for (int i = 0; i < FootPrintOptionCount; i++)
         {
            if (value == FOOT_PRINT_OPTION_TEXT[i])
            {
               itemFound = true;
               index = i;
               break;
            }
         }
         
         if (itemFound)
         {
            footPrints = value;
            footPrintOption = (FootPrintOption)index;
            return true;
         }
         else
         {
            SubscriberException se;
            se.SetDetails(errorMessageFormat.c_str(), value.c_str(),
                          PARAMETER_TEXT[id - GmatBaseParamCount].c_str(),
                          "All, None");
            throw se;
         }
      }
   default:
      return OrbitPlot::SetStringParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label, const std::string &value)
//------------------------------------------------------------------------------
bool GroundTrackPlot::SetStringParameter(const std::string &label,
                                         const std::string &value)
{
   #if DBGLVL_PARAM_STRING
   MessageInterface::ShowMessage
      ("GroundTrackPlot::SetStringParameter()<%s> label=%s, value=%s \n",
       instanceName.c_str(), label.c_str(), value.c_str());
   #endif
   
   return SetStringParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// virtual std::string GetRefObjectName(const Gmat::ObjectType type) const
//------------------------------------------------------------------------------
std::string GroundTrackPlot::GetRefObjectName(const Gmat::ObjectType type) const
{
   if (type == Gmat::CELESTIAL_BODY)
      return centralBodyName;
   
   return OrbitPlot::GetRefObjectName(type);
}


//------------------------------------------------------------------------------
// virtual bool HasRefObjectTypeArray()
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
bool GroundTrackPlot::HasRefObjectTypeArray()
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
const ObjectTypeArray& GroundTrackPlot::GetRefObjectTypeArray()
{
   refObjectTypes.clear();
   return OrbitPlot::GetRefObjectTypeArray();
}


//------------------------------------------------------------------------------
// virtual const StringArray& GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
const StringArray& GroundTrackPlot::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   refObjectNames.clear();
   refObjectNames = OrbitPlot::GetRefObjectNameArray(type);
   
   if (type == Gmat::UNKNOWN_OBJECT || type == Gmat::SPACE_POINT ||
       type == Gmat::CELESTIAL_BODY)
      refObjectNames.push_back(centralBodyName);
   
   return refObjectNames;
}


//------------------------------------------------------------------------------
// virtual GmatBase* GetRefObject(const Gmat::ObjectType type,
//                                const std::string &name)
//------------------------------------------------------------------------------
GmatBase* GroundTrackPlot::GetRefObject(const Gmat::ObjectType type,
                                        const std::string &name)
{
   // Any ref orbjet declared in this class?
   if (type == Gmat::CELESTIAL_BODY && name == centralBodyName)
      return centralBody;
   
   return OrbitPlot::GetRefObject(type, name);
}


//------------------------------------------------------------------------------
// virtual bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
//                           const std::string &name = "")
//------------------------------------------------------------------------------
/**
 * Set reference object pointer.
 *
 * @param <obj>  Reference object pointer to set to given object type and name
 * @param <type> Reference object type
 * @param <name> Reference object name
 */
//------------------------------------------------------------------------------
bool GroundTrackPlot::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                   const std::string &name)
{
   #if DBGLVL_OBJ
   MessageInterface::ShowMessage
      ("GroundTrackPlot::SetRefObject() this=<%p>'%s', obj=<%p>'%s', type=%d[%s], name='%s'\n",
       this, GetName().c_str(), obj, obj->GetName().c_str(), type,
       obj->GetTypeName().c_str(), name.c_str());
   #endif
   
   std::string realName = name;
   if (name == "")
      realName = obj->GetName();
   
   if (obj->IsOfType(Gmat::SPACE_POINT))
   {
      if (realName == centralBodyName)
         centralBody = (CelestialBody*)obj;
   }
   
   return OrbitPlot::SetRefObject(obj, type, realName);
}


//---------------------------------
// protected methods
//---------------------------------

//------------------------------------------------------------------------------
// bool UpdateSolverData()
//------------------------------------------------------------------------------
bool GroundTrackPlot::UpdateSolverData()
{
   int size = mCurrEpochArray.size();
   int last = size - 1;
   
   #if DBGLVL_SOLVER_CURRENT_ITER
   MessageInterface::ShowMessage("===> num buffered data = %d\n", size);
   MessageInterface::ShowMessage("==========> now update solver plot\n");
   #endif
   
   if (size == 0)
      return true;
   
   UnsignedIntArray colorArray = mScOrbitColorArray;
   if (runstate == Gmat::SOLVING)
      colorArray = mScTargetColorArray;
   else
      colorArray = mScOrbitColorArray;
   
   // Update plot with last iteration data
   for (int i=0; i<size-1; i++)
   {
      #if DBGLVL_SOLVER_CURRENT_ITER > 1
      for (int sc=0; sc<mScCount; sc++)
         MessageInterface::ShowMessage
            ("   i=%d, sc=%d, solver epoch = %f, X,Y,Z = %f, %f, %f\n", i, sc,
             mCurrEpochArray[i], mCurrXArray[i][sc], mCurrYArray[i][sc],
             mCurrZArray[i][sc]);
      #endif

      // future work
      #if 0
      // Just buffer data up to last point - 1
      PlotInterface::
         UpdateGlPlot(instanceName, mOldName, mCurrScArray[i],
                      mCurrEpochArray[i], mCurrXArray[i], mCurrYArray[i],
                      mCurrZArray[i], mCurrVxArray[i], mCurrVyArray[i],
                      mCurrVzArray[i], colorArray, true, mSolverIterOption, false);
      #endif
   }
   
   // Buffer last point and Update the plot
   // future work
   #if 0
   PlotInterface::
      UpdateGlPlot(instanceName, mOldName, mCurrScArray[last],
                   mCurrEpochArray[last], mCurrXArray[last], mCurrYArray[last],
                   mCurrZArray[last], mCurrVxArray[last], mCurrVyArray[last],
                   mCurrVzArray[last], colorArray, true, mSolverIterOption, true);
   #endif
   
   // clear arrays
   mCurrScArray.clear();
   mCurrEpochArray.clear();
   mCurrXArray.clear();
   mCurrYArray.clear();
   mCurrZArray.clear();
   mCurrVxArray.clear();
   mCurrVyArray.clear();
   mCurrVzArray.clear();
   
   if (runstate == Gmat::SOLVING)
   {
      #if 0
      // future work
      PlotInterface::TakeGlAction(instanceName, "ClearSolverData");
      #endif
   }
   return true;
}


//------------------------------------------------------------------------------
// bool Distribute(const Real *dat, Integer len)
//------------------------------------------------------------------------------
bool GroundTrackPlot::Distribute(const Real *dat, Integer len)
{
   #if DBGLVL_UPDATE
   MessageInterface::ShowMessage
      ("===========================================================================\n"
       "OrbitView::Distribute() instanceName=%s, active=%d, isEndOfRun=%d, "
       "isEndOfReceive=%d\n   mAllSpCount=%d, mScCount=%d, len=%d, runstate=%d\n",
       instanceName.c_str(), active, isEndOfRun, isEndOfReceive, mAllSpCount,
       mScCount, len, runstate);
   #endif
   
   if (GmatGlobal::Instance()->GetRunMode() == GmatGlobal::TESTING_NO_PLOTS)
      return true;
   
   if (!active || mScCount <= 0)
      return true;
   
   // test isEndOfRun first
   if (isEndOfRun)
   {
      // future work
      #if 0
      return PlotInterface::SetGlEndOfRun(instanceName);
      #else
      return true;
      #endif
   }
   
   if (isEndOfReceive)
   {
      if ((mSolverIterOption == SI_CURRENT) &&
          (runstate == Gmat::SOLVING || runstate == Gmat::SOLVEDPASS))
      {
         UpdateSolverData();
      }
      else
      {
         // future work
         #if 0
         return PlotInterface::RefreshGlPlot(instanceName);
         #else
         return true;
         #endif
      }
   }
   
   
   if (len <= 0)
      return true;
   
   
   #if DBGLVL_DATA
   MessageInterface::ShowMessage("%s, len=%d\n", GetName().c_str(), len);
   for (int i=0; i<len; i++)
      MessageInterface::ShowMessage("%.11f  ", dat[i]);
   MessageInterface::ShowMessage("\n");
   #endif
   
   //------------------------------------------------------------
   // if targeting and draw target is None, just return
   //------------------------------------------------------------
   if ((mSolverIterOption == SI_NONE) && (runstate == Gmat::SOLVING))
   {
      #if DBGLVL_UPDATE > 1
      MessageInterface::ShowMessage
         ("   Just returning: SolverIterations is %d and runstate is %d\n",
          mSolverIterOption, runstate);
      #endif
      
      return true;
   }
   
   //------------------------------------------------------------
   // update plot data
   //------------------------------------------------------------
   
   mNumData++;
   
   #if DBGLVL_UPDATE > 1
   MessageInterface::ShowMessage
      ("   mNumData=%d, mDataCollectFrequency=%d, currentProvider=<%p>\n",
       mNumData, mDataCollectFrequency, currentProvider);
   #endif
   
   if ((mNumData % mDataCollectFrequency) == 0)
   {
      
      Integer status = BufferOrbitData(dat, len);
      
      // if solving and plotting current iteration just return
      if (status == 2)
         return true;
      
      
      #if DBGLVL_UPDATE > 0
      MessageInterface::ShowMessage("==========> now update 3D View\n");
      #endif
      
      bool solving = false;
      UnsignedIntArray colorArray = mScOrbitColorArray;
      if (runstate == Gmat::SOLVING)
      {
         solving = true;
         colorArray = mScTargetColorArray;
      }
      
      bool inFunction = false;
      if (currentProvider && currentProvider->TakeAction("IsInFunction"))
         inFunction = true;
      
      bool update = (mNumCollected % mUpdatePlotFrequency) == 0;
      
      // future work
      #if 0
      PlotInterface::
         UpdateGlPlot(instanceName, mOldName, mScNameArray,
                      dat[0], mScXArray, mScYArray, mScZArray,
                      mScVxArray, mScVyArray, mScVzArray,
                      colorArray, solving, mSolverIterOption, update, inFunction);
      #endif
      
      if (update)
         mNumCollected = 0;
   }
   
   //loj: always return true otherwise next subscriber will not call ReceiveData()
   //     in Publisher::Publish()
   return true;
}


//------------------------------------------------------------------------------
// const std::string* GetFootPrintOptionList()
//------------------------------------------------------------------------------
const std::string* GroundTrackPlot::GetFootPrintOptionList()
{
   return FOOT_PRINT_OPTION_TEXT;
}


//---------------------------------------------------------------------------
// Gmat::ObjectType GetPropertyObjectType(const Integer id) const
//---------------------------------------------------------------------------
Gmat::ObjectType GroundTrackPlot::GetPropertyObjectType(const Integer id) const
{
   if (id == CENTRAL_BODY)
      return Gmat::CELESTIAL_BODY;
   
   return OrbitPlot::GetPropertyObjectType(id);
}


//---------------------------------------------------------------------------
// const StringArray& GetPropertyEnumStrings(const Integer id) const
//---------------------------------------------------------------------------
const StringArray& GroundTrackPlot::GetPropertyEnumStrings(const Integer id) const
{
   if (id == SHOW_FOOT_PRINTS)
      return footPrintOptions;
   
   return OrbitPlot::GetPropertyEnumStrings(id);
}


//---------------------------------------------------------------------------
// const StringArray& GetPropertyEnumStrings(const std::string &label) const
//---------------------------------------------------------------------------
const StringArray& GroundTrackPlot::GetPropertyEnumStrings(const std::string &label) const
{
   return GetPropertyEnumStrings(GetParameterID(label));
}
