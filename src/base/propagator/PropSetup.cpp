//$Id$
//------------------------------------------------------------------------------
//                              PropSetup
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2003/10/15
//
/**
 * Defines propagator setup operations.
 */
//------------------------------------------------------------------------------

#include "PropSetup.hpp"
#include "PropSetupException.hpp"
#include "PhysicalModel.hpp"
#include "RungeKutta89.hpp"
#include "PointMassForce.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_PROPSETUP
//#define DEBUG_PROPSETUP_SET
//#define DEBUG_PROPSETUP_CLONE
//#define DEBUG_PROPSETUP_DELETE
//#define DEBUG_PROPSETUP_GEN_STRING

//#ifndef DEBUG_MEMORY
//#define DEBUG_MEMORY
//#endif

//---------------------------------
// static data
//---------------------------------

/**
 * @note
 * Since we set some Propagator's property through PropSetup, such as
 * 'Propagator.InitialStepSize', properties owend by owning objects were
 * added here so that Validator can create corresponding element wrappers 
 * without going through owning object's property list to make Validator
 * job easy. The Validator will simply call GetParameterID() of PropSetup
 * to find out valid property or not. (LOJ: 2008.06.12)
 */

const std::string
PropSetup::PARAMETER_TEXT[PropSetupParamCount - GmatBaseParamCount] =

{
   "FM",
   "Type",
   "InitialStepSize",
   "Accuracy",
   "ErrorThreshold",
   "SmallestInterval",
   "MinStep",
   "MaxStep",
   "MaxStepAttempts",
   "LowerError",
   "TargetError",
};


const Gmat::ParameterType
PropSetup::PARAMETER_TYPE[PropSetupParamCount - GmatBaseParamCount] =
{
   Gmat::OBJECT_TYPE,  // "FM"
   Gmat::OBJECT_TYPE,  // "Type"
   Gmat::REAL_TYPE,    // "InitialStepSize",
   Gmat::REAL_TYPE,    // "Accuracy",
   Gmat::REAL_TYPE,    // "ErrorThreshold",
   Gmat::REAL_TYPE,    // "SmallestInterval",
   Gmat::REAL_TYPE,    // "MinStep",
   Gmat::REAL_TYPE,    // "MaxStep",
   Gmat::INTEGER_TYPE, // "MaxStepAttempts",
   Gmat::REAL_TYPE,    // "LowerError",
   Gmat::REAL_TYPE,    // "TargetError",
};

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// PropSetup(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Constructor.
 */
//------------------------------------------------------------------------------
PropSetup::PropSetup(const std::string &name)
   : GmatBase(Gmat::PROP_SETUP, "PropSetup", name)
{
   // GmatBase data
   objectTypes.push_back(Gmat::PROP_SETUP);
   objectTypeNames.push_back("PropSetup");
   
   parameterCount = PropSetupParamCount;
   // Propagator is named or unnamed owned object which means that Propagator is not
   // created by Create command but by handling owned object in the Interpreter.
   ownedObjectCount += 1;
   
   mInitialized = false;
   
   // Name it Internal* so that they can be deleted when new Propagator or ForceModel
   // is set. These names are not actual names but tells whether they can be deleted or not.
   // When Propagator or ForceModes is cloned these names are set to "" so that they
   // can be deleted.
   mPropagatorName = "InternalPropagator";
   mForceModelName = "InternalForceModel";
   
   // Create default Integrator and ForceModel
   mPropagator = new RungeKutta89("RungeKutta89");
   mForceModel = new ForceModel(mForceModelName);
   PhysicalModel *pmf = new PointMassForce;
   mForceModel->AddForce(pmf);
   
   #ifdef DEBUG_MEMORY
   MessageInterface::ShowMessage
      ("+++ PropSetup::PropSetup() '%s', mPropagator = new RungeKutta89(""), "
       "<%p>\n",  GetName().c_str(), mPropagator);
   MessageInterface::ShowMessage
      ("+++ PropSetup::PropSetup() '%s', mForceModel = new ForceModel(InternalForceModel), "
       "<%p>\n",  GetName().c_str(), mForceModel);
   MessageInterface::ShowMessage
      ("+++ PropSetup::PropSetup() '%s', PhysicalModel *pmf = new PointMassForce, "
       "<%p>\n",  GetName().c_str(), pmf);
   #endif
}


//------------------------------------------------------------------------------
// PropSetup(const PropSetup &ps)
//------------------------------------------------------------------------------
/**
 * Copy constructor.
 */
//------------------------------------------------------------------------------
PropSetup::PropSetup(const PropSetup &ps)
   : GmatBase(ps)
{
   #ifdef DEBUG_PROPSETUP
   MessageInterface::ShowMessage
      ("PropSetup::PropSetup() entered, Propagator=<%p>, ForceModel=<%p>\n",
       ps.mPropagator, ps.mForceModel);
   #endif
   
   ownedObjectCount = ps.ownedObjectCount;
   
   // PropSetup data
   mInitialized = false;
   mPropagatorName = "";
   mForceModelName = "";
   mPropagator = NULL;
   mForceModel = NULL;
   
   if (ps.mPropagator != NULL)
      mPropagatorName = ps.mPropagator->GetName();
   if (ps.mForceModel != NULL)
      mForceModelName = ps.mForceModel->GetName();
   
   // first delete old propagator and forcemodel (loj: 2008.11.04)
   DeleteOwnedObject(PROPAGATOR);
   DeleteOwnedObject(FORCE_MODEL);
   ClonePropagator(ps.mPropagator);
   CloneForceModel(ps.mForceModel);
   
   #ifdef DEBUG_PROPSETUP
   MessageInterface::ShowMessage
      ("PropSetup::PropSetup() exiting, Propagator=<%p><%s> '%s'\n   "
       "ForceModel=<%p><%s> '%s'\n", mPropagator,
       mPropagator ? mPropagator->GetTypeName().c_str() : "NULL",
       mPropagator ? mPropagator->GetName().c_str() : "NULL", mForceModel,
       mForceModel ? mForceModel->GetTypeName().c_str() : "NULL",
       mForceModel ? mForceModel->GetName().c_str() : "NULL");
   #endif
}

//------------------------------------------------------------------------------
// PropSetup& operator= (const PropSetup &ps)
//------------------------------------------------------------------------------
/**
 * Assignment operator.
 */
//------------------------------------------------------------------------------
PropSetup& PropSetup::operator= (const PropSetup &ps)
{
   #ifdef DEBUG_PROPSETUP
   MessageInterface::ShowMessage
      ("PropSetup::operator=() entered, Propagator=<%p>, ForceModel=<%p>\n",
       ps.mPropagator, ps.mForceModel);
   #endif
   
   if (this == &ps)
      return *this;
   
   GmatBase::operator=(ps);
   
   // PropSetup data
   mInitialized = false;
   mPropagatorName = "";
   mForceModelName = "";
   mPropagator = NULL;
   mForceModel = NULL;
   
   if (ps.mPropagator != NULL)
      mPropagatorName = ps.mPropagator->GetName();
   if (ps.mForceModel != NULL)
      mForceModelName = ps.mForceModel->GetName();
   
   DeleteOwnedObject(PROPAGATOR);
   DeleteOwnedObject(FORCE_MODEL);
   ClonePropagator(ps.mPropagator);
   CloneForceModel(ps.mForceModel);
   
   #ifdef DEBUG_PROPSETUP
   MessageInterface::ShowMessage
      ("PropSetup::operator=() exiting, Propagator=<%p><%s> '%s'\n   "
       "ForceModel=<%p><%s> '%s'\n", mPropagator,
       mPropagator ? mPropagator->GetTypeName().c_str() : "NULL",
       mPropagator ? mPropagator->GetName().c_str() : "NULL", mForceModel,
       mForceModel ? mForceModel->GetTypeName().c_str() : "NULL",
       mForceModel ? mForceModel->GetName().c_str() : "NULL");
   #endif
   
   return *this;
}

//------------------------------------------------------------------------------
// virtual ~PropSetup()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
PropSetup::~PropSetup()
{
   #ifdef DEBUG_PROPSETUP
   MessageInterface::ShowMessage
      ("PropSetup::~PropSetup() entered, Propagator=<%p>, ForceModel=<%p>\n",
       mPropagator, mForceModel);
   #endif
   
   DeleteOwnedObject(PROPAGATOR);
   DeleteOwnedObject(FORCE_MODEL);
   
   #ifdef DEBUG_PROPSETUP
   MessageInterface::ShowMessage("PropSetup::~PropSetup() exiting\n");
   #endif
}

//------------------------------------------------------------------------------
// bool IsInitialized()
//------------------------------------------------------------------------------
/**
 * @return true if pointers of Propagator and ForceModel are not NULL and
 *    there is at least one Force in the ForceModel; false otherwise.
 */
//------------------------------------------------------------------------------
bool PropSetup::IsInitialized()
{
   return mInitialized;
}

//------------------------------------------------------------------------------
// Propagator* GetPropagator()
//------------------------------------------------------------------------------
/**
 *@return internal Propagator pointer
 */
//------------------------------------------------------------------------------
Propagator* PropSetup::GetPropagator()
{
   #if DEBUG_PROPSETUP_GET
   MessageInterface::ShowMessage
      ("PropSetup::GetPropagator() mPropagator=<%p>, name='%s'\n",
       mPropagator, mPropagator->GetName().c_str());
   #endif
   
   return mPropagator;
}

//------------------------------------------------------------------------------
// ForceModel* GetForceModel()
//------------------------------------------------------------------------------
/**
 *@return internal ForceModel pointer
 */
//------------------------------------------------------------------------------
ForceModel* PropSetup::GetForceModel()
{
   return mForceModel;
}

//------------------------------------------------------------------------------
// void SetPropagator(Propagator *propagator)
//------------------------------------------------------------------------------
/**
 * Sets internal propagator pointer to given propagator.
 *
 *@param <*propagator> propagator pointer to set internal propagator to
 */
//------------------------------------------------------------------------------
void PropSetup::SetPropagator(Propagator *propagator)
{
   #ifdef DEBUG_PROPSETUP_SET
   MessageInterface::ShowMessage
      ("PropSetup::SetPropagator() this=<%p> '%s' entered, mPropagator=<%p>, "
       "propagator=<%p>\n", this, GetName().c_str(), mPropagator, propagator);
   #endif
   
   if (propagator == NULL)
      throw PropSetupException("SetPropagator() failed: propagator is NULL");
   
   DeleteOwnedObject(PROPAGATOR);
   ClonePropagator(propagator);
}

//------------------------------------------------------------------------------
// void SetForceModel(ForceModel *forceModel)
//------------------------------------------------------------------------------
/**
 * Sets internal force model pointer to given force model.
 *
 *@param <*forceModel> ForceModel pointer to set internal force model to
 */
//------------------------------------------------------------------------------
void PropSetup::SetForceModel(ForceModel *forceModel)
{
   #ifdef DEBUG_PROPSETUP_SET
   MessageInterface::ShowMessage
      ("PropSetup::SetForceModel() this=<%p> '%s' entered, mForceModel=<%p>, "
       "forceModel=<%p>\n", this, GetName().c_str(), mForceModel, forceModel);
   #endif
   
   if (forceModel == NULL)
      throw PropSetupException("SetForceModel() failed: ForceModel is NULL");
   
   DeleteOwnedObject(FORCE_MODEL);
   CloneForceModel(forceModel);
   
   #ifdef DEBUG_PROPSETUP_SET
   MessageInterface::ShowMessage
      ("PropSetup::SetForceModel() returning, mForceModel=<%p>\n", mForceModel);
   #endif
}


//------------------------------------------------------------------------------
// void AddForce(PhysicalModel *force)
//------------------------------------------------------------------------------
/**
 * Adds a force to force model.
 */
//------------------------------------------------------------------------------
void PropSetup::AddForce(PhysicalModel *force)
{
   mForceModel->AddForce(force);
}


//------------------------------------------------------------------------------
// PhysicalModel* GetForce(Integer index)
//------------------------------------------------------------------------------
/**
 * @return Force pointer give by index, NULL if invalid index
 */
//------------------------------------------------------------------------------
PhysicalModel* PropSetup::GetForce(Integer index)
{
   return mForceModel->GetForce(index);      
}


//------------------------------------------------------------------------------
// Integer GetNumForces()
//------------------------------------------------------------------------------
/**
 * @return number of forces in the force model
 */
//------------------------------------------------------------------------------
Integer PropSetup::GetNumForces()
{
   return mForceModel->GetNumForces();
}

//------------------------------------------------------------------------------
// const std::string* GetParameterList() const
//------------------------------------------------------------------------------
/**
 * @return pointer to parameter name list
 */
//------------------------------------------------------------------------------
const std::string* PropSetup::GetParameterList() const
{
   return PARAMETER_TEXT;
}


//------------------------------------------------------------------------------
// Integer GetParameterCount(void) const
//------------------------------------------------------------------------------
Integer PropSetup::GetParameterCount(void) const
{
   Integer count = parameterCount;
   return count;
}


//------------------------------------
// Inherited methods from GmatBase
//------------------------------------

//---------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//---------------------------------------------------------------------------
/**
 * Renames reference objects used in this class.
 *
 * @param <type> reference object type.
 * @param <oldName> object name to be renamed.
 * @param <newName> new object name.
 * 
 * @return true if object name changed, false if not.
 */
//---------------------------------------------------------------------------
bool PropSetup::RenameRefObject(const Gmat::ObjectType type,
                                const std::string &oldName,
                                const std::string &newName)
{
   // There is nothing to check for now
   return true;
}


//---------------------------------------------------------------------------
// virtual bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
//                           const std::string &name = "");
//---------------------------------------------------------------------------
/*
 * @see GmatBase
 */
//---------------------------------------------------------------------------
bool PropSetup::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                             const std::string &name)
{
   #ifdef DEBUG_PROPSETUP_SET
   MessageInterface::ShowMessage
      ("PropSetup::SetRefObject() entered, obj=<%p><%s> '%s', type=%d, name='%s'\n",
       obj, obj ? obj->GetTypeName().c_str() : "NULL",
       obj ? obj->GetName().c_str() : "NULL", type, name.c_str());
   #endif
   
   if (obj == NULL)
      return false;
   
   switch (type)
   {
   case Gmat::PROPAGATOR:
      SetPropagator((Propagator*)obj);
      return true;
   case Gmat::FORCE_MODEL:
      SetForceModel((ForceModel*)obj);
      return true;;
   default:
      return false;
   }
}


//------------------------------------------------------------------------------
//  GmatBase* GetOwnedObject(Integer whichOne)
//------------------------------------------------------------------------------
/**
 * This method returns the unnamed objects owned by the PropSetup.
 *
 * The current implementation only contains one PropSetup owned object: the 
 * Propagator.
 * 
 * @return Pointer to the owned object.
 */
//------------------------------------------------------------------------------
GmatBase* PropSetup::GetOwnedObject(Integer whichOne)
{
   // Propagator is named or unnamed owned object(loj: 2008.11.05)
   if (whichOne == ownedObjectCount - 1)   // If this works, make it more usable
      return mPropagator;
   return GmatBase::GetOwnedObject(whichOne);
}


//---------------------------------------------------------------------------
// bool IsOwnedObject(Integer id) const
//---------------------------------------------------------------------------
bool PropSetup::IsOwnedObject(Integer id) const
{
   if (id == PROPAGATOR || id == FORCE_MODEL)
      return true;
   else
      return false;
}


//------------------------------------------------------------------------------
//  GmatBase* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the PropSetup.
 *
 * @return clone of the PropSetup.
 *
 */
//------------------------------------------------------------------------------
GmatBase* PropSetup::Clone(void) const
{
   return (new PropSetup(*this));
}


//---------------------------------------------------------------------------
//  void Copy(const GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Sets this object to match another one.
 * 
 * @param orig The original that is being copied.
 */
//---------------------------------------------------------------------------
void PropSetup::Copy(const GmatBase* orig)
{
   // We don't want to copy instanceName
   std::string name = instanceName;
   operator=(*((PropSetup *)(orig)));
   instanceName = name;
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
const ObjectTypeArray& PropSetup::GetRefObjectTypeArray()
{
   // We need to add in the property order since Interpreter querrys for
   // object type using property id
   refObjectTypes.clear();
   refObjectTypes.push_back(Gmat::PROPAGATOR);
   refObjectTypes.push_back(Gmat::FORCE_MODEL);
   return refObjectTypes;
}


//------------------------------------------------------------------------------
//  const StringArray& GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * Retrieves the list of ref objects used by the member forces.
 *
 * @param <type> The type of object desired, or Gmat::UNKNOWN_OBJECT for the
 *               full list.
 * 
 * @return the list of object names.
 * 
 */
//------------------------------------------------------------------------------
const StringArray& PropSetup::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   refObjectNames.clear();
   if (mPropagatorName != "")
      if (type == Gmat::PROPAGATOR || type == Gmat::UNKNOWN_OBJECT)
         refObjectNames.push_back(mPropagatorName);
   if (mForceModelName != "")
      if (type == Gmat::FORCE_MODEL || type == Gmat::UNKNOWN_OBJECT)
         refObjectNames.push_back(mForceModelName);
   return refObjectNames;
}


//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Gmat::ParameterType PropSetup::GetParameterType(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < PropSetupParamCount)
      return PARAMETER_TYPE[id - GmatBaseParamCount];
   else
      return GmatBase::GetParameterType(id);
}

//------------------------------------------------------------------------------
// std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string PropSetup::GetParameterTypeString(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < PropSetupParamCount)
      return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
   else
      return GmatBase::GetParameterTypeString(id);
}

//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string PropSetup::GetParameterText(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < PropSetupParamCount)
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   else
      return GmatBase::GetParameterText(id);
}


//------------------------------------------------------------------------------
// Integer GetParameterID(const std::string str)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Integer PropSetup::GetParameterID(const std::string &str) const
{
   for (int i=GmatBaseParamCount; i<PropSetupParamCount; i++)
   {
      if (str == PropSetup::PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }
   
   return GmatBase::GetParameterID(str);
}


//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//---------------------------------------------------------------------------
bool PropSetup::IsParameterReadOnly(const Integer id) const
{
   if (id == FORCE_MODEL || id == PROPAGATOR)
      return false;
   else if (id >= INITIAL_STEP_SIZE && id <= TARGET_ERROR)
      return true;
   else
      return GmatBase::IsParameterReadOnly(id);
}


//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <label> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not.
 */
//---------------------------------------------------------------------------
bool PropSetup::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string PropSetup::GetStringParameter(const Integer id) const
{
   #ifdef DEBUG_PROPSETUP_GET
   MessageInterface::ShowMessage
      ("PropSetup::GetStringParameter() '%s' entered, id=%d\n", id);
   #endif
   std::string name;
   switch (id)
   {
   case PROPAGATOR:
      if (mPropagator)
         name = mPropagator->GetName();
      else
         name = "UndefinedPropagator";
      break;
   case FORCE_MODEL:
      if (mForceModel)
         name = mForceModel->GetName();
      else
         name = "UndefinedForceModel";
      break;
   default:
      return GmatBase::GetStringParameter(id);
   }
   #ifdef DEBUG_PROPSETUP_GET
   MessageInterface::ShowMessage
      ("PropSetup::GetStringParameter() '%s' returning '%s'\n", name.c_str());
   #endif
   return name;
}

//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string PropSetup::GetStringParameter(const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
bool PropSetup::SetStringParameter(const Integer id, const std::string &value)
{
   #ifdef DEBUG_PROPSETUP_SET
   MessageInterface::ShowMessage
      ("PropSetup::SetStringParameter() '%s', id=%d, value='%s'\n",
       GetName().c_str(), id, value.c_str());
   #endif
      
   switch (id)
   {
   case PROPAGATOR:
      mPropagatorName = value;
      return true;
   case FORCE_MODEL:
      mForceModelName = value;
      return true;
   default:
      return GmatBase::SetStringParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label, const std::string &value)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
bool PropSetup::SetStringParameter(const std::string &label, 
                                   const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// virtual Real GetRealParameter(const Integer id) const
//------------------------------------------------------------------------------
/*
 * This method provides call-through to propagator
 */
//------------------------------------------------------------------------------
Real PropSetup::GetRealParameter(const Integer id) const
{
   switch (id)
   {
      case ACCURACY:
      case INITIAL_STEP_SIZE:
      case ERROR_THRESHOLD:
      case SMALLEST_INTERVAL:
      case MIN_STEP:
      case MAX_STEP:
      case MAX_STEP_ATTEMPTS:
      case LOWER_ERROR:
      case TARGET_ERROR:
         {
            // Get actual id
            Integer actualId = GetOwnedObjectId(id, Gmat::PROPAGATOR);
            return mPropagator->GetRealParameter(actualId);
         }
   default:
      return GmatBase::GetRealParameter(id);
   }
}


//------------------------------------------------------------------------------
// virtual Real GetRealParameter(const std::string &label) const
//------------------------------------------------------------------------------
Real PropSetup::GetRealParameter(const std::string &label) const
{
   return GetRealParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// virtual Real SetRealParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
/*
 * This method provides call-through to propagator
 */
//------------------------------------------------------------------------------
Real PropSetup::SetRealParameter(const Integer id, const Real value)
{
   switch (id)
   {
      case ACCURACY:
      case INITIAL_STEP_SIZE:
      case ERROR_THRESHOLD:
      case SMALLEST_INTERVAL:
      case MIN_STEP:
      case MAX_STEP:
      case MAX_STEP_ATTEMPTS:
      case LOWER_ERROR:
      case TARGET_ERROR:
         {
            // Get actual id
            Integer actualId = GetOwnedObjectId(id, Gmat::PROPAGATOR);
            return mPropagator->SetRealParameter(actualId, value);
         }
   default:
      return GmatBase::SetRealParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// virtual Real SetRealParameter(const std::string &label, const Real value)
//------------------------------------------------------------------------------
Real PropSetup::SetRealParameter(const std::string &label, const Real value)
{
   return SetRealParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// virtual Integer GetIntegerParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer PropSetup::GetIntegerParameter(const Integer id) const
{
   switch (id)
   {
      case MAX_STEP_ATTEMPTS:
         {
            // Get actual id
            Integer actualId = GetOwnedObjectId(id, Gmat::PROPAGATOR);
            return mPropagator->GetIntegerParameter(actualId);
         }
   default:
      return GmatBase::GetIntegerParameter(id);
   }
}


//------------------------------------------------------------------------------
// virtual Integer GetIntegerParameter(const std::string &label) const
//------------------------------------------------------------------------------
Integer PropSetup::GetIntegerParameter(const std::string &label) const
{
   return GetIntegerParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// virtual Integer SetIntegerParameter(const Integer id, const Integer value)
//------------------------------------------------------------------------------
Integer PropSetup::SetIntegerParameter(const Integer id, const Integer value)
{
   switch (id)
   {
      case MAX_STEP_ATTEMPTS:
         {
            // Get actual id
            Integer actualId = GetOwnedObjectId(id, Gmat::PROPAGATOR);
            return mPropagator->SetIntegerParameter(actualId, value);
         }
   default:
      return GmatBase::SetIntegerParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// virtual Integer SetIntegerParameter(const std::string &label, const Integer value)
//------------------------------------------------------------------------------
Integer PropSetup::SetIntegerParameter(const std::string &label, const Integer value)
{
   return SetIntegerParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Sets mInitialized to true if pointers of Propagator and ForceModel are not
 * NULL and there is at least one Force in the ForceModel; false otherwise
 */
//------------------------------------------------------------------------------
bool PropSetup::Initialize()
{
   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage("PropSetup::Initialize() entered \n");
   #endif
   mInitialized = true;

   if (mPropagator == NULL)
   {
      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage(
            "PropSetup::Initialize() mPropagator is NULL\n");
      #endif
      mInitialized = false;
   }
   
   if (mForceModel == NULL)
   {
      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage(
            "PropSetup::Initialize() mForceModel is NULL\n");
      #endif
      mInitialized = false;
   }
   else if (mForceModel->GetNumForces() == 0)
   {
      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage(
            "PropSetup::Initialize() NumForces is 0\n");
      #endif
      mInitialized = false;
   }
   
   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage(
         "PropSetup::Initialize() initialized = %d\n", mInitialized);
   #endif
   
   if (mInitialized == true)
   {
      mPropagator->SetPhysicalModel(mForceModel);
      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage(
            "PropSetup::Initialize() after SetPhysicalModel(%s) \n",
            mForceModel->GetName().c_str());
      #endif

      mPropagator->Initialize();
      
      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage(
            "PropSetup::Initialize() after mPropagator->Initialize() \n");
      #endif
   }
   
   return true;
}


//------------------------------------------------------------------------------
// const std::string& GetGeneratingString(Gmat::WriteMode mode,
//            const std::string &prefix, const std::string &useName)
//------------------------------------------------------------------------------
/**
 * Provides special handling for the scripting for PropSetups.
 *
 * @param mode Specifies the type of serialization requested.
 * @param prefix Optional prefix appended to the object's name
 * @param useName Name that replaces the object's name.
 *
 * @return A string containing the scrit used to construct the PropSetup.
 */
//------------------------------------------------------------------------------
const std::string& PropSetup::GetGeneratingString(Gmat::WriteMode mode,
            const std::string &prefix, const std::string &useName)
{
   #ifdef DEBUG_PROPSETUP_GEN_STRING
   MessageInterface::ShowMessage
      ("PropSetup::GetGeneratingString() '%s' entered, mForceModel=<%p> '%s'\n",
       GetName().c_str(), mForceModel,
       mForceModel ? mForceModel->GetName().c_str() : "NULL");
   #endif
   std::string gen, fmName = "", temp;
   bool showForceModel = false;
   if (mForceModel != NULL)
   {
      temp = mForceModel->GetName();
      if (temp == "")
      {
         fmName = instanceName + "_ForceModel";
         showForceModel = true;
      }
      else
         fmName = temp;
      
      if (mode == Gmat::SHOW_SCRIPT)
         showForceModel = true;
      
      #ifdef DEBUG_PROPSETUP_GEN_STRING
      MessageInterface::ShowMessage
         ("   fmName='%s', showForceModel=%d\n", fmName.c_str(), showForceModel);
      #endif
      
      if (showForceModel)
         gen = mForceModel->GetGeneratingString(mode, prefix, fmName) + "\n";
   }
   
   gen += GmatBase::GetGeneratingString(mode, prefix, useName);
   generatingString = gen;
   
   return generatingString;
}


//---------------------------------
// private methods
//---------------------------------

//------------------------------------------------------------------------------
// void ClonePropagator(Propagator *prop)
//------------------------------------------------------------------------------
void PropSetup::ClonePropagator(Propagator *prop)
{
   #ifdef DEBUG_PROPSETUP_CLONE
   MessageInterface::ShowMessage
      ("PropSetup::ClonePropagator() entered, prop=<%p>\n", prop);
   #endif
   if (prop != NULL)
   {
      mPropagatorName = "";
      mPropagator = (Propagator *)(prop->Clone());
      #ifdef DEBUG_MEMORY
      MessageInterface::ShowMessage
         ("+++ PropSetup::ClonePropagator() '%s', mPropagator = prop->Clone(), "
          "<%p>\n",  GetName().c_str(), mPropagator);
      #endif
   }
   else
   {
      mPropagatorName = "";
      mPropagator = NULL;
   }
   #ifdef DEBUG_PROPSETUP_CLONE
   MessageInterface::ShowMessage
      ("PropSetup::ClonePropagator() exiting, mPropagatorName='%s', mPropagator=<%p>\n",
       mPropagatorName.c_str(), mPropagator);
   #endif
}


//------------------------------------------------------------------------------
// void CloneForceModel(ForceModel *fm)
//------------------------------------------------------------------------------
void PropSetup::CloneForceModel(ForceModel *fm)
{
   #ifdef DEBUG_PROPSETUP_CLONE
   MessageInterface::ShowMessage
      ("PropSetup::CloneForceModel() entered, fm=<%p>\n", fm);
   #endif
   if (fm != NULL)
   {
      mForceModelName = "";
      mForceModel = (ForceModel *)(fm->Clone());
      #ifdef DEBUG_MEMORY
      MessageInterface::ShowMessage
         ("+++ PropSetup::CloneForceModel() '%s', mForceModel = fm->Clone(), "
          "<%p>\n",  GetName().c_str(), mForceModel);
      #endif
   }
   else
   {
      mForceModelName = "";
      mForceModel = NULL;
   }
   #ifdef DEBUG_PROPSETUP_CLONE
   MessageInterface::ShowMessage
      ("PropSetup::CloneForceModel() exiting, mForceModelName='%s', mForceModel=<%p>\n",
       mForceModelName.c_str(), mForceModel);
   #endif
}


//------------------------------------------------------------------------------
// void DeleteOwnedObject(Integer id)
//------------------------------------------------------------------------------
/**
 * Deletes internal or cloned owned object. Owned objects are named Internal*
 * in the consturctor. When Propagator or ForceModes is cloned their names are
 * set to "" so that those can be deleted.
 */
//------------------------------------------------------------------------------
void PropSetup::DeleteOwnedObject(Integer id)
{
   // Since Propagator and ForceModel are cloned delete them here. (loj: 2008.11.05)
   if (id == PROPAGATOR)
   {
      #ifdef DEBUG_PROPSETUP_DELETE
      MessageInterface::ShowMessage
         ("PropSetup::DeleteOwnedObject() mPropagator=<%p>, mPropagatorName='%s'\n",
          mPropagator, mPropagatorName.c_str());
      #endif
      if (mPropagator != NULL)
      {
         if (mPropagatorName == "" || mPropagatorName == "InternalPropagator")
         {
            #ifdef DEBUG_MEMORY
            MessageInterface::ShowMessage
               ("--- PropSetup::DeleteOwnedObject() '%s', deleting mPropagator <%p>\n",
                GetName().c_str(), mPropagator);
            #endif
            delete mPropagator;
            mPropagatorName = "";
            mPropagator = NULL;
         }
      }
   }
   else if (id == FORCE_MODEL)
   {
      #ifdef DEBUG_PROPSETUP_DELETE
      MessageInterface::ShowMessage
         ("PropSetup::DeleteOwnedObject() mForceModel=<%p>, mForceModelName='%s'\n",
          mForceModel, mForceModelName.c_str());
      #endif
      if (mForceModel != NULL)
      {
         // delete cloned ForceModel 
         if (mForceModelName == "" || mForceModelName == "InternalForceModel")
         {
            #ifdef DEBUG_MEMORY
            MessageInterface::ShowMessage
               ("--- PropSetup::DeleteOwnedObject() '%s', deleting mForceModel <%p>\n",
                GetName().c_str(), mForceModel);
            #endif
            delete mForceModel;
            mForceModelName = "";
            mForceModel = NULL;
         }
      }
   }
}


//------------------------------------------------------------------------------
// Integer GetOwnedObjectId(Integer id, Gmat::ObjectType objType) const
//------------------------------------------------------------------------------
/**
 * Returns property id of owned object.
 */
//------------------------------------------------------------------------------
Integer PropSetup::GetOwnedObjectId(Integer id, Gmat::ObjectType objType) const
{
   Integer actualId = -1;
   
   try
   {
      if (objType == Gmat::PROPAGATOR)
      {
         if (mPropagator == NULL)
            throw PropSetupException
               ("PropSetup::GetOwnedObjectId() failed: Propagator is NULL");
         
         actualId = mPropagator->GetParameterID(GetParameterText(id));
      }
      else if (objType == Gmat::FORCE_MODEL)
      {
         if (mForceModel == NULL)
            throw PropSetupException
               ("PropSetup::GetOwnedObjectId() failed: ForceModel is NULL");
         
         actualId = mForceModel->GetParameterID(GetParameterText(id));
      }
   }
   catch (BaseException &e)
   {
      throw;
   }
   
   return actualId;
}

