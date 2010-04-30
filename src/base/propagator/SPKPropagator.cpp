//$Id$
//------------------------------------------------------------------------------
//                             SPKPropagator
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: Mar 26, 2010 by Darrel Conway (Thinking Systems)
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under the FDSS 
// contract, Task 28
//
/**
 * Implementation for the SPKPropagator class
 */
//------------------------------------------------------------------------------


#include "SPKPropagator.hpp"
#include "MessageInterface.hpp"
#include "FileManager.hpp"

//#define DEBUG_INITIALIZATION
//#define DEBUG_PROPAGATION
//#define TEST_TDB_ROUND_TRIP


/// SPKPropagator parameter labels
const std::string SPKPropagator::PARAMETER_TEXT[
                 SPKPropagatorParamCount - EphemerisPropagatorParamCount] =
{
      "SPKFiles"                    //SPKFILENAMES
};

/// SPKPropagator parameter types
const Gmat::ParameterType SPKPropagator::PARAMETER_TYPE[
                 SPKPropagatorParamCount - EphemerisPropagatorParamCount] =
{
      Gmat::STRINGARRAY_TYPE        //SPKFILENAMES
};


SPKPropagator::SPKPropagator(const std::string &name) :
   EphemerisPropagator        ("SPK", name),
   skr                        (NULL)
{
   // GmatBase data
  objectTypeNames.push_back("SPK");
  parameterCount = SPKPropagatorParamCount;
}


SPKPropagator::~SPKPropagator()
{
   if (skr)
      delete skr;
}


SPKPropagator::SPKPropagator(const SPKPropagator & spk) :
   EphemerisPropagator        (spk),
   skr                        (NULL)
{
}


SPKPropagator & SPKPropagator::operator =(const SPKPropagator & spk)
{
   if (this != &spk)
   {
      EphemerisPropagator::operator=(spk);

      skr = NULL;
   }

   return *this;
}


GmatBase* SPKPropagator::Clone() const
{
   return new SPKPropagator(*this);
}


std::string SPKPropagator::GetParameterText(const Integer id) const
{
   if (id >= EphemerisPropagatorParamCount && id < SPKPropagatorParamCount)
      return PARAMETER_TEXT[id - EphemerisPropagatorParamCount];
   return EphemerisPropagator::GetParameterText(id);
}


Integer SPKPropagator::GetParameterID(const std::string &str) const
{
   for (Integer i = EphemerisPropagatorParamCount;
         i < SPKPropagatorParamCount; ++i)
   {
       if (str == PARAMETER_TEXT[i - EphemerisPropagatorParamCount])
           return i;
   }

   return EphemerisPropagator::GetParameterID(str);
}


Gmat::ParameterType SPKPropagator::GetParameterType(const Integer id) const
{
   if (id >= EphemerisPropagatorParamCount && id < SPKPropagatorParamCount)
      return PARAMETER_TYPE[id - EphemerisPropagatorParamCount];
   return EphemerisPropagator::GetParameterType(id);
}


std::string SPKPropagator::GetParameterTypeString(const Integer id) const
{
   if (id >= EphemerisPropagatorParamCount && id < SPKPropagatorParamCount)
      return EphemerisPropagator::PARAM_TYPE_STRING[GetParameterType(id)];
   return EphemerisPropagator::GetParameterTypeString(id);
}


std::string SPKPropagator::GetParameterUnit(const Integer id) const
{
   return EphemerisPropagator::GetParameterUnit(id);
}


bool SPKPropagator::IsParameterReadOnly(const Integer id) const
{
   if (id == SPKFILENAMES)
      return true;
   return EphemerisPropagator::IsParameterReadOnly(id);
}


bool SPKPropagator::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}


std::string SPKPropagator::GetStringParameter(const Integer id) const
{
   return EphemerisPropagator::GetStringParameter(id);
}


bool SPKPropagator::SetStringParameter(const Integer id,
      const std::string &value)
{
   if (id == SPKFILENAMES)
   {
      if (value != "")
         if (find(spkFileNames.begin(), spkFileNames.end(), value) ==
               spkFileNames.end())
            spkFileNames.push_back(value);
      return true;         // Idempotent, so return true
   }

   return EphemerisPropagator::SetStringParameter(id, value);
}


std::string SPKPropagator::GetStringParameter(const Integer id,
      const Integer index) const
{
   if (id == SPKFILENAMES)
   {
      if ((index >= 0) && (index < (Integer)spkFileNames.size()))
         return spkFileNames[index];
      return "";
   }

   return EphemerisPropagator::GetStringParameter(id, index);
}


bool SPKPropagator::SetStringParameter(const Integer id,
      const std::string &value, const Integer index)
{
   if (id == SPKFILENAMES)
   {
      if ((index >= 0) && (index < (Integer)spkFileNames.size()))
      {
         spkFileNames[index] = value;
         return true;
      }
      return false;
   }

   return EphemerisPropagator::SetStringParameter(id, value, index);
}


const StringArray& SPKPropagator::GetStringArrayParameter(
      const Integer id) const
{
   if (id == SPKFILENAMES)
      return spkFileNames;
   return EphemerisPropagator::GetStringArrayParameter(id);
}


const StringArray& SPKPropagator::GetStringArrayParameter(const Integer id,
      const Integer index) const
{
   return EphemerisPropagator::GetStringArrayParameter(id, index);
}


std::string SPKPropagator::GetStringParameter(const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}


bool SPKPropagator::SetStringParameter(const std::string &label,
      const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}


std::string SPKPropagator::GetStringParameter(const std::string &label,
      const Integer index) const
{
   return GetStringParameter(GetParameterID(label), index);
}


bool SPKPropagator::SetStringParameter(const std::string &label,
      const std::string &value, const Integer index)
{
   return SetStringParameter(GetParameterID(label), value, index);
}


const StringArray& SPKPropagator::GetStringArrayParameter(
      const std::string &label) const
{
   return GetStringArrayParameter(GetParameterID(label));
}


const StringArray& SPKPropagator::GetStringArrayParameter(
      const std::string &label, const Integer index) const
{
   return GetStringArrayParameter(GetParameterID(label), index);
}


bool SPKPropagator::Initialize()
{
   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage("SPKPropagator::Initialize() entered\n");
   #endif

   bool retval = false;

   if (EphemerisPropagator::Initialize())
   {
      // If skr already set, just keep it
      if (skr == NULL)
         skr = new SpiceOrbitKernelReader;

      stepTaken = 0.0;
      j2ET = j2000_c();   // CSPICE method to return Julian date of J2000 (TDB)

      FileManager *fm = FileManager::Instance();
      std::string fullPath = fm->GetFullPathname(FileManager::PLANETARY_SPK_FILE);

      if (skr->IsLoaded(fullPath) == false)
         skr->LoadKernel(fullPath);

      if (propObjects.size() != 1)
         throw PropagatorException("SPICE propagators (i.e. \"SKP\" "
               "propagators) require exactly one SpaceObject.");

      naifIds.clear();
      for (UnsignedInt i = 0; i < propObjects.size(); ++i)
      {
         Integer id = propObjects[i]->GetIntegerParameter("NAIFId");
         naifIds.push_back(id);

         // Load the SPICE files for each propObject
         StringArray spices;
         if (propObjects[i]->IsOfType(Gmat::SPACECRAFT))
            spices = propObjects[i]->GetStringArrayParameter(
                  "OrbitSpiceKernelName");
         else
            throw PropagatorException("Spice (SPK) propagators only work for "
                  "Spacecraft right now.");

         if (spices.size() == 0)
            throw PropagatorException("Spice (SPK) propagator requires at "
                  "least one orbit SPICE kernel,");

         std::string ephemPath = fm->GetPathname(FileManager::EPHEM_PATH);
         for (UnsignedInt j = 0; j < spices.size(); ++j)
         {
            fullPath = spices[j];

            // Check to see if this name includes path information
            // If no path designation slash character is found, add the default path
            if ((fullPath.find('/') == std::string::npos) &&
                (fullPath.find('\\') == std::string::npos))
            {
               fullPath = ephemPath + fullPath;
            }

            if (skr->IsLoaded(fullPath) == false)
               skr->LoadKernel(fullPath);

            if (find(spkFileNames.begin(), spkFileNames.end(), fullPath) ==
                  spkFileNames.end())
               spkFileNames.push_back(fullPath);
         }

         // Load the initial data point
         if (skr)
         {
            try
            {
               Rvector6  outState;

               for (UnsignedInt i = 0; i < propObjects.size(); ++i)
               {
                  std::string scName = propObjectNames[i];
                  Integer id = naifIds[i];

                  currentEpoch = initialEpoch + timeFromEpoch /
                        GmatTimeUtil::SECS_PER_DAY;
                  if ((currentEpoch < ephemStart) || (currentEpoch > ephemEnd))
                  {
                     std::stringstream errmsg;
                     errmsg.precision(16);
                     errmsg << "The SPKPropagator "
                            << instanceName
                            << " is attempting to initialize outside of the "
                               "timespan  of the ephemeris data; halting.  ";
                     errmsg << "The current SPICE ephemeris covers the A.1 "
                               "modified Julian span ";
                     errmsg << ephemStart << " to " << ephemEnd << " and the "
                              "requested epoch is " << currentEpoch << ".\n";
                     throw PropagatorException(errmsg.str());
                  }
                  outState = skr->GetTargetState(scName, id, currentEpoch,
                        centralBody);

                  std::memcpy(state, outState.GetDataVector(),
                        dimension*sizeof(Real));
               }

               UpdateSpaceObject(currentEpoch);

               retval = true;
            }
            catch (BaseException &e)
            {
               MessageInterface::ShowMessage(e.GetFullMessage());
               retval = false;
            }
         }
      }

      SetEphemSpan();
   }

   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage("SPKPropagator::Initialize(): Start state "
            "at epoch %.12lf is [", currentEpoch);
      for (Integer i = 0; i < dimension; ++i)
      {
         MessageInterface::ShowMessage("%.12lf", state[i]);
         if (i < dimension-1)
            MessageInterface::ShowMessage("   ");
         else
            MessageInterface::ShowMessage("]\n");
      }
      MessageInterface::ShowMessage("SPKPropagator::Initialize() finished\n");
   #endif

   return retval;
}


bool SPKPropagator::Step()
{
   #ifdef DEBUG_PROPAGATION
      MessageInterface::ShowMessage("SPKPropagator::Step() entered: "
            "initialEpoch = %.12lf; stepsize = %.12lf; "
            "timeFromEpoch = %.12lf\n", initialEpoch, ephemStep, timeFromEpoch);
   #endif

   bool retval = false;

   if (skr)
   {
      try
      {
         Rvector6  outState;

         for (UnsignedInt i = 0; i < propObjects.size(); ++i)
         {
            std::string scName = propObjectNames[i];
            Integer id = naifIds[i];

            timeFromEpoch += ephemStep;
            stepTaken = ephemStep;
            currentEpoch = initialEpoch + timeFromEpoch /
                  GmatTimeUtil::SECS_PER_DAY;

            if ((currentEpoch < ephemStart) || (currentEpoch > ephemEnd))
            {
               std::stringstream errmsg;
               errmsg.precision(16);
               errmsg << "The SPKPropagator "
                      << instanceName
                      << " is attempting to step outside of the span of the "
                         "ephemeris data; halting.  ";
               errmsg << "The current SPICE ephemeris covers the A.1 modified "
                         "Julian span ";
               errmsg << ephemStart << " to " << ephemEnd << " and the "
                     "requested epoch is " << currentEpoch << ".\n";
               throw PropagatorException(errmsg.str());
            }

            outState = skr->GetTargetState(scName, id, currentEpoch,
                  centralBody);

            /**
             *  @todo: When SPKProp can evolve more than one spacecraft, these
             *  memcpy lines need revision
             */
            std::memcpy(state, outState.GetDataVector(),
                  dimension*sizeof(Real));

            #ifdef DEBUG_PROPAGATION
               MessageInterface::ShowMessage("State at epoch %.12lf is [",
                     currentEpoch);
               for (Integer i = 0; i < dimension; ++i)
               {
                  MessageInterface::ShowMessage("%.12lf", state[i]);
                  if (i < 5)
                     MessageInterface::ShowMessage("   ");
                  else
                     MessageInterface::ShowMessage("]\n");
               }
            #endif
         }

         retval = true;
      }
      catch (BaseException &e)
      {
         MessageInterface::ShowMessage(e.GetFullMessage());
         retval = false;
      }
   }

   #ifdef DEBUG_PROPAGATION
      else
         MessageInterface::ShowMessage("skr was not initialized]\n");
   #endif

   return retval;
}


bool SPKPropagator::RawStep()
{
   bool retval = false;
   return retval;
}


Real SPKPropagator::GetStepTaken()
{
   return stepTaken;
}


void SPKPropagator::UpdateState()
{
   #ifdef DEBUG_EXECUTION
      MessageInterface::ShowMessage("Updating state to epoch %.12lf\n",
            currentEpoch);
   #endif

   if (skr)
   {
      try
      {
         Rvector6  outState;

         for (UnsignedInt i = 0; i < propObjects.size(); ++i)
         {
            std::string scName = propObjectNames[i];
            Integer id = naifIds[i];

            if ((currentEpoch < ephemStart) || (currentEpoch > ephemEnd))
            {
               std::stringstream errmsg;
               errmsg.precision(16);
               errmsg << "The SPKPropagator "
                      << instanceName
                      << " is attempting to access state data outside of the "
                         "span of the ephemeris data; halting.  ";
               errmsg << "The current SPICE ephemeris covers the A.1 modified "
                         "Julian span "
                      << ephemStart << " to " << ephemEnd << " and the "
                         "requested epoch is " << currentEpoch << ".\n";
               throw PropagatorException(errmsg.str());
            }

            outState = skr->GetTargetState(scName, id, currentEpoch,
                  centralBody);

            /**
             *  @todo: When SPKProp can evolve more than one spacecraft, this
             *  memcpy line needs revision
             */
            std::memcpy(state, outState.GetDataVector(),
                  dimension*sizeof(Real));

            #ifdef DEBUG_PROPAGATION
               MessageInterface::ShowMessage("State at epoch %.12lf is [",
                     currentEpoch);
               for (Integer i = 0; i < dimension; ++i)
               {
                  MessageInterface::ShowMessage("%.12lf", state[i]);
                  if (i < 5)
                     MessageInterface::ShowMessage("   ");
                  else
                     MessageInterface::ShowMessage("]\n");
               }
            #endif
         }
      }
      catch (BaseException &e)
      {
         MessageInterface::ShowMessage(e.GetFullMessage());
      }
   }
}


//------------------------------------------------------------------------------
// void SetEphemSpan(Integer whichOne)
//------------------------------------------------------------------------------
/**
 * Determines the start and end epoch for the SPICE ephemerides associated with
 * the propagated spacecraft
 *
 * @param whichOne Not currrently used.
 */
//------------------------------------------------------------------------------
void SPKPropagator::SetEphemSpan(Integer whichOne)
{
   if (whichOne < 0)
      throw PropagatorException("SPKPropagator::SetEphemSpan(Integer whichOne):"
            " Invalid index");

   if (skr)
   {
      // @todo: When the SPKPropagator supports more than one spacecraft, the
      // ephem span needs to be modified to track spans for each spacecraft
      for (UnsignedInt i = 0; i < naifIds.size(); ++i)
         skr->GetCoverageStartAndEnd(spkFileNames, naifIds[i], ephemStart,
               ephemEnd);

      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage("EphemSpan is [%.12lf %.12lf]\n",
               ephemStart, ephemEnd);
      #endif
   }
}
