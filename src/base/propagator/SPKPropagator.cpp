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

      // todo: Remove hardcoded path here
      if (skr->IsLoaded("./files/planetary_ephem/spk/de421.bsp") == false)
         skr->LoadKernel("./files/planetary_ephem/spk/de421.bsp");

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

         for (UnsignedInt j = 0; j < spices.size(); ++j)
         {
            if (skr->IsLoaded(spices[j]) == false)
               skr->LoadKernel(spices[j]);

            if (find(spkFileNames.begin(), spkFileNames.end(), spices[j]) ==
                  spkFileNames.end())
               spkFileNames.push_back(spices[j]);
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
                     errmsg << "The current SPICE ephemeris covers the A.1 modified "
                               "Julian span ";
                     errmsg << ephemStart << " to " << ephemEnd << " and the "
                              "requested epoch is " << currentEpoch << ".\n";
                     throw PropagatorException(errmsg.str());
                  }
                  outState = skr->GetTargetState(scName, id, currentEpoch,
                        centralBody);

                  std::memcpy(j2kState, outState.GetDataVector(),
                        dimension*sizeof(Real));
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
         MessageInterface::ShowMessage("%.12lf", j2kState[i]);
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
      MessageInterface::ShowMessage("SPKPropagator::Step() entered; stepsize = "
            "%.12lf; timeFromEpoch = %.12lf\n", ephemStep, timeFromEpoch);
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
            std::memcpy(j2kState, outState.GetDataVector(),
                  dimension*sizeof(Real));
            std::memcpy(state, outState.GetDataVector(),
                  dimension*sizeof(Real));

            #ifdef DEBUG_PROPAGATION
               MessageInterface::ShowMessage("State at epoch %.12lf is [",
                     currentEpoch);
               for (Integer i = 0; i < dimension; ++i)
               {
                  MessageInterface::ShowMessage("%.12lf", j2kState[i]);
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
             *  @todo: When SPKProp can evolve more than one spacecraft, these
             *  memcpy lines need revision
             */
            std::memcpy(j2kState, outState.GetDataVector(),
                  dimension*sizeof(Real));
            std::memcpy(state, outState.GetDataVector(),
                  dimension*sizeof(Real));

            #ifdef DEBUG_PROPAGATION
               MessageInterface::ShowMessage("State at epoch %.12lf is [",
                     currentEpoch);
               for (Integer i = 0; i < dimension; ++i)
               {
                  MessageInterface::ShowMessage("%.12lf", j2kState[i]);
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


void SPKPropagator::SetEphemSpan(Integer whichOne)
{
   if (whichOne < 0)
      throw PropagatorException("SPKPropagator::SetEphemSpan(Integer whichOne):"
            " Invalid index");

   if (skr)
   {
      // Load the coverage data
      SPICEDOUBLE_CELL(cover, 200000);
      spkcov_c (spkFileNames[0].c_str(), naifIds[0], &cover);

      // Find out how many intervals are in the file
      SpiceInt niv = wncard_c(&cover);

      // Report the interval(s)
      SpiceDouble b, e;
      int i;

      // Get the endpoints of the ith interval.
      wnfetd_c(&cover, 0, &b, &e);
      ephemStart = skr->SpiceTimeToA1(b);
      ephemEnd   = skr->SpiceTimeToA1(e);

      for (i = 1; i < niv; ++i)
      {
         // Get the endpoints of the ith interval.
         wnfetd_c(&cover, i, &b, &e);
         if (ephemStart > skr->SpiceTimeToA1(b))
            ephemStart = skr->SpiceTimeToA1(b);
         if (ephemEnd < skr->SpiceTimeToA1(e))
            ephemEnd   = skr->SpiceTimeToA1(e);
      }
   }
}
