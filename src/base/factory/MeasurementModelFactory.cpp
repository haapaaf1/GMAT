//$Header$
//------------------------------------------------------------------------------
//                            MeasurementModelFactory
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/07/23
//
/**
 *  Implementation code for the MeasurementModelFactory class, responsible
 *  for creating MeasurementModel objects.
 */
//------------------------------------------------------------------------------

#include "gmatdefs.hpp"
#include "MeasurementModelFactory.hpp"
#include "MessageInterface.hpp"  // temporary

// Here we include the header files that define our supported models
//#include "GeometricAzElMeasurementModel.hpp"
#include "GeometricRangeMeasurementModel.hpp"
#include "GeometricRangeAzElMeasurementModel.hpp"
//#include "GeometricRangeRaDecMeasurementModel.hpp"

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  MeasurementModel* CreateMeasurementModel(const std::string &ofType,
//                                           const std::string &withName)
//------------------------------------------------------------------------------
/**
 * This method creates and returns an object of the requested
 * MeasurementModel class.
 *
 * @param <ofType> type of MeasurementModel object to create and return.
 * @param <withName> the name for the newly-created MeasurementModel object.
 *
 * @return A pointer to the created object.
 */
//------------------------------------------------------------------------------
MeasurementModel* MeasurementModelFactory::CreateMeasurementModel
            (const std::string &ofType, const std::string &withName)
{

    if (ofType == "Range" || ofType == "GeometricRange")
      return new GeometricRangeMeasurementModel(withName);
    else if (ofType == "RangeAzEl" || ofType == "GeometricRangeAzEl")
      return new GeometricRangeAzElMeasurementModel(withName);
//    else if (ofType == "RangeRate" || ofType == "GeometricRangeRate")
//      return new GeometricRangeRate(withName);
    //else if (ofType == "LightTime")
    //  return new LightTime(withName);
    //else if (ofType == "VariableTransmitterRange")
    //  return new VariableTransmitterRange(withName);
    //else if (ofType == "AntennaTracking")
    //  return new AntennaTracking(withName);
    //else if (ofType == "SunSensor")
    //  return new SunSensor(withName);
    //else if (ofType == "StarSensor")
    //  return new StarSensor(withName);
    //else if (ofType == "GyroPackage")
    //  return new GyroPackage(withName);
    //else if (ofType == "HorizonSensor")
    //  return new HorizonSensor(withName);
    //else if (ofType == "Videometers")
    //  return new Videometers(withName);
    //else if (ofType == "CoherentDoppler")
    //  return new CoherentDoppler(withName);
    //else if (ofType == "NonCoherentDoppler")
    //  return new NonCoherentDoppler(withName);
    //else if (ofType == "VariableTransmitterDoppler")
    //  return new VariableTransmitterDoppler(withName);
    //else if (ofType == "IntegratedDopplerCount")
    //  return new IntegratedDopplerCount(withName);
    //else if (ofType == "IMU")
    //  return new IMU(withName);
    //else if (ofType == "Magnetometer")
    //  return new Magnetometer(withName);
    //else if (ofType == "AzEl" || ofType == "AOAzEl" || ofType == "GeometricAzEl")
    //  return new GeometricAzEl(withName);
//    else if (ofType == "RaDec" ||ofType == "AORaDec" || ofType == "GeometricRaDec")
//      return new GeometricRaDec(withName);
//    else if (ofType == "RangeRaDec" || ofType == "GeometricRangeRaDec")
//      return new GeometricRangeRaDec(withName);

   return NULL;
}


//------------------------------------------------------------------------------
//  MeasurementModelFactory()
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class MeasurementModelFactory.
 * (default constructor)
 */
//------------------------------------------------------------------------------
MeasurementModelFactory::MeasurementModelFactory() :
    Factory     (Gmat::MEASUREMENT_MODEL)
{
   if (creatables.empty())
   {

        creatables.push_back("GeometricRange");
        creatables.push_back("GeometricRangeAzEl");
        //creatables.push_back("GeometricRangeRate");
        //creatables.push_back("LightTime");
        //creatables.push_back("VariableTransmitterRange");
        //creatables.push_back("AntennaTracking");
        //creatables.push_back("SunSensor");
        //creatables.push_back("StarSensor");
        //creatables.push_back("GyroPackage");
        //creatables.push_back("HorizonSensor");
        //creatables.push_back("Videometers");
        //creatables.push_back("CoherentDoppler");
        //creatables.push_back("NonCoherentDoppler");
        //creatables.push_back("VariableTransmitterDoppler");
        //creatables.push_back("IntegratedDopplerCount");
        //creatables.push_back("IMU");
        //creatables.push_back("Magnetometer");
        //creatables.push_back("GeometricAzEl");
        //creatables.push_back("GeometricRaDec");
        //creatables.push_back("GeometricRangeRaDec");

   }
}

//------------------------------------------------------------------------------
//  MeasurementModelFactory(StringArray createList)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class MeasurementModelFactory.
 *
 * @param <createList> list of creatable MeasurementModel objects
 *
 */
//------------------------------------------------------------------------------
MeasurementModelFactory::MeasurementModelFactory(StringArray createList) :
Factory(createList, Gmat::MEASUREMENT_MODEL)
{
}


//------------------------------------------------------------------------------
//  MeasurementModelFactory(const MeasurementModelFactory& fact)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class MeasurementModelFactory.
 * (copy constructor)
 *
 * @param <fact> the factory object to copy to "this" factory.
 */
//------------------------------------------------------------------------------
MeasurementModelFactory::MeasurementModelFactory
    (const MeasurementModelFactory& fact) :
    Factory     (fact)
{
   if (creatables.empty())
   {

       creatables.push_back("GeometricRange");
       creatables.push_back("GeometricRangeAzEl");
       // creatables.push_back("GeometricRangeRate");
        //creatables.push_back("LightTime");
        //creatables.push_back("VariableTransmitterRange");
        //creatables.push_back("AntennaTracking");
        //creatables.push_back("SunSensor");
        //creatables.push_back("StarSensor");
        //creatables.push_back("GyroPackage");
        //creatables.push_back("HorizonSensor");
        //creatables.push_back("Videometers");
        //creatables.push_back("CoherentDoppler");
        //creatables.push_back("NonCoherentDoppler");
        //creatables.push_back("VariableTransmitterDoppler");
        //creatables.push_back("IntegratedDopplerCount");
        //creatables.push_back("IMU");
        //creatables.push_back("Magnetometer");
        //creatables.push_back("GeometricAzEl");
        //creatables.push_back("GeometricRaDec");
        //creatables.push_back("GeometricRangeRaDec");

   }
}


//------------------------------------------------------------------------------
//  CommandFactory& operator= (const CommandFactory& fact)
//------------------------------------------------------------------------------
/**
 * MeasurementModelFactory operator for the MeasurementModelFactory base class.
 *
 * @param <fact> the MeasurementModelFactory object that is copied.
 *
 * @return "this" MeasurementModelFactory with data set to match the
 * input factory (fact).
 */
//------------------------------------------------------------------------------
MeasurementModelFactory& MeasurementModelFactory::operator=
    (const MeasurementModelFactory& fact)
{
   Factory::operator=(fact);
   return *this;
}


//------------------------------------------------------------------------------
// ~MeasurementModelFactory()
//------------------------------------------------------------------------------
/**
 * Destructor for the MeasurementModelFactory base class.
 */
//------------------------------------------------------------------------------
MeasurementModelFactory::~MeasurementModelFactory()
{
}

//---------------------------------
//  protected methods
//---------------------------------

//---------------------------------
//  private methods
//---------------------------------

