//------------------------------------------------------------------------------
//                         GeometricMeasurement
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/06/29
//
/**
 * Definition of the geometric measurement base class.
 */
//------------------------------------------------------------------------------


#ifndef GEOMETRICMEASUREMENT_HPP_
#define GEOMETRICMEASUREMENT_HPP_

#include "EstimationDefs.hpp"
#include "MeasurementData.hpp"

#include "Rmatrix.hpp"
#include "SpacePoint.hpp"


/**
 * GeometricMeasurement is the base class for measurements based on geometry
 *
 * The GeometricMeasurement class is a CoreMeasurement class that calculates
 * measurement values based on the position and velocity of several SpacePoints.
 * Typical GeometricMeasurements are the range between two points, the range
 * rate between the points, and angles from one point to another.
 *
 * GeometricMeasurement objects are CoreMeasurement objects.  As such, they
 * should never be encountered outside of a MeasurementModel container.  The
 * GeometricMeasurement class is derived from GmatBase so that GMAT's factory
 * subsystem can be used to create instances of the class.  These instances are
 * constructed and passed to a MeasurementModel when the MeasurementModel type
 * is set.
 */
class GeometricMeasurement : public /*CoreMeasurement*/ GmatBase
{
public:
   GeometricMeasurement(const std::string &type, const std::string &nomme = "");
   // Abstract to prevent instantiation
   virtual ~GeometricMeasurement() = 0;
   GeometricMeasurement(const GeometricMeasurement& gm);
   GeometricMeasurement&      operator=(const GeometricMeasurement& gm);

   virtual bool               Initialize();

   // Here are the parameter access shells in case we need them later
//   virtual std::string        GetParameterText(const Integer id) const;
//   virtual Integer            GetParameterID(const std::string &str) const;
//   virtual Gmat::ParameterType
//                              GetParameterType(const Integer id) const;
//   virtual std::string        GetParameterTypeString(const Integer id) const;

   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name = "");
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name,
                                     const Integer index);

   // Move to CoreMeasurement when it is created
   MeasurementData*           GetMeasurementDataPointer();
   Rmatrix*                   GetDerivativePointer();

   virtual const MeasurementData&
                              CalculateMeasurement(bool withDerivatives = true);
   virtual const Rmatrix&     CalculateMeasurementDerivatives();

   void                       SetUniqueId(Integer id);

protected:
   MeasurementData            currentMeasurement;
   Rmatrix                    currentDerivatives;

   StringArray                participantNames;
   SpacePoint                 *anchorPoint;
   std::vector<SpacePoint*>   participants;

   Integer                    uniqueId;

   /// Support members for the range vector calculation
   Rvector3                   p1Loc;
   Rvector3                   p2Loc;
   Rvector3                   rangeVec;
   Integer                    satEpochID;

   virtual bool               Evaluate(bool withDerivatives = false) = 0;
   void                       CalculateRangeVector();

   /// Enumerated parameter IDs
   enum
   {
       PARTICIPANT = GmatBaseParamCount,
       GeometricMeasurementParamCount
   };

//   // In case they are needed
//   /// Array of supported parameters
//   static const std::string PARAMETER_TEXT[GeometricMeasurementParamCount -
//                                           GmatBaseParamCount];
//   /// Array of parameter types
//   static const Gmat::ParameterType PARAMETER_TYPE[GeometricMeasurementParamCount -
//                                                   GmatBaseParamCount];
};

#endif /* GEOMETRICMEASUREMENT_HPP_ */
