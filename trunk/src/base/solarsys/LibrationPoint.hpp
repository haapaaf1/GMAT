//$Id$
//------------------------------------------------------------------------------
//                                  LibrationPoint
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Wendy C. Shoan
// Created: 2005.04.13
//
/**
 * This is the base class for libration points.
 *
 * @note This is an abstract class.
 */
//------------------------------------------------------------------------------

#ifndef LibrationPoint_hpp
#define LibrationPoint_hpp

#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "SpacePoint.hpp"
#include "CalculatedPoint.hpp"
#include "A1Mjd.hpp"
#include "Rmatrix.hpp"
#include "Rvector6.hpp"
#include "TimeTypes.hpp"

/**
 *
 * LibrationPoint derives from CalculatedPoint.
 *
 */
class GMAT_API LibrationPoint : public CalculatedPoint
{
public:
   // default constructor, specifying libration point name
   LibrationPoint(const std::string &itsName = "");
   // copy constructor
   LibrationPoint(const LibrationPoint &lp);
   // operator=
   LibrationPoint& operator=(const LibrationPoint &lp);
   // destructor
   virtual ~LibrationPoint();
   
   // methods inherited from SpacePoint, that must be implemented here
   // in the derived class
   virtual const Rvector6 GetMJ2000State(const A1Mjd &atTime);
   virtual const Rvector3 GetMJ2000Position(const A1Mjd &atTime);
   virtual const Rvector3 GetMJ2000Velocity(const A1Mjd &atTime);   
   
   // Parameter access methods - overridden from GmatBase
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const; 
   virtual Gmat::ParameterType 
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;
   
   virtual std::string  GetStringParameter(const Integer id) const;
   virtual std::string  GetStringParameter(const std::string &label) const;
   virtual bool         SetStringParameter(const Integer id, 
                                           const std::string &value);
   virtual bool         SetStringParameter(const std::string &label, 
                                           const std::string &value);
   // These two added to satisfy GCC, which apparently has trouble finding 
   // the methods inherited from CalculatedPoint
   virtual bool         SetStringParameter(const Integer id, 
                                           const std::string &value,
                                           const Integer index);
   virtual bool         SetStringParameter(const std::string &label, 
                                           const std::string &value,
                                           const Integer index);
   
   virtual const        ObjectTypeArray& GetRefObjectTypeArray();
   virtual const StringArray& 
                        GetRefObjectNameArray(const Gmat::ObjectType type);
   virtual bool         SetRefObject(GmatBase *obj, 
                                     const Gmat::ObjectType type,
                                     const std::string &name = "");
   
   virtual GmatBase*    Clone() const;
   virtual void         Copy(const GmatBase* orig);
   
   
protected:

   enum
   {
      PRIMARY_BODY_NAME = CalculatedPointParamCount, 
      SECONDARY_BODY_NAME,
      WHICH_POINT,
      LibrationPointParamCount
   };
   static const std::string
      PARAMETER_TEXT[LibrationPointParamCount - CalculatedPointParamCount];
   static const Gmat::ParameterType
      PARAMETER_TYPE[LibrationPointParamCount - CalculatedPointParamCount];
   
   static const Real CONVERGENCE_TOLERANCE;// = 1.0e-8;
   static const Real MAX_ITERATIONS;//        = 2000;
   
   std::string primaryBodyName;
   std::string secondaryBodyName;
   std::string whichPoint;
   
   SpacePoint  *primaryBody;
   SpacePoint  *secondaryBody;
   
private:
   
   //------------------------------------------------------------------------------
   //  void CheckBodies()
   //------------------------------------------------------------------------------
   /**
    * Method that checks to make sure the bodyList has
    * been defined appropriately (i.e. all are of a type that makes sense).
    *
    */
    //------------------------------------------------------------------------------
   virtual void CheckBodies();      
};
#endif // LibrationPoint_hpp

