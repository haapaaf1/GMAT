//$Id: FlowerConstellation.hpp 5835 2008-09-12 00:01:15Z djcinsb $
//------------------------------------------------------------------------------
//                              FlowerConstellation
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2010/03/12
//
/**
 *
 * Implements the Flower Constellation Lattice Theory using the Formation
 * class to contain the satellite objects that are generated.
 *
 */
//------------------------------------------------------------------------------

#ifndef _FLOWERCONSTELLATION_HPP
#define	_FLOWERCONSTELLATION_HPP

#include "Formation.hpp"
#include "Satellite.hpp"

class GMAT_API FlowerConstellation : public Formation
{

public:

    FlowerConstellation();
    FlowerConstellation(const FlowerConstellation& orig);
    virtual ~FlowerConstellation();

    bool Initialize();

    // Access methods derived classes can override
    virtual std::string  GetParameterText(const Integer id) const;
    virtual Integer      GetParameterID(const std::string &str) const;
    virtual Gmat::ParameterType
                         GetParameterType(const Integer id) const;
    virtual std::string  GetParameterTypeString(const Integer id) const;
    virtual bool         IsParameterReadOnly(const Integer id) const;

    enum FC_PARAMS
    {
        NUMORBITS_ID = FormationParamCount,
        NUMSATS_ID,
        FCParamCount
    };

private:

   /// Array of supported parameters
   static const std::string
      PARAMETER_TEXT[FCParamCount - FormationParamCount];
   /// Array of parameter types
   static const Gmat::ParameterType
      PARAMETER_TYPE[FCParamCount - FormationParamCount];
   
    // Number of orbit planes
    Integer no;
    // Number of satellites
    Integer ns;



};

#endif	/* _FLOWERCONSTELLATION_HPP */

