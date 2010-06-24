//$Header$
//------------------------------------------------------------------------------
//                             TDRSSUTDFObType
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2010/03/01
//
/**
 *
 * This class specifies the TDRSS UTDF observation data type which has certain
 * variable data in the base UTDF format pre-specified and constant.
 *
 */
//------------------------------------------------------------------------------

#ifndef _TDRSSUTDFDATAFIELD_HPP
#define	_TDRSSUTDFDATAFIELD_HPP

#include "UTDFObType.hpp"

class TDRSSUTDFObType : public TDRSSUTDFObType
{

public :

    TDRSSUTDFObType(const std::string &type, const std::string &name);
    TDRSSUTDFObType(const TDRSSUTDFObType &ob);
    const TDRSSUTDFObType& operator=(const TDRSSUTDFObType &ob);
    virtual ~TDRSSUTDFObType();
    
    bool Validate(string record);

#endif	/* _TDRSSUTDFDATAFIELD_HPP */

