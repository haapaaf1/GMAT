//$Header$
//------------------------------------------------------------------------------
//                             UTDFObType
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
 * This class specifies the UTDF base observation data type from which the
 * various data format observation types flow.
 *
 */
//------------------------------------------------------------------------------

#ifndef _UTDFDATAFIELD_HPP
#define	_UTDFDATAFIELD_HPP

#include "ObType.hpp"
#include "NetworkHeader.hpp"

class UTDFObType : public ObType
{

public :

    UTDFObType(const std::string &type, const std::string &name);
    UTDFObType(const UTDFObType &ob);
    const UTDFObType& operator=(const UTDFObType &ob);
    virtual ~UTDFObType();

    void SetNetworkHeader(NetworkHeader *myNetworkHeader);
    NetworkHeader* GetNetworkHeader();

    virtual bool Validate() const = 0;

    friend class UTDFDataFile;

protected:

    // Pointer to the network header record associated with this data point
    NetworkHeader *networkHeader;

};

#endif	/* _UTDFDATAFIELD_HPP */

