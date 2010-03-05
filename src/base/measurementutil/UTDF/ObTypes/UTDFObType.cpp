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


#include "UTDFObType.hpp";

//------------------------------------------------------------------------------
//  UTDFObType(const std::string &type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
UTDFObType::UTDFObType(const std::string &type, const std::string &name) :
   ObType(type, name),
   networkHeader(NULL)
{
}

//------------------------------------------------------------------------------
//  UTDFObType(const UTDFObType &ob)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
UTDFObType::UTDFObType(const UTDFObType &ob) : ObType(ob),
    networkHeader(ob.networkHeader)
{
}

//---------------------------------------------------------------------------
//  UTDFObType& operator=(const UTDFObType &ob)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <ob> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const UTDFObType& UTDFObType::operator=(const UTDFObType &ob)
{
   if (&ob == this)
      return *this;

   ObType::operator=(ob);

   networkHeader = ob.networkHeader;

   return *this;
}

//------------------------------------------------------------------------------
//  ~UTDFObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
UTDFObType::~UTDFObType()
{
}

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// void SetNetworkHeader(NetworkHeader *myNetworkHeader)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the UTDF Network Header variable construct.
 *
 */
//------------------------------------------------------------------------------
void UTDFObType::SetNetworkHeader(NetworkHeader *myNetworkHeader)
{
   networkHeader = myNetworkHeader;
}

//------------------------------------------------------------------------------
// NetworkHeader* GetNetworkHeader()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the UTDF Network Header variable construct
 *
 * @return The pointer to the UTDF Network Header
 *
 */
//------------------------------------------------------------------------------
NetworkHeader* UTDFObType::GetNetworkHeader()
{
   return networkHeader;
}