//$Header$
//------------------------------------------------------------------------------
//                                  TimeData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2004/01/12
//
/**
 * Declares Time related data class.
 */
//------------------------------------------------------------------------------
#ifndef TimeData_hpp
#define TimeData_hpp

#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "RefData.hpp"

class GMAT_API TimeData : public RefData
{
public:

    TimeData();
    TimeData(const TimeData &ct);
    TimeData& operator= (const TimeData& right);
    virtual ~TimeData();

    bool IsInitialEpochSet();
    Real GetInitialEpoch();
    void SetInitialEpoch(const Real &initialEpoch);
    
    Real GetCurrentTimeReal(const std::string &str);
    Real GetElapsedTimeReal(const std::string &str);

    //loj: future build
    //std::string GetCurrentTimeString(const std::string &str);
    //std::string GetElapsedTimeString(const std::string &str);
    
    // The inherited methods from RefData
    virtual bool ValidateRefObjects(GmatBase *param);
    virtual const std::string* GetValidObjectList() const;
    
protected:
    
    // The inherited methods from RefData
    virtual void Initialize(); //loj: 3/31/04 added
    virtual bool IsValidObject(GmatBase *obj);

    Real mInitialEpoch;
    bool mIsInitialEpochSet;
    
    const static Real MJD_OFFSET = 2430000.0; //loj: check the value
    const static Real TIME_REAL_UNDEFINED = -9876543210.1234;

    enum
    {
        SPACECRAFT = 0,
        TimeDataObjectCount
    };
    
    static const std::string VALID_OBJECT_LIST[TimeDataObjectCount];
    
};
#endif // TimeData_hpp

