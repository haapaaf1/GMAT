//$Header$
//------------------------------------------------------------------------------
//                             ProcessDataFile
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/07/30
//
/**
 *
 * Implements LatLonHgt Base class to contain the associated elements
 * Latitude, Longitude, Height, and a flag to determine geocentric versus
 * geodetic angles.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessDataFile_hpp
#define	ProcessDataFile_hpp

#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include "DataFormats.hpp"

using namespace DataFormats; // for data type variable definitions

class ProcessDataFile
{

public:
    
    ProcessDataFile();
    virtual ~ProcessDataFile();

    // Specific data type processing functions
    virtual bool GetData(slr_header &mySLRheader);
    virtual bool GetData(slr_header &mySLRheader, slr_obtype &mySLRdata);
    virtual bool GetData(tle_obtype &myTLEdata);
    virtual bool GetData(b3_obtype &myB3data);

    // String processing utility functions
    std::string Trim(std::string s);
    std::string Ilrs2Cospar(std::string ilrsSatnum);
    template <class T> bool from_string(T& t, const std::string& s, 
                 std::ios_base& (*f)(std::ios_base&));
    bool Overpunch(std::string code, Integer &digit, Integer &sign );

    // functions to extract a line from file
    bool ReadLineFromFile( std::string &line );
    std::string ReadLineFromFile();

    std::string GetLine(Integer &lineNum);
    void SetLine(std::string &line, Integer &lineNum);

    // Get/Set isOpen variable
    bool GetIsOpen() const;
    void SetIsOpen(const bool &flag);
    
    // File name functions
    std::string GetFileName();
    void SetFileName(std::string &myFileName);
    void SetFileName(const char* myFileName);
    
    bool OpenFile();
    bool CloseFile();
    
    bool IsEOF();
    
    enum B3_TYPE_REPS {
	RANGERATEONLY_ID = 1,
        AZEL_ID,
	RAZEL_ID,
	RAZELRR_ID,
	RAZELRR2_ID,
	RADEC_ID,
	RANGEONLY_ID,
	AZELSENSORPOS_ID = 8,
	RADECSENSORPOS_ID,
	EndB3TypeReps    
    };
  
protected:
    
    static const Integer MAX_LINES = 3;
    std::string lineFromFile[MAX_LINES];
    std::string dataFileName;
    std::ifstream myFile;
    bool isOpen;

};

#endif	/* _ProcessDataFile_hpp */

