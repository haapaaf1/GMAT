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

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include "sstream"
#include "DataFormats.hpp"

using namespace DataFormats; // for data type variable definitions

class ProcessDataFile : public GmatBase
{

public:

    ProcessDataFile(const std::string &itsType, const std::string &itsName);
    ProcessDataFile(const ProcessDataFile &pdf);
    virtual ~ProcessDataFile();

    virtual GmatBase *Clone() const;

    
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

    const std::string* GetDataFormatDescriptions() const;
    std::string GetDataFormatNameText(const Integer &id) const;
    Integer GetDataFormatID(const std::string &label);

    void SetDataFormatID(Integer mName);
    Integer GetDataFormatID() const;
  
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

private:
    
    static const Integer NUM_DATAFORMATS = 3;
    static const std::string DATAFORMAT_DESCRIPTIONS[NUM_DATAFORMATS];
    
protected:

    enum DATAFILE_REPS {
	B3_ID = 0,
	SLR_ID,
	TLE_ID,
	EndDataFormatReps
    };

    // Name of the measurement model being used
    Integer dataFormatID;
    static const Integer MAX_LINES = 3;
    std::string lineFromFile[MAX_LINES];
    std::string dataFileName;
    std::ifstream myFile;
    bool isOpen;

};


//------------------------------------------------------------------------------
// template <class T> bool ProcessDataFile::from_string(T& t,
//       const std::string& s,
//                 std::ios_base& (*f)(std::ios_base&))
//------------------------------------------------------------------------------
/**
 * Typesafe conversion from string to integer, float, etc
 */
//------------------------------------------------------------------------------

template <class T> bool ProcessDataFile::from_string(T& t, const std::string& s,
                 std::ios_base& (*f)(std::ios_base&))
{
  std::istringstream iss(s);
  return !(iss >> f >> t).fail();
}


#endif	/* _ProcessDataFile_hpp */

