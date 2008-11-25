//$Header$
//------------------------------------------------------------------------------
//                             DataFile
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

#ifndef DataFile_hpp
#define	DataFile_hpp

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include "sstream"
#include "DataFormats.hpp"
#include "DataFileException.hpp"
#include "MessageInterface.hpp"

using namespace DataFormats; // for data type variable definitions

class DataFile : public GmatBase
{

public:

    DataFile(const std::string &itsType, const std::string &itsName);
    DataFile(const DataFile &pdf);
    virtual ~DataFile();

    // Initialization happens here
    virtual bool Initialize();

    // Methods overridden from the GmatBase clase
    virtual GmatBase *Clone() const;
    virtual void        Copy(const GmatBase* orig);      
    virtual std::string     GetParameterText(const Integer id) const;
    virtual Integer         GetParameterID(const std::string &str) const;
    virtual Gmat::ParameterType
                           GetParameterType(const Integer id) const;
    virtual bool        IsParameterReadOnly(const Integer id) const;
    virtual bool        IsParameterReadOnly(const std::string &label) const;
    virtual std::string GetStringParameter(const Integer id) const;
    virtual bool SetStringParameter(const Integer id, const std::string &value);

    virtual Integer GetIntegerParameter(const Integer id) const;
    virtual Integer GetIntegerParameter(const std::string &label) const;
    virtual Integer SetIntegerParameter(const Integer id, const Integer value);
    virtual Integer SetIntegerParameter(const std::string &label, const Integer value);
  
    // Specific data type processing functions
    virtual bool GetData(std::ifstream &theFile, slr_header &mySLRheader);
    virtual bool GetData(std::ifstream &theFile, slr_header &mySLRheader, slr_obtype &mySLRdata);
    virtual bool GetData(std::ifstream &theFile, tle_obtype &myTLEdata);
    virtual bool GetData(std::ifstream &theFile, b3_obtype &myB3data);

    // String processing utility functions
    std::string Trim(std::string s);
    template <class T> bool from_string(T& t, const std::string& s,
                 std::ios_base& (*f)(std::ios_base&));
    bool Overpunch(std::string code, Integer &digit, Integer &sign );

    // functions to extract a line from file
    bool ReadLineFromFile(std::ifstream &theFile, std::string &line );
    std::string ReadLineFromFile(std::ifstream &theFile);

    std::string GetLine(Integer &lineNum);
    void SetLine(std::string &line, Integer &lineNum);
    
    const std::string* GetFileFormatDescriptions() const;
    std::string GetFileFormatDescriptionText(const Integer &id) const;
    Integer GetFileFormatID(const std::string &label);
    void SetFileFormatID(const std::string &label);

    void SetNumLines(const Integer &nl);
    Integer GetNumLines() const;
    
    void SetFileFormatID(const Integer &mName);
    Integer GetFileFormatID() const;

    void SetFileFormatName(const std::string &fName);
    std::string GetFileFormatName() const;
  
    // Get/Set isOpen variable
    bool GetIsOpen() const;
    void SetIsOpen(const bool &flag);

    // File name functions
    std::string GetFileName();
    void SetFileName(std::string &myFileName);
    void SetFileName(const char* myFileName);

    bool OpenFile(std::ifstream &theFile);
    bool CloseFile(std::ifstream &theFile);

    bool IsEOF(std::ifstream &theFile);
    
    enum OBSERVATION_TYPE_REPS {
	RANGE_ID = 0,
	RANGERATE_ID,
	AZIMUTH_ID,
	AZIMUTHRATE_ID,
	ELEVATION_ID,
	ELEVATIONRATE_ID,
	RIGHTASCENSION_ID,
	RIGHTASCENSIONRATE_ID,
	DECLINATION_ID,
	DECLINATIONRATE_ID,
	TWOWAYTIMEOFFLIGHT_ID,
	CARTESIANSTATE_ID,
	X_ID,
	XDOT_ID,
	Y_ID,
	YDOT_ID,
	Z_ID,
	ZDOT_ID,
	ORBITELEMENTSTATE_ID,
	SEMIMAJAXIS_ID,
	ECCENTRICITY_ID,
	INCLINATION_ID,
	ARGPER_ID,
	RAAN_ID,
	TRUEANOM_ID,
	EndObservationTypeReps
    };

private:
    
    static const Integer NUM_FILEFORMATS = 3;
    static const std::string FILEFORMAT_DESCRIPTIONS[NUM_FILEFORMATS];
    
protected:

       /// Published parameters for data files

   enum
   {
      FILENAME_ID   = GmatBaseParamCount,
      FILEFORMAT_ID,
      NUMLINES_ID,
      DataFileParamCount
   };

   static const std::string    PARAMETER_TEXT[DataFileParamCount -
                                              GmatBaseParamCount];
   static const Gmat::ParameterType
                               PARAMETER_TYPE[DataFileParamCount -
                                              GmatBaseParamCount];
   
    enum DATAFILE_REPS {
	B3_ID = 0,
	SLR_ID,
	TLE_ID,
	EndFileFormatReps
    };

    // ID of the measurement model being used
    std::string fileFormatName;
    Integer fileFormatID;
    
        
    // numLines tells the file reader how many lines to read in at a time
    // This is important for TLE's that can have 2 or 3 lines of data
    // depending if a comment line is included for each TLE
    Integer numLines;
    StringArray lineFromFile;
    std::string dataFileName;
    bool isOpen;

};


//------------------------------------------------------------------------------
// template <class T> bool DataFile::from_string(T& t,
//       const std::string& s,
//                 std::ios_base& (*f)(std::ios_base&))
//------------------------------------------------------------------------------
/**
 * Typesafe conversion from string to integer, float, etc
 */
//------------------------------------------------------------------------------

template <class T> bool DataFile::from_string(T& t, const std::string& s,
                 std::ios_base& (*f)(std::ios_base&))
{
  std::istringstream iss(s);
  return !(iss >> f >> t).fail();
}


#endif	/* _DataFile_hpp */

