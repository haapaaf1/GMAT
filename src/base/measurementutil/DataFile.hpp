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
 * Implements the DataFile base class that allows a local file to be processed
 * according to a specified ASCII and/or binary data file format.
 *
 */
//------------------------------------------------------------------------------

#ifndef DataFile_hpp
#define	DataFile_hpp

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include "sstream"
#include "DataFormats.hpp"
#include "DataFileException.hpp"
#include "MessageInterface.hpp"
#include <pcrecpp.h>
#include <math.h>
#include "StringUtil.hpp"           // for ToString()

using namespace std; // so we don't have to type std::cout and std::endl

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

    virtual bool        IsParameterReadOnly(const Integer id) const;
    virtual bool        IsParameterReadOnly(const std::string &label) const;
    
    virtual std::string GetStringParameter(const Integer id) const;
    virtual bool SetStringParameter(const Integer id, const std::string &value);
    virtual Integer GetIntegerParameter(const Integer id) const;
    virtual Integer GetIntegerParameter(const std::string &label) const;
    virtual Integer SetIntegerParameter(const Integer id, const Integer value);
    virtual Integer SetIntegerParameter(const std::string &label, const Integer value);

    // Method to obtain data from the file
    virtual bool GetData();

    // Method to write data to file
    virtual bool WriteData();
    virtual bool WriteDataHeader();
    virtual bool WriteDataSubHeader();
    virtual bool WriteMetadata();

    // String processing utility functions
    std::string Trim(std::string s);
    template <class T> bool from_string(T& t, const std::string& s,
                 std::ios_base& (*f)(std::ios_base&));
    bool ReverseOverpunch(std::string code, Integer &digit, Integer &sign );
    std::string Overpunch(const Real &number );

    // functions to sort the data
    //bool SortData();

    // functions to extract a line from file
    bool ReadLineFromFile(std::string &line);
    std::string ReadLineFromFile();
    
    // methods to get/set DataFile object parameters
    void SetNumLines(const Integer &nl);
    Integer GetNumLines() const;

    void SetNumMeasurements(const Integer &nm);
    Integer GetNumMeasurements() const;
    
    void SetFileFormatID(const Integer &mName);
    Integer GetFileFormatID() const;

    void SetFileFormatName(const std::string &fName);
    std::string GetFileFormatName() const;
  
    bool GetIsOpen() const;
    void SetIsOpen(const bool &flag);

    bool GetIsSorted() const;
    void SetIsSorted(const bool &flag);

    std::string GetFileName();
    void SetFileName(std::string &myFileName);
    void SetFileName(const char* myFileName);

    // Open/Close file methods
    bool OpenFile();
    bool CloseFile();

    // Test to see if we are at end of file
    bool IsEOF();

    // methods to get/set descriptions of allowable file formats
    const std::string* GetFileFormatDescriptions() const;
    std::string GetFileFormatDescriptionText(const Integer &id) const;
    Integer GetFileFormatID(const std::string &label);
    void SetFileFormatID(const std::string &label);
    
    virtual std::string GetParameterText(const Integer id) const;
    virtual Integer GetParameterID(const std::string &str) const;
    virtual Gmat::ParameterType GetParameterType(const Integer id) const;
    virtual std::string GetParameterTypeString(const Integer id) const;
    
    // Measurement Data Access functions
    bool AdvanceToNextOb();
    bool BackUpToPreviousOb();    
 
    enum DATATYPE_REPS
    {
	EndDataTypeReps = 0,
    };

    enum TIMESYSTEM_REPS
    {
	EndTimeReps = 0,
    };
    
    enum DATAFILE_REPS
    {
	B3_ID = 0,
	SLR_ID,
	TLE_ID,
        SP3C_ID,
        RINEX_ID,
        UTDF_ID,
	TRK234_ID,
	CCSDS_TDM_ID,
	CCSDS_OPM_ID,
	CCSDS_OEM_ID,
	CCSDS_APM_ID,
	CCSDS_AEM_ID,
	EndFileFormatReps
    };

private:
    
    static const std::string FILEFORMAT_DESCRIPTIONS[EndFileFormatReps];

protected:

  /// Published parameters for data files
   enum
   {
      FILENAME_ID   = GmatBaseParamCount,
      FILEFORMAT_ID,
      NUMLINES_ID,
      READWRITEMODE_ID,
      DataFileParamCount
   };

   static const std::string    PARAMETER_TEXT[DataFileParamCount -
                                              GmatBaseParamCount];
   static const Gmat::ParameterType
                               PARAMETER_TYPE[DataFileParamCount -
                                              GmatBaseParamCount];
   
    // File format and filename of the data being used
    std::string fileFormatName;
    Integer fileFormatID;
    std::string dataFileName;
           
    // numLines tells the file reader how many lines to read in at a time
    // This is important for TLE's that can have 2 or 3 lines of data
    // depending if a comment line is included for each TLE
    Integer numLines;

    Integer numMeasurements;

    // Flag to indicate if the file is opened
    bool isOpen;
    
    // Flag to indicate if the data has been chronologically sorted
    bool isSorted;

    // Flag to indicate if the file is open for reading or writing
    std::string readWriteMode;

    fstream *theFile;

};


//------------------------------------------------------------------------------
// template <class T> bool from_string(T& t, const std::string& s,
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

