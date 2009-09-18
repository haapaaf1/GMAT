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
#include "Obtype.hpp"
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
    const DataFile& operator=(const DataFile &pdf);
    
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

    // Methods to write data to the file defined on this object
    virtual bool WriteData(Obtype *myObtype);
    virtual bool WriteDataHeader();
    virtual bool WriteDataSubHeader();
    virtual bool WriteMetadata();
    
    // Methods to write data to external files
    virtual bool WriteData(fstream *myFile, Obtype *myObtype);
    virtual bool WriteDataHeader(fstream *myFile);
    virtual bool WriteDataSubHeader(fstream *myFile);
    virtual bool WriteMetadata(fstream *myFile);

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

    Integer GetNumMeasurements() const;
    
    void SetFileFormatID(const Integer &mName);
    Integer GetFileFormatID() const;

    void SetFileFormatName(const std::string &fName);
    std::string GetFileFormatName() const;
  
    bool GetIsOpen() const;
    void SetIsOpen(const bool &flag);

    std::string GetFileName();
    void SetFileName(std::string &myFileName);
    void SetFileName(const char* myFileName);

    fstream* GetFstream() const;
    void SetFstream(fstream *myFstream);

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

    // Sorting Functions
    void SortByEpoch(bool sortOrder = DESCENDING);
    void SortBySatID(bool sortOrder = DESCENDING);
    void SortBySensorID(bool sortOrder = DESCENDING);
    void SortByInternationalDesignator(bool sortOrder = DESCENDING);

    Integer GetSortedBy() const;
    void SetSortedBy(const Integer sortID);
    bool IsSortedByEpoch() const;
    bool IsSortedBySatID() const;
    bool IsSortedBySensorID() const;
    bool IsSortedByInternationalDesignator() const;
   
    enum SORTORDER
    {
	ASCENDING = 0,
	DESCENDING
    };
    
    // Ascending epoch sorting function

    struct AscendingEpochSort
    {
	bool operator()(Obtype* rpStart, Obtype* rpEnd)
	{
	    return rpStart->GetEpoch() < rpEnd->GetEpoch();
	}
    };

    // Descending epoch sorting function

    struct DescendingEpochSort
    {
	bool operator()(Obtype* rpStart, Obtype* rpEnd)
	{
	    return rpStart->GetEpoch() > rpEnd->GetEpoch();
	}
    };

    // Ascending satellite ID sorting function

    struct AscendingSatIDSort
    {
	bool operator()(Obtype* rpStart, Obtype* rpEnd)
	{
	    return rpStart->GetSatID() < rpEnd->GetSatID();
	}
    };

    // Descending satellite ID sorting function

    struct DescendingSatIDSort
    {
	bool operator()(Obtype* rpStart, Obtype* rpEnd)
	{
	    return rpStart->GetSatID() > rpEnd->GetSatID();
	}
    };
    
    // Ascending sensor ID sorting function

    struct AscendingSensorIDSort
    {
	bool operator()(Obtype* rpStart, Obtype* rpEnd)
	{
	    return rpStart->GetSensorID() < rpEnd->GetSensorID();
	}
    };

    // Descending sensor ID sorting function

    struct DescendingSensorIDSort
    {
	bool operator()(Obtype* rpStart, Obtype* rpEnd)
	{
	    return rpStart->GetSensorID() > rpEnd->GetSensorID();
	}
    };
    
    // Ascending sensor ID sorting function

    struct AscendingInternationalDesignatorSort
    {
	bool operator()(Obtype* rpStart, Obtype* rpEnd)
	{
	    return rpStart->GetInternationalDesignator() < 
		     rpEnd->GetInternationalDesignator();
	}
    };

    // Descending sensor ID sorting function

    struct DescendingInternationalDesignatorSort
    {
	bool operator()(Obtype* rpStart, Obtype* rpEnd)
	{
	    return rpStart->GetInternationalDesignator() > 
		     rpEnd->GetInternationalDesignator();
	}
    };
    
    // Measurement Data Access functions
    bool AdvanceToNextOb();
    bool BackUpToPreviousOb();
    
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
   
    static const std::string DataFile::OPTIONAL_SIGN;
    static const std::string DataFile::MANDATORY_DIGITS;
    static const std::string DataFile::DECIMAL_POINT;
    static const std::string DataFile::OPTIONAL_DIGITS;
    static const std::string DataFile::OPTIONAL_EXPONENT;

    static const std::string DataFile::REGEX_NUMBER;
    static const std::string DataFile::REGEX_LETTER;
    static const std::string DataFile::REGEX_DATE;
    static const std::string DataFile::REGEX_SCINUMBER;
    static const std::string DataFile::REGEX_CCSDS_DATE;
    static const std::string DataFile::REGEX_CCSDS_KEYWORD;

    // File format and filename of the data being used
    std::string fileFormatName;
    Integer fileFormatID;
    std::string dataFileName;
           
    // numLines tells the file reader how many lines to read in at a time
    // This is important for TLE's that can have 2 or 3 lines of data
    // depending if a comment line is included for each TLE
    Integer numLines;

    // The vector container of data
    ObtypeVector theData;
            
    //Current iterators pointing at data
    ObtypeVector::const_iterator i_theData;

    // Flag to indicate if the file is opened
    bool isOpen;
    
    // Enumeration to indicate if the data has been sorted
    Integer sortedBy;

    enum FILESORT_REPS
    {
        NOT_SORTED = 0,
        SORTED_BY_EPOCH,
        SORTED_BY_SATID,
        SORTED_BY_SENSORID,
        SORTED_BY_INTERNATIONALDESIGNATOR,
        EndFileSortReps
    };

    // Flag to indicate if the file is open for reading or writing
    std::string readWriteMode;

    // This is the pointer to the input/output file stream
    // A new fstream is created at construction but the actual
    // file is associated using OpenFile()
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

