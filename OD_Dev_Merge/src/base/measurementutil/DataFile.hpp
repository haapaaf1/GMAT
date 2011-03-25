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

#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <pcrecpp.h>
#include <math.h>
#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include "DataFormats.hpp"
#include "ObType.hpp"
#include "DataFileException.hpp"
#include "MessageInterface.hpp"
#include "StringUtil.hpp"           // for ToString()
#include "A1Date.hpp"
#include "A1Mjd.hpp"
#include "UtcDate.hpp"
#include "DateUtil.hpp"
#include "RealUtilities.hpp"

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
    virtual void        Copy(const GmatBase* orig);      

    virtual bool        IsParameterReadOnly(const Integer id) const;
    virtual bool        IsParameterReadOnly(const std::string &label) const;

    // Functions for getting/setting DataFile parameters from the script
    virtual std::string GetStringParameter(const Integer id) const;
    virtual bool SetStringParameter(const Integer id, const std::string &value);
    virtual Integer GetIntegerParameter(const Integer id) const;
    virtual Integer GetIntegerParameter(const std::string &label) const;
    virtual Integer SetIntegerParameter(const Integer id, const Integer value);
    virtual Integer SetIntegerParameter(const std::string &label, const Integer value);

    // String processing utility functions for reading/writing
    std::string Trim(std::string s);
    bool ReverseOverpunch(std::string code, Integer &digit, Integer &sign );

    // functions to extract a line from ASCII file
    std::string ReadLineFromFile();

    // Functions to extract a binary record
    std::string ReadRecordFromFile(const Integer reclen);

    // Binary utility functions
    Integer ByteToInt(const string b);
    
    // methods to get/set DataFile object parameters
    void SetNumLines(const Integer &nl);
    Integer GetNumLines() const;

    // methods to get/set the ObtypeVector
    void SetTheData(const ObTypeVector &myData);
    ObTypeVector GetTheData() const;

    // Method to retrieve number of observations available after
    // the file has been processed
    Integer GetNumMeasurements() const;
    
    void SetReadWriteMode(const std::string &mode);
    std::string GetReadWriteMode() const;

    void SetFileFormatID(const Integer &mName);
    Integer GetFileFormatID() const;

    void SetFileFormatName(const std::string &fName);
    std::string GetFileFormatName() const;
  
    bool GetIsOpen() const;
    void SetIsOpen(const bool &flag);

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

    void SetPrecision(const Integer &value);
    Integer GetPrecision() const;
    bool GetScientific() const;
    void SetScientific(const bool &flag);
    bool GetShowPoint() const;
    void SetShowPoint(const bool &flag);

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
	bool operator()(ObType* rpStart, ObType* rpEnd)
	{
	    return rpStart->GetEpoch() < rpEnd->GetEpoch();
	}
    };

    // Descending epoch sorting function

    struct DescendingEpochSort
    {
	bool operator()(ObType* rpStart, ObType* rpEnd)
	{
	    return rpStart->GetEpoch() > rpEnd->GetEpoch();
	}
    };

    // Ascending satellite ID sorting function

    struct AscendingSatIDSort
    {
	bool operator()(ObType* rpStart, ObType* rpEnd)
	{
	    return rpStart->GetSatID() < rpEnd->GetSatID();
	}
    };

    // Descending satellite ID sorting function

    struct DescendingSatIDSort
    {
	bool operator()(ObType* rpStart, ObType* rpEnd)
	{
	    return rpStart->GetSatID() > rpEnd->GetSatID();
	}
    };
    
    // Ascending sensor ID sorting function

    struct AscendingSensorIDSort
    {
	bool operator()(ObType* rpStart, ObType* rpEnd)
	{
	    return rpStart->GetSensorID() < rpEnd->GetSensorID();
	}
    };

    // Descending sensor ID sorting function

    struct DescendingSensorIDSort
    {
	bool operator()(ObType* rpStart, ObType* rpEnd)
	{
	    return rpStart->GetSensorID() > rpEnd->GetSensorID();
	}
    };
    
    // Ascending sensor ID sorting function

    struct AscendingInternationalDesignatorSort
    {
	bool operator()(ObType* rpStart, ObType* rpEnd)
	{
	    return rpStart->GetInternationalDesignator() < 
		     rpEnd->GetInternationalDesignator();
	}
    };

    // Descending sensor ID sorting function

    struct DescendingInternationalDesignatorSort
    {
	bool operator()(ObType* rpStart, ObType* rpEnd)
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
        TDRSSUTDF_ID,
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

    // Used for reading in binary data and converting to the various data types
    unsigned int myUInt;
    unsigned long int myULongInt;
    unsigned short int myUShortInt;
    long int myLongInt;
    short int myShortInt;
    int myInt;

    /// Published parameters for data files
    enum
    {
        FILENAME_ID   = GmatBaseParamCount,
        FILEFORMAT_ID,
        NUMLINES_ID,
        READWRITEMODE_ID,
        SCIENTIFICMODE_ID,
        SHOWPOINTMODE_ID,
        PRECISION_ID,
        RECORDLENGTH_ID,
        DataFileParamCount
    };

    static const std::string    PARAMETER_TEXT[DataFileParamCount -
                                              GmatBaseParamCount];
    static const Gmat::ParameterType
                               PARAMETER_TYPE[DataFileParamCount -
                                              GmatBaseParamCount];
   
    static const std::string OPTIONAL_SIGN;
    static const std::string MANDATORY_DIGITS;
    static const std::string DECIMAL_POINT;
    static const std::string OPTIONAL_DIGITS;
    static const std::string OPTIONAL_EXPONENT;

    static const std::string REGEX_NUMBER;
    static const std::string REGEX_INTEGER;
    static const std::string REGEX_LETTER;
    static const std::string REGEX_DATE;
    static const std::string REGEX_SCINUMBER;

    // File format and filename of the data being used
    std::string fileFormatName;
    Integer fileFormatID;
    std::string dataFileName;

    /// Flag to set format using scientific notation
    bool scientific;
    /// Flag to set format showing a decimal point after real numbers
    bool showPoint;
    /// This variable specifies the number of decimal places of precision
    Integer precision;  
           
    // numLines tells the file reader how many lines to read in at a time
    // This is important for TLE's that can have 2 or 3 lines of data
    // depending if a comment line is included for each TLE
    Integer numLines;

    // The vector container of data
    ObTypeVector theData;
            
    //Current iterators pointing at data
    ObTypeVector::const_iterator i_theData;

    // Flag to indicate if the file is opened
    bool isOpen;

    // Flag to indicate if file format allows comments to be written
    bool commentsAllowed;
    
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

    // Flag to indicate if the data file is binary or ASCII
    bool isBinary;

    // For binary files, the length of each record
    Integer recordLength;

    // This is the pointer to the input/output file stream
    // A new fstream is created at construction but the actual
    // file is associated using OpenFile()
    fstream *theFile;

public:

    // Method to obtain a data point from the fstream pointer to file
    virtual bool GetData(ObType *myObType) = 0;

    // This method dumps all the data contained in theData to file
    virtual bool WriteAllTheData();
    
    // Methods to write data to the file defined on this object
    virtual bool WriteData(const ObType *myObType) = 0;
    virtual bool WriteDataHeader(const ObType *myObType);
    virtual bool WriteDataSubHeader(const ObType *myObType);
    virtual bool WriteMetaData(const ObType *myObType);
    virtual bool WriteComment(const std::string myComment);
    virtual bool WriteComment(const StringArray myComments);
    
};

#endif	/* _DataFile_hpp */

