/* 
 * File:   ProcessCCSDSDataFile.hpp
 * Author: matthewwilkins
 *
 * Created on September 10, 2009, 3:19 PM
 */

#ifndef _ProcessCCSDSDATAFILE_HPP
#define	_ProcessCCSDSDATAFILE_HPP

#include "DataFile.hpp"
#include "DataFormats.hpp"

class ProcessCCSDSDataFile : public DataFile
{

public:
    
    ProcessCCSDSDataFile(const std::string &itsType, const std::string &itsName);
    ProcessCCSDSDataFile(const ProcessCCSDSDataFile &CCSDSdf);
    const ProcessCCSDSDataFile& operator=(const ProcessCCSDSDataFile &CCSDSdf);
    virtual ~ProcessCCSDSDataFile();
    
    CCSDSObtype::ccsds_header* GetHeader();
    void SetHeader(CCSDSObtype::ccsds_header *myHeader);
    
    bool GetCCSDSHeader(std::string firstline, CCSDSObtype::ccsds_header* myHeader);
    bool GetCCSDSData(std::string &lff, CCSDSObtype::ccsds_data *myData,
                      CCSDSObtype *myOb);
    
protected:

    // Pointer to current header
    CCSDSObtype::ccsds_header *currentCCSDSHeader;

};

#endif	/* _ProcessCCSDSDATAFILE_HPP */

