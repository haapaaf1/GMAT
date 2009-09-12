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
    virtual ~ProcessCCSDSDataFile();
    
    CCSDSObtype::ccsds_header* GetHeader();
    void SetHeader(CCSDSObtype::ccsds_header *myHeader);
    
    bool GetCCSDSHeader(std::string firstline);
    virtual bool GetCCSDSMetadata(std::string &lff);
    
protected:

    // Pointer to current header
    CCSDSObtype::ccsds_header *currentCCSDSHeader;

};

#endif	/* _ProcessCCSDSDATAFILE_HPP */

