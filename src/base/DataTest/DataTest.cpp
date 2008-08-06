#include <stdio.h>
#include <stdlib.h>
//#include "gmatdefs.hpp"
#include "Rvector3.hpp"
#include <ProcessDataFile.hpp>
#include <iostream>
#include <fstream>
#include <pcrecpp.h>

using namespace std;

int main(void) {


    cout << "------------------------------" << endl;
    cout << "        TLE Test Case" << endl;
    cout << "------------------------------" << endl;
    
    cout << endl;
     
    std::string line;
    std::string line2;
    ProcessDataFile myData;
    
    myData.SetFileName("TLE.txt");
    myData.OpenFile();
    
    // Read first two lines from the TLE file
    line = myData.ReadLineFromFile();
    line2 = myData.ReadLineFromFile();
    
    do {

	// Output original data to screen for comparison
	cout << endl << line << endl << line2 << endl;		
	cout << endl;

	// Initialize structs
	ProcessDataFile::tle_obtype myTLE = {0};		

	// Convert TLE format to usable data
	if (myData.ProcessDataFile::ProcessTLEData(line,line2,myTLE))
	{

	    // Output resulting struct data to screen
	
	    cout << "Satnum = " << myTLE.satnum << endl;	
	    cout << "Class = " << myTLE.securityClassification << endl;		
	    cout << "IntlDesignator = " << myTLE.intlDesignator << endl;		
	    cout << "Year = " << myTLE.epochYear << endl;		
	    printf("Day Of Year = %16.8f\n",myTLE.epochDayOfYear);	
	    printf("Ndotby2 = %16.8f\n",myTLE.ndotby2);	
	    printf("Bstar = %16.8g\n",myTLE.bstar);	
	    printf("Ndotby6 = %16.8g\n",myTLE.nddotby6);	
	    cout << "EphemType = " << myTLE.ephemerisType << endl;		
	    cout << "ElementNum = " << myTLE.elementNum << endl;		
	    printf("Inclination = %16.8f\n",myTLE.inclination);	
	    printf("Eccentricity = %16.8f\n",myTLE.eccentricity);	
	    printf("RAAN = %16.8f\n",myTLE.raan);	
	    printf("Argument of Perigee = %16.8f\n",myTLE.argPerigee);	
	    printf("Mean Anomaly = %16.8f\n",myTLE.meanAnomaly);	
	    printf("Mean Motion = %17.14f\n",myTLE.meanMotion);	
	    cout << "Rev Num = " << myTLE.revolutionNum << endl;	    
	    cout << "\n******************************************************\n";

	} else {
	    
	    cout << "Error processing this TLE!!" << endl;
	    cout << "\n******************************************************\n";

	}
	
	// Read two lines from the TLE file
	line = myData.ReadLineFromFile();
	line2 = myData.ReadLineFromFile();
	
    } while ( ! myData.IsEOF() && line.c_str() != "" && line2.c_str() != ""); 
    
    myData.CloseFile();

    cout << "------------------------------" << endl;
    cout << "       SLR Test Case" << endl;
    cout << "------------------------------" << endl;
 
    ProcessDataFile mySLRData;
    mySLRData.SetFileName("SLR.txt");
    mySLRData.OpenFile();
        
    while ( ! mySLRData.IsEOF() ) {

	// Read in a line
	line = mySLRData.ReadLineFromFile();
	line = mySLRData.Trim(line);

	// This is supposed to be five digits but sometimes it is less
	if (line.size() < 5 && pcrecpp::RE("^9+$").FullMatch(line))
	    { line = "99999"; }
	if (line.size() < 5 && pcrecpp::RE("^8+$").FullMatch(line)) 
	    { line = "88888"; }
				
	if (line == "99999" || line == "88888") {

	    // Initialize structs
	    ProcessDataFile::slr_header mySLRhead = {0};
	    
	    // set SLR type variable so that we know how
	    // to process the rest of the data. this requires
	    // a conversion from string to integer
	    mySLRData.from_string<int>(mySLRhead.slrType,line,std::dec);
	
	    // read header line
	    std::string headerline = mySLRData.ReadLineFromFile();
	    cout << endl;
	    cout << headerline << endl;
	    cout << endl;

	    if (mySLRData.ProcessSLRHeader(headerline,mySLRhead))
	    {

		cout << "SLR Type = " << mySLRhead.slrType << endl;
		cout << "ILRS Satnum = " << mySLRhead.ilrsSatnum << endl;
		std::string intlDesignator = mySLRData.ilrs2cospar(mySLRhead.ilrsSatnum);
		cout << "COSPAR Satnum = " + intlDesignator << endl;
		cout << "Year = " << mySLRhead.year << endl;
		cout << "DOY = " << mySLRhead.dayOfYear << endl;
		cout << "CDP Pad ID = " << mySLRhead.cdpPadID << endl;
		cout << "CDP Sys Num = " << mySLRhead.cdpSysNum << endl;
		cout << "CDP Occupancy Num = " << mySLRhead.cdpOccupancySequenceNum << endl;
		printf("Wavelength = %16.8g\n",mySLRhead.wavelength);
		cout << "Cal Sys Delay = " << mySLRhead.calSysDelay << endl;
		cout << "Cal Delay Shift = " << mySLRhead.calDelayShift << endl;
		cout << "NPD Window Indicator = " << mySLRhead.normalPointWindowIndicator << endl;
		cout << "Sys Delay = " << mySLRhead.rmsSysDelay << endl;
		cout << "Epoch Scale Indicator = " << mySLRhead.epochTimeScaleIndicator << endl;
		cout << "SysCal Indicator = " << mySLRhead.sysCalMethodIndicator << endl;
		cout << "SCH Indicator = " << mySLRhead.schIndicator << endl;
		cout << "SCI Indicator = " << mySLRhead.sciIndicator << endl;
		cout << "Pass RMS = " << mySLRhead.passRMS << endl;
		cout << "Data Quality Indicator = " << mySLRhead.dataQualAssessmentIndicator << endl;
		cout << "Format Revision Num = " << mySLRhead.formatRevisionNum << endl;
		cout << "\n******************************************************\n";

		line = mySLRData.ReadLineFromFile();
		line = mySLRData.Trim(line);
		cout << endl;
		cout << line << endl;
		cout << endl;	    

		do {		
    
		    ProcessDataFile::slr_obtype mySLR = {0};
 
		    if (mySLRData.ProcessSLRData(line,mySLRhead,mySLR))
		    {
			printf("Time of Firing = %16.12g\n",mySLR.timeOfLaserFiring);
			printf("Two Way Time of Flight = %16.12g\n",mySLR.twoWayTimeOfFlight);
			cout << "RMS Range = " << mySLR.binRMSRange << endl;
			printf("Surface Pressure = %16.8g\n",mySLR.surfacePressure);
			printf("Surface Temp = %16.8g\n",mySLR.surfaceTemp);
			cout << "Relative Humidity = "  << mySLR.relativeHumidity << endl;
			cout << "Num Raw Ranges = "  << mySLR.numRawRanges << endl;
			cout << "Data Release Flag = "  << mySLR.dataReleaseFlag << endl;
			cout << "Raw Range Factor = "  << mySLR.rawRangeFactor << endl;
			cout << "NPD Window Indicator 2 = "  << mySLR.normalPointWindowIndicator2 << endl;		
			printf("Signal to Noise Ratio = %16.8g\n",mySLR.signalToNoiseRatio);
			cout << "Burst Cal Sys Delay = " << mySLR.burstCalSysDelay << endl;
			cout << "Signal Strength Indicator = " << mySLR.signalStrength << endl;
			cout << "Angle Origin Indicator = " << mySLR.angleOriginIndicator << endl;
			printf("Azimuth = %16.8g\n",mySLR.az);
			printf("Elevation = %16.8g\n",mySLR.el);
			cout << "\n******************************************************\n";

		    } else {
			
			cout << "Error processing this line of SLR data!!" << endl;
			cout << "\n******************************************************\n";
		    }
		
		    // Read next line and then check to see
		    // if it is marked as a new data section
		    line = mySLRData.ReadLineFromFile();
		    line = mySLRData.Trim(line);
		    cout << endl;
		    cout << line << endl;
		    cout << endl;	    
		    
		} while ( line != "99999" && line != "88888" && !mySLRData.IsEOF() );
		
		
	    } else {
		
		cout << "Error processing SLR header line!" << endl;
		cout << "\n******************************************************\n";
		
	    }
	
	}

    }

    mySLRData.CloseFile();
    

    cout << "------------------------------" << endl;
    cout << "        B3 Test Case" << endl;
    cout << "------------------------------" << endl;
    
    cout << endl;
     
    ProcessDataFile myB3Data;
    
    myB3Data.SetFileName("B3.txt");
    myB3Data.OpenFile();
    
    // Read a line from the B3 file
    line = myB3Data.ReadLineFromFile();
   
    do {

	// Output original data to screen for comparison
	cout << endl << line << endl;		
	cout << endl;

	// Initialize structs
	ProcessDataFile::b3_obtype myB3 = {0};

	// Convert TLE format to usable data
	myB3Data.ProcessDataFile::ProcessB3Data(line,myB3);

//	if (myB3Data.ProcessDataFile::ProcessB3Data(line,myB3))
//	{

	    // Output resulting struct data to screen
	    cout << "Class = " << myB3.securityClassification << endl;		
	    cout << "Satnum = " << myB3.satelliteID << endl;	
	    cout << "Sensor ID = " << myB3.sensorID << endl;		
	    cout << "Year = " << myB3.year << endl;		
	    cout << "Day of Year = " << myB3.dayOfYear << endl;		
	    cout << "Hour = " << myB3.hour << endl;		
	    cout << "Minutes = " << myB3.minute << endl;		
	    printf("Seconds = %16.8f\n",myB3.seconds);	
	    printf("Elevation = %16.8g\n",myB3.elevation);	
	    printf("Azimuth = %16.8g\n",myB3.azimuth);	
	    printf("Declination = %16.8f\n",myB3.declination);	
	    printf("Right Ascension = %16.8f\n",myB3.rightAscension);	
	    printf("Range = %16.8f\n",myB3.range);	
	    printf("Range Rate = %16.8f\n",myB3.rangeRate);	
	    printf("ECF X = %16.8f\n",myB3.ecf_X);	
	    printf("ECF Y = %16.8f\n",myB3.ecf_Y);	
	    printf("ECF Z = %16.8f\n",myB3.ecf_Z);	
	    cout << "\n******************************************************\n";

//	} else {
//	    
//	    cout << "Error processing this B3 line!!" << endl;
//	    cout << "\n******************************************************\n";
//
//	}
	
	// Read another line from the B3 data file
	line = myB3Data.ReadLineFromFile();
	
    } while ( ! myB3Data.IsEOF() && line.c_str() != "" && line2.c_str() != ""); 
    
    myB3Data.CloseFile();
    
    return 0;

}
