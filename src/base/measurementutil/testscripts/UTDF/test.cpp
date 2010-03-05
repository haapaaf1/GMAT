/* 
 * File:   test.cpp
 * Author: mwilkins
 *
 * Created on March 1, 2010, 3:28 PM
 */

#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <math.h>

using namespace std;

static int byteToInt(string b)
{
    int val=0;
    for (int i=b.length()-1, j = 0; i >= 0; i--,j++)
    {
        val += (b[i] & 0xff) << (8*j);
    }
    return val;
}

static bool Validate(ifstream &file)
{
    char byte[1];
    short int zeros;

    // Fixed control character
    file.seekg(0);
    file.read(byte,1);
    if ((int)byte[0] != 0x1)
        return false;

    // Fixed not used for TDRSS (zeros)
    file.seekg(36);
    file.read((char*)&zeros,2);
    if (zeros != 0)
        return false;

    // Fixed to specify 18-m az/el transmit antenna at WSGT
    file.seekg(42);
    file.read(byte,1);
    if ((int)byte[0] != 0x60)
        return false;

    // Fixed to specify 18-m az/el receive antenna at WSGT
    file.seekg(44);
    file.read(byte,1);
    if ((int)byte[0] != 0x60)
        return false;

    // Fixed zeros
    file.seekg(68);
    file.read((char*)&zeros,2);
    if (zeros != 0)
        return false;

    // Fixed control characters
    file.seekg(70);
    file.read(byte,1);
    if ((int)byte[0] != 0x4)
        return false;

    // Fixed control characters
    file.seekg(71);
    file.read(byte,1);
    if ((int)byte[0] != 0xf)
        return false;

    return true;
}

int main(int argc, char** argv) {
    
    ifstream myFile("data.bin", ios::in | ios::binary);

    if(!Validate(myFile))
    {
        cout << "Error!" << endl;
    }
    else
    {

        char record[72];

        char byte[1],bytes[2],my3Bytes[3],my4Bytes[4],my6Bytes[6];
        double convertAngles = 360*pow(2,-32);
        double convertRTLT = (299792458.0/512)*1.0e-9;

        char router[2];
        short int sic,vid;
        int year, secsOfYear,microsecsOfSec, angle1, angle2;
        long int RTLT, dopplerCount, frequency;
        int groundTransmitAntennaID, groundReceiveAntennaID;
        int forwardLinkTDRSSSIC, returnLinkTDRSSSIC;
        int maReturnLinkID, tdrsTrackingDataOnlyFlag, trackingServiceConfiguration;
        int azElValidity, dopplerValidity, rangeValidity;
        int serviceType, frequencyBand;
        int trackerType, lastFrameIndicator, sampleRateFieldFlag, sampleRate;
        int tdrsOrientationDataValidity, beamOrientationDataValidity;
        int forwardLinkID, returnLinkID;
        short int yaw, roll, pitch;
        int beamAz, beamEl;
        int userBitRate, tdrsTrackingDataTransponderID;
        int dopplerCompensationFlag, pnLockAtReceiverFlag;
        int carrierLockAtReceiverFlag, dopplerExtractorNumber;
        int rangeExtractorNumber, forwardGCEChain, returnGCEChain;

        myFile.read(record,72);

        router[0] = record[1];
        router[1] = record[2];

        year = record[3];

        bytes[0] = record[4];
        bytes[1] = record[5];
        sic = byteToInt(bytes);

        bytes[0] = record[6];
        bytes[1] = record[7];
        vid = byteToInt(bytes);

        my4Bytes[0] = record[8];
        my4Bytes[1] = record[9];
        my4Bytes[2] = record[10];
        my4Bytes[3] = record[11];
        secsOfYear = byteToInt(my4Bytes);

        my4Bytes[0] = record[12];
        my4Bytes[1] = record[13];
        my4Bytes[2] = record[14];
        my4Bytes[3] = record[15];
        microsecsOfSec = byteToInt(my4Bytes);

        my4Bytes[0] = record[16];
        my4Bytes[1] = record[17];
        my4Bytes[2] = record[18];
        my4Bytes[3] = record[19];
        angle1 = byteToInt(my4Bytes);

        my4Bytes[0] = record[20];
        my4Bytes[1] = record[21];
        my4Bytes[2] = record[22];
        my4Bytes[3] = record[23];
        angle2 = byteToInt(my4Bytes);

        my6Bytes[0] = record[24];
        my6Bytes[1] = record[25];
        my6Bytes[2] = record[26];
        my6Bytes[3] = record[27];
        my6Bytes[2] = record[28];
        my6Bytes[3] = record[29];
        RTLT = byteToInt(my6Bytes);

        my6Bytes[0] = record[30];
        my6Bytes[1] = record[31];
        my6Bytes[2] = record[32];
        my6Bytes[3] = record[33];
        my6Bytes[2] = record[34];
        my6Bytes[3] = record[35];
        dopplerCount = byteToInt(my6Bytes);

        my4Bytes[0] = record[38];
        my4Bytes[1] = record[39];
        my4Bytes[2] = record[40];
        my4Bytes[3] = record[41];
        frequency = byteToInt(my4Bytes);

        groundTransmitAntennaID = record[43];
        groundReceiveAntennaID = record[45];

        byte[0] = record[46];
        forwardLinkTDRSSSIC = (byte[0] & 0x0f) + 1299;
        returnLinkTDRSSSIC = (byte[0] >> 4) + 1299;

        byte[0] = record[47];
        maReturnLinkID = (byte[0] & 0x1f);
        tdrsTrackingDataOnlyFlag = (( byte[0] & 0x04 ) != 0) ? 1 : 0;
        trackingServiceConfiguration = (byte[0] >> 6 );

        byte[0] = record[48];
        azElValidity = (( byte[0] & 0x04 ) != 0) ? 1 : 0;
        dopplerValidity = (( byte[0] & 0x02 ) != 0) ? 1 : 0;
        rangeValidity = (( byte[0] & 0x01 ) != 0) ? 1 : 0;

        byte[0] = record[49];
        serviceType = (byte[0] & 0x0f);
        frequencyBand = (byte[0] >> 4);

        bytes[0] = record[50];
        bytes[1] = record[51];
        lastFrameIndicator = (( bytes[0] & 0x08 ) != 0) ? 1 : 0;
        sampleRateFieldFlag = (( bytes[0] & 0x04 ) != 0) ? 1 : 0;
        trackerType = (bytes[0] >> 4);
        sampleRate = (bytes[0] << 6) * 0xf0;
        sampleRate =+ bytes[1];

        byte[0] = record[52];
        tdrsOrientationDataValidity = (( byte[0] & 0x80 ) != 0) ? 1 : 0;
        beamOrientationDataValidity = (( byte[0] & 0x40 ) != 0) ? 1 : 0;
        forwardLinkID = (byte[0] >> 3 & 0x07 );
        returnLinkID = (byte[0] & 0x07);

        byte[0] = record[53];
        userBitRate = (byte[0] & 0xc0 >> 6 );
        tdrsTrackingDataTransponderID = (byte[0] & 0x3f );

        bytes[0] = record[54];
        bytes[1] = record[55];
        yaw = byteToInt(bytes);

        bytes[0] = record[56];
        bytes[1] = record[57];
        roll = byteToInt(bytes);

        bytes[0] = record[58];
        bytes[1] = record[59];
        pitch = byteToInt(bytes);

        my3Bytes[0] = record[60];
        my3Bytes[1] = record[61];
        my3Bytes[2] = record[62];
        beamAz = byteToInt(my3Bytes);

        my3Bytes[0] = record[63];
        my3Bytes[1] = record[64];
        my3Bytes[2] = record[65];
        beamEl = byteToInt(my3Bytes);

        byte[0] = record[66];
        dopplerCompensationFlag = (( byte[0] & 0x80 ) != 0) ? 1 : 0;
        pnLockAtReceiverFlag = (( byte[0] & 0x40 ) != 0) ? 1 : 0;
        carrierLockAtReceiverFlag = (( byte[0] & 0x20 ) != 0) ? 1 : 0;
        dopplerExtractorNumber = ( byte[0] & 0x1f );
        
        byte[0] = record[67];
        rangeExtractorNumber = ( byte[0] >> 4 );
        forwardGCEChain = ( byte[0] & 0x0c >> 2 );
        returnGCEChain = ( byte[0] & 0x03 );

        cout.precision(16);
        cout.setf(std::ios::showpoint);
        cout.setf(std::ios::left);

        cout << router[0] << router[1] << endl;
        cout << year << endl;
        cout << sic << endl;
        cout << vid << endl;
        cout << secsOfYear << endl;
        cout << microsecsOfSec << endl;
        cout << angle1*convertAngles << endl;
        cout << angle2*convertAngles << endl;
        cout << RTLT*convertRTLT << endl;
        cout << dopplerCount << endl;
        cout << frequency*0.1 << endl;
        cout << groundTransmitAntennaID << endl;
        cout << groundReceiveAntennaID << endl;
        cout << forwardLinkTDRSSSIC << endl;
        cout << returnLinkTDRSSSIC << endl;
        cout << maReturnLinkID << endl;
        cout << tdrsTrackingDataOnlyFlag << endl;
        cout << trackingServiceConfiguration << endl;
        cout << azElValidity << endl;
        cout << dopplerValidity << endl;
        cout << rangeValidity << endl;
        if (frequencyBand == 0x03)
            cout << "S-Band" << endl;
        else if (frequencyBand == 0x06)
            cout << "Ku-Band" << endl;
        else
            cout << "Invalid Frequency Band" << endl;
        if (serviceType == 0x01)
            cout << "Not Used" << endl;
        else if (serviceType == 0x02)
            cout << "Simulation Service" << endl;
        else if (serviceType == 0x04)
            cout << "Normal Service" << endl;
        else
            cout << "Invalid Service Type" << endl;
        cout << trackerType << endl;
        cout << lastFrameIndicator << endl;
        cout << sampleRateFieldFlag << endl;
        cout << sampleRate << endl;
        cout << tdrsOrientationDataValidity << endl;
        cout << beamOrientationDataValidity << endl;
        cout << forwardLinkID << endl;
        cout << returnLinkID << endl;
        cout << userBitRate << endl;
        cout << tdrsTrackingDataTransponderID << endl;
        cout << yaw << endl;
        cout << roll << endl;
        cout << pitch << endl;
        cout << beamAz << endl;
        cout << beamEl << endl;
        cout << dopplerCompensationFlag << endl;
        cout << pnLockAtReceiverFlag << endl;
        cout << carrierLockAtReceiverFlag << endl;
        cout << dopplerExtractorNumber << endl;
        cout << rangeExtractorNumber << endl;
        cout << forwardGCEChain << endl;
        cout << returnGCEChain << endl;

        
    }

    return (EXIT_SUCCESS);

}
/*
static long byteToLong(string b)
{
    long val = 0;
    for (int i = b.length()-1, j = 0; i >= 0; i--,j += 2 )
    {
        // low 4 bits
        int tmp = b[i] & 0x0f;
        // high 4 bits
        int tmp2 = (b[i] & 0xf0) >> 4;
        // multiply by base
        val += tmp * (Math.pow(16, j));
        val += tmp2 * (Math.pow(16, j+1));
    }
    return val;
}

*/