/* 
 * File:   randtest.cpp
 * Author: mwilkins
 *
 * Created on September 8, 2008, 12:46 PM
 */

#define __cplusplus

#include <stdio.h>
#include <stdlib.h>
#include "gmatdefs.hpp"
#include "RandomNumber.hpp"
#include <gsl_randist.h>
#include <gsl_cdf.h>
#include <gsl_rng.h>
#include <ctime>

int main(void) {
    
    RandomNumber myRand;

    myRand.ClockSeed();
            
    FILE *FID = fopen("randout.txt","w");

    Integer i;
    for (i=1; i<=1000; i++) {
	fprintf (FID,"%f\n",myRand.Gaussian());
    }
    
    fclose(FID);
    
    return (EXIT_SUCCESS);
}

