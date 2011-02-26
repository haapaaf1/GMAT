#include "TestApp.hpp"

#include <iostream>
#include "Cartesian.hpp"
#include "Rvector3.hpp"

#include "Moderator.hpp"


int main(int argc, char *argv[])
{
   int retval = 0;

   std::cout << "\n*************************************\n"
             << "*** GMAT Library Interface Tester ***\n" 
             << "*************************************\n";

   // Build a core object
   Cartesian *myCart;
   myCart = new Cartesian;

   std::cout << "Initial state data is \n   " 
             << myCart->GetPosition(0) << ", "
             << myCart->GetPosition(1) << ", "
             << myCart->GetPosition(2) << ", "
             << myCart->GetVelocity(0) << ", "
             << myCart->GetVelocity(1) << ", "
             << myCart->GetVelocity(2) << "\n";
   
   Rvector3 pos, vel;
   pos(0) = 7000.0;
   pos(1) = 1000.0;
   pos(2) = 0.0;
   vel(0) = 0.0;
   vel(1) = -0.5;
   vel(2) = 7.5;

   myCart->SetPosition(pos);
   myCart->SetVelocity(vel);

   std::cout << "\nAfter setting, state data is \n   " 
             << myCart->GetPosition(0) << ", "
             << myCart->GetPosition(1) << ", "
             << myCart->GetPosition(2) << ", "
             << myCart->GetVelocity(0) << ", "
             << myCart->GetVelocity(1) << ", "
             << myCart->GetVelocity(2) << "\n";

   std::cout << "\nAccessing the Moderator singleton\n";
   Moderator *mod = Moderator::Instance();
   mod->Initialize("gmat_startup_file.txt");
   std::cout << "Success!?!\n";


   delete myCart;
   std::cout << "\n*************************************\n"
             << "***       Finished Testing!       ***\n"
             << "*************************************\n" << std::endl;
   return retval;
} 
