#include "InternalOptimizer.hpp"

InternalOptimizer::InternalOptimizer(std::string type, std::string name) :
   Optimizer      (type, name)
{
   objectTypeNames.push_back("InternalOptimizer");
}

InternalOptimizer::~InternalOptimizer()
{
}

InternalOptimizer::InternalOptimizer(const InternalOptimizer &opt) :
   Optimizer      (opt)
{
}

InternalOptimizer& InternalOptimizer::operator=(const InternalOptimizer& opt)
{
   if (&opt != this)
   {
      Optimizer::operator=(opt);
   }
   
   return *this;
}

bool InternalOptimizer::Initialize()
{
   bool retval = Optimizer::Initialize();
   
   return retval;
}
