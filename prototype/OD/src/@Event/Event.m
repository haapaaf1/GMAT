classdef Event < handle
   
   properties  (SetAccess = 'public')
        isTerminal;
        Mode;
        eventValue;
        eventRate;
        type;
        funcHandles;
        rootValues;
   end
    
   methods
       
       function event = Event()
           
       end
       
   end
    
end