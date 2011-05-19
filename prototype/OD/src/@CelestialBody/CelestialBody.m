
classdef CelestialBody < handle
    
    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------

    %-----  Set the public data
    properties  (SetAccess = 'protected')
        
        centralBodyId         = 11;
        jplId                 = 3;
        tdbEpoch              = 2451545
    end
    
    %----------------------------------------------------------------------
    %  Define the object's methods
    %----------------------------------------------------------------------
    
    methods
        
        %----- Intialize
        function obj = CelestialBody(~)
            
            addpath('C:\Users\sphughe1\Documents\GMAT Files\Jazz\trunk\test\script\extern\utils\KSC_DEReader\ephm')
            init_eph('DE405.dat');
            
        end % Initialize
        
        function obj = setJPLId(obj,jplId)
            obj.jplId = jplId;
        end
        
        function state = GetState(obj,tdbEpoch)
            if nargin == 2
                obj.tdbEpoch = tdbEpoch;
            end
            state = pleph(obj.tdbEpoch,obj.jplId,3,1);
        end       
        
    end % methods
    
end % classdef