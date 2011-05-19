classdef Hardware < handle
    
    %----------------------------------------------------------------------
    %  Define the object's data
    %----------------------------------------------------------------------
    
    %----- Set the public properties
    properties  (SetAccess = 'public')
        CoordinateSystem = 'BodyFixed';
        Direction        = [1 0 0]';
        Location         = [1 0 0]';
    end

    %----- Set the protected properties
    properties  (SetAccess = 'protected')     
         
    end
    
    %----------------------------------------------------------------------
    %  Define the object's methods
    %----------------------------------------------------------------------

    %----- Set the methods
    methods
        
        %  The constructor
        function obj = Harware(obj)

        end
               
    end % methods
    
end