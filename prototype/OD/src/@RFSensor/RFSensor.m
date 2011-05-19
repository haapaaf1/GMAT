classdef RFSensor < Hardware
    
    %----------------------------------------------------------------------
    %  Define the object's data
    %----------------------------------------------------------------------
    
    %----- Set the public properties
    properties  (SetAccess = 'public') 
        Id = 123456789;
        PrimaryAntenna = '';
        
        %---- Delay model Data
        DelayModel           = 'Constant';
        Delay                = .000001;

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
        function obj = RFSensor(obj)

        end
               
    end % methods
    
end