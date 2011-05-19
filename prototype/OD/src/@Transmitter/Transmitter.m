classdef Transmitter < RFSensor
    
    %----------------------------------------------------------------------
    %  Define the object's data
    %----------------------------------------------------------------------
    
    %----- Set the public properties
    properties  (SetAccess = 'public')
          Name                 = 'Transmitter1';
          FrequencyModel = 'Constant';  % Ramped is the other option
          Frequency      = 2090.659968; %MHz
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
        
        %  Calculate the transmitter frequency.  
        function freq = GetOutputFrequency(obj,time)
            
            switch obj.FrequencyModel
                
                case 'Constant'
                    
                    freq = obj.Frequency
                    
                case 'Ramped'
                    
                    %Interpolate the Ramp Data
                    
            end
            
        end
               
    end % methods
    
end