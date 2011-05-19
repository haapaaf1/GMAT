classdef Receiver < RFSensor
    
    %----------------------------------------------------------------------
    %  Define the object's data
    %----------------------------------------------------------------------
    
    %----- Set the public properties
    properties  (SetAccess = 'public')
          Name                 = 'Receiver1';
          CenterFrequency      = 2090.659968;
          BandWidth            = 200
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
        
        function flag =  IsFeasible(obj,freqIn)
                
            if freqIn > obj.CenterFrequency - obj.BandWidth & ...
               freqIn < obj.CenterFrequency + obj.BandWidth     
           
               flag = 1;
               
            else 
                
               flag = 0;
                
            end
        end
        
    end % methods
    
end