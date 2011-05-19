classdef Transponder < RFSensor
    
    %----------------------------------------------------------------------
    %  Define the object's data
    %----------------------------------------------------------------------
    
    %----- Set the public properties
    properties  (SetAccess = 'public')
        
          %---- General data
          Name                 = 'Transponder1';
         
          %---- Output Frequency Data
          OutputFrequencyModel = 'CoherentRelay';;
          TurnAroundRatio      = 240/221;

          %---- Input Frequency Data
          InputFrequencyModel  = 'CenterAndBandwidth' % NonCohereent Relay is other option
          InputCenterFrequency = 2090.659968;
          InputBandwidth       = 200;
          
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
        
        %  This function checks input frequency is within the receiver's
        %  bandwidth.
        function flag =  IsInputFrequencyFeasible(obj,freqIn)
                
            switch obj.InputFrequencyModel  
                
                case 'CenterAndBandwidth';
                    
                    %  Check to see if input frequency is between the upper
                    %  and lower bounds of the allowabl input
                    if freqIn > obj.InputCenterFrequency - obj.InputBandwidth & ...
                            freqIn < obj.InputCenterFrequency + obj.InputBandwidth
                        
                        flag = 1;
                        
                    else
                        
                        flag = 0;
                        
                    end
            end
        end
               
        %  This function calculates output frequency if input frequency is
        %  acceptable.
        function freqOut = GetOutputFrequency(obj,freqIn)
            
            if IsInputFrequencyFeasible(obj,freqIn)
           
                freqOut = freqIn*obj.TurnAroundRatio;
               
            else 
                
                freqOut = [];
                
            end
            
        end
               
    end % methods
    
end