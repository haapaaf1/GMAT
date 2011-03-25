
%===== Modification History
% 04/16/09 S. Hughes.  Initial Version
% 04/16/09 S. Hughes.  Fixed typo in function Meas
% 04/24/09 S. Hughes.  Changed property accesses for many properties

classdef Measurement < handle

    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------
    
    %----- Set the public properties
    properties  (SetAccess = 'public')
        Filename           = ' '
        AddDataType        = {};
    end

    %----- Set the protected properties
    properties  (SetAccess = 'protected')     
        Bias               = 0;
        BiasStdDev
        BiasNoise
        BiasTimeConstant
    end
    
    %----- Set the private properties
    properties  (SetAccess = 'protected')     
        numDataTypes
        Obs                
        Epochs
        Participants       = {};
        dataTypeID         = [];
    end
    
    %----------------------------------------------------------------------
    %  Define the object's methods
    %----------------------------------------------------------------------

    %----- Set the methods
    methods
        
        function Meas = Measurement(Meas)

        end
        
        %----- Assign all fields of current object to input object
        function Assignment(obj,obj2)
            %Major kludge.  Need deep assignment algorithm!!!!
            obj.Bias = obj2.Bias;
        end % Assignment
        
    end % methods

end % classdef