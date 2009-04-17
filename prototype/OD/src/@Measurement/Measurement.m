
%===== Modification History
% 04/16/09 S. Hughes.  Initial Version
% 04/16/09 S. Hughes.  Fixed typo in function Meas

classdef Measurement < handle

    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------
    
    %----- Set the public properties
    properties  (SetAccess = 'public')
        TruthSource        = 'Simulation';
        Filename           = ' '
        FileFormat         = 'B3';
        LightTimeModel     = 'LineofSight';
        TroposphereModel   = 'Saastamoinen';
        IonosphereModel    = 'ISI';
        GenRelativityModel = 'Moyer';
        AddDataType        = {};
        Participants       = {};
        Simulator          = {};
        dataTypeID         = [];
    end

    %----- Set the private properties
    properties  (SetAccess = 'protected')     
        numDataTypes
        Obs                
        Epochs
    end
    
    %----------------------------------------------------------------------
    %  Define the object's methods
    %----------------------------------------------------------------------

    %----- Set the methods
    methods
        
        function Meas = Measurement(Meas)
            Simulator.StartTime = {};
            Simulator.StopTime  = {};
            Simulator.TimeStep   = [];
            Meas.Simulator = Simulator;
        end
        
    end % methods

end % classdef