classdef MeasurementSimulator < handle

    % Set the public properties
    properties  (SetAccess = 'public')
        Measurements  = {}
        Propagator
        InitialEpoch
        FinalEpoch
        MeasurementTimeStep
        Filename
        Fileformat
    end

    % Set the private properties
    properties  (SetAccess = 'private')
       MeasManager
       SimulationEpochs;
    end

    % Set the methods
    methods

        %-----  Constructor
        function Simulator = MeasurementSimulator(Simulator)
            
            %  Create the estimator's state manager
            Simulator.MeasManager = MeasurementManager;
            
        end
        
        %----- Initialize the state manager
        function Simulator = Initialize(Simulator,Sandbox)
            
            %--------------------------------------------------------------
            % - Get handle to propagator
            % - Initialize the propagator state manager
            % - Initialize the measurents
            %--------------------------------------------------------------

            %  Get handle to propagator
            Simulator.Propagator = Sandbox.GetHandle(Simulator.Propagator);
                       
            %  Initialize the measurements.
            Simulator.MeasManager.InitializeforSim(Sandbox,Simulator);
            
            %  Calculate vector of measurement times
            dt = Simulator.MeasurementTimeStep/86400;
            Simulator.SimulationEpochs = [Simulator.InitialEpoch:dt:Simulator.FinalEpoch]';
            
            
            
        end % Initialize
        
    end % methods

end % classdef