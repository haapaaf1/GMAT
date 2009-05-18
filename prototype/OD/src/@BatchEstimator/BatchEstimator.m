classdef BatchEstimator < handle

    % Set the public properties
    properties  (SetAccess = 'public')
        MaxIterations = 4;
        RelTolerance  = 1e-5;
        AbsTolerance  = 1e-5;
        Measurements  = {};
        SolveFor      = {};
        Propagator;
        ESV;
        SolutionEpoch;
        RunMode       = 'Solve'
        UseAprioriCovariance = 0;
    end

    % Set the private properties
    properties  (SetAccess = 'private')
        ESM;
        MeasManager;
        numStates;
    end

    % Set the methods
    methods

        %-----  Constructor
        function Estimator = BatchEstimator(Estimator)
            
            %  Create the estimator's state manager
            Estimator.ESM = EstimatorStateManager;
            Estimator.MeasManager = MeasurementManager;
            
        end
        
        %----- Initialize the state manager
        function Estimator = Initialize(Estimator,Sandbox)
            
            %--------------------------------------------------------------
            % - Get handle to propagator
            % - Initialize the estimator
            % - Initialize the estimator state manager
            % - Initialize the propagator state manager
            % - Initialize the measurement manager
            %--------------------------------------------------------------



            %  Initialize the ESM for the estimator
            Estimator.ESM = Estimator.ESM.Initialize(Sandbox,Estimator);
            
            %  Get handle to propagator:  Replace string with handle!
            Estimator.Propagator = Sandbox.GetHandle(Estimator.Propagator);
            
            %  Intialize the PSM for the propagator
            Estimator.Propagator.PSM.Initialize(Estimator.ESM);
            
            %  Initialize the measurement manager and measurements.
            Estimator.MeasManager.Initialize(Sandbox,Estimator);
            
            
        end % Initialize
        
    end % methods

end % classdef