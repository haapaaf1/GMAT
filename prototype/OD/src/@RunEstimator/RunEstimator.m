
%  04/16/09 S. Hughes.  Updated to handle infeasbile measurements.

classdef RunEstimator < handle

    % Set the public properties
    properties  (SetAccess = 'public')

        Estimator

    end

    % Set the methods
    methods
        
        %------------------------------------------------------------------
        %-----  Initialize
        %------------------------------------------------------------------
         function obj = RunEstimator(Estimator)
             
            global theSandbox

            %----- Add the estimator
            obj.Estimator = theSandbox.GetHandle(Estimator);
            theSandbox.AddCommand(obj);

        end
        
        %------------------------------------------------------------------
        %-----  Initialize
        %------------------------------------------------------------------

        function obj = Initialize(obj,theSandbox)

            %----- Initialize the estimator
            %obj.Estimator.Initialize(theSandbox);

        end

        %------------------------------------------------------------------
        %----- Prepare to Solve
        %------------------------------------------------------------------

        % - Set up the estimation state vector
        function RunEst = PreparetoSolve(RunEst)

            RunEst.Estimator.ESV = RunEst.Estimator.ESM.GetStates();

        end %  Prepare to Solve

        %------------------------------------------------------------------
        %----- Prepare to Propagate
        %------------------------------------------------------------------

        function RunEst = PreparetoPropagate(RunEst)

            %---- KLUDGE TO GET EPOCH FOR PROPAGATOR
            RunEst.Estimator.Propagator.Epoch =RunEst.Estimator.Propagator.PSM.Objects{1}.Epoch;

            %Prop.PSV = PSV;
            RunEst.Estimator.Propagator.PSV   = RunEst.Estimator.Propagator.PSM.GetStates();

        end % Prepare to Propagate

        %------------------------------------------------------------------
        %----- Execute:  Solve the problem.
        %------------------------------------------------------------------

        function Execute(RunEst)

            global TestCase theSandbox

            %----- Call prepare to solve to populate ESV with values.
            RunEst.PreparetoSolve();

            %----- Extract attached objects and variables to shorten later code
            Prop        = RunEst.Estimator.Propagator;
            Estimator   = RunEst.Estimator;
            measManager = Estimator.MeasManager;
            numObs      = measManager.numObs;
            Epochs      = measManager.Epochs;
            Obs         = measManager.Obs;

            %----- Clone the objects
            estObjectClone  = Estimator.ESM.CloneObjects();
            propObjectClone = Prop.PSM.CloneObjects();

            %----- Display related stuff
            disp(' Iter    norm(Yo - Yc)     norm(dx)');
            disp('------------------------------------')
            formatstr   = '%5.0f  %14.6g %14.6g %12.4g %12.3g %12.3g %12.3g %s  %s';

            if strcmp(Estimator.RunMode,'Simulate')
                                   
            elseif strcmp(Estimator.RunMode,'Solve')

                %----- The Estimator Loop
                oldRMS = 0;  newRMS = 1e12; normdx = 1e12; iter = 1;
                numStates = Estimator.ESM.numStates;
                while abs( newRMS - oldRMS ) > Estimator.RelTolerance ...
                        & iter <= Estimator.MaxIterations ...
                        & normdx > Estimator.AbsTolerance;

                    %------------------------------------------------------
                    %  Initializations for the next iteration
                    %------------------------------------------------------
                    infoMat   = zeros(numStates,numStates);  % Set info mat to zer
                    residMat  = zeros(numStates,1);          % Set rediduals to zero
                    Estimator.ESM.SetObjectstoClones;        % Set Objects to Clones                 
                    Estimator.ESM.SetStates(Estimator.ESV);  % Update states based on ESV
                    RunEst = RunEst.PreparetoPropagate();    % Update PSV
                    
                    %------------------------------------------------------
                    %----- Perform the accumulation
                    %------------------------------------------------------
                    residCount = 0;
                    for i = 1:numObs

                        %  Step to next measurement epoch
                        Prop = Prop.SteptoEpoch(Epochs(i));

                        %  KLUDGE - THIS WILL BE AVOIDED BY MODS TO MEASUREMENT MANAGER 
                        %  TO HANDLE THE PARTIALS MAP
                        dgdvv        = zeros(1,3);
                        if TestCase == 1
                            [y,htilde,isFeasible] = measManager.GetMeasurement(i);
                            Htilde       = [htilde dgdvv];
                        elseif TestCase == 2
                            [y,htilde,isFeasible] = measManager.GetMeasurement(i);
                             Htilde       = [htilde dgdvv 1];
                        elseif TestCase == 3 || TestCase == 4 ;
                             [y,htilde,isFeasible] = measManager.GetMeasurement(i);
                             Htilde       = [htilde dgdvv 1 -htilde];
                        else
                            [y,htilde,isFeasible] = measManager.GetMeasurement(i);
                             Htilde       = [htilde dgdvv 1];
                        end

                        %  Calculate the H matrix and accumulate
                        observations(i,1) = y;
                        STM          = Estimator.ESM.GetSTM;
                        Hi           = Htilde*STM;
                        resid(i,1)   = Obs(i,1) - y;
                        infoMat      = infoMat + Hi'*Hi;
                        residMat     = residMat + Hi'*resid(i,1);
                        
                    end

                    %  Solve the normal equations and update the ESV
                    oldRMS = newRMS;
                    newRMS = sqrt( sum(resid) / numObs );
                    dx     = inv(infoMat)*residMat;
                    R      = chol(infoMat);
                    z      = inv(R')*residMat;
                    x      = inv(R)*z;
                    normdx = norm(dx);
                    Estimator.ESV = Estimator.ESV + dx;

                    %  Output iteration data
                    iterdata = sprintf(formatstr,iter,norm(resid)/numObs,norm(dx));
                    disp(iterdata);
                    iter = iter + 1;

                end

                %  Display convergence method
                if iter > Estimator.MaxIterations
                    disp(' ')
                    disp('Batch Estimator Did Not Converge Before Reaching Max Iterations!!')
                else
                    disp(' ')
                    disp('Batch Estimator Has Converged!!')
                end

                %  Plot the residuals
                figure(1);
                plot(1:1:numObs,resid,'*')

                %  Copy object clones in ESM.Objects
                Estimator.ESM.SetObjectstoClones;        % Set Objects to Clones
                Estimator.ESM.SetStates(Estimator.ESV);  % Update states based on ESV

                % -- This is another KLUDGE!!  Find out where this should
                % go and move it there.  Also, generalize the output.
                for i = 1:Estimator.ESM.numObjects

                    for j = 1:size(Estimator.ESM.ParamIds{i},2)

                        Id = Estimator.ESM.ParamIds{i}(j);
                        State = Estimator.ESM.Objects{i}.GetState(Id);
                        disp(' ');
                        if Id == 201
                            disp('Spacecraft State Estimate is:')
                        elseif Id ==401
                            disp('Measurement Bias Estimate is:')
                        elseif Id ==301
                            disp('Ground Station Location Estimate is:')
                        end
                        disp(State)

                    end
                end

            end

        end % Execute
    end
end