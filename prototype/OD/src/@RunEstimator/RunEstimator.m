
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

            %----- The Estimator Loop
            oldRMS = 0;  newRMS = 1e12; normdx = 1e12; iter = 1;
            numStates = Estimator.ESM.numStates;
            while abs( newRMS - oldRMS ) > Estimator.RelTolerance ...
                    & iter <= Estimator.MaxIterations ...
                    & normdx > Estimator.AbsTolerance;

                %------------------------------------------------------
                %  Initializations for the next iteration
                %------------------------------------------------------
                Estimator.ESM.SetObjectstoClones;        % Set Objects to Clones
                Estimator.ESM.SetStates(Estimator.ESV);  % Update states based on ESV
                RunEst = RunEst.PreparetoPropagate();    % Update PSV
                if Estimator.UseAprioriCovariance
                    if iter == 1
                        aprioriCov          = Estimator.ESM.GetCovariance;
                        invaprioriCov       = inv(aprioriCov);
                        x0bar = Estimator.ESV;
                    else
                        x0bar = x0bar - Estimator.ESV;
                    end
                    infoMat  = invaprioriCov;
                    residMat = invaprioriCov*x0bar;
                else
                    infoMat   = zeros(numStates,numStates);  % Set info mat to zer
                    residMat  = zeros(numStates,1);          % Set rediduals to zero
                end

                %------------------------------------------------------
                %----- Perform the accumulation
                %------------------------------------------------------
                residCount = 0;

                resid      = [];
                for i = 1:numObs

                    %  Step to next measurement epoch
                    Prop = Prop.SteptoEpoch(Epochs(i));
                    [y,Htilde,isFeasible] = measManager.GetMeasurement(i);
                    newResid    = Obs(i,:)' - y;
                    resid       = [resid; newResid];
                    STM         = Estimator.ESM.GetSTM;
                    Hi          = Htilde*STM;
                    infoMat     = infoMat + Hi'*Hi;
                    residMat    = residMat + Hi'*newResid;

                end
                %------------------------------------------------------
                %  Solve the normal equations and update the ESV
                %------------------------------------------------------
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
                %  Plot the residuals
                figure(1);
                plot(1:1:size(resid,1),resid,'*')

            end
            
            %  Display convergence method
            if iter > Estimator.MaxIterations
                disp(' ')
                disp('Batch Estimator Did Not Converge Before Reaching Max Iterations!!')
            else
                disp(' ')
                disp('Batch Estimator Has Converged!!')
            end

            %  Copy object clones in ESM.Objects
            Estimator.ESM.SetObjectstoClones;        % Set Objects to Clones
            Estimator.ESM.SetStates(Estimator.ESV);  % Update states based on ESV
            Estimator.ESM.SetCovariance(inv(infoMat))

            % -- This is another KLUDGE!!  Find out where this should
            % go and move it there.  Also, generalize the output.
            for i = 1:Estimator.ESM.numObjects

                for j = 1:size(Estimator.ESM.ParamIds{i},2)

                    Id    = Estimator.ESM.ParamIds{i}(j);
                    State = Estimator.ESM.Objects{i}.GetState(Id);
                    Cov   = Estimator.ESM.Objects{i}.GetCovariance(Id);
                    disp(' ');
                    if Id == 201
                        disp('Spacecraft State Estimate is:')
                    elseif Id ==401
                        disp('Measurement Bias Estimate is:')
                    elseif Id ==301
                        disp('Ground Station Location Estimate is:')
                    end
                    disp(State)
                    if Id == 201
                        disp('Spacecraft Covariance is:')
                    elseif Id ==401
                        disp('Measurement Bias Covariance is:')
                    elseif Id ==301
                        disp('Ground Station Location Covariance is:')
                    end
                    disp(Cov)

                end
            end

        end % Execute
    end
end