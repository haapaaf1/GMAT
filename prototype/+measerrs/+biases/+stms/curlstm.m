classdef curlstm < measerrs.biases.stms.rampstm
    % Class for STM's of biases with rectlinear drift rates.
    methods
        function Phi = stateTransitionMatrix(~,dt)
            Phi(2:3,2:3) = ...
                stateTransitionMatrix@measerrs.biases.stms.rampstm([],dt);
            Phi(1,1:3) = [1 dt dt^2/2];
        end % function
    end % methods
end % classdef