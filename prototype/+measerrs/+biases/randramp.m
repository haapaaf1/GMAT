classdef randramp < measerrs.biases.bias
    % Class for biases whose derivative is a random constant.
    methods
        function rr = randramp(P)
            rr = rr@measerrs.biases.bias(P);
            rr.biasStatePartial = [1 0];
            rr.stm = measerrs.biases.stms.rampstm;
        end % function
    end % methods
end % classdef