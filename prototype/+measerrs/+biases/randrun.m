classdef randrun < measerrs.biases.bias
    % Class for biases whose drift is a random walk.
    methods
        function rr = randrun(P,Q)
            rr = rr@measerrs.biases.bias(P);
            rr.biasStatePartial = [1 0];
            rr.stm = measerrs.biases.stms.rampstm;
            rr.pnoise = measerrs.biases.pnoises.rrunpn(Q);
        end % function
    end % methods
end % classdef