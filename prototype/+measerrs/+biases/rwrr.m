classdef rwrr < measerrs.biases.bias
    % Class for which bias is a random walk, and drift is a random walk.
    methods
        function rwr = rwrr(P,q1,q2)
            rwr = rwr@measerrs.biases.bias(P);
            rwr.stm = measerrs.biases.stms.rampstm;
            rwr.pnoise = measerrs.biases.pnoises.rwrrpn(q1,q2);
        end % function
    end % methods
end % classdef