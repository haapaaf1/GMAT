classdef fogm < measerrs.biases.bias
    % Class for biases that are first-order Gauss-Markov processes.
    methods
        function fgm = fogm(p,q,tau)
            fgm = fgm@measerrs.biases.bias(p);
            fgm.stm = measerrs.biases.stms.expstm(tau);
            fgm.pnoise = measerrs.biases.pnoises.fogmpn(q,tau);
        end % function
    end % methods
end % classdef