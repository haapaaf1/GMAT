classdef sogm < measerrs.biases.bias
    % Class for biases that are second-order Gauss-Markov processes.
    methods
        function sgm = sogm(p,q,w_n,zeta)
            sgm = sgm@measerrs.biases.bias(p);
            sgm.stm = measerrs.biases.stms.vibstm(w_n,zeta);
            sgm.pnoise = measerrs.biases.pnoises.sogmpn(q,w_n,zeta);
        end % function
    end % methods
end % classdef