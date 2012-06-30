classdef fsgm < measerrs.biases.bias
    % Class in which bias is a FOGM, and drift is a SOGM.
    methods
        function fs = fsgm(p,q1,q2,tau,w_n,zeta)
            fs = fs@measerrs.biases.bias(p);
            fs.stm = measerrs.biases.stms.exvibstm(tau,w_n,zeta);
            fs.pnoise = measerrs.biases.pnoises.fsgmpn(q1,q2,tau,w_n,zeta);
        end % function
    end % methods
end % classdef