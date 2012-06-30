classdef rwrrrz < measerrs.biases.bias
    % Class for which bias, drift, and drift rate are all random walks.
    methods
        function rwrz = rwrrrz(P,q1,q2,q3)
            rwrz = rwrz@measerrs.biases.bias(P);
            rwrz.stm = measerrs.biases.stms.curlstm;
            rwrz.pnoise = measerrs.biases.pnoises.rwrrrzpn(q1,q2,q3);
        end % function
    end % methods
end % classdef