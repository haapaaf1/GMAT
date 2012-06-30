classdef randzoom < measerrs.biases.bias
    % Class for biases whose drift rate is a random walk.
    methods
        function rz = randzoom(P,Q)
            rz = rz@measerrs.biases.bias(P);
            rz.biasStatePartial = [1 0 0];
            rz.stm = measerrs.biases.stms.curlstm;
            rz.pnoise = measerrs.biases.pnoises.rzoompn(Q);
        end % function
    end % methods
end % classdef