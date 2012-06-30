function x = covsmpl(P)
% COVSMPL  Generate realizations of zero-mean Gaussian r.v.
%
% X = COVSMPL(P) generates realizations of a Gaussian random
% vector whose mean is zero and covariance is P.  It uses the
% Cholesky decomposition to get the "matrix square root" of P.
% If P has any zeros eigenvalues, this won't work, so use the
% eigenvalue decomposition instead, and force any negative
% eigenvalues to be zero.
n = size(P,1);
try
    % Need to use the transpose because of Matlab's convention
    % that R'*R = P, where R = chol(P).
    x = chol(P)'*randn(n,1);
catch %#ok<CTCH>
    try
        [V,D] = eig(P);
        D(D<0) = 0;
    catch %#ok<CTCH>
        [V,D] = deal(NaN(size(P)));
    end
    x = V*sqrt(D)*randn(n,1);
end