function [mid] = Bisection(func,a,b,tol,maxiter)

%  Assumption: One of f(a) is ? 0 and the other is ? 0
fa = feval(func,a);
if fa <= 0 
    lo = a;
    hi = b;
else
    lo = b;
    hi = a;
end

%  Perform bisection
mid = lo + (hi-lo)/2;
iter = 0;
while abs(hi-lo) > tol && iter <= maxiter
    iter = iter + 1;
    fmid = feval(func,mid);
    if fmid <= 0 
        lo = mid;
    else
        hi = mid;
    end
    mid = lo + (hi-lo)/2;
end


