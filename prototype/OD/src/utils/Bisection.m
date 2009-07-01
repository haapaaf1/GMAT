function [mid] = Bisection(func,a,b,tol,maxiter)

%  Assumption:  f(a) is < 0 and f(b) is > 0
fa = feval(func,a);
if fa <= 0 
    lo = a;
    hi = b;
else
    lo = b;
    hi = a;
end

%  Perform bisection
iter = 0;
while abs(hi-lo) > tol && iter <= maxiter
    iter = iter + 1;
    mid = lo + (hi-lo)/2;
    fmid = feval(func,mid);
    if fmid <= 0 
        lo = mid;
    else
        hi = mid;
    end
end


