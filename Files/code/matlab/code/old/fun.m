%% gives a new section

%% Section 2

A=[1,3;2,4];

%always put a semicollon at the end, otherwise it will not stop

B=A*A;

C = A.*A;

%% Section 3

D = A(1,:);

%double dots gives you all of the first row

E = A(:,2);

D = A(1,:); %returns the first row of matrix A, [1 3].
E = A(:,2); %returns the second column of matrix A, [3 4]T

i = 2:2:10; %creates the row vector i = [2 4 6 8 10].
F = max(A); %returns the max along the columns of A, [2 4].
G = max(A,[],2); %returns the max along the rows of A, [3 4]T


