% create matrix of random numbers of size 5x5
data = rand(5,5)


% takes the upper triangle (not including the diagonal line)
upper_triu = triu(data,1)

% creates a vector of the upper triangle
% goes from left to right, and up to down
% i.e. column 2 row 1, column 3 row 1, column 3 row 2, column 4 row 1, etc.
vector = data(triu(true(size(data)),1))

% export that vector to read into r
