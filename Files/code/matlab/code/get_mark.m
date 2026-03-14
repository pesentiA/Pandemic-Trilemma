%% returns deterministic marks for the macro students

function mark = get_mark(student)
    % match student name and allocate mark
    switch student
        case 'Andreas'
            mark = 65;
        case 'Ran'
            mark = 95;
        case 'Alex'
            mark = 85;
        case 'Lukas'
            mark = 75;
        case 'Aulis'
            mark = 99;
        otherwise
            mark = NaN; % Not a Number (NaN)
   end
end