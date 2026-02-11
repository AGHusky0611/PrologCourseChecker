% --- FIRST YEAR: 1st Semester ---
% format: course(Code, Name, Units).
course(cs111, 'Introduction to Computing', 3).
course(cs112, 'Computer Programming 1', 3).
course(cs113, 'Discrete Structures', 3).

% --- FIRST YEAR: 2nd Semester ---
course(cs121, 'Digital Logic Design', 3).
course(cs122, 'Computer Programming 2', 3).
course(cs123, 'Architecture and Organization', 3).

% --- FIRST YEAR: Short Term ---
course(cs131, 'Software Modeling and Analysis', 3).
course(cs132, 'Mathematics for Computer Science', 3).

% --- SECOND YEAR: 1st Semester ---
course(cs211, 'Data Structures', 3).
course(cs212, 'Operating Systems', 3).
course(cs213, 'Human Computer Interaction', 3).

% --- SECOND YEAR: 2nd Semester ---
course(cs221, 'Information Management', 3).
course(cs222, 'Computer Programming 3', 3).
course(cs223, 'Automata and Formal Languages', 3).

% --- SECOND YEAR: Short Term ---
course(cs231, 'Computer Networks', 3).

% --- THIRD YEAR: 1st Semester ---
course(cs311, 'Applications Development', 3).
course(cs312, 'Web Systems Development', 3).
course(cs313, 'Software Engineering', 3).
course(cs314, 'Social and Personal Development in the ICT Workplace', 3).
course(cs315, 'Technology-Assisted Presentation and Presentation', 3).
course(csm316, 'Numerical Methods for Computer Science', 3).

% --- THIRD YEAR: 2nd Semester ---
course(cs321, 'Artificial Intelligence', 3).
course(cs322, 'Data Science', 3).
course(cs323, 'Modeling and Simulation', 3).
course(cs324, 'Methods of Research in Computer Science', 3).
course(cs325, 'Structure of Programming Languages', 3).

% --- THIRD YEAR: Short Term ---
course(cs331, 'Practicum', 6).

% --- FOURTH YEAR: 1st Semester ---
course(cs411, 'CS Thesis 1', 3).
course(cs412, 'Information Assurance and Security', 3).
course(cs413, 'Professional Practice and Cyberethics', 3).

% --- FOURTH YEAR: 2nd Semester ---
course(cs421, 'CS Thesis 2', 3).
course(cs422, 'Distributed Computing', 3).
course(for_lang1, 'Foreign Language 1', 3).

% --- ELECTIVES (CSE Subjects) ---
course(cse10, 'Advanced Computer Architecture', 3).
course(cse11, 'Advanced Operating Systems', 3).
course(cse12, 'Advanced Networking and IoT', 3).
course(cse13, 'Advanced Information Management', 3).
course(cse14, 'Advanced Software Engineering', 3).
course(cse15, 'Data Mining', 3).
course(cse16, 'Design and Analysis of Algorithms', 3).
course(cse17, 'Operations Research', 3).
course(cse18, 'Machine Learning', 3).
course(cse19, 'Compiler Design', 3).
course(cse20, 'Advanced Security Concepts', 3).
course(cse21, 'Multimedia Systems', 3).
course(cse22, 'Advanced Applications Development', 3).
course(cse23, 'Computer Graphics', 3).
course(cse24, 'Game Design and Development', 3).
course(cse25, 'UX Design and Concepts', 3).

% --- GENERAL EDUCATION SUBJECTS ---
course(gmath, 'Mathematics in the Modern World', 3).
course(gart, 'Art Appreciation', 3).
course(ghist, 'Readings in Philippine History', 3).
course(fithw, 'Fitness (Health and Wellness)', 2).
course(cfe101, 'God''s Journey with His People', 3).
course(gself, 'Understanding the Self', 3).
course(gpcom, 'Purposive Communication', 3).
course(genvi, 'Environmental Science', 3).
course(fitcs, 'Fitness (Combative Sports)', 2).
course(cfe102, 'Christian Morality in Our Times', 3).
course(gsts, 'Science, Technology, and Society', 3).
course(grva, 'Reading Visual Art', 3).
course(nstpcwts1, 'Foundations of Service', 3).
course(fitoa, 'Physical Activity Towards Health and Fitness (Outdoor and Adventure)', 2).
course(cfe103, 'Catholic Foundation of Mission', 3).
course(grizal, 'The Life and Works of Rizal', 3).
course(gentrep, 'The Entrepreneurial Mind', 3).
course(gethics, 'Ethics', 3).
course(gcworld, 'The Contemporary World', 3).
course(nstpcwts2, 'Social Awareness and Empowerment for Service', 3).
course(fitaq, 'Physical Activity Towards Health and Fitness (Aquatics)', 2).
course(cfe104, 'CICM Missionary Identity', 3).
course(cfe105a, 'CICM in Action: Justice, Peace, and Integrity of Creation, Indigenous Peoples, and Interreligious Dialogue', 1.5).
course(cfe105b, 'CICM in Action:Environmental Planning and Management and Disaster Risk Reduction Management', 1.5).
course(cfe106a, 'Embracing the CICM Mission 1', 1.5).
course(cfe106b, 'Embracing the CICM Mission 2', 1.5).

% --- PREREQUISITE RULES ---
% =====================================================================

% First Year 
prereq(cs131, cs111).

prereq(cs121, cs111).

prereq(cs122, cs112).

prereq(cs123, cs112).
prereq(cfe102, cfe101).

% 2nd Year Prereqs
prereq(cs211, second_year_standing).
prereq(cs212, second_year_standing).
prereq(cs213, second_year_standing).
prereq(cs221, second_year_standing).
prereq(cs222, second_year_standing).
prereq(cs223, second_year_standing).
prereq(cs231, second_year_standing).

prereq(cs211, cs112). prereq(cs211, cs113).
prereq(cs212, cs122). prereq(cs212, cs122l).
prereq(cs231, cs212).
prereq(cs221, cs211).
prereq(cs222, cs122).
prereq(cs223, cs112). prereq(cs223, cs132).

prereq(cfe104, cfe103).
prereq(nstpcwts2, nstpcwts1).

% 3rd Year Prereqs
prereq(cs311, third_year_standing).
prereq(cs312, third_year_standing).
prereq(cs313, third_year_standing).
prereq(cs314, third_year_standing).
prereq(cs315, third_year_standing).
prereq(csm316, third_year_standing).
prereq(cs321, third_year_standing).
prereq(cs322, third_year_standing).
prereq(cs323, third_year_standing).
prereq(cs324, third_year_standing).
prereq(cs325, third_year_standing).

% to get practicum must be 4th yr standing
prereq(cs331, fourth_year_standing).

prereq(cs311, cs122).
prereq(cs312, cs211).
prereq(cs313, cs131). prereq(cs313, cs231).
prereq(cs314, cs111). prereq(cs314, gself).
prereq(cs315, cs111). prereq(cs315, gpcom).
prereq(csm316, cs132).
prereq(cs321, cs132). prereq(cs321, cs211).
prereq(cs322, cs221).
prereq(cs323, cs132). prereq(cs323, cs211).
prereq(cs325, cs211).
prereq(cfe105a, cfe104). prereq(cfe105a, cfe103).
prereq(cfe105b, cfe105a).

% 4th Year Prereqs
prereq(cs411, fourth_year_standing).
prereq(cs412, fourth_year_standing).
prereq(cs413, fourth_year_standing).
prereq(cs421, fourth_year_standing).
prereq(cs422, fourth_year_standing).

prereq(cs411, cs324).
prereq(cs421, cs411).
prereq(cfe106a, cfe105b).
prereq(cs331, fourth_year_standing).

% 4th Year Prereqs
prereq(cs411, cs324).
prereq(cs411, fourth_year_standing).
prereq(cs421, cs411).
prereq(cfe106a, cfe105b).
prereq(cfe106b, cfe106a).

% Electives
prereq(cse10, third_year_standing).
prereq(cse11, third_year_standing).
prereq(cse12, third_year_standing).
prereq(cse13, third_year_standing).
prereq(cse14, third_year_standing).
prereq(cse15, third_year_standing).
prereq(cse16, third_year_standing).
prereq(cse17, third_year_standing).
prereq(cse18, third_year_standing).
prereq(cse19, third_year_standing).
prereq(cse20, third_year_standing).
prereq(cse21, third_year_standing).
prereq(cse22, third_year_standing).
prereq(cse23, third_year_standing).
prereq(cse24, third_year_standing).
prereq(cse25, third_year_standing).

% A student can enroll if ALL prerequisites are in their FinishedList.
is_eligible(Course, FinishedList) :-
    course(Course, _, _),
    findall(P, prereq(Course, P), Required),
    forall(member(X, Required), (
        (X == second_year_standing) -> has_second_year_standing(FinishedList)
        ; (X == third_year_standing) -> has_third_year_standing(FinishedList)
        ; (X == fourth_year_standing) -> has_fourth_year_standing(FinishedList)
        ; member(X, FinishedList)
    )).

% --- YEAR STANDING RULES ---
has_second_year_standing(Finished) :-
    member(cs121, Finished),
    member(cs122, Finished),
    member(cs123, Finished),
    member(gself, Finished),
    member(gpcom, Finished),
    member(genvi, Finished),
    member(fitcs, Finished),
    member(cfe102, Finished),
    member(cself, Finished),
    member(gpcom, Finished),
    member(genvi, Finished),
    member(fitcs, Finished),
    member(cfe102, Finished).

has_third_year_standing(Finished) :-
    has_second_year_standing(Finished),
    member(cs211, Finished),
    member(cs212, Finished).

has_fourth_year_standing(Finished) :-
    has_third_year_standing(Finished),
    member(cs313, Finished),
    member(cs312, Finished).

% Helper to find what is missing
what_is_missing(Course, Finished, Missing) :-
    findall(P, (prereq(Course, P), \+ member(P, Finished)), Missing).

% Rule to calculate total units (Needed for Year Standing)
total_units([], 0).
total_units([H|T], Total) :-
    course(H, _, Units),
    total_units(T, Rest),
    Total is Units + Rest.