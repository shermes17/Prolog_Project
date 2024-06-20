/*
Sean Hermes
CPSC 3520
SDE1


The following is a course scheduling tool in Prolog.
The first part defines a database of courses with their pre-requisites
and co-requisites taken from the current Computer Engineering undergraduate
curriculum.  It does not include courses picked from a list such as
humanities, STS, and tedchnical electives, only the required courses.
Next comes a section of predicats that defines which of these courses have
already been taken and the egrade assigned.  We aren't going to use the
grade but you can see how we might expand it.  I only have a couple of
these entered, you can use them, remove them, or add more if you like to
test different situations.
After that are rules.  Near the bottom the main user interface is the
check_course and check_schedule predicates that check to see if all the
requirements are satisfied.  The first takes a single course name, and
the latter takes a list of courses.  Both return a list of missing 
pre-requisites and a list of missing co-requisites.  In the case of
check_schedule if a co-requisite course is listed in the schedule it
should not appear as missing.  If a course has already been taken, the
code prints a simple error message and returns fail.  The other predicates
represent a stepwise implementation of these two, and simple utilities
that let you explore the relationships in the database.  They are fairly
easy so you should do them first, and then use them to implement the more
complex ones.  The last predicate is suggest_course that tries to pick a
course that has not been taken and has all of its requisites completed.
You are free to add other predicates you feel you need in order to
implement the required ones.  There is more than one way to solve the
problems.  You will also want to look at built-in predicates either via
the documentation or the apropos and help predicates in SWIPL.  In
particular these predicates may be useful:

member(Item,List).
append(List1,List2,List+List2).
*/

/* course( <course-name>, <hours>, <pre-req list>, <co-req list> ). */

course(ch1010,4,[],[]).
course(cpsc1110,3,[],[]).
course(ece2010,3,[],[]).
course(ece2020,3,[math1080],[]).
course(ece2090,1,[],[ece2010]).
course(ece2110,1,[],[ece2020]).
course(ece2120,1,[ece2110],[ece2620]).
course(ece2220,3,[cpsc1110],[]).
course(ece2230,3,[ece2220],[]).
course(ece2620,3,[ece2020,phys2210,math2060],[]).
course(ece2720,3,[ece2010,cpsc1110],[ece2730]).
course(ece2730,1,[],[ece2720]).
course(ece3110,1,[ece2120],[]).
course(ece3170,3,[ece2620,math2080],[ece3300]).
course(ece3220,3,[ece2720,ece2230],[]).
course(ece3270,3,[ece3710],[]).
course(ece3300,3,[ece2620,math2080],[]).
course(ece3520,3,[ece2230],[math4190]).
course(ece3710,3,[ece2720],[ece2620]).
course(ece3720,1,[],[ece3710]).
course(ece4090,3,[ece3300],[]).
course(ece4950,2,[ece3710,ece2230,ece3200],[ece4090]).
course(ece4960,2,[ece3270,ece3520,ece4950,ece4090],[]).
course(engl1030,3,[],[]).
course(engr1020,3,[],[]).
course(engr1410,3,[engr1020],[]).
course(math1060,4,[],[]).
course(math1080,4,[math1060],[]).
course(math2060,4,[math1080],[]).
course(math2080,4,[math2060],[]).
course(math3110,3,[math1080],[]).
course(math4190,3,[math3110],[]).
course(phys1120,3,[],[math1060]).
course(phys2210,3,[],[math1080]).

/* completed( <course-name>, <grade> ) - add to or modify these as needed for testing */
/* grades of F are not considered completed */


completed(phys2210,a).
completed(phys1120,b).
completed(math4190,c).
completed(math3110,d).
completed(math2080,f).


schedule(ece3270).
schedule(ece4950).
schedule(ece3720).



/* RULES */

/* returns true if all courses in the List are completed */ 
list_complete([]). % base case
list_complete([Course | Tail]) :-  
    completed(Course, Grade), Grade \= f, % grade is passing, grade !=  f
    list_complete(Tail). % Recurse to the Tail list 

/* Input: Req-List is a list of prerequisite courses loaded from one of the course facts.
 * Output: Missing-reqs is a list of courses in Req-List that are Not completed.
 
 * use this by checking the course first to get the Prereqs:
 *   ,,,  course(C,P,_), missing_reqs(P,PM) ...
 * Now PM is a list of prereqs missing.
 */
missing_req(RL, ML) :-
    missing_req_helper(RL,[], ML).

missing_req_helper([], ML, ML).
% use accumulator to add to list for each iteration
missing_req_helper([Course | Tail], Accumulator, ML) :-
    (list_complete([Course]) -> missing_req_helper(Tail, Accumulator, ML); % if course is completed, move on
    % if course is not completed, add to list, move on
    append(Accumulator, [Course], NewAcc),
    missing_req_helper(Tail, NewAcc, ML)
    ).



/* return true if all pre-reqs are satisified for Course */
get_prereqs(Course, PrereqList) :- course(Course, _, PrereqList, _).
prereq_satisfied(Course):- 
    get_prereqs(Course, PrereqList), % gets list of prereqs for specific class
    list_complete(PrereqList). % checks if list is complete
   
   

/* return a list of missing pre-reqs in output list NP for input Course */
prereq_missing(Course, NP) :-
    get_prereqs(Course, PrereqList), % gets list of prereq courses
    prereq_missing_helper(PrereqList, [], NP).

prereq_missing_helper([], NP, NP). % base case
prereq_missing_helper([Prereq | Tail], Accumulator, NP) :-
    (list_complete([Prereq]) -> prereq_missing_helper(Tail, Accumulator, NP); % if prereq passed, move to next
    % if prereq not passed, append to list, then move to next
    append(Accumulator, [Prereq], NewAcc), 
    prereq_missing_helper(Tail, NewAcc, NP)). % go to next course


/* return true if all co-reqs are satisified for Course */
/* coreqs will be taken with the current course, so will have to  be currently scheduled*/
get_coreqs(Course, CoReqList) :- course(Course, _, _, CoReqList).
coreq_satisfied(Course):-
    get_coreqs(Course, CoReqList), % retrieve list of coreqs
    cur_Scheduled(CoReqList). % checks if all coreqs are scheduled with the course

cur_Scheduled([]).
cur_Scheduled([Course | Tail]) :-  
    schedule(Course),  % course is scheduled
    cur_Scheduled(Tail). % recurse


/* return a list of missing co-reqs in output list NC for input Course */
coreq_missing(Course,NC):-
    get_coreqs(Course, CoReqList), % retrieve list of coreqs
    coreq_missing_helper(CoReqList,[],NC).

coreq_missing_helper([], NC, NC). % base case
coreq_missing_helper([Coreq | Tail], Accumulator, NC) :-
    (cur_Scheduled([Coreq]) -> coreq_missing_helper(Tail,Accumulator,NC); % if coreq is sceduled, recurse
    % if not, append to missing list, then recurse
    append(Accumulator,[Coreq], NewAcc),
    coreq_missing_helper(Tail,NewAcc,NC)).


/* return lists of missing pre-reqs in LP and co-reqs in LC for course C */
/* prints a message if course C is already completed */
check_course(C,LP,LC):-
    % if course c has been completed print message
    (completed(C,Grade), Grade \= f -> write(C), write(" has already been taken!"),nl;
    % if course has not been complete check for missing prereq and coreq
    prereq_missing(C,LP),
    coreq_missing(C,LC)).


/* return lists of all missing pre-reqs in LP and all missing co-reqs in LC */
/* for ALL courses in LA or returns true if there are none missing */
check_schedule(LA, LP, LC):-
    check_schedule_helper(LA,P,[],C,[]),
    (P = [], C = [] -> true; % none missing return ture  
        sort(P,LP), sort(C,LC)). % sorting lists removes duplicates
       
 
check_schedule_helper([],LP,LP,LC,LC).
check_schedule_helper([Course|Tail],LP, AccLP, LC, AccLC) :-
    check_course(Course,NewLP,NewLC), % get preList and coList
    % append to accumulated lists
    append(AccLP,NewLP,NewAccLP), 
    append(AccLC,NewLC,NewAccLC),
    check_schedule_helper(Tail,LP,NewAccLP,LC,NewAccLC). %recurse
    


/* return courses (one at a time) that are not completed and do have their */
/* requisites satisfied. Return in the variable C */

suggest_course(C):-
    course(C,_,_,_), % get each course individually
    get_prereqs(C,PrereqList), % get prereqs for course
    % course is not completed or failed since you would have to retake the course
    not((completed(C,Grade),Grade \= f)), 
    list_complete(PrereqList). % prereqs are satified
    


/* given the current set of completed courses in the  database, compute the gpr */
/* each course has the number of hours as the second item in the predicate */
/* gpr = SUM_i(grade_i * hours_i)/total_hours */
compute_gpr(Gpr) :-
    findall(Course, completed(Course, _), List), % retrieve list of completed courses
    compute_gpr_helper(List, 0, 0, Gpr).

compute_gpr_helper([], Sum, Hours, Gpr) :- % base case
    /*base case final computation
     cant use compute_total_hours since if a course is failed, the hours that is 
     counted for can influence gpa */
    Gpr is Sum / Hours.

compute_gpr_helper([Course | Tail], Sum, SumHours, Gpr) :-
    completed(Course,Grade), % Gets grade
    grade_to_num(Grade, Num), % converts grade to its numerical value
    get_hours(Course, Hours), % gets hours of current course
    NewSum is Sum + (Num * Hours), % gets sum
    NewHours is SumHours + Hours, % adds to total hour sums
    compute_gpr_helper(Tail, NewSum, NewHours, Gpr). % recurse


grade_to_num(Grade,Num):-  % converts Grade letter to its corresponding numerical weight
    (Grade = a -> Num is 4.0;
    (Grade = b -> Num is 3.0;
    (Grade = c -> Num is 2.0; 
    (Grade = d -> Num is 1.0;
    Num is 0.0 )))).

/* given the current set of completed courses in the database, compute the total */
/* hours completed.  Be sure not to award hours for an F */
get_hours(Course, Hours) :-course(Course, Hours, _, _). % retrieves specific course hours

compute_total_hours(Hrs) :-
    % retrieve list of all completed course
    findall(Course, completed(Course, _), List),
    % run helper to go through each course and sum hours together
    cth_helper(List, 0, Hrs).

cth_helper([], Hrs, Hrs). % base case
cth_helper([Course | Tail], Acc, Hrs) :-
    %if course has an f, move to next in the list
    (completed(Course,Grade), Grade = f ->  cth_helper(Tail, Acc,Hrs); 
    % if course was passed, get the hours and add them to the accumulated total
    get_hours(Course, Hours),
    NewAcc is Acc + Hours, % sum of hours
    cth_helper(Tail, NewAcc, Hrs)). % recurse
    


  
    