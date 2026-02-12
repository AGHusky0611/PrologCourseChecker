:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_files)).

:- consult(courses).

% --- SERVER SETUP ---
start_server(Port) :-
    http_server(http_dispatch, [
        port(Port),
        bind_address('0.0.0.0') 
    ]).

:- set_setting(http:cors, [*]).

% --- API ROUTES ---
:- http_handler('/api/courses', get_all_courses, []).
:- http_handler('/api/path', find_prerequisite_path, []).
:- http_handler('/api/check', check_eligibility_handler, []).
:- http_handler(root(.), http_reply_from_files('static', []), [prefix]).

% --- ROUTE HANDLERS ---

% GET /api/courses
get_all_courses(_Request) :-
    findall(
        json{course: Code, prerequisites: Prereqs},
        (
            course(Code, _, _),
            findall(P, prereq(Code, P), Prereqs)
        ),
        AllCourses
    ),
    reply_json(json{courses: AllCourses}).

% POST /api/path
find_prerequisite_path(Request) :-
    http_read_json_dict(Request, Payload),
    atom_string(Course, Payload.course),
    ( course(Course, _, _) ->
        ( findall(P, find_all_prereqs(Course, P), RawPath) ->
            list_to_set(RawPath, Path),
            reply_json(json{path: Path})
        ;
            reply_json(json{path: []})
        )
    ;
        reply_json(json{error: "Course not found", path: []})
    ).

% POST /api/check
check_eligibility_handler(Request) :-
    http_read_json_dict(Request, Payload),
    atom_string(Course, Payload.course),
    
    % Convert the list of strings (Payload.finished) into a list of atoms (Finished)
    maplist(atom_string, Finished, Payload.finished),

    ( is_eligible(Course, Finished) ->
        reply_json(json{eligible: true})
    ; 
        what_is_missing(Course, Finished, Missing),
        reply_json(json{eligible: false, missing: Missing})
    ).

% --- HELPER PREDICATES ---

% Recursively find all prerequisites for a course
find_all_prereqs(Course, Prereq) :-
    prereq(Course, P),
    ( Prereq = P ; find_all_prereqs(P, Prereq) ).

% --- MAIN ---
start :-
    % Railway provides the port via the PORT environment variable
    (getenv('PORT', PortStr) -> atom_number(PortStr, Port) ; Port = 8080),
    start_server(Port),
    format('~n------------------------------------~n'),
    format('Prolog Server Live on Port: ~w~n', [Port]),
    format('------------------------------------~n'),
    % This keeps the main thread alive so the container doesn't exit
    thread_get_message(_).