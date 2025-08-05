:- module(prolog_session_manager, [
    create_session/2,
    switch_session/1,
    delete_session/1,
    save_session_state/1,
    restore_session_state/1,
    get_session_state/2,
    list_sessions/1,
    current_session/1,
    session_exists/1,
    clear_session_state/1,
    export_session_state/2,
    import_session_state/2
]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(library(terms)).
:- use_module(library(filesex)).
:- use_module(library(semweb/rdf11)).

% Dynamic predicates for session management
:- dynamic(session_config/6).      % session_config(Id, Name, CreatedAt, LastAccessed, IsActive, Metadata)
:- dynamic(session_state/2).       % session_state(SessionId, StateData)
:- dynamic(current_session_id/1).  % current_session_id(SessionId)
:- dynamic(session_fact/2).        % session_fact(SessionId, Fact)
:- dynamic(session_rule/2).        % session_rule(SessionId, Rule)
:- dynamic(session_file/2).        % session_file(SessionId, FilePath)
:- dynamic(session_module/2).      % session_module(SessionId, Module)
:- dynamic(session_variable/3).    % session_variable(SessionId, VarName, Value)
:- dynamic(session_rdf_triple/5).  % session_rdf_triple(SessionId, Subject, Predicate, Object, Graph)

% Initialize session management
:- initialization(init_session_manager).

init_session_manager :-
    format(user_error, '[prolog_session_manager] Initializing session manager~n', []),
    % Create default session if none exists
    (   current_session_id(_)
    ->  true
    ;   create_default_session
    ).

create_default_session :-
    get_time(Now),
    SessionId = 'default',
    assertz(session_config(SessionId, 'Default Session', Now, Now, true, _{type: default})),
    assertz(current_session_id(SessionId)),
    format(user_error, '[prolog_session_manager] Created default session~n', []).

%! create_session(+Name, -SessionId) is det.
%  Create a new session with the given name
create_session(Name, SessionId) :-
    uuid(SessionId),
    get_time(Now),
    assertz(session_config(SessionId, Name, Now, Now, false, _{type: user_created})),
    format(user_error, '[prolog_session_manager] Created session ~w (~w)~n', [SessionId, Name]).

%! switch_session(+SessionId) is det.
%  Switch to the specified session
switch_session(SessionId) :-
    (   session_config(SessionId, _, _, _, _, _)
    ->  % Deactivate current session
        (   current_session_id(CurrentId)
        ->  retract(current_session_id(CurrentId)),
            retract(session_config(CurrentId, Name1, Created1, _, _, Meta1)),
            get_time(Now),
            assertz(session_config(CurrentId, Name1, Created1, Now, false, Meta1))
        ;   true
        ),
        % Activate new session
        retract(session_config(SessionId, Name2, Created2, _, _, Meta2)),
        get_time(Now2),
        assertz(session_config(SessionId, Name2, Created2, Now2, true, Meta2)),
        assertz(current_session_id(SessionId)),
        % Load session state
        load_session_context(SessionId),
        format(user_error, '[prolog_session_manager] Switched to session ~w (~w)~n', [SessionId, Name2])
    ;   throw(error(existence_error(session, SessionId), context(switch_session/1, 'Session does not exist')))
    ).

%! delete_session(+SessionId) is det.
%  Delete a session and all its associated data
delete_session(SessionId) :-
    (   session_config(SessionId, Name, _, _, IsActive, _)
    ->  (   IsActive = true
        ->  throw(error(permission_error(delete, session, SessionId), context(delete_session/1, 'Cannot delete active session')))
        ;   % Remove all session data
            retractall(session_config(SessionId, _, _, _, _, _)),
            retractall(session_state(SessionId, _)),
            retractall(session_fact(SessionId, _)),
            retractall(session_rule(SessionId, _)),
            retractall(session_file(SessionId, _)),
            retractall(session_module(SessionId, _)),
            retractall(session_variable(SessionId, _, _)),
            retractall(session_rdf_triple(SessionId, _, _, _, _)),
            format(user_error, '[prolog_session_manager] Deleted session ~w (~w)~n', [SessionId, Name])
        )
    ;   throw(error(existence_error(session, SessionId), context(delete_session/1, 'Session does not exist')))
    ).

%! save_session_state(+SessionId) is det.
%  Save the current state of a session
save_session_state(SessionId) :-
    (   session_config(SessionId, _, _, _, _, _)
    ->  % Clear existing state
        retractall(session_state(SessionId, _)),
        retractall(session_fact(SessionId, _)),
        retractall(session_rule(SessionId, _)),
        retractall(session_file(SessionId, _)),
        retractall(session_module(SessionId, _)),
        retractall(session_variable(SessionId, _, _)),
        retractall(session_rdf_triple(SessionId, _, _, _, _)),
        
        % Save current facts (user-defined predicates)
        forall(
            (current_predicate(F/A), functor(Head, F, A), 
             \+ predicate_property(Head, built_in),
             \+ predicate_property(Head, imported_from(_)),
             clause(Head, Body)),
            (term_string(Head :- Body, RuleStr),
             assertz(session_rule(SessionId, RuleStr)))
        ),
        
        % Save facts (ground terms)
        forall(
            (current_predicate(F/A), functor(Head, F, A),
             \+ predicate_property(Head, built_in),
             \+ predicate_property(Head, imported_from(_)),
             call(Head),
             ground(Head)),
            (term_string(Head, FactStr),
             assertz(session_fact(SessionId, FactStr)))
        ),
        
        % Save RDF triples if available
        (   current_predicate(rdf/3)
        ->  forall(
                rdf(S, P, O),
                assertz(session_rdf_triple(SessionId, S, P, O, default))
            )
        ;   true
        ),
        
        % Save loaded files information
        forall(
            source_file(Head, File),
            (   \+ predicate_property(Head, built_in),
                assertz(session_file(SessionId, File))
            )
        ),
        
        get_time(Timestamp),
        StateData = _{
            timestamp: Timestamp,
            facts_count: 0,
            rules_count: 0,
            files_count: 0,
            rdf_triples_count: 0
        },
        
        % Count saved items
        aggregate_all(count, session_fact(SessionId, _), FactsCount),
        aggregate_all(count, session_rule(SessionId, _), RulesCount),
        aggregate_all(count, session_file(SessionId, _), FilesCount),
        aggregate_all(count, session_rdf_triple(SessionId, _, _, _, _), RDFCount),
        
        FinalStateData = StateData.put([
            facts_count: FactsCount,
            rules_count: RulesCount,
            files_count: FilesCount,
            rdf_triples_count: RDFCount
        ]),
        
        assertz(session_state(SessionId, FinalStateData)),
        format(user_error, '[prolog_session_manager] Saved state for session ~w: ~w facts, ~w rules, ~w files, ~w RDF triples~n', 
               [SessionId, FactsCount, RulesCount, FilesCount, RDFCount])
    ;   throw(error(existence_error(session, SessionId), context(save_session_state/1, 'Session does not exist')))
    ).

%! restore_session_state(+SessionId) is det.
%  Restore the state of a session
restore_session_state(SessionId) :-
    (   session_state(SessionId, StateData)
    ->  % Clear current knowledge base (be careful!)
        clear_user_knowledge_base,
        
        % Restore facts
        forall(
            session_fact(SessionId, FactStr),
            (catch(
                (term_string(Fact, FactStr), assertz(Fact)),
                Error,
                format(user_error, '[prolog_session_manager] Error restoring fact ~w: ~w~n', [FactStr, Error])
            ))
        ),
        
        % Restore rules
        forall(
            session_rule(SessionId, RuleStr),
            (catch(
                (term_string(Rule, RuleStr), assertz(Rule)),
                Error,
                format(user_error, '[prolog_session_manager] Error restoring rule ~w: ~w~n', [RuleStr, Error])
            ))
        ),
        
        % Restore RDF triples
        forall(
            session_rdf_triple(SessionId, S, P, O, Graph),
            (catch(
                rdf_assert(S, P, O, Graph),
                Error,
                format(user_error, '[prolog_session_manager] Error restoring RDF triple ~w ~w ~w: ~w~n', [S, P, O, Error])
            ))
        ),
        
        % Restore loaded files (re-consult them)
        forall(
            session_file(SessionId, File),
            (catch(
                consult(File),
                Error,
                format(user_error, '[prolog_session_manager] Error re-consulting file ~w: ~w~n', [File, Error])
            ))
        ),
        
        FactsCount = StateData.get(facts_count),
        RulesCount = StateData.get(rules_count),
        FilesCount = StateData.get(files_count),
        RDFCount = StateData.get(rdf_triples_count),
        
        format(user_error, '[prolog_session_manager] Restored state for session ~w: ~w facts, ~w rules, ~w files, ~w RDF triples~n', 
               [SessionId, FactsCount, RulesCount, FilesCount, RDFCount])
    ;   throw(error(existence_error(session_state, SessionId), context(restore_session_state/1, 'Session state does not exist')))
    ).

%! get_session_state(+SessionId, -StateData) is det.
%  Get the state data for a session
get_session_state(SessionId, StateData) :-
    (   session_state(SessionId, StateData)
    ->  true
    ;   StateData = _{timestamp: 0, facts_count: 0, rules_count: 0, files_count: 0, rdf_triples_count: 0}
    ).

%! list_sessions(-Sessions) is det.
%  List all sessions with their configurations
list_sessions(Sessions) :-
    findall(
        _{id: Id, name: Name, created_at: CreatedAt, last_accessed: LastAccessed, is_active: IsActive, metadata: Metadata},
        session_config(Id, Name, CreatedAt, LastAccessed, IsActive, Metadata),
        Sessions
    ).

%! current_session(-SessionId) is det.
%  Get the current active session ID
current_session(SessionId) :-
    current_session_id(SessionId).

%! session_exists(+SessionId) is semidet.
%  Check if a session exists
session_exists(SessionId) :-
    session_config(SessionId, _, _, _, _, _).

%! clear_session_state(+SessionId) is det.
%  Clear all state data for a session
clear_session_state(SessionId) :-
    retractall(session_state(SessionId, _)),
    retractall(session_fact(SessionId, _)),
    retractall(session_rule(SessionId, _)),
    retractall(session_file(SessionId, _)),
    retractall(session_module(SessionId, _)),
    retractall(session_variable(SessionId, _, _)),
    retractall(session_rdf_triple(SessionId, _, _, _, _)),
    format(user_error, '[prolog_session_manager] Cleared state for session ~w~n', [SessionId]).

%! export_session_state(+SessionId, +FilePath) is det.
%  Export session state to a file
export_session_state(SessionId, FilePath) :-
    (   session_exists(SessionId)
    ->  open(FilePath, write, Stream),
        % Write session metadata
        session_config(SessionId, Name, CreatedAt, LastAccessed, IsActive, Metadata),
        format(Stream, '%% Session: ~w (~w)~n', [SessionId, Name]),
        format(Stream, '%% Created: ~w, Last Accessed: ~w, Active: ~w~n', [CreatedAt, LastAccessed, IsActive]),
        format(Stream, '%% Metadata: ~w~n~n', [Metadata]),
        
        % Write facts
        format(Stream, '%% Facts~n', []),
        forall(
            session_fact(SessionId, FactStr),
            format(Stream, '~w.~n', [FactStr])
        ),
        
        % Write rules
        format(Stream, '~n%% Rules~n', []),
        forall(
            session_rule(SessionId, RuleStr),
            format(Stream, '~w.~n', [RuleStr])
        ),
        
        % Write RDF triples
        format(Stream, '~n%% RDF Triples~n', []),
        forall(
            session_rdf_triple(SessionId, S, P, O, Graph),
            format(Stream, 'rdf_assert(~q, ~q, ~q, ~q).~n', [S, P, O, Graph])
        ),
        
        close(Stream),
        format(user_error, '[prolog_session_manager] Exported session ~w to ~w~n', [SessionId, FilePath])
    ;   throw(error(existence_error(session, SessionId), context(export_session_state/2, 'Session does not exist')))
    ).

%! import_session_state(+SessionId, +FilePath) is det.
%  Import session state from a file
import_session_state(SessionId, FilePath) :-
    (   session_exists(SessionId)
    ->  (   exists_file(FilePath)
        ->  % Clear existing state
            clear_session_state(SessionId),
            % Consult the file to load the state
            catch(
                consult(FilePath),
                Error,
                (format(user_error, '[prolog_session_manager] Error importing session state: ~w~n', [Error]),
                 throw(Error))
            ),
            % Save the imported state
            save_session_state(SessionId),
            format(user_error, '[prolog_session_manager] Imported session state from ~w to session ~w~n', [FilePath, SessionId])
        ;   throw(error(existence_error(file, FilePath), context(import_session_state/2, 'File does not exist')))
        )
    ;   throw(error(existence_error(session, SessionId), context(import_session_state/2, 'Session does not exist')))
    ).

% Helper predicates

%! load_session_context(+SessionId) is det.
%  Load the context (state) for a session
load_session_context(SessionId) :-
    (   session_state(SessionId, _)
    ->  restore_session_state(SessionId)
    ;   format(user_error, '[prolog_session_manager] No saved state for session ~w~n', [SessionId])
    ).

%! clear_user_knowledge_base is det.
%  Clear user-defined predicates from the knowledge base
clear_user_knowledge_base :-
    % This is a dangerous operation - only clear user-defined predicates
    forall(
        (current_predicate(F/A), 
         functor(Head, F, A),
         \+ predicate_property(Head, built_in),
         \+ predicate_property(Head, imported_from(_)),
         \+ atom_concat('$', _, F),  % Don't touch system predicates
         F \= session_config,        % Don't touch session management predicates
         F \= session_state,
         F \= current_session_id,
         F \= session_fact,
         F \= session_rule,
         F \= session_file,
         F \= session_module,
         F \= session_variable,
         F \= session_rdf_triple),
        abolish(F/A)
    ),
    % Clear RDF database if available
    (   current_predicate(rdf_retractall/3)
    ->  rdf_retractall(_, _, _)
    ;   true
    ).

%! uuid(-UUID) is det.
%  Generate a simple UUID-like identifier
uuid(UUID) :-
    get_time(Time),
    A is random(1000000),
    B is random(1000000),
    format(atom(UUID), 'session_~w_~w_~w', [Time, A, B]).

% Session-aware query execution predicates

%! session_assert(+SessionId, +Term) is det.
%  Assert a term in the context of a specific session
session_assert(SessionId, Term) :-
    (   current_session_id(SessionId)
    ->  assertz(Term)
    ;   term_string(Term, TermStr),
        assertz(session_fact(SessionId, TermStr))
    ).

%! session_retract(+SessionId, +Term) is det.
%  Retract a term in the context of a specific session
session_retract(SessionId, Term) :-
    (   current_session_id(SessionId)
    ->  retract(Term)
    ;   term_string(Term, TermStr),
        retract(session_fact(SessionId, TermStr))
    ).

%! session_call(+SessionId, +Goal) is nondet.
%  Call a goal in the context of a specific session
session_call(SessionId, Goal) :-
    (   current_session_id(SessionId)
    ->  call(Goal)
    ;   % Switch to session temporarily
        current_session_id(CurrentId),
        switch_session(SessionId),
        call_cleanup(
            call(Goal),
            switch_session(CurrentId)
        )
    ).

% Integration with existing query tracking
:- if(current_predicate(register_query/2)).
session_register_query(SessionId, QueryId, Status) :-
    register_query(QueryId, Status),
    assertz(session_variable(SessionId, last_query_id, QueryId)).
:- endif.

:- if(current_predicate(update_query_status/3)).
session_update_query_status(SessionId, QueryId, Status, Progress) :-
    update_query_status(QueryId, Status, Progress),
    assertz(session_variable(SessionId, last_query_status, Status)).
:- endif.