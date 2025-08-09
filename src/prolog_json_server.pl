:- use_module(library(http/http_server)).
:- use_module(library(http/json)).
:- use_module(library(http/http_cors)).
:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(pldoc)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/websocket)).
:- use_module(prolog_session_manager).

% Enhanced Reasoning Features - CLP and Probabilistic Logic
:- use_module(library(clpfd)).
:- use_module(library(clpr)).
:- use_module(library(clpq)).
:- use_module(library(random)).

% Dynamic predicates for enhanced reasoning
:- dynamic(probabilistic_fact/2).  % probabilistic_fact(Fact, Probability)
:- dynamic(user_logic_module/3).   % user_logic_module(Name, Rules, MetaInterpreter)
:- dynamic(clp_constraint/2).      % clp_constraint(Domain, Constraint)

% Enable CORS for local development (http://localhost:3060)
:- set_setting(http:cors, [*]).

% Global state for query tracking and WebSocket connections
:- dynamic(query_status/4).  % query_status(Id, Status, StartTime, Progress)
:- dynamic(websocket_connection/2).  % websocket_connection(Id, WebSocket)

% Main entry point: start HTTP server
main :-
    main(3060).

% Main entry point with port parameter
main(Port) :-
    format(user_error, '[prolog_json_server] main/1 started with port ~w~n', [Port]),
    format(user_error, '[prolog_json_server] SWI-Prolog version: ~w~n', [90209]),
    (   current_predicate(pldoc:doc_comment/4)
    ->  format(user_error, '[prolog_json_server] PlDoc predicate pldoc:doc_comment/4 is available.~n', [])
    ;   format(user_error, '[prolog_json_server] PlDoc predicate pldoc:doc_comment/4 is NOT available.~n', [])
    ),
    % Stop any existing server on this port first
    catch(http_stop_server(Port, []), _, true),
    % Start the HTTP server
    catch(
        http_server(handler, [port(Port)]),
        Error,
        (
            format(user_error, '[prolog_json_server] Failed to start HTTP server: ~w~n', [Error]),
            halt(1)
        )
    ),
    format(user_error, '[prolog_json_server] HTTP server started on port ~w~n', [Port]),
    % Keep the server running by waiting for messages
    repeat,
    sleep(1),
    fail.

% HTTP request handler
handler(Request) :-
    % Check if this is a WebSocket upgrade request
    (   memberchk(upgrade(websocket), Request)
    ->  websocket_handler(Request)
    ;   % Regular HTTP request
        cors_enable,
        http_read_json_dict(Request, DictIn, [value_string_as(atom)]),
        format(user_error, '[prolog_json_server] Received request: ~w~n', [DictIn]),
        % Handle file upload endpoint
        ( DictIn = _{cmd:upload_file, id:Id, filename:Filename, content_base64:ContentB64}
        -> catch(
                handle_upload_file(Filename, ContentB64, Id, DictOut),
                Err,
                (
                    format(user_error, '[prolog_json_server] Error in handle_upload_file: ~w~n', [Err]),
                    DictOut = _{id:Id, status:error, error:Err}
                )
            ),
            reply_json_dict(DictOut, [serialize_unknown(true)])
        ;
        (   DictIn = _{cmd:Cmd, id:Id}
        ->  catch(
                handle_cmd(Cmd, DictIn, Id, DictOut),
                Err,
                (
                    format(user_error, '[prolog_json_server] Error in handle_cmd: ~w~n', [Err]),
                    DictOut = _{id:Id, status:error, error:Err}
                )
            ),
            reply_json_dict(DictOut, [serialize_unknown(true)])
        ;   DictIn = [_|_] % Batch request
        ->  maplist(process_batch_element, DictIn, Responses),
            reply_json_dict(Responses, [serialize_unknown(true)])
        ;   reply_json_dict(_{status:error, error:'missing_cmd_or_id'}, [serialize_unknown(true)])
        )
        )
    ).

% Handle file upload (base64 in JSON)
handle_upload_file(Filename, ContentB64, Id, DictOut) :-
    catch(
        (   setup_call_cleanup(
                open(Filename, write, Stream, [type(binary)]),
                (   base64:base64(ContentB64, Bytes),
                    format(user_error, '[prolog_json_server] Writing ~w bytes to ~w~n', [Bytes, Filename]),
                    format(Stream, '~s', [Bytes])
                ),
                close(Stream)
            ),
            % Consult the uploaded file
            (   catch(user:consult(Filename), Err, (
                        format(user_error, '[prolog_json_server] Consult after upload failed: ~w~n', [Err]),
                        DictOut = _{id:Id, status:error, error:Err},
                        fail
                    ))
            ->  DictOut = _{id:Id, status:ok, result:'uploaded_and_consulted'}
            )
        ),
        Err,
        (   format(user_error, '[prolog_json_server] File upload failed: ~w~n', [Err]),
            DictOut = _{id:Id, status:error, error:Err}
        )
    ).

% Process a single batch element
process_batch_element(DictIn, DictOut) :-
    (   DictIn = _{cmd:Cmd, id:Id}
    ->  catch(
            handle_cmd(Cmd, DictIn, Id, DictOut),
            Err,
            (
                format(user_error, '[prolog_json_server] Error in batch handle_cmd: ~w~n', [Err]),
                DictOut = _{id:Id, status:error, error:Err}
            )
        )
    ;   DictOut = _{status:error, error:'missing_cmd_or_id'}
    ).

% Handle individual commands
handle_cmd(Cmd, Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_cmd/3 called with Cmd=~w~n', [Cmd]),
    (   (string(Cmd) -> atom_string(CmdAtom, Cmd); CmdAtom = Cmd),
        ( CmdAtom = version ->
            current_prolog_flag(version, V),
            DictOut = _{id:Id, status:ok, version:V}
        ; CmdAtom = query ->
            (   _{goal:GoalStr} :< Dict
            ->  (   catch(term_string(Goal, GoalStr), ErrGoal, (
                        format(user_error, '[prolog_json_server] Invalid goal: ~w~n', [ErrGoal]),
                        reply_error('invalid_goal', ErrGoal, Dict, DictOut),
                        fail
                    )),
                    format(user_error, '[TRACE] [prolog_json_server] Parsed Goal=~q~n', [Goal]),
                    (   Goal =.. [consult, FileArg], (string(FileArg); atom(FileArg))
                    ->  (   (atom(FileArg) -> FileStr = FileArg ; FileStr = FileArg),
                            format(user_error, '[TRACE] [prolog_json_server] consult as query: detected consult(~w) and make/0 for id=~w~n', [FileStr, Id]),
                            (   catch((user:consult(FileStr), make), Err, (
                                    format(user_error, '[TRACE] [prolog_json_server] consult as query: consult+make failed for id=~w: ~w~n', [Id, Err]),
                                    reply_error('consult_failed', Err, Dict, DictOut),
                                    fail
                                ))
                            ->  DictOut = _{id:Id, status:ok, result:'consulted'},
                                format(user_error, '[TRACE] [prolog_json_server] consult as query: consult+make succeeded for id=~w~n', [Id])
                            )
                        )
                    ;   (   get_dict(time_limit, Dict, TimeLimit0), number(TimeLimit0)
                        ->  TimeLimit = TimeLimit0
                        ;   TimeLimit = 5
                        ),
                        % Check for streaming parameters
                        (   get_dict(streaming, Dict, true)
                        ->  get_dict(max_results_per_chunk, Dict, MaxChunk, 50),
                            format(user_error, '[prolog_json_server] Streaming enabled with max_chunk=~w~n', [MaxChunk])
                        ;   MaxChunk = 0
                        ),
                        (   term_variables(Goal, Vars), Vars \= []
                        ->  register_query(Id, running),
                            run_query_in_thread_findall_streaming(Vars, Goal, TimeLimit, MaxChunk, Id, ThreadResult),
                            (   ThreadResult = time_limit_exceeded
                            ->  DictOut = _{id:Id, status:error, error:'time_limit_exceeded'}
                            ;   ThreadResult = error(CallErr)
                            ->  reply_error('call_failed', CallErr, Dict, DictOut)
                            ;   ThreadResult = ok(Results, StreamingInfo)
                            ->  (   StreamingInfo = no_streaming
                                ->  DictOut = _{id:Id, status:ok, results:Results}
                                ;   StreamingInfo = streaming(TotalCount, ChunkSize, IsLarge)
                                ->  DictOut = _{id:Id, status:ok, results:Results,
                                              streaming_info:_{total_count:TotalCount, chunk_size:ChunkSize,
                                                             is_large_result:IsLarge, has_more:(TotalCount > ChunkSize)}}
                                )
                            ;   DictOut = _{id:Id, status:error, error:'goal_failed'}
                            )
                        ;   with_output_to(string(Output),
                                run_query_in_thread_det(Goal, TimeLimit, ThreadResult)
                            ),
                            (   ThreadResult = time_limit_exceeded
                            ->  DictOut = _{id:Id, status:error, error:'time_limit_exceeded'}
                            ;   ThreadResult = error(CallErr)
                            ->  reply_error('call_failed', CallErr, Dict, DictOut)
                            ;   ThreadResult = ok
                            ->  DictOut = _{id:Id, status:ok, output:Output}
                            ;   DictOut = _{id:Id, status:error, error:'goal_failed'}
                            )
                        )
                    )
                )
            ;   reply_error('missing_goal', 'Query request missing goal', Dict, DictOut)
            )
        ; CmdAtom = consult ->
            (   _{file:File} :< Dict
            ->  (   catch(user:consult(File), Err, (
                        format(user_error, '[prolog_json_server] Consult failed: ~w~n', [Err]),
                        reply_error('consult_failed', Err, Dict, DictOut),
                        fail
                    ))
                ->  DictOut = _{id:Id, status:ok, result:'consulted'}
                )
            ;   reply_error('missing_file', 'Consult request missing file', Dict, DictOut)
            )
        ; CmdAtom = help ->
            (   _{predicate:Pred} :< Dict
            ->  format(user_error, '[prolog_json_server] handle_cmd: help predicate=~w~n', [Pred]),
                catch(
                    (
                        format(user_error, '[prolog_json_server] [DEBUG] About to call get_predicate_doc(~w, Doc)~n', [Pred]),
                        get_predicate_doc(Pred, Doc),
                        format(user_error, '[prolog_json_server] [DEBUG] get_predicate_doc returned: ~w~n', [Doc]),
                        DictOut = _{id:Id, status:ok, doc:Doc}
                    ),
                    Err,
                    (
                        format(user_error, '[prolog_json_server] [DEBUG] get_predicate_doc threw: ~w~n', [Err]),
                        DictOut = _{id:Id, status:error, error:Err}
                    )
                )
            ;   reply_error('missing_predicate', 'Help request missing predicate', Dict, DictOut)
            )
        ; CmdAtom = n3_load ->
            handle_n3_load(Dict, Id, DictOut)
        ; CmdAtom = n3_list ->
            handle_n3_list(Dict, Id, DictOut)
        ; CmdAtom = n3_reason ->
            handle_n3_reason(Dict, Id, DictOut)
        ; CmdAtom = n3_explain ->
            handle_n3_explain(Dict, Id, DictOut)
        ; CmdAtom = session_create ->
            handle_session_create(Dict, Id, DictOut)
        ; CmdAtom = session_switch ->
            handle_session_switch(Dict, Id, DictOut)
        ; CmdAtom = session_delete ->
            handle_session_delete(Dict, Id, DictOut)
        ; CmdAtom = session_list ->
            handle_session_list(Dict, Id, DictOut)
        ; CmdAtom = session_current ->
            handle_session_current(Dict, Id, DictOut)
        ; CmdAtom = session_save_state ->
            handle_session_save_state(Dict, Id, DictOut)
        ; CmdAtom = session_restore_state ->
            handle_session_restore_state(Dict, Id, DictOut)
        ; CmdAtom = session_get_state ->
            handle_session_get_state(Dict, Id, DictOut)
        ; CmdAtom = session_export ->
            handle_session_export(Dict, Id, DictOut)
        ; CmdAtom = session_import ->
            handle_session_import(Dict, Id, DictOut)
        ; CmdAtom = clp_solve ->
            handle_clp_solve(Dict, Id, DictOut)
        ; CmdAtom = clp_constraint ->
            handle_clp_constraint(Dict, Id, DictOut)
        ; CmdAtom = probabilistic_query ->
            handle_probabilistic_query(Dict, Id, DictOut)
        ; CmdAtom = probabilistic_fact ->
            handle_probabilistic_fact(Dict, Id, DictOut)
        ; CmdAtom = logic_module_register ->
            handle_logic_module_register(Dict, Id, DictOut)
        ; CmdAtom = logic_module_query ->
            handle_logic_module_query(Dict, Id, DictOut)
        ; CmdAtom = logic_module_list ->
            handle_logic_module_list(Dict, Id, DictOut)
        ;   DictOut = _{id:Id, status:error, error:'unknown_command'}
        )
    ).

% Reply with error dictionary
reply_error(Code, Msg, _Dict, DictOut) :-
    format(user_error, '[prolog_json_server] Error ~w: ~w~n', [Code, Msg]),
    DictOut = _{status:error, error:Code, message:Msg}.

% Get predicate documentation
get_predicate_doc(Pred, Doc) :-
    format(user_error, '[prolog_json_server] [DEBUG] get_predicate_doc called with Pred=~w~n', [Pred]),
    ( atom(Pred) -> atom_string(Pred, PredStr) ; PredStr = Pred ),
    format(user_error, '[prolog_json_server] [DEBUG] PredStr=~w~n', [PredStr]),
    ( sub_atom(PredStr, _, _, _, '/') ->
        split_string(PredStr, "/", "", [NameStr, ArityStr]),
        atom_string(Name, NameStr),
        number_string(Arity, ArityStr),
        functor(Head, Name, Arity)
    ;   atom_string(Name, PredStr),
        current_predicate(Name/Arity),
        functor(Head, Name, Arity)
    ),
    format(user_error, '[prolog_json_server] [DEBUG] Head=~w~n', [Head]),
    ( predicate_property(Head, imported_from(Module)) -> true
    ; predicate_property(Head, module(Module)) -> true
    ; Module = user ),
    format(user_error, '[prolog_json_server] [DEBUG] Module=~w~n', [Module]),
    ( predicate_property(Head, built_in) -> IsBuiltin = true ; IsBuiltin = false ),
    format(user_error, '[prolog_json_server] [DEBUG] IsBuiltin=~w~n', [IsBuiltin]),
    ( doc_comment(Head, Summary, Args, Examples) ->
        format(user_error, '[prolog_json_server] doc_comment found: Summary=~w Args=~w Examples=~w~n', [Summary, Args, Examples])
    ;   format(user_error, '[prolog_json_server] doc_comment NOT found for ~w~n', [Head]),
        Summary = 'No documentation found.',
        Args = [],
        Examples = []
    ),
    Doc = _{name:Name, arity:Arity, module:Module, is_builtin:IsBuiltin, summary:Summary, args:Args, examples:Examples}.

% Extract documentation using PlDoc
doc_comment(Head, Summary, Args, Examples) :-
    (   predicate_property(Head, foreign)
    ->  Summary = 'Foreign (C) predicate.',
        Args = [],
        Examples = []
    ;   (   doc_comment_from_pldoc(Head, Summary, Args, Examples)
        ->  true
        ;   Summary = 'No documentation found.',
            Args = [],
            Examples = []
        )
    ).

% Try PlDoc extraction
doc_comment_from_pldoc(Head, Summary, Args, Examples) :-
    catch(
        (
            format(user_error, '[prolog_json_server] [DEBUG] Attempting to load PlDoc...~n', []),
            use_module(library(pldoc)),
            format(user_error, '[prolog_json_server] [DEBUG] PlDoc loaded.~n', []),
            predicate_property(Head, visible),
            (   pldoc:doc_comment(Head, _, Summary0, Comments)
            ->  format(user_error, '[prolog_json_server] [DEBUG] pldoc:doc_comment found: Summary=~w, Comments=~w~n', [Summary0, Comments]),
                Summary = Summary0,
                findall(_{name:ArgName, description:Description}, member(arg(ArgName, Description), Comments), Args),
                findall(Example, member(example(Example), Comments), Examples)
            ;   format(user_error, '[prolog_json_server] [DEBUG] PlDoc doc_comment/4 not found, returning fallback doc.~n', []),
                Summary = 'No documentation found (PlDoc missing).',
                Args = [],
                Examples = []
            )
        ),
        Err,
        (
            format(user_error, '[prolog_json_server] doc_comment_from_pldoc: ERROR ~w~n', [Err]),
            Summary = 'No documentation found (PlDoc error).',
            Args = [],
            Examples = []
        )
    ).

% Threaded query execution for queries with variables
run_query_in_thread_findall(Vars, Goal, TimeLimit, Result) :-
    thread_self(Self),
    format(user_error, '[thread] Creating findall worker/supervisor for ~w~n', [Goal]),
    thread_create(run_query_worker_findall(Vars, Goal, Self), WorkerId, [detached(false)]),
    thread_create(run_query_supervisor(WorkerId, Self, TimeLimit), SupervisorId, [detached(false)]),
    format(user_error, '[thread] Waiting for message from worker or supervisor~n', []),
    thread_get_message(ResultMsg),
    format(user_error, '[thread] Received message: ~w~n', [ResultMsg]),
    thread_join(WorkerId, _),
    thread_join(SupervisorId, _),
    Result = ResultMsg.

% Query tracking predicates
register_query(Id, Status) :-
    get_time(StartTime),
    assertz(query_status(Id, Status, StartTime, 0)),
    broadcast_query_status(Id, Status, 0).

update_query_status(Id, Status, Progress) :-
    retract(query_status(Id, _, StartTime, _)),
    assertz(query_status(Id, Status, StartTime, Progress)),
    broadcast_query_status(Id, Status, Progress).

broadcast_query_status(Id, Status, Progress) :-
    forall(websocket_connection(_, WS),
           (   catch(
                   ws_send(WS, json(_{type: query_status_update,
                                    query_id: Id,
                                    status: Status,
                                    progress: Progress})),
                   Error,
                   (format(user_error, '[websocket] Failed to send status update: ~w~n', [Error]),
                    retract(websocket_connection(_, WS)))
               )
           )).

% Threaded query execution with streaming support and progress tracking
run_query_in_thread_findall_streaming(Vars, Goal, TimeLimit, MaxChunk, Result) :-
    run_query_in_thread_findall_streaming(Vars, Goal, TimeLimit, MaxChunk, unknown, Result).

run_query_in_thread_findall_streaming(Vars, Goal, TimeLimit, MaxChunk, QueryId, Result) :-
    thread_self(Self),
    format(user_error, '[thread] Creating streaming findall worker/supervisor for ~w (max_chunk=~w)~n', [Goal, MaxChunk]),
    (   QueryId \= unknown -> register_query(QueryId, running) ; true),
    thread_create(run_query_worker_findall_streaming(Vars, Goal, Self, MaxChunk, QueryId), WorkerId, [detached(false)]),
    thread_create(run_query_supervisor(WorkerId, Self, TimeLimit, QueryId), SupervisorId, [detached(false)]),
    format(user_error, '[thread] Waiting for message from worker or supervisor~n', []),
    thread_get_message(ResultMsg),
    format(user_error, '[thread] Received message: ~w~n', [ResultMsg]),
    thread_join(WorkerId, _),
    thread_join(SupervisorId, _),
    (   QueryId \= unknown, ResultMsg = ok(_, _) -> update_query_status(QueryId, completed, 100) ; true),
    (   QueryId \= unknown, ResultMsg = error(_) -> update_query_status(QueryId, error, 0) ; true),
    Result = ResultMsg.

run_query_worker_findall_streaming(Vars, Goal, Parent, MaxChunk) :-
    run_query_worker_findall_streaming(Vars, Goal, Parent, MaxChunk, unknown).

run_query_worker_findall_streaming(Vars, Goal, Parent, MaxChunk, QueryId) :-
    (   catch(findall(Vars, call(Goal), AllResults), E, (
            format(user_error, '[thread] Streaming worker error: ~w~n', [E]),
            (QueryId \= unknown -> update_query_status(QueryId, error, 0) ; true),
            thread_send_message(Parent, error(E)),
            fail
        ))
    ->  length(AllResults, TotalCount),
        format(user_error, '[thread] Streaming worker found ~w results~n', [TotalCount]),
        (QueryId \= unknown -> update_query_status(QueryId, running, 75) ; true),
        (   MaxChunk > 0, TotalCount > MaxChunk
        ->  % Large result set - return first chunk with streaming info
            length(FirstChunk, MaxChunk),
            append(FirstChunk, _, AllResults),
            format(user_error, '[thread] Streaming: returning first ~w of ~w results~n', [MaxChunk, TotalCount]),
            (QueryId \= unknown -> update_query_status(QueryId, running, 90) ; true),
            thread_send_message(Parent, ok(FirstChunk, streaming(TotalCount, MaxChunk, true)))
        ;   % Small result set - return all results
            format(user_error, '[thread] Streaming: returning all ~w results (no chunking needed)~n', [TotalCount]),
            (QueryId \= unknown -> update_query_status(QueryId, running, 90) ; true),
            thread_send_message(Parent, ok(AllResults, no_streaming))
        )
    ;   (QueryId \= unknown -> update_query_status(QueryId, error, 0) ; true),
        thread_send_message(Parent, error('goal_failed'))
    ).

run_query_worker_findall(Vars, Goal, Parent) :-
    (   catch(findall(Vars, call(Goal), Results), E, (
            format(user_error, '[thread] Worker error: ~w~n', [E]),
            thread_send_message(Parent, error(E)),
            fail
        ))
    ->  thread_send_message(Parent, ok(Results))
    ;   thread_send_message(Parent, error('goal_failed'))
    ).

% Threaded query execution for deterministic goals
run_query_in_thread_det(Goal, TimeLimit, Result) :-
    thread_self(Self),
    format(user_error, '[thread] Creating det worker/supervisor for ~w~n', [Goal]),
    thread_create(run_query_worker_det(Goal, Self), WorkerId, [detached(false)]),
    thread_create(run_query_supervisor(WorkerId, Self, TimeLimit), SupervisorId, [detached(false)]),
    format(user_error, '[thread] Waiting for message from worker or supervisor~n', []),
    thread_get_message(ResultMsg),
    format(user_error, '[thread] Received message: ~w~n', [ResultMsg]),
    thread_join(WorkerId, _),
    thread_join(SupervisorId, _),
    Result = ResultMsg.

run_query_worker_det(Goal, Parent) :-
    (   catch(call(Goal), E, (
            format(user_error, '[thread] Worker error: ~w~n', [E]),
            thread_send_message(Parent, error(E)),
            fail
        ))
    ->  thread_send_message(Parent, ok)
    ;   thread_send_message(Parent, error('goal_failed'))
    ).

% Supervisor for time limits with query tracking
run_query_supervisor(WorkerId, Parent, TimeLimit) :-
    run_query_supervisor(WorkerId, Parent, TimeLimit, unknown).

run_query_supervisor(WorkerId, Parent, TimeLimit, QueryId) :-
    sleep(TimeLimit),
    (   thread_property(WorkerId, status(running))
    ->  thread_signal(WorkerId, throw('$aborted')),
        format(user_error, '[thread] Supervisor: Time limit exceeded, aborting worker ~w~n', [WorkerId]),
        (QueryId \= unknown -> update_query_status(QueryId, timeout, 0) ; true),
        thread_send_message(Parent, time_limit_exceeded)
    ;   true
    ).

% WebSocket handler for real-time notifications
websocket_handler(Request) :-
    http_upgrade_to_websocket(Request, websocket_loop, []).

websocket_loop(WebSocket) :-
    gensym(ws_, WSId),
    assertz(websocket_connection(WSId, WebSocket)),
    format(user_error, '[websocket] Client ~w connected~n', [WSId]),
    
    % Send current query statuses to new client
    findall(_{query_id: Id, status: Status, progress: Progress},
            query_status(Id, Status, _, Progress),
            CurrentQueries),
    (   CurrentQueries \= []
    ->  ws_send(WebSocket, json(_{type: query_status_batch, queries: CurrentQueries}))
    ;   true
    ),
    
    % Handle incoming messages
    repeat,
    (   ws_receive(WebSocket, Message, [format(json)])
    ->  (   handle_websocket_message(Message, WebSocket, WSId)
        ->  fail  % Continue loop
        ;   true  % Exit loop
        )
    ;   % Connection closed
        retract(websocket_connection(WSId, WebSocket)),
        format(user_error, '[websocket] Client ~w disconnected~n', [WSId]),
        true
    ).

% Handle WebSocket messages
handle_websocket_message(json(Message), WebSocket, _WSId) :-
    (   Message.get(type) = cancel_query,
        Message.get(query_id) = QueryId
    ->  % Handle query cancellation
        (   query_status(QueryId, Status, _, _),
            memberchk(Status, [running, pending])
        ->  update_query_status(QueryId, cancelled, 0),
            ws_send(WebSocket, json(_{type: query_cancelled, query_id: QueryId}))
        ;   ws_send(WebSocket, json(_{type: error, message: 'Query not found or already completed'}))
        )
    ;   Message.get(type) = get_query_status,
        Message.get(query_id) = QueryId
    ->  % Send specific query status
        (   query_status(QueryId, Status, StartTime, Progress)
        ->  ws_send(WebSocket, json(_{type: query_status,
                                    query_id: QueryId,
                                    status: Status,
                                    start_time: StartTime,
                                    progress: Progress}))
        ;   ws_send(WebSocket, json(_{type: error, message: 'Query not found'}))
        )
    ;   % Unknown message type
        ws_send(WebSocket, json(_{type: error, message: 'Unknown message type'}))
    ).

% N3 Command Handlers

% Handle N3 load command
handle_n3_load(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_n3_load called~n', []),
    (   _{file:File} :< Dict
    ->  catch(
            (
                validate_n3_file(File),
                rdf_load(File, [format(turtle), graph(n3_graph)]),
                rdf_statistics(triples(Count)),
                format(user_error, '[prolog_json_server] N3 file loaded: ~w triples~n', [Count]),
                DictOut = _{id:Id, status:ok, result:'loaded', triples_count:Count}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] N3 load failed: ~w~n', [Err]),
                reply_error('n3_load_failed', Err, Dict, DictOut)
            )
        )
    ;   (   _{content:Content} :< Dict
        ->  catch(
                (
                    validate_n3_content(Content),
                    atom_string(ContentAtom, Content),
                    rdf_load_turtle_string(ContentAtom, Triples, []),
                    maplist(assert_triple, Triples),
                    length(Triples, Count),
                    format(user_error, '[prolog_json_server] N3 content loaded: ~w triples~n', [Count]),
                    DictOut = _{id:Id, status:ok, result:'loaded', triples_count:Count}
                ),
                Err,
                (
                    format(user_error, '[prolog_json_server] N3 content load failed: ~w~n', [Err]),
                    reply_error('n3_content_load_failed', Err, Dict, DictOut)
                )
            )
        ;   reply_error('missing_file_or_content', 'N3 load request missing file or content', Dict, DictOut)
        )
    ).

% Handle N3 list command with pagination support
handle_n3_list(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_n3_list called~n', []),
    catch(
        (
            get_dict(limit, Dict, Limit, 100),
            get_dict(offset, Dict, Offset, 0),
            get_dict(format, Dict, Format, 'readable'),
            get_dict(streaming, Dict, Streaming, false),
            
            findall(Triple, (rdf(S, P, O, n3_graph), Triple = _{subject:S, predicate:P, object:O}), AllTriples),
            length(AllTriples, TotalCount),
            
            % Apply pagination
            (   Offset > 0
            ->  length(SkipList, Offset),
                append(SkipList, RestTriples, AllTriples)
            ;   RestTriples = AllTriples
            ),
            
            (   Limit > 0, length(RestTriples, RestCount), RestCount > Limit
            ->  length(LimitedTriples, Limit),
                append(LimitedTriples, _, RestTriples),
                HasMore = true
            ;   LimitedTriples = RestTriples,
                HasMore = false
            ),
            
            length(LimitedTriples, ReturnedCount),
            
            (   Format = 'readable'
            ->  maplist(format_triple_readable, LimitedTriples, FormattedTriples)
            ;   FormattedTriples = LimitedTriples
            ),
            
            % Add pagination info if streaming or large result set
            (   (Streaming = true; TotalCount > 100)
            ->  PaginationInfo = _{
                    total_count: TotalCount,
                    returned_count: ReturnedCount,
                    offset: Offset,
                    limit: Limit,
                    has_more: HasMore,
                    next_offset: (HasMore -> Offset + Limit; null)
                },
                DictOut = _{id:Id, status:ok, triples:FormattedTriples, pagination:PaginationInfo}
            ;   DictOut = _{id:Id, status:ok, triples:FormattedTriples, total_count:TotalCount, returned_count:ReturnedCount}
            )
        ),
        Err,
        (
            format(user_error, '[prolog_json_server] N3 list failed: ~w~n', [Err]),
            reply_error('n3_list_failed', Err, Dict, DictOut)
        )
    ).

% Handle N3 reason command
handle_n3_reason(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_n3_reason called~n', []),
    catch(
        (
            get_dict(goal, Dict, GoalStr, ''),
            (   GoalStr \= ''
            ->  term_string(Goal, GoalStr),
                findall(Result, n3_reason_goal(Goal, Result), Results),
                length(Results, Count),
                format(user_error, '[prolog_json_server] N3 reasoning found ~w results~n', [Count]),
                DictOut = _{id:Id, status:ok, results:Results, count:Count}
            ;   % General reasoning - find all inferred triples
                findall(Triple, n3_infer_triple(Triple), InferredTriples),
                length(InferredTriples, Count),
                maplist(format_triple_readable, InferredTriples, FormattedTriples),
                format(user_error, '[prolog_json_server] N3 reasoning inferred ~w triples~n', [Count]),
                DictOut = _{id:Id, status:ok, inferred_triples:FormattedTriples, count:Count}
            )
        ),
        Err,
        (
            format(user_error, '[prolog_json_server] N3 reason failed: ~w~n', [Err]),
            reply_error('n3_reason_failed', Err, Dict, DictOut)
        )
    ).

% Handle N3 explain command
handle_n3_explain(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_n3_explain called~n', []),
    (   _{goal:GoalStr} :< Dict
    ->  catch(
            (
                term_string(Goal, GoalStr),
                n3_explain_goal(Goal, ProofTree),
                format_proof_tree(ProofTree, FormattedProof),
                format(user_error, '[prolog_json_server] N3 explanation generated~n', []),
                DictOut = _{id:Id, status:ok, proof:FormattedProof, goal:GoalStr}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] N3 explain failed: ~w~n', [Err]),
                reply_error('n3_explain_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_goal', 'N3 explain request missing goal', Dict, DictOut)
    ).

% N3 Reasoning Implementation

% Validate N3 file
validate_n3_file(File) :-
    (   atom(File) -> true ; string(File) -> true ; throw('invalid_file_type')),
    (   exists_file(File) -> true ; throw('file_not_found')),
    file_name_extension(_, Ext, File),
    (   memberchk(Ext, [n3, ttl, turtle]) -> true ; throw('invalid_file_extension')).

% Validate N3 content
validate_n3_content(Content) :-
    (   string(Content) -> true ; atom(Content) -> true ; throw('invalid_content_type')),
    atom_length(Content, Len),
    (   Len > 0 -> true ; throw('empty_content')),
    (   Len < 1000000 -> true ; throw('content_too_large')).

% Assert a triple into the RDF database
assert_triple(rdf(S, P, O)) :-
    rdf_assert(S, P, O, n3_graph).

% Load turtle string and parse triples
rdf_load_turtle_string(Content, Triples, _Options) :-
    setup_call_cleanup(
        open_string(Content, Stream),
        rdf_read_turtle(Stream, Triples, []),
        close(Stream)
    ).

% Format triple for readable output
format_triple_readable(_{subject:S, predicate:P, object:O}, _{subject:SFormatted, predicate:PFormatted, object:OFormatted}) :-
    format_uri_readable(S, SFormatted),
    format_uri_readable(P, PFormatted),
    format_uri_readable(O, OFormatted).

% Format URI with readable prefixes
format_uri_readable(URI, Formatted) :-
    (   atom(URI)
    ->  (   atom_concat('http://example.org/', Local, URI)
        ->  atom_concat(':', Local, Formatted)
        ;   atom_concat('http://www.w3.org/1999/02/22-rdf-syntax-ns#', Local, URI)
        ->  atom_concat('rdf:', Local, Formatted)
        ;   atom_concat('http://www.w3.org/2000/01/rdf-schema#', Local, URI)
        ->  atom_concat('rdfs:', Local, Formatted)
        ;   Formatted = URI
        )
    ;   Formatted = URI
    ).

% N3 reasoning goal
n3_reason_goal(Goal, Result) :-
    call(Goal),
    term_variables(Goal, Vars),
    (   Vars = []
    ->  Result = true
    ;   copy_term(Goal-Vars, Result-Vars)
    ).

% Infer triples using N3 rules
n3_infer_triple(_{subject:S, predicate:P, object:O}) :-
    % Basic RDFS inference
    (   rdf(S, P, O, n3_graph)
    ;   % Subclass inference: if X subClassOf Y and A type X, then A type Y
        rdf(S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Class, n3_graph),
        rdf(Class, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', SuperClass, n3_graph),
        P = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        O = SuperClass
    ;   % N3 rule inference: {?x a :Person} => {?x a :Mortal}
        rdf(S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://example.org/Person', n3_graph),
        P = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        O = 'http://example.org/Mortal'
    ).

% N3 explanation with proof tree
n3_explain_goal(Goal, ProofTree) :-
    n3_meta_interpreter(Goal, ProofTree).

% Meta-interpreter for proof tracing
n3_meta_interpreter(Goal, proof(Goal, SubProofs)) :-
    (   Goal = (A, B)
    ->  n3_meta_interpreter(A, ProofA),
        n3_meta_interpreter(B, ProofB),
        SubProofs = [ProofA, ProofB]
    ;   Goal = rdf(S, P, O)
    ->  (   rdf(S, P, O, n3_graph)
        ->  SubProofs = [fact(rdf(S, P, O))]
        ;   n3_infer_triple(_{subject:S, predicate:P, object:O})
        ->  SubProofs = [inference(rdf(S, P, O))]
        ;   fail
        )
    ;   call(Goal)
    ->  SubProofs = [builtin(Goal)]
    ;   fail
    ).

% Format proof tree for display
format_proof_tree(proof(Goal, SubProofs), _{goal:GoalStr, type:proof, subproofs:FormattedSubProofs}) :-
    term_string(Goal, GoalStr),
    maplist(format_proof_tree, SubProofs, FormattedSubProofs).
format_proof_tree(fact(Fact), _{fact:FactStr, type:fact}) :-
    term_string(Fact, FactStr).
format_proof_tree(inference(Inference), _{inference:InferenceStr, type:inference}) :-
    term_string(Inference, InferenceStr).
format_proof_tree(builtin(Builtin), _{builtin:BuiltinStr, type:builtin}) :-
    term_string(Builtin, BuiltinStr).

% Session Management Command Handlers

% Handle session create command
handle_session_create(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_session_create called~n', []),
    (   _{name:Name} :< Dict
    ->  catch(
            (
                create_session(Name, SessionId),
                DictOut = _{id:Id, status:ok, session_id:SessionId, name:Name}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Session create failed: ~w~n', [Err]),
                reply_error('session_create_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_name', 'Session create request missing name', Dict, DictOut)
    ).

% Handle session switch command
handle_session_switch(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_session_switch called~n', []),
    (   _{session_id:SessionId} :< Dict
    ->  catch(
            (
                switch_session(SessionId),
                DictOut = _{id:Id, status:ok, session_id:SessionId, message:'Session switched successfully'}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Session switch failed: ~w~n', [Err]),
                reply_error('session_switch_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_session_id', 'Session switch request missing session_id', Dict, DictOut)
    ).

% Handle session delete command
handle_session_delete(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_session_delete called~n', []),
    (   _{session_id:SessionId} :< Dict
    ->  catch(
            (
                delete_session(SessionId),
                DictOut = _{id:Id, status:ok, session_id:SessionId, message:'Session deleted successfully'}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Session delete failed: ~w~n', [Err]),
                reply_error('session_delete_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_session_id', 'Session delete request missing session_id', Dict, DictOut)
    ).

% Handle session list command
handle_session_list(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_session_list called~n', []),
    catch(
        (
            list_sessions(Sessions),
            length(Sessions, Count),
            DictOut = _{id:Id, status:ok, sessions:Sessions, count:Count}
        ),
        Err,
        (
            format(user_error, '[prolog_json_server] Session list failed: ~w~n', [Err]),
            reply_error('session_list_failed', Err, Dict, DictOut)
        )
    ).

% Handle session current command
handle_session_current(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_session_current called~n', []),
    catch(
        (
            (   current_session(SessionId)
            ->  DictOut = _{id:Id, status:ok, current_session:SessionId}
            ;   DictOut = _{id:Id, status:ok, current_session:null, message:'No active session'}
            )
        ),
        Err,
        (
            format(user_error, '[prolog_json_server] Session current failed: ~w~n', [Err]),
            reply_error('session_current_failed', Err, Dict, DictOut)
        )
    ).

% Handle session save state command
handle_session_save_state(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_session_save_state called~n', []),
    (   _{session_id:SessionId} :< Dict
    ->  catch(
            (
                save_session_state(SessionId),
                get_session_state(SessionId, StateData),
                DictOut = _{id:Id, status:ok, session_id:SessionId, state:StateData, message:'Session state saved successfully'}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Session save state failed: ~w~n', [Err]),
                reply_error('session_save_state_failed', Err, Dict, DictOut)
            )
        )
    ;   % Save current session if no session_id provided
        catch(
            (
                (   current_session(CurrentSessionId)
                ->  save_session_state(CurrentSessionId),
                    get_session_state(CurrentSessionId, StateData),
                    DictOut = _{id:Id, status:ok, session_id:CurrentSessionId, state:StateData, message:'Current session state saved successfully'}
                ;   reply_error('no_active_session', 'No active session to save', Dict, DictOut)
                )
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Session save state failed: ~w~n', [Err]),
                reply_error('session_save_state_failed', Err, Dict, DictOut)
            )
        )
    ).

% Handle session restore state command
handle_session_restore_state(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_session_restore_state called~n', []),
    (   _{session_id:SessionId} :< Dict
    ->  catch(
            (
                restore_session_state(SessionId),
                get_session_state(SessionId, StateData),
                DictOut = _{id:Id, status:ok, session_id:SessionId, state:StateData, message:'Session state restored successfully'}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Session restore state failed: ~w~n', [Err]),
                reply_error('session_restore_state_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_session_id', 'Session restore state request missing session_id', Dict, DictOut)
    ).

% Handle session get state command
handle_session_get_state(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_session_get_state called~n', []),
    (   _{session_id:SessionId} :< Dict
    ->  catch(
            (
                get_session_state(SessionId, StateData),
                DictOut = _{id:Id, status:ok, session_id:SessionId, state:StateData}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Session get state failed: ~w~n', [Err]),
                reply_error('session_get_state_failed', Err, Dict, DictOut)
            )
        )
    ;   % Get current session state if no session_id provided
        catch(
            (
                (   current_session(CurrentSessionId)
                ->  get_session_state(CurrentSessionId, StateData),
                    DictOut = _{id:Id, status:ok, session_id:CurrentSessionId, state:StateData}
                ;   reply_error('no_active_session', 'No active session', Dict, DictOut)
                )
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Session get state failed: ~w~n', [Err]),
                reply_error('session_get_state_failed', Err, Dict, DictOut)
            )
        )
    ).

% Handle session export command
handle_session_export(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_session_export called~n', []),
    (   _{session_id:SessionId, file_path:FilePath} :< Dict
    ->  catch(
            (
                export_session_state(SessionId, FilePath),
                DictOut = _{id:Id, status:ok, session_id:SessionId, file_path:FilePath, message:'Session exported successfully'}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Session export failed: ~w~n', [Err]),
                reply_error('session_export_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_parameters', 'Session export request missing session_id or file_path', Dict, DictOut)
    ).

% Handle session import command
handle_session_import(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_session_import called~n', []),
    (   _{session_id:SessionId, file_path:FilePath} :< Dict
    ->  catch(
            (
                import_session_state(SessionId, FilePath),
                get_session_state(SessionId, StateData),
                DictOut = _{id:Id, status:ok, session_id:SessionId, file_path:FilePath, state:StateData, message:'Session imported successfully'}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Session import failed: ~w~n', [Err]),
                reply_error('session_import_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_parameters', 'Session import request missing session_id or file_path', Dict, DictOut)
    ).

% Enhanced Reasoning Features Implementation

% CLP (Constraint Logic Programming) Command Handlers

% Handle CLP solve command
handle_clp_solve(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_clp_solve called~n', []),
    (   _{domain:Domain, variables:Variables, constraints:Constraints} :< Dict
    ->  catch(
            (
                validate_clp_domain(Domain),
                parse_clp_variables(Variables, VarList),
                parse_clp_constraints(Domain, Constraints, ConstraintList),
                solve_clp_problem(Domain, VarList, ConstraintList, Solution),
                format(user_error, '[prolog_json_server] CLP solution found~n', []),
                DictOut = _{id:Id, status:ok, domain:Domain, solution:Solution}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] CLP solve failed: ~w~n', [Err]),
                reply_error('clp_solve_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_parameters', 'CLP solve request missing domain, variables, or constraints', Dict, DictOut)
    ).

% Handle CLP constraint command
handle_clp_constraint(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_clp_constraint called~n', []),
    (   _{domain:Domain, constraint:Constraint} :< Dict
    ->  catch(
            (
                validate_clp_domain(Domain),
                parse_clp_constraint(Domain, Constraint, ParsedConstraint),
                assertz(clp_constraint(Domain, ParsedConstraint)),
                format(user_error, '[prolog_json_server] CLP constraint added~n', []),
                DictOut = _{id:Id, status:ok, domain:Domain, constraint:Constraint, message:'Constraint added successfully'}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] CLP constraint failed: ~w~n', [Err]),
                reply_error('clp_constraint_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_parameters', 'CLP constraint request missing domain or constraint', Dict, DictOut)
    ).

% Probabilistic Logic Command Handlers

% Handle probabilistic query command
handle_probabilistic_query(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_probabilistic_query called~n', []),
    (   _{goal:GoalStr} :< Dict
    ->  catch(
            (
                term_string(Goal, GoalStr),
                get_dict(samples, Dict, Samples, 1000),
                get_dict(method, Dict, Method, 'monte_carlo'),
                probabilistic_inference(Goal, Method, Samples, Probability, Evidence),
                format(user_error, '[prolog_json_server] Probabilistic query completed~n', []),
                DictOut = _{id:Id, status:ok, goal:GoalStr, probability:Probability,
                          evidence:Evidence, method:Method, samples:Samples}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Probabilistic query failed: ~w~n', [Err]),
                reply_error('probabilistic_query_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_goal', 'Probabilistic query request missing goal', Dict, DictOut)
    ).

% Handle probabilistic fact command
handle_probabilistic_fact(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_probabilistic_fact called~n', []),
    (   _{fact:FactStr, probability:Probability} :< Dict
    ->  catch(
            (
                term_string(Fact, FactStr),
                validate_probability(Probability),
                assertz(probabilistic_fact(Fact, Probability)),
                format(user_error, '[prolog_json_server] Probabilistic fact added~n', []),
                DictOut = _{id:Id, status:ok, fact:FactStr, probability:Probability, message:'Probabilistic fact added successfully'}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Probabilistic fact failed: ~w~n', [Err]),
                reply_error('probabilistic_fact_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_parameters', 'Probabilistic fact request missing fact or probability', Dict, DictOut)
    ).

% User-defined Logic Modules Command Handlers

% Handle logic module register command
handle_logic_module_register(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_logic_module_register called~n', []),
    (   _{name:Name, rules:Rules} :< Dict
    ->  catch(
            (
                validate_module_name(Name),
                parse_logic_rules(Rules, ParsedRules),
                get_dict(meta_interpreter, Dict, MetaInterpreter, 'default'),
                register_logic_module(Name, ParsedRules, MetaInterpreter),
                format(user_error, '[prolog_json_server] Logic module registered~n', []),
                DictOut = _{id:Id, status:ok, name:Name, rules_count:_,
                          meta_interpreter:MetaInterpreter, message:'Logic module registered successfully'}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Logic module register failed: ~w~n', [Err]),
                reply_error('logic_module_register_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_parameters', 'Logic module register request missing name or rules', Dict, DictOut)
    ).

% Handle logic module query command
handle_logic_module_query(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_logic_module_query called~n', []),
    (   _{module:ModuleName, goal:GoalStr} :< Dict
    ->  catch(
            (
                term_string(Goal, GoalStr),
                query_logic_module(ModuleName, Goal, Results, ProofTrace),
                length(Results, Count),
                format(user_error, '[prolog_json_server] Logic module query completed~n', []),
                DictOut = _{id:Id, status:ok, module:ModuleName, goal:GoalStr,
                          results:Results, count:Count, proof_trace:ProofTrace}
            ),
            Err,
            (
                format(user_error, '[prolog_json_server] Logic module query failed: ~w~n', [Err]),
                reply_error('logic_module_query_failed', Err, Dict, DictOut)
            )
        )
    ;   reply_error('missing_parameters', 'Logic module query request missing module or goal', Dict, DictOut)
    ).

% Handle logic module list command
handle_logic_module_list(Dict, Id, DictOut) :-
    format(user_error, '[prolog_json_server] handle_logic_module_list called~n', []),
    catch(
        (
            findall(_{name:Name, rules_count:RulesCount, meta_interpreter:MetaInterpreter},
                   (user_logic_module(Name, Rules, MetaInterpreter),
                    length(Rules, RulesCount)),
                   Modules),
            length(Modules, Count),
            format(user_error, '[prolog_json_server] Logic module list completed~n', []),
            DictOut = _{id:Id, status:ok, modules:Modules, count:Count}
        ),
        Err,
        (
            format(user_error, '[prolog_json_server] Logic module list failed: ~w~n', [Err]),
            reply_error('logic_module_list_failed', Err, Dict, DictOut)
        )
    ).

% CLP Implementation

% Validate CLP domain
validate_clp_domain(Domain) :-
    memberchk(Domain, [fd, r, q]),
    !.
validate_clp_domain(Domain) :-
    throw(invalid_clp_domain(Domain)).

% Parse CLP variables
parse_clp_variables(Variables, VarList) :-
    (   is_list(Variables)
    ->  maplist(parse_clp_variable, Variables, VarList)
    ;   throw(invalid_variables_format(Variables))
    ).

parse_clp_variable(VarSpec, Var) :-
    (   atom(VarSpec)
    ->  Var = VarSpec
    ;   string(VarSpec)
    ->  atom_string(Var, VarSpec)
    ;   throw(invalid_variable_spec(VarSpec))
    ).

% Parse CLP constraints
parse_clp_constraints(Domain, Constraints, ConstraintList) :-
    (   is_list(Constraints)
    ->  maplist(parse_clp_constraint(Domain), Constraints, ConstraintList)
    ;   parse_clp_constraint(Domain, Constraints, Constraint),
        ConstraintList = [Constraint]
    ).

parse_clp_constraint(Domain, ConstraintStr, Constraint) :-
    (   atom(ConstraintStr)
    ->  term_string(Constraint, ConstraintStr)
    ;   string(ConstraintStr)
    ->  term_string(Constraint, ConstraintStr)
    ;   Constraint = ConstraintStr
    ),
    validate_clp_constraint(Domain, Constraint).

validate_clp_constraint(fd, Constraint) :-
    clpfd_constraint(Constraint), !.
validate_clp_constraint(r, Constraint) :-
    clpr_constraint(Constraint), !.
validate_clp_constraint(q, Constraint) :-
    clpq_constraint(Constraint), !.
validate_clp_constraint(Domain, Constraint) :-
    throw(invalid_constraint_for_domain(Domain, Constraint)).

% Basic constraint validation (simplified)
clpfd_constraint(X #= Y) :- !.
clpfd_constraint(X #\= Y) :- !.
clpfd_constraint(X #< Y) :- !.
clpfd_constraint(X #> Y) :- !.
clpfd_constraint(X #=< Y) :- !.
clpfd_constraint(X #>= Y) :- !.
clpfd_constraint(X in Low..High) :- !.
clpfd_constraint(all_different(List)) :- is_list(List), !.

clpr_constraint(X =:= Y) :- !.
clpr_constraint(X =\= Y) :- !.
clpr_constraint(X < Y) :- !.
clpr_constraint(X > Y) :- !.
clpr_constraint(X =< Y) :- !.
clpr_constraint(X >= Y) :- !.

clpq_constraint(X =:= Y) :- !.
clpq_constraint(X =\= Y) :- !.
clpq_constraint(X < Y) :- !.
clpq_constraint(X > Y) :- !.
clpq_constraint(X =< Y) :- !.
clpq_constraint(X >= Y) :- !.

% Solve CLP problem
solve_clp_problem(fd, Variables, Constraints, Solution) :-
    maplist(call, Constraints),
    label(Variables),
    create_variable_solution(Variables, Solution).

solve_clp_problem(r, Variables, Constraints, Solution) :-
    maplist(call, Constraints),
    create_variable_solution(Variables, Solution).

solve_clp_problem(q, Variables, Constraints, Solution) :-
    maplist(call, Constraints),
    create_variable_solution(Variables, Solution).

create_variable_solution(Variables, Solution) :-
    maplist(create_var_binding, Variables, Solution).

create_var_binding(Var, _{variable:VarName, value:Value}) :-
    term_string(Var, VarName),
    (   var(Var)
    ->  Value = unbound
    ;   Value = Var
    ).

% Probabilistic Logic Implementation

% Validate probability
validate_probability(P) :-
    number(P),
    P >= 0.0,
    P =< 1.0,
    !.
validate_probability(P) :-
    throw(invalid_probability(P)).

% Probabilistic inference using Monte Carlo sampling
probabilistic_inference(Goal, monte_carlo, Samples, Probability, Evidence) :-
    monte_carlo_sampling(Goal, Samples, SuccessCount, TotalSamples),
    Probability is SuccessCount / TotalSamples,
    Evidence = _{success_count:SuccessCount, total_samples:TotalSamples}.

% Monte Carlo sampling
monte_carlo_sampling(Goal, Samples, SuccessCount, Samples) :-
    monte_carlo_loop(Goal, Samples, 0, SuccessCount).

monte_carlo_loop(_, 0, Acc, Acc) :- !.
monte_carlo_loop(Goal, N, Acc, Result) :-
    N > 0,
    (   sample_probabilistic_goal(Goal)
    ->  Acc1 is Acc + 1
    ;   Acc1 = Acc
    ),
    N1 is N - 1,
    monte_carlo_loop(Goal, N1, Acc1, Result).

% Sample a probabilistic goal
sample_probabilistic_goal(Goal) :-
    (   probabilistic_fact(Goal, Probability)
    ->  random(R),
        R =< Probability
    ;   call(Goal)  % Deterministic goal
    ).

% User-defined Logic Modules Implementation

% Validate module name
validate_module_name(Name) :-
    (   atom(Name) ; string(Name) ),
    !.
validate_module_name(Name) :-
    throw(invalid_module_name(Name)).

% Parse logic rules
parse_logic_rules(Rules, ParsedRules) :-
    (   is_list(Rules)
    ->  maplist(parse_logic_rule, Rules, ParsedRules)
    ;   parse_logic_rule(Rules, ParsedRule),
        ParsedRules = [ParsedRule]
    ).

parse_logic_rule(RuleStr, Rule) :-
    (   atom(RuleStr)
    ->  term_string(Rule, RuleStr)
    ;   string(RuleStr)
    ->  term_string(Rule, RuleStr)
    ;   Rule = RuleStr
    ),
    validate_logic_rule(Rule).

validate_logic_rule((Head :- Body)) :- !.
validate_logic_rule(Head) :- !.  % Fact

% Register logic module
register_logic_module(Name, Rules, MetaInterpreter) :-
    retractall(user_logic_module(Name, _, _)),
    assertz(user_logic_module(Name, Rules, MetaInterpreter)).

% Query logic module
query_logic_module(ModuleName, Goal, Results, ProofTrace) :-
    user_logic_module(ModuleName, Rules, MetaInterpreter),
    query_with_module_rules(Goal, Rules, MetaInterpreter, Results, ProofTrace).

query_with_module_rules(Goal, Rules, default, Results, ProofTrace) :-
    findall(Goal-Proof,
           (member(Rule, Rules),
            apply_rule_with_proof(Rule, Goal, Proof)),
           ResultsWithProof),
    maplist(extract_result, ResultsWithProof, Results),
    maplist(extract_proof, ResultsWithProof, ProofTrace).

query_with_module_rules(Goal, Rules, custom, Results, ProofTrace) :-
    custom_meta_interpreter(Goal, Rules, Results, ProofTrace).

extract_result(Result-_, Result).
extract_proof(_-Proof, Proof).

% Apply rule with proof tracking
apply_rule_with_proof((Head :- Body), Goal, proof(rule_application, Head, Body)) :-
    unify_with_occurs_check(Goal, Head),
    call(Body).
apply_rule_with_proof(Head, Goal, proof(fact, Head)) :-
    unify_with_occurs_check(Goal, Head).

% Custom meta-interpreter for user modules
custom_meta_interpreter(Goal, Rules, [Goal], [proof(custom_meta, Goal)]) :-
    member(Rule, Rules),
    apply_rule_with_proof(Rule, Goal, _).

% Initialize the server
:- initialization(main, main).