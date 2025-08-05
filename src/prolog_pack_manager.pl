% Prolog Pack Management Server
% Provides HTTP endpoints for SWI-Prolog pack management operations

:- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(prolog_pack)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(debug)).

% HTTP route handlers for pack management
:- http_handler(root(pack/list_available), handle_list_available, []).
:- http_handler(root(pack/list_installed), handle_list_installed, []).
:- http_handler(root(pack/install), handle_install, [method(post)]).
:- http_handler(root(pack/uninstall), handle_uninstall, [method(post)]).
:- http_handler(root(pack/update), handle_update, [method(post)]).
:- http_handler(root(pack/info), handle_pack_info, []).
:- http_handler(root(pack/search), handle_search, []).
:- http_handler(root(pack/outdated), handle_outdated, []).

%! handle_list_available(+Request) is det.
%
%  Handle request to list all available packs
handle_list_available(Request) :-
    cors_enable(Request, [methods([get, post, options])]),
    catch(
        (   list_available_packs(Packs),
            reply_json_dict(_{status: ok, packs: Packs})
        ),
        Error,
        (   format_error(Error, ErrorMsg),
            reply_json_dict(_{status: error, error: ErrorMsg})
        )
    ).

%! handle_list_installed(+Request) is det.
%
%  Handle request to list installed packs
handle_list_installed(Request) :-
    cors_enable(Request, [methods([get, post, options])]),
    catch(
        (   list_installed_packs(Packs),
            reply_json_dict(_{status: ok, packs: Packs})
        ),
        Error,
        (   format_error(Error, ErrorMsg),
            reply_json_dict(_{status: error, error: ErrorMsg})
        )
    ).

%! handle_install(+Request) is det.
%
%  Handle pack installation request
handle_install(Request) :-
    cors_enable(Request, [methods([get, post, options])]),
    http_read_json_dict(Request, Data),
    PackName = Data.get(pack_name),
    (   PackName == @(null) ->
        reply_json_dict(_{status: error, error: "Pack name is required"})
    ;   catch(
            (   install_pack_safe(PackName, Result),
                reply_json_dict(_{status: ok, result: Result})
            ),
            Error,
            (   format_error(Error, ErrorMsg),
                reply_json_dict(_{status: error, error: ErrorMsg})
            )
        )
    ).

%! handle_uninstall(+Request) is det.
%
%  Handle pack uninstallation request
handle_uninstall(Request) :-
    cors_enable(Request, [methods([get, post, options])]),
    http_read_json_dict(Request, Data),
    PackName = Data.get(pack_name),
    (   PackName == @(null) ->
        reply_json_dict(_{status: error, error: "Pack name is required"})
    ;   catch(
            (   uninstall_pack_safe(PackName, Result),
                reply_json_dict(_{status: ok, result: Result})
            ),
            Error,
            (   format_error(Error, ErrorMsg),
                reply_json_dict(_{status: error, error: ErrorMsg})
            )
        )
    ).

%! handle_update(+Request) is det.
%
%  Handle pack update request
handle_update(Request) :-
    cors_enable(Request, [methods([get, post, options])]),
    http_read_json_dict(Request, Data),
    PackName = Data.get(pack_name),
    (   PackName == @(null) ->
        reply_json_dict(_{status: error, error: "Pack name is required"})
    ;   catch(
            (   update_pack_safe(PackName, Result),
                reply_json_dict(_{status: ok, result: Result})
            ),
            Error,
            (   format_error(Error, ErrorMsg),
                reply_json_dict(_{status: error, error: ErrorMsg})
            )
        )
    ).

%! handle_pack_info(+Request) is det.
%
%  Handle pack information request
handle_pack_info(Request) :-
    cors_enable(Request, [methods([get, post, options])]),
    http_parameters(Request, [pack_name(PackName, [])]),
    catch(
        (   get_pack_info(PackName, Info),
            reply_json_dict(_{status: ok, info: Info})
        ),
        Error,
        (   format_error(Error, ErrorMsg),
            reply_json_dict(_{status: error, error: ErrorMsg})
        )
    ).

%! handle_search(+Request) is det.
%
%  Handle pack search request
handle_search(Request) :-
    cors_enable(Request, [methods([get, post, options])]),
    http_parameters(Request, [query(Query, [])]),
    catch(
        (   search_packs(Query, Packs),
            reply_json_dict(_{status: ok, packs: Packs})
        ),
        Error,
        (   format_error(Error, ErrorMsg),
            reply_json_dict(_{status: error, error: ErrorMsg})
        )
    ).

%! handle_outdated(+Request) is det.
%
%  Handle request to list outdated packs
handle_outdated(Request) :-
    cors_enable(Request, [methods([get, post, options])]),
    catch(
        (   list_outdated_packs(Packs),
            reply_json_dict(_{status: ok, packs: Packs})
        ),
        Error,
        (   format_error(Error, ErrorMsg),
            reply_json_dict(_{status: error, error: ErrorMsg})
        )
    ).

%! list_available_packs(-Packs) is det.
%
%  Get list of all available packs from configured servers
list_available_packs(Packs) :-
    findall(Pack, available_pack(Pack), PackList),
    sort(PackList, Packs).

%! available_pack(-Pack) is nondet.
%
%  Generate available packs from pack servers
available_pack(Pack) :-
    pack_search('', Results),
    member(Pack, Results).

%! list_installed_packs(-Packs) is det.
%
%  Get list of installed packs with their information
list_installed_packs(Packs) :-
    findall(Pack, installed_pack(Pack), PackList),
    sort(PackList, Packs).

%! installed_pack(-Pack) is nondet.
%
%  Generate installed pack information
installed_pack(Pack) :-
    pack_list_installed(InstalledPacks),
    member(pack(Name, _, _), InstalledPacks),
    get_pack_info(Name, Pack).

%! install_pack_safe(+PackName, -Result) is det.
%
%  Safely install a pack with proper error handling
install_pack_safe(PackName, Result) :-
    validate_pack_name(PackName),
    (   pack_installed(PackName) ->
        Result = "Pack already installed"
    ;   pack_install(PackName, [interactive(false), upgrade(true)]),
        Result = "Pack installed successfully"
    ).

%! uninstall_pack_safe(+PackName, -Result) is det.
%
%  Safely uninstall a pack with proper error handling
uninstall_pack_safe(PackName, Result) :-
    validate_pack_name(PackName),
    (   \+ pack_installed(PackName) ->
        Result = "Pack is not installed"
    ;   pack_remove(PackName),
        Result = "Pack uninstalled successfully"
    ).

%! update_pack_safe(+PackName, -Result) is det.
%
%  Safely update a pack with proper error handling
update_pack_safe(PackName, Result) :-
    validate_pack_name(PackName),
    (   \+ pack_installed(PackName) ->
        Result = "Pack is not installed"
    ;   pack_upgrade(PackName),
        Result = "Pack updated successfully"
    ).

%! get_pack_info(+PackName, -Info) is det.
%
%  Get detailed information about a pack
get_pack_info(PackName, Info) :-
    validate_pack_name(PackName),
    findall(Key-Value, pack_info_item(PackName, Key, Value), InfoPairs),
    dict_pairs(Info, pack_info, InfoPairs).

%! pack_info_item(+PackName, -Key, -Value) is nondet.
%
%  Extract individual pack information items
pack_info_item(PackName, name, PackName).
pack_info_item(PackName, Key, Value) :-
    pack_info(PackName, Info),
    member(Key(Value), Info).
pack_info_item(PackName, installed, true) :-
    pack_installed(PackName).
pack_info_item(PackName, installed, false) :-
    \+ pack_installed(PackName).

%! search_packs(+Query, -Packs) is det.
%
%  Search for packs matching the query
search_packs(Query, Packs) :-
    (   Query == '' ->
        list_available_packs(Packs)
    ;   pack_search(Query, Results),
        maplist(pack_result_to_info, Results, Packs)
    ).

%! pack_result_to_info(+PackResult, -PackInfo) is det.
%
%  Convert pack search result to standardized info format
pack_result_to_info(pack(Name, Title, Version, Author, Home, Download), Info) :-
    Info = _{
        name: Name,
        title: Title,
        version: Version,
        author: Author,
        home: Home,
        download: Download
    }.

%! list_outdated_packs(-Packs) is det.
%
%  Get list of installed packs that have updates available
list_outdated_packs(Packs) :-
    findall(Pack, outdated_pack(Pack), PackList),
    sort(PackList, Packs).

%! outdated_pack(-Pack) is nondet.
%
%  Generate outdated pack information
outdated_pack(Pack) :-
    pack_list_installed(InstalledPacks),
    member(pack(Name, _, _), InstalledPacks),
    pack_info(Name, version(CurrentVersion)),
    pack_property(Name, latest_version(LatestVersion)),
    CurrentVersion \= LatestVersion,
    get_pack_info(Name, PackInfo),
    Pack = PackInfo.put(_{
        current_version: CurrentVersion,
        latest_version: LatestVersion,
        outdated: true
    }).

%! validate_pack_name(+PackName) is det.
%
%  Validate that a pack name is safe and well-formed
validate_pack_name(PackName) :-
    atom(PackName),
    atom_length(PackName, Len),
    Len > 0,
    Len < 100,
    \+ sub_atom(PackName, _, _, _, '..'),
    \+ sub_atom(PackName, _, _, _, '/'),
    \+ sub_atom(PackName, _, _, _, '\\').

%! pack_installed(+PackName) is semidet.
%
%  Check if a pack is currently installed
pack_installed(PackName) :-
    pack_list_installed(InstalledPacks),
    member(pack(PackName, _, _), InstalledPacks).

%! format_error(+Error, -ErrorMsg) is det.
%
%  Format error terms into human-readable messages
format_error(Error, ErrorMsg) :-
    (   Error = error(Formal, Context) ->
        format_error_formal(Formal, Context, ErrorMsg)
    ;   Error = pack_error(Msg) ->
        ErrorMsg = Msg
    ;   term_string(Error, ErrorMsg)
    ).

%! format_error_formal(+Formal, +Context, -ErrorMsg) is det.
%
%  Format formal error terms
format_error_formal(existence_error(pack, PackName), _, ErrorMsg) :-
    format(string(ErrorMsg), "Pack '~w' does not exist", [PackName]).
format_error_formal(permission_error(install, pack, PackName), _, ErrorMsg) :-
    format(string(ErrorMsg), "Permission denied to install pack '~w'", [PackName]).
format_error_formal(resource_error(network), _, "Network error occurred").
format_error_formal(Formal, Context, ErrorMsg) :-
    format(string(ErrorMsg), "Error: ~w (Context: ~w)", [Formal, Context]).

%! start_pack_server is det.
%
%  Start the pack management HTTP server
start_pack_server :-
    start_pack_server(3061).

%! start_pack_server(+Port) is det.
%
%  Start the pack management HTTP server on specified port
start_pack_server(Port) :-
    catch(http_stop_server(Port, []), _, true),
    http_server(http_dispatch, [port(Port)]),
    format('Pack management server started on port ~w~n', [Port]).

%! stop_pack_server is det.
%
%  Stop the pack management HTTP server
stop_pack_server :-
    stop_pack_server(3061).

%! stop_pack_server(+Port) is det.
%
%  Stop the pack management HTTP server on specified port
stop_pack_server(Port) :-
    catch(http_stop_server(Port, []), _, true),
    format('Pack management server stopped on port ~w~n', [Port]).

% Initialize debug settings
:- debug(pack_manager).

% Auto-start server when module is loaded (optional)
% :- initialization(start_pack_server, now).