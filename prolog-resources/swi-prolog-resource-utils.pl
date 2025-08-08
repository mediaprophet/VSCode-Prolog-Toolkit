% swi-prolog-resource-utils.pl
% Utility predicates for resource usage and artifact tracking

:- module(resource_utils, [get_resource_usage/1, save_artifact/3]).

% get_resource_usage(-Usage)
% Usage is a dict with keys cpu, memory, and duration (ms)
get_resource_usage(Usage) :-
    statistics(cputime, CPU),
    statistics(memory, Mem),
    statistics(walltime, [_, Wall]),
    Usage = _{cpu:CPU, memory:Mem, duration:Wall}.

% save_artifact(+Name, +Path, +Type)
% Register an artifact for the current query context
save_artifact(Name, Path, Type) :-
    % This is a stub. Integration with the Node.js backend required.
    format('Artifact: ~w ~w ~w~n', [Name, Path, Type]).
