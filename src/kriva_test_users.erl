-module(kriva_test_users).
-behaviour(application).

-define(WORK_FACTOR, 12). %% increase to 12 for production

-record(users,{name,password}).

-export([start/2,stop/1]).

-export([compare/2,new/2,update/3,delete/2]).

-export([install/1]).

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes,application,start,[mnesia]),
    mnesia:create_table(users,[{attributes, record_info(fields,users)},
                               {disc_copies,Nodes}]),
    rpc:multicall(Nodes,application,stop,[mnesia]).

new(Name,Password) ->
    Hash=erlpass:hash(Password,?WORK_FACTOR),
    F = fun() ->
                User=mnesia:read({users,Name}),
                case User of 
                    [] ->
                        mnesia:write(users,#users{name=Name,password=Hash},
                                     write);
                    _ ->
                        already_defined
                end                    
        end,
    %% rets already_defined | ok
    mnesia:activity(transaction,F).

update(Name,Password_Old,Password) ->
    Hash=erlpass:hash(Password,?WORK_FACTOR),
    F = fun() ->
                User=mnesia:read({users,Name}),
                case User of 
                    [] ->
                        not_defined;
                    [#users{password=Hash_Old}|_] ->
                        case erlpass:match(Password_Old,Hash_Old) of
                            true ->
                                mnesia:write(users,
                                             #users{name=Name,
                                                    password=Hash},write);
                            false ->
                                bad_password
                        end;
                    _ ->
                        malformed_table
                        
                end                    
        end,
    %% rets not_defined | ok | bad_password | malformed_table
    mnesia:activity(transaction,F).

delete(Name,Password) ->
    F = fun() ->
                User=mnesia:read({users,Name}),
                case User of 
                    [] ->
                        not_defined;
                    [#users{password=Hash}|_] ->
                        case erlpass:match(Password,Hash) of
                            true ->
                                mnesia:delete(users,
                                              Name
                                             ,write);
                            false ->
                                bad_password
                        end;
                    _ ->
                        malformed_table
                        
                end                    
        end,
    %% rets not_defined | ok | bad_password
    mnesia:activity(transaction,F).


compare(Name,Password)->
    User=lookup(Name),
    case User of 
        [] ->
            not_defined;
        [User1|_] ->
            erlpass:match(Password,User1#users.password)
    end.
    
start(normal,[]) ->    
    mnesia:wait_for_tables([users], 5000),
    kriva_test_users_sup:start_link().

stop(_) ->
    ok.


%% Internal
lookup(Name)->
    F = fun() ->
                mnesia:read({users,Name})
        end,
    mnesia:activity(transaction,F).
