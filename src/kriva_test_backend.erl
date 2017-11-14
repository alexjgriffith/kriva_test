-module(kriva_test_backend).

-behaviour(oauth2_backend).

%% API
-export([
         start/0,
         stop/0,
         add_user/2,
         delete_user/2,
         add_client/2, add_client/3,
         delete_client/1
        ]).

%% Callbacks

%%% Behavior API
-export([authenticate_user/2]).
-export([authenticate_client/2]).
-export([get_client_identity/2]).
-export([associate_access_code/3]).
-export([associate_refresh_token/3]).
-export([associate_access_token/3]).
-export([resolve_access_code/2]).
-export([resolve_refresh_token/2]).
-export([resolve_access_token/2]).
-export([revoke_access_code/2]).
-export([revoke_access_token/2]).
-export([revoke_refresh_token/2]).
-export([get_redirection_uri/2]).
-export([verify_redirection_uri/3]).
-export([verify_client_scope/3]).
-export([verify_resowner_scope/3]).
-export([verify_scope/3]).

-define(ACCESS_TOKEN_TABLE, access_tokens).
-define(REFRESH_TOKEN_TABLE,refresh_tokens).
-define(USER_TABLE,users).
-define(CLIENT_TABLE,clients).

-define(TABLES,[?ACCESS_TOKEN_TABLE,
                ?REFRESH_TOKEN_TABLE,
                ?USER_TABLE,
                ?CLIENT_TABLE]).

%% Turn into a mnesia table
-record(client, {client_id,client_secret,redirect_uri}).

start()->
    ets:new(?ACCESS_TOKEN_TABLE,[named_table,public]),
    ets:new(?REFRESH_TOKEN_TABLE,[named_table,public]),
    ets:new(?CLIENT_TABLE,[named_table,public]),
    kriva_test_users:start(normal,[]).

stop() ->
    ets:delete(?ACCESS_TOKEN_TABLE),
    ets:delete(?REFRESH_TOKEN_TABLE),
    ets:delete(?CLIENT_TABLE),
    kriva_test_users:stop().

add_user(Username,Password) ->
    kriva_test_users:new(Username,Password).

delete_user(Username,Password) ->
    kriva_test_users:delete(Username,Password).


%% Update once clinets are actually implemented
add_client(Id, Secret, RedirectUri) ->
    put(?CLIENT_TABLE, Id, #client{client_id = Id,
                                   client_secret = Secret,
                                   redirect_uri = RedirectUri
                                  }).

add_client(Id, Secret) ->
    add_client(Id, Secret, undefined).

delete_client(Id) ->
    delete(?CLIENT_TABLE, Id).

%%%===================================================================
%%% OAuth2 backend functions
%%%===================================================================

authenticate_user(Username, Password) ->
    case kriva_test_users:compare(Username,Password) of
        true ->
            {ok, {<<"user">>, Username}};
        false ->
            {error, badpass};
        not_defined ->
            {error, notfound}
    end.

authenticate_client(ClientId, ClientSecret ) ->
    case get(?CLIENT_TABLE, ClientId) of
        {ok, #client{client_secret = ClientSecret}} ->
            {ok, {<<"client">>, ClientId}};
        {ok, #client{client_secret = _WrongSecret}} ->
            {error, badsecret};
        _ ->
            {error, notfound}
    end.

get_client_identity(ClientId, _) ->
    case get(?CLIENT_TABLE, ClientId) of
        {ok, _} ->
            {ok, {<<"client">>, ClientId}};
        _ ->
            {error, notfound}
    end.

associate_access_code(AccessCode, Context, _AppContext) ->
    associate_access_token(AccessCode, Context, _AppContext).

associate_refresh_token(RefreshToken, Context, _) ->
    put(?REFRESH_TOKEN_TABLE, RefreshToken, Context).

associate_access_token(AccessToken, Context, _) ->
    put(?ACCESS_TOKEN_TABLE, AccessToken, Context).


resolve_access_code(AccessCode, _AppContext) ->
    resolve_access_token(AccessCode, _AppContext).

resolve_refresh_token(RefreshToken, _AppContext) ->
    resolve_access_token(RefreshToken, _AppContext).

resolve_access_token(AccessToken, _) ->
    %% The case trickery is just here to make sure that
    %% we don't propagate errors that cannot be legally
    %% returned from this function according to the spec.
    case get(?ACCESS_TOKEN_TABLE, AccessToken) of
        Value = {ok, _} ->
            Value;
        Error = {error, notfound} ->
            Error
    end.

revoke_access_code(AccessCode, _AppContext) ->
    revoke_access_token(AccessCode, _AppContext).

revoke_access_token(AccessToken, _) ->
    delete(?ACCESS_TOKEN_TABLE, AccessToken),
    ok.

revoke_refresh_token(_RefreshToken, _) ->
    ok.

get_redirection_uri(ClientId, _) ->
    case get(?CLIENT_TABLE, ClientId) of
        {ok, #client{redirect_uri = RedirectUri}} ->
            {ok, RedirectUri};
        Error = {error, notfound} ->
            Error
    end.

verify_redirection_uri(ClientId, ClientUri, _) ->
    case get(?CLIENT_TABLE, ClientId) of
        {ok, #client{redirect_uri = RedirUri}} when ClientUri =:= RedirUri ->
            {ok, RedirUri};
        _Error ->
            {error, mismatch}
    end.

verify_client_scope(_ClientId, Scope, _) ->
    {ok, Scope}.

verify_resowner_scope(_ResOwner, Scope, _) ->
    {ok, Scope}.

verify_scope(Scope, Scope, _) ->
    {ok, Scope};
verify_scope(_, _, _) ->
    {error, invalid_scope}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            {error, notfound};
        [{_Key, Value}] ->
            {ok, Value}
    end.

put(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}),
    ok.

delete(Table, Key) ->
ets:delete(Table, Key).
    
