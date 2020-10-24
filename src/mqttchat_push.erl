-module(mqttchat_push).

-behaviour(gen_server).

-export([start/2, stop/1, start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([sync_push/3,async_push/4, vapid_keys/1,testc/0]).

-define(SERVER, ?MODULE).

-include("include/records.hrl").


testc()->
    application:start(mqttchat_push),
    {ok, PrivtPem} = file:read_file("/home/vernemq/plugins/mqttchat/keys/es_private_key.pem"),
    mqttchat_push:start(mqttchat, PrivtPem),

    Subscription= #subscription{endpoint = <<"https://fcm.googleapis.com/fcm/send/fiKSpaSo0TA:APA91bEkdUO6ncb45rCLNljjPxjlI1uRoLNyG2107kLSD3p6HTGPMuJodGHkrWKmbb8y9dK3Afi3Tvydil9fbgLBE64X7LPnqYzPHn2aEGwruKOdaQImehlDaX_4_1VbE6hYp6zqSio1">>,
                   p256dh = <<"BH0mbVKNm1yeSYvtkNdp-eMFSXj9z42g5KmTcja22E7vwR7rrvudpVfpPzffx6Pf1c6CZiFc-B7VfwhcRUtBD14">>, 
                   auth = <<"mkMQoKf4cd3o_Ysl2YubGA">>},
    Payload= #{ title => <<"hello title">>, body => <<"cbody content">> } ,              
    mqttchat_push:sync_push(mqttchat, Subscription, Payload).               


-spec start(Name::atom(),PemPrivKey ::binary()) -> 
    {'error',_} | {'ok','undefined' | pid()} | {'ok','undefined' | pid(),_}.
start(Name, PemPrivKey ) when is_atom(Name), is_binary(PemPrivKey)->  
    mqttchat_push_sup:start_child(Name, PemPrivKey).



-spec sync_push(Name::atom(), Subscription::subscription, Payload::map())-> {'error',_} | ok .
sync_push(Name,#subscription{endpoint = _, p256dh = _, auth =_} = Subscription, Payload) when is_map(Payload) ->
gen_server:call( Name, {send, Subscription, Payload }).



-spec async_push(Name::atom(),Subscription::subscription, Payload::map(), Fun::fun())-> {'error',_} | ok .
async_push(Name, #subscription{endpoint = _, p256dh = _, auth =_} = Subscription, Payload, Fun) when is_map(Payload) ->
gen_server:call(Name, {send, Subscription, Payload , Fun }).



-spec vapid_keys(Name::atom()) ->  { _, _}.
vapid_keys(Name) when is_atom(Name)->
gen_server:call( Name, {send, <<"ec_keys">> }).   


stop(Name) ->
   gen_server:call(Name, stop).


%% OTP
start_link(Name, PemPrivKey ) ->
    gen_server:start_link({local, Name}, ?MODULE, [PemPrivKey], []).

init([PemPrivKey]) ->    
    {ok, #state{ private_key = PemPrivKey }}.


handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};


handle_call({send, Subscription, Payload }, _From, #state{ private_key = PemPrivKey} = State) ->
     Reply = mqttchat_push_send:sync_push(PemPrivKey,Subscription,Payload),
    {reply, Reply, State};


handle_call({send, Subscription, Payload ,Fun }, _From, #state{ private_key = PemPrivKey} = State) ->
    mqttchat_push_send:async_push(PemPrivKey,Subscription,Payload, Fun),
    {reply, ok, State};


handle_call({send , <<"ec_keys">>}, _From, #state{ private_key = PemPrivKey } = State) ->
    Reply = mqttchat_push_send:pem_to_ec(PemPrivKey),
    {reply, Reply, State};


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
