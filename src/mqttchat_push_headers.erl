-module(mqttchat_push_headers).
-export ([jwt_token/2,aud/1,jwt_data/1]).



aud(EndPoint) ->
{ok, {Scheme, _, Host,_,_, _}} = http_uri:parse(binary_to_list(EndPoint)),
binary_to_list(atom_to_binary(Scheme,utf8)) ++  "://" ++ Host .


exp()->
{Mega, Secs, _} = now(),
Mega*1000000 + Secs  + 3600 .



jwt_data(EndPoint)->
    #{
       aud  =>  list_to_binary(aud(EndPoint)),
       exp  =>  exp() ,
       sub  => <<"mailto: mqttchat@mqtt-chat.com">>
    }.


jwt_token(EndPoint,PemPrivateKey)->
   {<<"ES256">>, PrivateKey} = jose_pem:parse_key(PemPrivateKey),
   erlang:display(PrivateKey),
   jose_jws_compact:encode(jwt_data(EndPoint),<<"ES256">>,PrivateKey).  


