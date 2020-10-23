-module(mqttchat_push_send).
-export([sync_push/3,async_push/4,pem_to_ec/1]).

-define (TTL , "60").

-include("include/records.hrl").



sync_push( PemPrivKey ,Subscription, Payload)->
Request= build_request(PemPrivKey ,Subscription, Payload),
httpc:request(post, Request, [{ssl,[{verify,0}]}],[{body_format, binary}]).



async_push(PemPrivKey ,Subscription, Payload, Fun) ->
Request= build_request(PemPrivKey ,Subscription, Payload),
httpc:request(post, Request, [{ssl,[{verify,0}]}],[{sync, false },{receiver, Fun},{body_format, binary}]).  



build_request(PemPrivKey ,#subscription{endpoint = Endpoint, p256dh = _, auth =_} = Subscription, Payload)->
 Token = mqttchat_push_headers:jwt_token(Endpoint , PemPrivKey),
 {Ciphertext, Salt, ServerPublicKey} = mqttchat_push_payload:encrypt(Subscription,Payload), 
 { ApplicationServerKey, _ } = pem_to_ec(PemPrivKey),
 Headers=[
         {"Authorization", "WebPush " ++ binary_to_list(Token) },
         {"Crypto-Key", "p256ecdsa=" ++  binary_to_list(ApplicationServerKey) ++ ";dh=" ++ binary_to_list(base64_url:encode(ServerPublicKey))} ,
         {"Encryption", "salt=" ++ binary_to_list(base64_url:encode(Salt)) },
         {"Content-Encoding", "aesgcm"},
         {"TTL", ?TTL }
        ],
 Request={binary_to_list(Endpoint),Headers,"Content-Type: application/octet-stream",Ciphertext},
 Request.




pem_to_ec(PemPrivKey)->
  ECPrivateKeyPem1 = case public_key:pem_decode(PemPrivKey) of
                       [_, ECPrivateKeyPem] -> ECPrivateKeyPem;
                       [ECPrivateKeyPem] -> ECPrivateKeyPem
                     end,
{'ECPrivateKey',_,X,_,Y}= public_key:pem_entry_decode
(ECPrivateKeyPem1),
{ base64_url:encode(Y), base64_url:encode(X)}.

