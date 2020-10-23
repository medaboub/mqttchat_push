-module(mqttchat_push_payload).
-export([encrypt/2]).
-include("include/records.hrl").


encrypt(#subscription{ endpoint = _, p256dh = P256dh, auth =Auth} = _ ,Payload)->
 PayloadJson= jsx:encode(Payload),
 Key = { base64_url:decode(P256dh) , base64_url:decode(Auth)},
 webpush_encryption:encrypt(PayloadJson,Key,16).



