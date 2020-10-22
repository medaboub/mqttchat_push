# mqttchat_fcm
Erlang : Sending Push Notifications with VAPID keys and payload encryption.


## Generate PEM private key using openssl
```
$ openssl ecparam -genkey -name prime256v1 -out private_key.pem
```
## Start Mqttchat_fcm with prime256v1 PemPrivateKey
``` erlang
{ok, PrivPem} = file:read_file("/home/keys/rivate_key.pem"),
mqttchat_fcm:start(mqttchat, PrivPem).

```

## Send encrypted notification to chrome
``` erlang
Subscription= #subscription{
endpoint = <<"https://fcm.googleapis.com/fcm/send/fiKSpaSo0TA:APA91bEkdUO6ncb45rCLNljjPxjlI1uRoLNyG2107kLSD3p6HTGPMuJodGHkrWKmbb8y9dK3Afi3Tvydil9fbgLBE64X7LPnqYzPHn2aEGwruKOdaQImehlDaX_4_1VbE6hYp6zqSio1">>,
p256dh = <<"BH0mbVKNm1yeSYvtkNdp-eMFSXj9z42g5KmTcja22E7vwR7rrvudpVfpPzffx6Pf1c6CZiFc-B7VfwhcRUtBD14">>, 
auth = <<"mkMQoKf4cd3o_Ysl2YubGA">>
                           },                           
Payload= #{ title => <<"hello title">>, body => <<"cbody content">> } ,   

mqttchat_fcm:sync_push(mqttchat, Subscription, Payload).      

```
## Sent encrypted notification to mozilla

``` erlang
Subscription= #subscription{
endpoint = <<"https://fcm.googleapis.com/fcm/send/fiKSpaSo0TA:APA91bEkdUO6ncb45rCLNljjPxjlI1uRoLNyG2107kLSD3p6HTGPMuJodGHkrWKmbb8y9dK3Afi3Tvydil9fbgLBE64X7LPnqYzPHn2aEGwruKOdaQImehlDaX_4_1VbE6hYp6zqSio1">>,
p256dh = <<"BH0mbVKNm1yeSYvtkNdp-eMFSXj9z42g5KmTcja22E7vwR7rrvudpVfpPzffx6Pf1c6CZiFc-B7VfwhcRUtBD14">>, 
auth = <<"mkMQoKf4cd3o_Ysl2YubGA">>
                           },                           
Payload= #{ title => <<"hello title">>, body => <<"cbody content">> } ,   

mqttchat_fcm:sync_push(mqttchat, Subscription, Payload).      

```

## Get Application VAPID keys
``` erlang
mqttchat_fcm:vapid_keys(mqttchat).

```
