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
Payload= #{ msg => <<"hello world">> } ,   

mqttchat_fcm:sync_push(mqttchat, Subscription, Payload).      

```

#### Debug  
Request sample : 
```
{"https://fcm.googleapis.com/fcm/send/fiKSpaSo0TA:APA91bEkdUO6ncb45rCLNljjPxjlI1uRoLNyG2107kLSD3p6HTGPMuJodGHkrWKmbb8y9dK3Afi3Tvydil9fbgLBE64X7LPnqYzPHn2aEGwruKOdaQImehlDaX_4_1VbE6hYp6zqSio1",[{"Authorization","WebPush eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCJ9.eyJhdWQiOiJodHRwczovL2ZjbS5nb29nbGVhcGlzLmNvbSIsImV4cCI6MTYwMzQwMDI1Mywic3ViIjoibWFpbHRvOiBtcXR0Y2hhdEBtcXR0LWNoYXQuY29tIn0.3AQD1FJtFQbxv1xbYO8whGPGSuf2L9CWdkcANPt1vwDVmzFigZIHsr61z5a8mCW1q-jxJK2prZn1N4Cy9zVCng"},{"Crypto-Key","p256ecdsa=BKrtcEpg-FfmUYvX2wbdWLupsLDRR_hNOBPjKht_5UlQ8MkbYc8-O-_XLyqijlNvuZTkTj3bpMutYot5KU02pys;dh=BHuyMbtreGxK7o5nzduN4eVcgsFHDCdgQTL2Tmpw9q14vGRCwdECSRkWF-ywgx9ZtJ_MhiAHRLwSGDkZiaJ5rqQ"},{"Encryption","salt=uVvtjSfi_UpvXm0xscfaOA"},{"Content-Encoding","aesgcm"},{"TTL","60"}],"Content-Type: application/octet-stream",<<45,106,27,90,126,117,58,220,76,8,132,183,114,78,45,78,125,218,125,212,181,218,232,209,43,47,199,30,184,224,55,71,252,0,246,3,131,95,222,131,132,247,96,0,76,50,232,87,175,85,202,209,14,111,8,128,134,38,6,66,148,163,253,166,122,253,19,219,224,216,172,228,157,91,52,170,1,165,158,181>>}
```
Response :
```
{ok,{{"HTTP/1.1",201,"Created"},
     [{"date","Thu, 22 Oct 2020 19:57:58 GMT"},
      {"location",
       "https://fcm.googleapis.com/0:1603396678782533%e609af1cf9fd7ecd"},
      {"content-length","0"},
      {"content-type","text/html; charset=UTF-8"},
      {"x-content-type-options","nosniff"},
      {"x-frame-options","SAMEORIGIN"},
      {"x-xss-protection","0"},
      {"alt-svc",
       "h3-Q050=\":443\"; ma=2592000,h3-29=\":443\"; ma=2592000,h3-T051=\":443\"; ma=2592000,h3-T050=\":443\"; ma=2592000,h3-Q046=\":443\"; ma=2592000,h3-Q043=\":443\"; ma=2592000,quic=\":443\"; ma=2592000; v=\"46,43\""}],
     <<>>}}
```

## Sent encrypted notification to mozilla

``` erlang
Subscription= #subscription{
endpoint = <<"https://fcm.googleapis.com/fcm/send/fiKSpaSo0TA:APA91bEkdUO6ncb45rCLNljjPxjlI1uRoLNyG2107kLSD3p6HTGPMuJodGHkrWKmbb8y9dK3Afi3Tvydil9fbgLBE64X7LPnqYzPHn2aEGwruKOdaQImehlDaX_4_1VbE6hYp6zqSio1">>,
p256dh = <<"BH0mbVKNm1yeSYvtkNdp-eMFSXj9z42g5KmTcja22E7vwR7rrvudpVfpPzffx6Pf1c6CZiFc-B7VfwhcRUtBD14">>, 
auth = <<"mkMQoKf4cd3o_Ysl2YubGA">>
                           },                           
Payload= #{ msg => <<"hello world">> } , 

mqttchat_fcm:sync_push(mqttchat, Subscription, Payload).      

```

## Get Application public and private VAPID keys
``` erlang
mqttchat_fcm:vapid_keys(mqttchat).

```
