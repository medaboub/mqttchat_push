1mqttchat_push
=====

An OTP application

Build
-----
$ rebar3 compile


## Generate PEM private key using openssl
```
openssl ecparam -name prime256v1 -genkey -noout -out es_private_key.pem   
```
## Start Mqttchat_fcm with PemPrivateKey
``` erlang
{ok, PrivPem} = file:read_file("/home/keys/private_key.pem"),
mqttchat_push:start(mqttchat, PrivPem).
```

## Send encrypted notification to chrome
``` erlang
Subscription= #subscription{
endpoint = <<"https://fcm.googleapis.com/fcm/send/fiKSpaSo0TA:APA91bEkdUO6ncb45rCLNljjPxjlI1uRoLNyG2107kLSD3p6HTGPMuJodGHkrWKmbb8y9dK3Afi3Tvydil9fbgLBE64X7LPnqYzPHn2aEGwruKOdaQImehlDaX_4_1VbE6hYp6zqSio1">>,
p256dh = <<"BH0mbVKNm1yeSYvtkNdp-eMFSXj9z42g5KmTcja22E7vwR7rrvudpVfpPzffx6Pf1c6CZiFc-B7VfwhcRUtBD14">>, 
auth = <<"mkMQoKf4cd3o_Ysl2YubGA">>
                           },                           
Payload= #{ msg => <<"hello world">> } ,   
mqttchat_push:sync_push(mqttchat, Subscription, Payload).      
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
Subscription= #subscription{endpoint = <<"https://updates.push.services.mozilla.com/wpush/v2/gAAAAABfj0dKYDPn8AzRi_7wjP3YbquNR3sXrmm_noliPkXuCbqLezKkdHjPZ00iZrLmfrl-D7eRlTT8_IRYANsW2jE8Zv1g1iRp1_Ch5RJAPeF9RfVbnORmyUFOM2qCB6PeWPrMQQDhYuocWIrD5JzSHOdJe8yj8-OEH07j-64fSTeKOSxZrDw">>,
                   p256dh = <<"BN2bWyK5_wYPOrJ7lbqnSR7aMIp1iKaNJZqzhxyLN_DckYYAHYJdzm2YP7tmWdbnEXFaDKsWl2Yh2Bej9nc5CXA">>, 
                   auth = <<"ci58BkrM_7HnIeI_mOhVkw">>},
    Payload= #{ msg => <<"hello word habibi">> } ,              
    mqttchat_push:sync_push(mqttchat, Subscription, Payload).
```

#### Debug  
Request sample : 
```
{"https://updates.push.services.mozilla.com/wpush/v2/gAAAAABfj0dKYDPn8AzRi_7wjP3YbquNR3sXrmm_noliPkXuCbqLezKkdHjPZ00iZrLmfrl-D7eRlTT8_IRYANsW2jE8Zv1g1iRp1_Ch5RJAPeF9RfVbnORmyUFOM2qCB6PeWPrMQQDhYuocWIrD5JzSHOdJe8yj8-OEH07j-64fSTeKOSxZrDw",[{"Authorization","WebPush eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCJ9.eyJhdWQiOiJodHRwczovL3VwZGF0ZXMucHVzaC5zZXJ2aWNlcy5tb3ppbGxhLmNvbSIsImV4cCI6MTYwMzQwMDQwNiwic3ViIjoibWFpbHRvOiBtcXR0Y2hhdEBtcXR0LWNoYXQuY29tIn0.qIRJlFzkvJ0ZK0Gzp4ZvVb_xU2Nn8Q0H1x0Pe7ft5DWJupvCAmitJhhz2FwkcJO0FqnYoK7ZTvk8_Ox6MMS98A"},{"Crypto-Key","p256ecdsa=BKrtcEpg-FfmUYvX2wbdWLupsLDRR_hNOBPjKht_5UlQ8MkbYc8-O-_XLyqijlNvuZTkTj3bpMutYot5KU02pys;dh=BBJUSxkBFgwaB0uMWQQQYzP8Gs22W67ImETqulNaj8uFO__ti0IhvbO_utN5eEdQoX2XyOzrQwTNUfxUkH1Az9o"},{"Encryption","salt=UcHWgsQ5BSs3p2I40dRZWw"},{"Content-Encoding","aesgcm"},{"TTL","60"}],"Content-Type: application/octet-stream",<<205,220,20,215,56,225,199,194,146,221,26,126,102,53,151,251,90,170,0,163,73,248,68,96,254,115,113,167,82,69,117,169,84,110,39,239,161,202,214,148,216,12,60,41,21,82,45,39,139,109,92,137,187,213,56,121,31,85,19,34,173>>}
```
Response :
```
{ok,{{"HTTP/1.1",201,"Created"},
     [{"connection","keep-alive"},
      {"date","Thu, 22 Oct 2020 20:00:31 GMT"},
      {"location",
       "https://updates.push.services.mozilla.com/m/gAAAAABfkeTf9A6mpbKzVPlhtnpSqk7nz2qaOu1MDRgwSbK3e7CYPLtQlDN9g9eBsD0RTgPYWUGUpdHkedF4T7M7jTj3OJX-kI46q8d6kINrn1YXBwRJIpVuvf6L41MH23R_EyOin9hiDFyZyYH99bTJmWZ--TJS_g0Ev4iUoa8aoOnWGNMxssRHKWOM_tet_x1G6B3qek1m"},
      {"server","nginx"},
      {"content-length","0"},
      {"content-type","text/html; charset=UTF-8"},
      {"access-control-allow-headers",
       "content-encoding,encryption,crypto-key,ttl,encryption-key,content-type,authorization"},
      {"access-control-allow-methods","POST"},
      {"access-control-allow-origin","*"},
      {"access-control-expose-headers",
       "location,www-authenticate"},
      {"strict-transport-security",
       "max-age=31536000;includeSubDomains"},
      {"ttl","60"}],
     <<>>}}
```
## Send async push notification 
``` erlang
Subscription= #subscription{
endpoint = <<"https://fcm.googleapis.com/fcm/send/fiKSpaSo0TA:APA91bEkdUO6ncb45rCLNljjPxjlI1uRoLNyG2107kLSD3p6HTGPMuJodGHkrWKmbb8y9dK3Afi3Tvydil9fbgLBE64X7LPnqYzPHn2aEGwruKOdaQImehlDaX_4_1VbE6hYp6zqSio1">>,
p256dh = <<"BH0mbVKNm1yeSYvtkNdp-eMFSXj9z42g5KmTcja22E7vwR7rrvudpVfpPzffx6Pf1c6CZiFc-B7VfwhcRUtBD14">>, 
auth = <<"mkMQoKf4cd3o_Ysl2YubGA">>
                           },                           
Payload= #{ msg => <<"hello world">> } ,   
Fun = fun(R) -> {_, HttpResp } = R,  erlang:display(HttpResp) end,
mqttchat_push:async_push(mqttchat, Subscription, Payload,Fun ). 
```

## Get application public and private VAPID keys
``` erlang
mqttchat_push:vapid_keys(mqttchat).
```

### sample
```
{<<"BKrtcEpg-FfmUYvX2wbdWLupsLDRR_hNOBPjKht_5UlQ8MkbYc8-O-_XLyqijlNvuZTkTj3bpMutYot5KU02pys">>,
 <<"HIDDEN">>}
```

@medaboub