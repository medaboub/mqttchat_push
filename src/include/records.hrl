-record(state, { private_key, public_key }).
-record(subscription, { endpoint , p256dh , auth }).
-record(encryption, { ciphertext, salt, server_public_key }).