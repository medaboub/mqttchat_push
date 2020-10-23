%%
%% @doc Web Push Payload Encryption.
%%
%% Based on:
%%   https://developers.google.com/web/updates/2016/03/web-push-encryption
%%   https://github.com/GoogleChrome/web-push-encryption
%%

-module(webpush_encryption).

-type publicKey()  :: binary().
-type privateKey() :: binary().
-type authToken() :: binary().
-type cipherText() :: binary().
-type plainText() :: binary().
-type salt() :: binary().
-type clientKey()  :: {publicKey(), authToken()}.
-type fullClientKeys() :: {publicKey(), privateKey(), authToken()}.
-type cryptData()  :: {cipherText(), salt(), publicKey()}.

-export_type([
	publicKey/0,
	privateKey/0,
	authToken/0,
	clientKey/0,
	fullClientKeys/0,
	cryptData/0
]).
-export([
	encrypt/3,
	decrypt/2
]).


%%%===================================================================
%%% API
%%%===================================================================

-spec encrypt(plainText(), clientKey(), non_neg_integer()) -> cryptData().
encrypt(Message, {ClientPublicKey, ClientAuthToken}, PaddingLength) ->
	{Salt, ServerPublicKey, ServerPrivateKey} = generate_server_keys(),
	Ikm = ikm(ClientPublicKey, ServerPrivateKey, ClientAuthToken),
	{ContentEncryptionKey, Nonce} = cek_and_nonce(ClientPublicKey, ServerPublicKey, Salt, Ikm),
	Plaintext = add_padding(PaddingLength, Message),
	Ciphertext = encrypt_payload(Plaintext, ContentEncryptionKey, Nonce),
	{Ciphertext, Salt, ServerPublicKey}.


-spec decrypt(cryptData(), fullClientKeys()) -> plainText().
decrypt({Ciphertext, Salt, ServerPublicKey}, {ClientPublicKey, ClientPrivateKey, ClientAuthToken}) ->
	Ikm = ikm(ServerPublicKey, ClientPrivateKey, ClientAuthToken),
	{ContentEncryptionKey, Nonce} = cek_and_nonce(ClientPublicKey, ServerPublicKey, Salt, Ikm),
	PaddedMessage = decrypt_payload(Ciphertext, ContentEncryptionKey, Nonce),
	remove_padding(PaddedMessage).


%%%===================================================================
%%% PRIVATE
%%%===================================================================

generate_server_keys() ->
	Salt = crypto:strong_rand_bytes(16),
	{ServerPublicKey, ServerPrivateKey} = crypto:generate_key(ecdh, prime256v1),
	{Salt, ServerPublicKey, ServerPrivateKey}.


ikm(TheirPublicKey, MyPrivateKey, ClientAuthToken) ->
	Params = crypto:ec_curve(prime256v1),
	SharedSecret = crypto:compute_key(ecdh, TheirPublicKey, MyPrivateKey, Params),
	AuthInfo = info("auth", none),
	hkdf(ClientAuthToken, SharedSecret, AuthInfo, 32).


cek_and_nonce(ClientPublicKey, ServerPublicKey, Salt, Ikm) ->
	Context = context(ClientPublicKey, ServerPublicKey),
	ContentEncryptionKeyInfo = info("aesgcm", Context),
	ContentEncryptionKey = hkdf(Salt, Ikm, ContentEncryptionKeyInfo, 16),
	NonceInfo = info("nonce", Context),
	Nonce = hkdf(Salt, Ikm, NonceInfo, 12),
	{ContentEncryptionKey, Nonce}.


hkdf(Salt, Ikm, Info, Length) ->
	KeyHmac = crypto:hmac_init(sha256, Salt),
	KeyHmac = crypto:hmac_update(KeyHmac, Ikm),
	Prk = crypto:hmac_final(KeyHmac),
	
	InfoHmac = crypto:hmac_init(sha256, Prk),
	InfoHmac = crypto:hmac_update(InfoHmac, Info),
	InfoHmac = crypto:hmac_update(InfoHmac, <<1>>),
	binary:part(crypto:hmac_final(InfoHmac), {0, Length}).


context(ClientPublicKey, PublicKey) ->
	<<0, (byte_size(ClientPublicKey)):16/big-unsigned-integer, ClientPublicKey/binary, (byte_size(PublicKey)):16/big-unsigned-integer, PublicKey/binary>>.


info(Type, Context) when is_list(Type) ->
	info(list_to_binary(Type), Context);
info(Type, none) ->
	<<"Content-Encoding: ", Type/binary, 0>>;
info(Type, Context) ->
	Encoding = info(Type, none),
	<<Encoding/binary, "P-256", Context/binary>>.


%%%===================================================================
%%% PRIVATE CRYPTO
%%%===================================================================

encrypt_payload(Plaintext, ContentEncryptionKey, Nonce) ->
	{CipherText, CipherTag} = crypto:block_encrypt(aes_gcm, ContentEncryptionKey, Nonce, {<<"">>, Plaintext}),
	<<CipherText/binary, CipherTag/binary>>.


decrypt_payload(Ciphertext, ContentEncryptionKey, Nonce) ->
	{Text, Tag} = split_cipher(Ciphertext),
	crypto:block_decrypt(aes_gcm, ContentEncryptionKey, Nonce, {<<"">>, Text, Tag}).


split_cipher(Cipher) ->
	<<Tag:16/binary, Text/binary>> = reverse(Cipher),
	{reverse(Text), reverse(Tag)}.


reverse(Bin) ->
   Size = erlang:size(Bin)*8,
   <<X:Size/integer-little>> = Bin,
   <<X:Size/integer-big>>.


%%%===================================================================
%%% PRIVATE PADDING
%%%===================================================================

add_padding(PaddingLength, Message) ->
	Length = <<PaddingLength:16/big-unsigned-integer>>,
	<<Length/binary, (binary:copy(<<0>>, PaddingLength))/binary, Message/binary>>.


remove_padding(PaddedMessage) ->
	<<PadLen:16/big-unsigned-integer, _Pad:PadLen/binary, PlainText/binary>> = PaddedMessage,
	PlainText.
