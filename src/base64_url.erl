-module(base64_url).
-compile(export_all).


encode(Data) ->
  Data1 = base64_encode_strip(lists:reverse(base64:encode_to_string(Data))),
  << << (urlencode_digit(D)) >> || <<D>> <= Data1 >>.
base64_encode_strip([$=|Rest]) ->
  base64_encode_strip(Rest);
base64_encode_strip(Result) ->
  list_to_binary(lists:reverse(Result)).


decode(Data) ->
  Data1 = << << (urldecode_digit(D)) >> || <<D>> <= Data >>,
  Data2 = case byte_size(Data1) rem 4 of
            2 -> <<Data1/binary, "==">>;
            3 -> <<Data1/binary, "=">>;
            _ -> Data1
          end,
  base64:decode(Data2).


urlencode_digit($/) -> $_;
urlencode_digit($+) -> $-;
urlencode_digit(D)  -> D.

urldecode_digit($_) -> $/;
urldecode_digit($-) -> $+;
urldecode_digit(D)  -> D.
