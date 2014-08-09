%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2014 1:59 PM
%%%-------------------------------------------------------------------
-module(v23_reader).
-author("aardvocate").

%% API
-export([read_v23/1, parse_frame_bin/4, find_v23_frame/2]).

find_v23_frame(FrameBin, ID3Data) when is_binary(ID3Data) ->
  case ID3Data of
    <<FrameID:4/binary, Size:32/integer, _A:1/integer, _B:1/integer, _C:1/integer, _Rem1:5, _I:1/integer, _J:1/integer, _K:1/integer, _Rem2:5, Rest/binary>> ->
      if
        FrameBin =:= FrameID ->
          {ok, found};
        true ->
          {_FrameContent, RestData} = split_binary(Rest, Size),
          find_v23_frame(FrameBin, RestData)
      end;
    _ ->
      {ok, not_found}
  end.

read_v23(ID3Data) ->
  Result = read_v23_frame(ID3Data, []),
  Result.

read_v23_frame(<<FrameID:4/binary, Size:32/integer, A:1/integer, B:1/integer, C:1/integer, _Rem1:5, I:1/integer, J:1/integer, K:1/integer, _Rem2:5, Rest/binary>>, Frames) when FrameID =/= <<0, 0, 0, 0>> ->
  Flags = {flags, [
    {tag_alter_preservation, erlp3_utils:reverse_boolean_code_to_atom(A)},
    {file_alter_preservation, erlp3_utils:reverse_boolean_code_to_atom(B)},
    {read_only, erlp3_utils:boolean_code_to_atom(C)},
    {compression, erlp3_utils:boolean_code_to_atom(I)},
    {encryption, erlp3_utils:boolean_code_to_atom(J)},
    {grouping_identity, erlp3_utils:boolean_code_to_atom(K)}
  ]},

  {FrameContent, ID3Data} = split_binary(Rest, Size),
  Frame = parse_frame_bin(FrameID, Size, Flags, FrameContent),
  read_v23_frame(ID3Data, [Frame | Frames]);

read_v23_frame(_, Frames) ->
  lists:reverse([Frame || Frame <- Frames, Frame =/= not_yet_implemented]).

parse_frame_bin(<<"COMR">>, Size, Flags, FrameContent) ->
  {comr, [
    {size, Size},
    Flags | parse_comr_content(FrameContent)
  ]};

parse_frame_bin(<<"COMM">>, Size, Flags, FrameContent) ->
  {comm, [
    {size, Size},
    Flags | v22_reader:parse_com_content(FrameContent)
  ]};

parse_frame_bin(<<"APIC">>, Size, Flags, FrameContent) ->
  {apic, [
    {size, Size},
    Flags | parse_pic_content(FrameContent)
  ]};

parse_frame_bin(<<"AENC">>, Size, Flags, FrameContent) ->
  {aenc, [
    {size, Size},
    Flags | v22_reader:parse_cra_content(FrameContent)
  ]};

parse_frame_bin(<<"ENCR">>, Size, Flags, FrameContent) ->
  case erlp3_utils:get_null_terminated_string_from_frame(FrameContent) of
    {OwnerID, Rest} ->
      <<MethodSym:8/integer, EncData/binary>> = Rest,
      {encr, [
        {size, Size},
        Flags,
        {owner_identifier, erlp3_utils:decode_string(OwnerID)},
        {method_symbol, MethodSym},
        {encryption_data, EncData}
      ]};
    _ ->
      invalid_bytes_detected
  end;

parse_frame_bin(<<"EQUA">>, Size, Flags, FrameContent) ->
  {equa, [
    {size, Size},
    Flags | v22_reader:parse_equ_content(FrameContent)
  ]};

parse_frame_bin(<<"ETCO">>, Size, Flags, FrameContent) ->
  {etco, [
    {size, Size},
    Flags | v22_reader:parse_etc_content(FrameContent)
  ]};

parse_frame_bin(<<"GEOB">>, Size, Flags, FrameContent) ->
  {geob, [
    {size, Size},
    Flags | v22_reader:parse_geo_content(FrameContent)
  ]};

parse_frame_bin(<<"GRID">>, Size, Flags, FrameContent) ->
  case erlp3_utils:get_null_terminated_string_from_frame(FrameContent) of
    {OwnerID, Rest} ->
      <<GroupSymbol:8/integer, GroupDependentData/binary>> = Rest,
      {grid, [
        {size, Size},
        Flags,
        {owner_identifier, erlp3_utils:decode_string(OwnerID)},
        {group_symbol, GroupSymbol},
        {group_dependent_data, GroupDependentData}
      ]}
  end;

parse_frame_bin(<<"IPLS">>, Size, Flags, FrameContent) ->
  {ipls, [
    {size, Size},
    Flags | v22_reader:parse_ipl_content(FrameContent)
  ]};

parse_frame_bin(<<"LINK">>, Size, Flags, FrameContent) ->
  {link, [
    {size, Size},
    Flags | v22_reader:parse_lnk_content(FrameContent)
  ]};

parse_frame_bin(<<"MCDI">>, Size, Flags, <<TOC/binary>>) ->
  {mcdi, [
    {size, Size},
    Flags,
    {table_of_contents, TOC}
  ]};

parse_frame_bin(<<"MLLT">>, Size, Flags, <<FBR:16/integer, BBR:24/integer, MBR:24/integer, BBD:8/integer, BMD:8/integer, DeviationInBytes:BBD/integer, DeviationInMilli:BMD/integer>>) ->
  {mllt, [
    {size, Size},
    Flags,
    {frames_between_reference, FBR},
    {bytes_between_reference, BBR},
    {milliseconds_between_reference, MBR},
    {bit_for_bytes_deviation, BBD},
    {bits_for_milliseconds_deviation, BMD},
    {deviation_in_bytes, DeviationInBytes},
    {deviation_in_milliseconds, DeviationInMilli}
  ]};

parse_frame_bin(<<"OWNE">>, Size, Flags, FrameContent) ->
  {owne, [
    {size, Size},
    Flags | parse_owne_content(FrameContent)
  ]};

parse_frame_bin(<<"PRIV">>, Size, Flags, FrameContent) ->
  case erlp3_utils:get_null_terminated_string_from_frame(FrameContent) of
    {OwnerID, PrivateData} ->
      {priv, [
        {size, Size},
        Flags,
        {owner_identifier, OwnerID},
        {private_data, PrivateData}
      ]};
    _ ->
      invalid_bytes_detected
  end;

parse_frame_bin(<<"POPM">>, Size, Flags, FrameContent) ->
  {popm, [
    {size, Size},
    Flags | v22_reader:parse_pop_content(FrameContent)
  ]};

parse_frame_bin(<<"POSS">>, Size, Flags, <<TimeStampFormat:8/integer, Rest/binary>>) ->
  Len = length(binary_to_list(Rest)) * 8,
  <<Position:Len/integer>> = Rest,
  {poss, [
    {size, Size},
    Flags,
    {timestamp_format, erlp3_utils:time_format_code_to_atom(TimeStampFormat)},
    {position, Position}
  ]};

parse_frame_bin(<<"PCNT">>, Size, Flags, FrameContent) ->
  Bits = Size * 8,
  <<PlayCount:Bits/integer>> = FrameContent,
  {pcnt, [
    {size, Size},
    Flags,
    {counter, PlayCount}
  ]};

parse_frame_bin(<<"RBUF">>, Size, Flags, <<BufferSize:24/integer, EmbeddedInfo:8/integer, NextFlagOffset:32/integer>>) ->
  {rbuf, [
    {size, Size},
    Flags,
    {buffer_size, BufferSize},
    {embedded_info, erlp3_utils:boolean_code_to_atom(EmbeddedInfo)},
    {next_flag_offset, NextFlagOffset}
  ]};

parse_frame_bin(<<"RBUF">>, Size, Flags, <<BufferSize:24/integer, EmbeddedInfo:8/integer>>) ->
  {rbuf, [
    {size, Size},
    Flags,
    {buffer_size, BufferSize},
    {embedded_info, erlp3_utils:boolean_code_to_atom(EmbeddedInfo)}
  ]};

parse_frame_bin(<<"RVAD">>, Size, Flags, <<0:6/integer, IncL:1/integer, IncR:1/integer, Len:8/integer, RVCR:Len/integer, RVCL:Len/integer, PVR:Len/integer, PVL:Len/integer>>) ->
  {rvad, [
    {size, Size},
    Flags,
    {inc_or_dec_right, IncR},
    {inc_or_dec_left, IncL},
    {bits_used_for_volume, Len},
    {relative_volume_change_right, RVCR},
    {relative_voilume_change_left, RVCL},
    {peak_volume_right, PVR},
    {peak_volume_left, PVL}
  ]};

parse_frame_bin(<<"RVAD">>, Size, Flags, <<0:4/integer, IncLB:1/integer, IncRB:1/integer, IncL:1/integer, IncR:1/integer, Len:8/integer, RVCR:Len/integer, RVCL:Len/integer, PVR:Len/integer, PVL:Len/integer,
RVCRB:Len/integer, RVCLB:Len/integer, PVRB:Len/integer, PVLB:Len/integer>>) ->
  {rvad, [
    {size, Size},
    Flags,
    {inc_or_dec_right, IncR},
    {inc_or_dec_left, IncL},
    {inc_or_dec_right_back, IncRB},
    {inc_or_dec_left_back, IncLB},
    {bits_used_for_volume, Len},
    {relative_volume_change_right, RVCR},
    {relative_voilume_change_left, RVCL},
    {peak_volume_right, PVR},
    {peak_volume_left, PVL},
    {relative_volume_change_right_back, RVCRB},
    {relative_voilume_change_left_back, RVCLB},
    {peak_volume_right_back, PVRB},
    {peak_volume_left_back, PVLB}
  ]};

parse_frame_bin(<<"RVAD">>, Size, Flags, <<0:4/integer, IncLB:1/integer, IncRB:1/integer, IncL:1/integer, IncR:1/integer, Len:8/integer, RVCR:Len/integer, RVCL:Len/integer, PVR:Len/integer, PVL:Len/integer,
RVCRB:Len/integer, RVCLB:Len/integer, PVRB:Len/integer, PVLB:Len/integer, RVCC:Len/integer, PVC:Len/integer>>) ->
  {rvad, [
    {size, Size},
    Flags,
    {inc_or_dec_right, IncR},
    {inc_or_dec_left, IncL},
    {inc_or_dec_right_back, IncRB},
    {inc_or_dec_left_back, IncLB},
    {bits_used_for_volume, Len},
    {relative_volume_change_right, RVCR},
    {relative_voilume_change_left, RVCL},
    {peak_volume_right, PVR},
    {peak_volume_left, PVL},
    {relative_volume_change_right_back, RVCRB},
    {relative_voilume_change_left_back, RVCLB},
    {peak_volume_right_back, PVRB},
    {peak_volume_left_back, PVLB},
    {relative_volume_change_center, RVCC},
    {peak_volume_center, PVC}
  ]};

parse_frame_bin(<<"RVAD">>, Size, Flags, <<0:4/integer, IncLB:1/integer, IncRB:1/integer, IncL:1/integer, IncR:1/integer, Len:8/integer, RVCR:Len/integer, RVCL:Len/integer, PVR:Len/integer, PVL:Len/integer,
RVCRB:Len/integer, RVCLB:Len/integer, PVRB:Len/integer, PVLB:Len/integer, RVCC:Len/integer, PVC:Len/integer, RVCB:Len/integer, PVB:Len/integer>>) ->
  {rvad, [
    {size, Size},
    Flags,
    {inc_or_dec_right, IncR},
    {inc_or_dec_left, IncL},
    {inc_or_dec_right_back, IncRB},
    {inc_or_dec_left_back, IncLB},
    {bits_used_for_volume, Len},
    {relative_volume_change_right, RVCR},
    {relative_voilume_change_left, RVCL},
    {peak_volume_right, PVR},
    {peak_volume_left, PVL},
    {relative_volume_change_right_back, RVCRB},
    {relative_voilume_change_left_back, RVCLB},
    {peak_volume_right_back, PVRB},
    {peak_volume_left_back, PVLB},
    {relative_volume_change_center, RVCC},
    {peak_volume_center, PVC},
    {relative_volume_change_bass, RVCB},
    {peak_volume_bass, PVB}
  ]};

parse_frame_bin(<<"RVRB">>, Size, Flags, <<RL:16/integer, RR:16/integer,
RBL:8/integer, RBR:8/integer, RFLTL:8/integer, RFLTR:8/integer,
RFRTR:8/integer, RFRTL:8/integer, PLTR:8/integer, PRTL:8/integer>>) ->
  {rev, [
    {size, Size},
    Flags,
    {reveb_left, RL},
    {reverb_right, RR},
    {reverb_bounces_left, RBL},
    {reverb_bounces_right, RBR},
    {reverb_feedback_left_to_left, RFLTL},
    {reverb_feedback_left_to_right, RFLTR},
    {reverb_feedback_right_to_right, RFRTR},
    {reverb_feedback_right_to_left, RFRTL},
    {premix_left_to_right, PLTR},
    {premix_right_to_left, PRTL}
  ]};

parse_frame_bin(<<"SYLT">>, Size, Flags, <<Enc:8/integer, Language:3/binary, TimeStampFormat:8/integer, ContentType:8/integer, Rest/binary>>) ->
  {sylt, [
    {size, Size},
    Flags,
    {encoding, Enc},
    {language, erlp3_utils:decode_string(Language)},
    {timestamp_format, erlp3_utils:time_format_code_to_atom(TimeStampFormat)},
    {content_type, erlp3_utils:slt_content_type_code_to_atom(ContentType)},
    {content_descriptor, erlp3_utils:decode_string(Enc, Rest)}
  ]};

parse_frame_bin(<<"SYTC">>, Size, Flags, <<TimeStampFormat:8/integer, TempoData/binary>>) ->
  {sytc, [
    {size, Size},
    Flags,
    {timestamp_format, erlp3_utils:time_format_code_to_atom(TimeStampFormat)},
    {tempo_data, TempoData}
  ]};

parse_frame_bin(<<"UFID">>, Size, Flags, FrameContent) ->
  case erlp3_utils:get_null_terminated_string_from_frame(FrameContent) of
    {OwnerID, Identifier} ->
      {ufid, [
        {size, Size},
        Flags,
        {owner_identifier, erlp3_utils:decode_string(OwnerID)},
        {identifier, Identifier}
      ]};
    _ ->
      invalid_bytes_detected
  end;

parse_frame_bin(<<"USER">>, Size, Flags, <<Enc:8/integer, Language:3/binary, TermsOfUse/binary>>) ->
  {user, [
    {size, Size},
    Flags,
    {encoding, Enc},
    {language, erlp3_utils:decode_string(Language)},
    {terms_of_use, erlp3_utils:decode_string(Enc, TermsOfUse)}
  ]
  };

parse_frame_bin(<<"USLT">>, Size, Flags, FrameContent) ->
  {uslt, [
    {size, Size},
    Flags | parse_uslt_content(FrameContent)
  ]};

parse_frame_bin(<<$T, _T2:1/binary, _T3:1/binary, _T4:1/binary>> = Header, Size, Flags, <<Enc:8/integer, Rest/binary>>) ->
  TextData = case erlp3_utils:get_null_terminated_string_from_frame(Rest) of
               {Data, _Rem} ->
                 Data;
               _ ->
                 Rest
             end,
  {erlp3_utils:header_to_atom(erlp3_utils:decode_string(Header)), [
    {size, Size},
    Flags,
    {encoding, Enc},
    {textstring, erlp3_utils:decode_string(Enc, TextData)}
  ]};

parse_frame_bin(<<$W, _W2:1/binary, _W3:1/binary, _W4:1/binary>> = Header, Size, Flags, <<URL/binary>>) ->
  {erlp3_utils:header_to_atom(erlp3_utils:decode_string(Header)), [
    {size, Size},
    Flags,
    {url, erlp3_utils:decode_string(URL)}
  ]};

parse_frame_bin(_FID, _Size, _Flags, _FrameContent) ->
  not_yet_implemented.

parse_uslt_content(<<Enc:8/integer, Language:3/binary, Rest/binary>>) ->
  case erlp3_utils:get_null_terminated_string_from_frame(Rest) of
    {ContentDesc, Lyrics} ->
      [
        {encoding, Enc},
        {language, erlp3_utils:decode_string(Language)},
        {content_descriptor, erlp3_utils:decode_string(Enc, ContentDesc)},
        {lyrics, erlp3_utils:decode_string(Enc, Lyrics)}
      ];
    _ ->
      invalid_bytes_detected
  end.

parse_owne_content(<<Enc:8/integer, FrameContent/binary>>) ->
  case erlp3_utils:get_null_terminated_string_from_frame(FrameContent) of
    {PricePayed, Rest} ->
      <<DateOfPurchase:8/binary, Seller/binary>> = Rest,
      [
        {encoding, Enc},
        {price_payed, erlp3_utils:decode_string(PricePayed)},
        {date_of_purchase, erlp3_utils:decode_string(DateOfPurchase)},
        {seller, erlp3_utils:decode_string(Enc, Seller)}
      ]
  end.

parse_comr_content(<<Enc:8/integer, FrameContent/binary>>) ->
  case erlp3_utils:get_null_terminated_string_from_frame(FrameContent) of
    {PriceStr, Rest} ->
      <<ValidUntil:8/binary, Rest2/binary>> = Rest,
      case erlp3_utils:get_null_terminated_string_from_frame(Rest2) of
        {ContactURL, Rest3} ->
          <<RecievedAs:8/integer, Rest4/binary>> = Rest3,
          case erlp3_utils:get_null_terminated_string_from_frame_skip_zeros(Rest4) of
            {NameOfSeller, Rest5} ->
              case erlp3_utils:get_null_terminated_string_from_frame_skip_zeros(Rest5) of
                {Description, Rest6} ->
                  case erlp3_utils:get_null_terminated_string_from_frame(Rest6) of
                    {PictureMimeType, SellerLogo} ->
                      [
                        {encoding, Enc},
                        {price_string, erlp3_utils:decode_string(PriceStr)},
                        {valid_until, erlp3_utils:decode_string(ValidUntil)},
                        {contact_url, erlp3_utils:decode_string(ContactURL)},
                        {recieved_as, erlp3_utils:recieved_as_code_to_atom(RecievedAs)},
                        {name_of_seller, erlp3_utils:decode_string(Enc, NameOfSeller)},
                        {description, erlp3_utils:decode_string(Enc, Description)},
                        {picture_mime_type, erlp3_utils:decode_string(PictureMimeType)},
                        {seller_logo, SellerLogo}
                      ];
                    _ ->
                      invalid_bytes_detected
                  end;
                _ ->
                  invalid_bytes_detected
              end;
            _ ->
              invalid_bytes_detected
          end;
        _ ->
          invalid_bytes_detected
      end;
    _ ->
      invalid_bytes_detected
  end.

parse_pic_content(<<Enc:8/integer, Rest/binary>>) ->
  case erlp3_utils:get_null_terminated_string_from_frame(Rest) of
    {MimeType, Rest2} ->
      <<PictureType:8/integer, Rest3/binary>> = Rest2,
      case erlp3_utils:get_null_terminated_string_from_frame(Rest3) of
        {Description, BinaryData} ->
          [
            {encoding, Enc},
            {mime_type, erlp3_utils:decode_string(MimeType)},
            {picture_type, erlp3_utils:pic_type_code_to_atom(PictureType)},
            {description, erlp3_utils:decode_string(Enc, Description)},
            {picture_data, BinaryData}
          ];
        _ ->
          invalid_bytes_detected
      end;
    _ ->
      invalid_bytes_detected
  end.
