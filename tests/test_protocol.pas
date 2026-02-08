{
  test_protocol.pas - Unit tests for Peer Wire Protocol
  
  Tests message encoding/decoding, handshake handling, and all protocol operations.
}

program test_protocol;

{$mode objfpc}{$H+}

uses
  SysUtils, protocol, sha1utils, utils, testframework;

{ ============================================================================ }
{ Handshake Tests                                                              }
{ ============================================================================ }

procedure TestHandshakeEncodeDecode;
var
  InfoHash: array[0..19] of Byte;
  PeerId: array[0..19] of Byte;
  Buffer: array[0..127] of Byte;
  HS: THandshake;
  Len: Integer;
  I: Integer;
begin
  WriteLn(#10'=== Testing Handshake Encoding/Decoding ===');
  
  { Setup test data }
  for I := 0 to 19 do
  begin
    InfoHash[I] := I;
    PeerId[I] := 255 - I;
  end;
  
  { Test 1: Encode handshake }
  FillChar(Buffer, SizeOf(Buffer), 0);
  Len := EncodeHandshake(InfoHash, PeerId, @Buffer, SizeOf(Buffer));
  TestResult('Encode handshake returns HANDSHAKE_LEN', 
             Len = HANDSHAKE_LEN);
  
  { Test 2: Decode handshake }
  FillChar(HS, SizeOf(HS), 0);
  TestResult('Decode handshake succeeds', 
             DecodeHandshake(@Buffer, Len, HS));
  
  { Test 3: Verify protocol length }
  TestResult('Protocol length is 19', 
             HS.ProtocolLen = PROTOCOL_LEN);
  
  { Test 4: Verify protocol string }
  TestResult('Protocol string matches', 
             StrPas(HS.Protocol) = PROTOCOL_STRING);
  
  { Test 5: Verify info hash }
  TestResult('Info hash matches', 
             CompareMem(@InfoHash, @HS.InfoHash, SHA1_HASH_SIZE));
  
  { Test 6: Verify peer ID }
  TestResult('Peer ID matches', 
             CompareMem(@PeerId, @HS.PeerId, SHA1_HASH_SIZE));
  
  { Test 7: Validate handshake }
  TestResult('Handshake validation succeeds', 
             ValidateHandshake(HS));
end;

procedure TestHandshakeValidation;
var
  HS: THandshake;
  Buffer: array[0..67] of Byte;
  I: Integer;
  ValidInfoHash: array[0..19] of Byte;
  ValidPeerId: array[0..19] of Byte;
begin
  WriteLn(#10'=== Testing Handshake Validation ===');
  
  { Setup valid data }
  for I := 0 to 19 do
  begin
    ValidInfoHash[I] := I + 1;  { Non-zero }
    ValidPeerId[I] := I;
  end;
  
  { Test 1: Valid handshake }
  EncodeHandshake(ValidInfoHash, ValidPeerId, @Buffer, SizeOf(Buffer));
  DecodeHandshake(@Buffer, SizeOf(Buffer), HS);
  TestResult('Valid handshake passes', 
             ValidateHandshake(HS));
  
  { Test 2: Invalid protocol length }
  HS.ProtocolLen := 18;
  TestResult('Wrong protocol length fails', 
             not ValidateHandshake(HS));
  HS.ProtocolLen := PROTOCOL_LEN;  { Restore }
  
  { Test 3: Invalid protocol string }
  HS.Protocol[0] := 'X';
  TestResult('Wrong protocol string fails', 
             not ValidateHandshake(HS));
  
  { Test 4: Zero info hash }
  FillChar(ValidInfoHash, SizeOf(ValidInfoHash), 0);
  EncodeHandshake(ValidInfoHash, ValidPeerId, @Buffer, SizeOf(Buffer));
  DecodeHandshake(@Buffer, SizeOf(Buffer), HS);
  TestResult('Zero info hash fails', 
             not ValidateHandshake(HS));
  
  { Test 5: Buffer too small }
  EncodeHandshake(ValidInfoHash, ValidPeerId, @Buffer, SizeOf(Buffer));
  FillChar(HS, SizeOf(HS), 0);
  TestResult('Buffer too small fails decode', 
             not DecodeHandshake(@Buffer, 10, HS));
end;

procedure TestHandshakeEdgeCases;
var
  InfoHash: array of Byte;
  PeerId: array of Byte;
  FixedInfoHash: array[0..19] of Byte;
  FixedPeerId: array[0..19] of Byte;
  Buffer: array[0..67] of Byte;
  Len: Integer;
  I: Integer;
begin
  WriteLn(#10'=== Testing Handshake Edge Cases ===');
  
  { Setup fixed arrays }
  for I := 0 to 19 do
  begin
    FixedInfoHash[I] := I;
    FixedPeerId[I] := I;
  end;
  
  { Test 1: nil buffer }
  Len := EncodeHandshake(FixedInfoHash, FixedPeerId, nil, SizeOf(Buffer));
  TestResult('nil buffer returns 0', 
             Len = 0);
  
  { Test 2: Buffer too small }
  Len := EncodeHandshake(FixedInfoHash, FixedPeerId, @Buffer, 10);
  TestResult('Small buffer returns 0', 
             Len = 0);
  
  { Test 3: Wrong info hash size (dynamic array) }
  SetLength(InfoHash, 19);  { Wrong size }
  SetLength(PeerId, 20);
  Len := EncodeHandshake(InfoHash, PeerId, @Buffer, SizeOf(Buffer));
  TestResult('Wrong info hash size returns 0', 
             Len = 0);
  
  { Test 4: Wrong peer ID size }
  SetLength(InfoHash, 20);
  SetLength(PeerId, 21);  { Wrong size }
  Len := EncodeHandshake(InfoHash, PeerId, @Buffer, SizeOf(Buffer));
  TestResult('Wrong peer ID size returns 0', 
             Len = 0);
end;

{ ============================================================================ }
{ Message Encoding Tests                                                       }
{ ============================================================================ }

procedure TestSimpleMessages;
var
  Buffer: array[0..63] of Byte;
  Len: Integer;
  Msg: TWireMessage;
  Decoded: TWireMessage;
  Consumed: Integer;
  Success: Boolean;
begin
  WriteLn(#10'=== Testing Simple Messages (Choke/Unchoke/Interested) ===');
  
  { Test 1: Choke }
  TestResult('BuildChoke with valid buffer', 
             BuildChoke(@Buffer, SizeOf(Buffer), Len));
  TestResult('Choke message is 5 bytes', 
             Len = 5);
  Success := DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Choke decodes successfully', 
             Success and (Decoded.MsgId = MSG_CHOKE));
  
  { Test 1b: Choke with small buffer fails }
  TestResult('BuildChoke with small buffer fails', 
             not BuildChoke(@Buffer, 4, Len));
  
  { Test 2: Unchoke }
  TestResult('BuildUnchoke with valid buffer', 
             BuildUnchoke(@Buffer, SizeOf(Buffer), Len));
  TestResult('Unchoke message is 5 bytes', 
             Len = 5);
  Success := DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Unchoke decodes successfully', 
             Success and (Decoded.MsgId = MSG_UNCHOKE));
  
  { Test 3: Interested }
  TestResult('BuildInterested with valid buffer', 
             BuildInterested(@Buffer, SizeOf(Buffer), Len));
  TestResult('Interested message is 5 bytes', 
             Len = 5);
  Success := DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Interested decodes successfully', 
             Success and (Decoded.MsgId = MSG_INTERESTED));
  
  { Test 4: Not Interested }
  TestResult('BuildNotInterested with valid buffer', 
             BuildNotInterested(@Buffer, SizeOf(Buffer), Len));
  TestResult('Not Interested message is 5 bytes', 
             Len = 5);
  Success := DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Not Interested decodes successfully', 
             Success and (Decoded.MsgId = MSG_NOT_INTERESTED));
  
  { Test 5: Keep-alive }
  Msg.Length := 0;
  Msg.MsgId := 255;
  Success := EncodeMessage(Msg, @Buffer, SizeOf(Buffer), Len);
  TestResult('Keep-alive encodes successfully', 
             Success and (Len = 4));
  Success := DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Keep-alive decodes successfully', 
             Success and (Decoded.Length = 0));
end;

procedure TestHaveMessage;
var
  Buffer: array[0..63] of Byte;
  Len: Integer;
  Decoded: TWireMessage;
  Consumed: Integer;
  Success: Boolean;
begin
  WriteLn(#10'=== Testing Have Message ===');
  
  { Test 1: Build and decode Have(42) }
  TestResult('BuildHave with valid buffer', 
             BuildHave(42, @Buffer, SizeOf(Buffer), Len));
  TestResult('Have message is 9 bytes', 
             Len = 9);
  
  Success := DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Have decodes successfully', 
             Success and (Decoded.MsgId = MSG_HAVE));
  TestResult('Have piece index is 42', 
             Decoded.HaveIndex = 42);
  
  { Test 2: Large piece index }
  TestResult('BuildHave with large index', 
             BuildHave(1000000, @Buffer, SizeOf(Buffer), Len));
  DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Have with large index decodes correctly', 
             Decoded.HaveIndex = 1000000);
  
  { Test 3: Piece index 0 }
  TestResult('BuildHave with index 0', 
             BuildHave(0, @Buffer, SizeOf(Buffer), Len));
  DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Have with index 0 decodes correctly', 
             Decoded.HaveIndex = 0);
  
  { Test 4: Buffer too small }
  TestResult('BuildHave with small buffer fails', 
             not BuildHave(0, @Buffer, 8, Len));
end;

procedure TestRequestMessage;
var
  Buffer: array[0..63] of Byte;
  Len: Integer;
  Decoded: TWireMessage;
  Consumed: Integer;
  Success: Boolean;
begin
  WriteLn(#10'=== Testing Request Message ===');
  
  { Valid request tests }
  TestResult('BuildRequest with valid buffer', 
             BuildRequest(5, 16384, 16384, @Buffer, SizeOf(Buffer), Len));
  TestResult('Request message is 17 bytes', 
             Len = 17);
  
  Success := DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Request decodes successfully', 
             Success and (Decoded.MsgId = MSG_REQUEST));
  TestResult('Request index is 5', 
             Decoded.ReqIndex = 5);
  TestResult('Request begin is 16384', 
             Decoded.ReqBegin = 16384);
  TestResult('Request length is 16384', 
             Decoded.ReqLength = 16384);
  
  { Error case tests - buffer too small }
  TestResult('BuildRequest rejects nil buffer', 
             not BuildRequest(0, 0, 16384, nil, 100, Len));
  TestResult('BuildRequest rejects small buffer', 
             not BuildRequest(0, 0, 16384, @Buffer, 16, Len));
end;

procedure TestPieceMessage;
var
  Buffer: array[0..65535] of Byte;
  Data: array[0..16383] of Byte;
  Len: Integer;
  I: Integer;
  Decoded: TWireMessage;
  Consumed: Integer;
  Success: Boolean;
begin
  WriteLn(#10'=== Testing Piece Message ===');
  
  { Setup test data }
  for I := 0 to 16383 do
    Data[I] := I mod 256;
  
  { Test 1: Build and decode piece }
  Success := BuildPiece(3, 32768, @Data, 16384, @Buffer, SizeOf(Buffer), Len);
  TestResult('Piece message builds successfully', 
             Success);
  TestResult('Piece message size correct', 
             Len = 13 + 16384);
  
  DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Piece decodes successfully', 
             Decoded.MsgId = MSG_PIECE);
  TestResult('Piece index is 3', 
             Decoded.PieceIndex = 3);
  TestResult('Piece begin is 32768', 
             Decoded.PieceBegin = 32768);
  TestResult('Piece data length is 16384', 
             Decoded.PieceDataLen = 16384);
  TestResult('Piece data matches', 
             CompareMem(@Data, Decoded.PieceData, 16384));
  
  { Test 2: Empty piece data }
  Success := BuildPiece(0, 0, nil, 0, @Buffer, SizeOf(Buffer), Len);
  TestResult('Empty piece builds successfully', 
             Success and (Len = 13));
  
  { Test 3: Buffer too small }
  Success := BuildPiece(0, 0, @Data, 16384, @Buffer, 100, Len);
  TestResult('Small buffer fails', 
             not Success);
end;

procedure TestBitfieldMessage;
var
  Buffer: array[0..1023] of Byte;
  Bitfield: array[0..63] of Byte;
  Len: Integer;
  I: Integer;
  Decoded: TWireMessage;
  Consumed: Integer;
  Success: Boolean;
begin
  WriteLn(#10'=== Testing Bitfield Message ===');
  
  { Setup bitfield }
  for I := 0 to 63 do
    Bitfield[I] := I * 3 mod 256;
  
  { Test 1: Build and decode }
  Success := BuildBitfield(@Bitfield, 64, @Buffer, SizeOf(Buffer), Len);
  TestResult('Bitfield message builds successfully', 
             Success);
  TestResult('Bitfield message size correct', 
             Len = 5 + 64);
  
  DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Bitfield decodes successfully', 
             Decoded.MsgId = MSG_BITFIELD);
  TestResult('Bitfield length is 64', 
             Decoded.BitfieldLen = 64);
  TestResult('Bitfield data matches', 
             CompareMem(@Bitfield, Decoded.BitfieldData, 64));
  
  { Test 2: Empty bitfield }
  Success := BuildBitfield(nil, 0, @Buffer, SizeOf(Buffer), Len);
  TestResult('Empty bitfield builds successfully', 
             Success and (Len = 5));
  
  { Test 3: Buffer too small }
  Success := BuildBitfield(@Bitfield, 64, @Buffer, 50, Len);
  TestResult('Small buffer fails', 
             not Success);
end;

procedure TestCancelMessage;
var
  Buffer: array[0..63] of Byte;
  Len: Integer;
  Decoded: TWireMessage;
  Consumed: Integer;
begin
  WriteLn(#10'=== Testing Cancel Message ===');
  
  TestResult('BuildCancel with valid buffer', 
             BuildCancel(10, 65536, 16384, @Buffer, SizeOf(Buffer), Len));
  TestResult('Cancel message is 17 bytes', 
             Len = 17);
  
  DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Cancel decodes successfully', 
             Decoded.MsgId = MSG_CANCEL);
  TestResult('Cancel index is 10', 
             Decoded.CancelIndex = 10);
  TestResult('Cancel begin is 65536', 
             Decoded.CancelBegin = 65536);
  TestResult('Cancel length is 16384', 
             Decoded.CancelLength = 16384);
end;

procedure TestPortMessage;
var
  Buffer: array[0..63] of Byte;
  Len: Integer;
  Decoded: TWireMessage;
  Consumed: Integer;
begin
  WriteLn(#10'=== Testing Port Message ===');
  
  TestResult('BuildPort with valid buffer', 
             BuildPort(6881, @Buffer, SizeOf(Buffer), Len));
  TestResult('Port message is 7 bytes', 
             Len = 7);
  
  DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('Port decodes successfully', 
             Decoded.MsgId = MSG_PORT);
  TestResult('Port is 6881', 
             Decoded.DhtPort = 6881);
  
  { Test high port }
  TestResult('BuildPort with high port', 
             BuildPort(65535, @Buffer, SizeOf(Buffer), Len));
  DecodeMessage(@Buffer, Len, Decoded, Consumed);
  TestResult('High port decodes correctly', 
             Decoded.DhtPort = 65535);
end;

{ ============================================================================ }
{ Message Decoding Edge Cases                                                  }
{ ============================================================================ }

procedure TestMessageCompleteness;
var
  Buffer: array[0..63] of Byte;
  Complete: Integer;
begin
  WriteLn(#10'=== Testing Message Completeness Check ===');
  
  { Test 1: Empty buffer }
  Complete := MessageComplete(nil, 0);
  TestResult('nil buffer returns 0', 
             Complete = 0);
  
  { Test 2: Partial length prefix }
  Complete := MessageComplete(@Buffer, 2);
  TestResult('Partial length prefix returns 0', 
             Complete = 0);
  
  { Test 3: Keep-alive complete }
  Buffer[0] := 0; Buffer[1] := 0; Buffer[2] := 0; Buffer[3] := 0;
  Complete := MessageComplete(@Buffer, 4);
  TestResult('Keep-alive complete returns 4', 
             Complete = 4);
  
  { Test 4: Complete choke message }
  Buffer[0] := 0; Buffer[1] := 0; Buffer[2] := 0; Buffer[3] := 1;
  Buffer[4] := MSG_CHOKE;
  Complete := MessageComplete(@Buffer, 5);
  TestResult('Complete choke returns 5', 
             Complete = 5);
  
  { Test 5: Incomplete message }
  Buffer[0] := 0; Buffer[1] := 0; Buffer[2] := 0; Buffer[3] := 13;
  Buffer[4] := MSG_REQUEST;
  Complete := MessageComplete(@Buffer, 10);  { Need 17 bytes }
  TestResult('Incomplete message returns 0', 
             Complete = 0);
end;

procedure TestInvalidMessages;
var
  Buffer: array[0..63] of Byte;
  Msg: TWireMessage;
  Consumed: Integer;
  Success: Boolean;
begin
  WriteLn(#10'=== Testing Invalid Message Handling ===');
  
  { Test 1: Invalid message ID }
  Buffer[0] := 0; Buffer[1] := 0; Buffer[2] := 0; Buffer[3] := 1;
  Buffer[4] := 100;  { Invalid ID }
  Success := DecodeMessage(@Buffer, 5, Msg, Consumed);
  TestResult('Invalid message ID fails', 
             not Success);
  
  { Test 2: nil buffer }
  Success := DecodeMessage(nil, 10, Msg, Consumed);
  TestResult('nil buffer fails', 
             not Success);
  
  { Test 3: Empty decode }
  FillChar(Msg, SizeOf(Msg), $FF);
  Success := DecodeMessage(@Buffer, 0, Msg, Consumed);
  TestResult('Empty buffer fails', 
             not Success);
end;

{ ============================================================================ }
{ Utility Function Tests                                                       }
{ ============================================================================ }

procedure TestUtilityFunctions;
begin
  WriteLn(#10'=== Testing Utility Functions ===');
  
  { Test 1: Message names }
  TestResult('MSG_CHOKE name is "Choke"', 
             MessageName(MSG_CHOKE) = 'Choke');
  TestResult('MSG_PIECE name is "Piece"', 
             MessageName(MSG_PIECE) = 'Piece');
  TestResult('Keep-alive name is "Keep-Alive"', 
             MessageName(255) = 'Keep-Alive');
  TestResult('Invalid ID name is "Unknown"', 
             MessageName(100) = 'Unknown');
  
  { Test 2: Valid message IDs }
  TestResult('MSG_CHOKE is valid', 
             IsValidMessageId(MSG_CHOKE));
  TestResult('MSG_PORT is valid', 
             IsValidMessageId(MSG_PORT));
  TestResult('100 is not valid', 
             not IsValidMessageId(100));
  
  { Test 3: Bitfield size calculation }
  TestResult('BitfieldBytes(0) = 0', 
             BitfieldBytes(0) = 0);
  TestResult('BitfieldBytes(1) = 1', 
             BitfieldBytes(1) = 1);
  TestResult('BitfieldBytes(8) = 1', 
             BitfieldBytes(8) = 1);
  TestResult('BitfieldBytes(9) = 2', 
             BitfieldBytes(9) = 2);
  TestResult('BitfieldBytes(100) = 13', 
             BitfieldBytes(100) = 13);
  TestResult('BitfieldBytes(-1) = 0', 
             BitfieldBytes(-1) = 0);
end;

{ ============================================================================ }
{ Round-Trip Tests                                                             }
{ ============================================================================ }

procedure TestRoundTrips;
var
  Buffer: array[0..65535] of Byte;
  EncodedMsg: TWireMessage;
  DecodedMsg: TWireMessage;
  BytesWritten: Integer;
  BytesConsumed: Integer;
  Data: array[0..1023] of Byte;
  I: Integer;
  Success: Boolean;
  DecodeSuccess: Boolean;
begin
  WriteLn(#10'=== Testing Encode/Decode Round-Trips ===');
  
  for I := 0 to 1023 do
    Data[I] := I mod 256;
  
  { Test 1: Have round-trip }
  EncodedMsg.Length := 5;
  EncodedMsg.MsgId := MSG_HAVE;
  EncodedMsg.HaveIndex := 12345;
  Success := EncodeMessage(EncodedMsg, @Buffer, SizeOf(Buffer), BytesWritten);
  DecodeSuccess := DecodeMessage(@Buffer, BytesWritten, DecodedMsg, BytesConsumed);
  TestResult('Have round-trip encode successful', Success);
  TestResult('Have round-trip decode successful', DecodeSuccess);
  TestResult('Have round-trip data correct', 
             DecodeSuccess and (DecodedMsg.HaveIndex = 12345));
  
  { Test 2: Request round-trip }
  EncodedMsg.Length := 13;
  EncodedMsg.MsgId := MSG_REQUEST;
  EncodedMsg.ReqIndex := 99;
  EncodedMsg.ReqBegin := 262144;
  EncodedMsg.ReqLength := 16384;
  Success := EncodeMessage(EncodedMsg, @Buffer, SizeOf(Buffer), BytesWritten);
  DecodeSuccess := DecodeMessage(@Buffer, BytesWritten, DecodedMsg, BytesConsumed);
  TestResult('Request round-trip encode successful', Success);
  TestResult('Request round-trip decode successful', DecodeSuccess);
  TestResult('Request round-trip data correct', 
             DecodeSuccess and (DecodedMsg.ReqIndex = 99) and 
             (DecodedMsg.ReqBegin = 262144) and (DecodedMsg.ReqLength = 16384));
  
  { Test 3: Bitfield round-trip }
  EncodedMsg.Length := 1 + 64;
  EncodedMsg.MsgId := MSG_BITFIELD;
  EncodedMsg.BitfieldData := @Data;
  EncodedMsg.BitfieldLen := 64;
  Success := EncodeMessage(EncodedMsg, @Buffer, SizeOf(Buffer), BytesWritten);
  DecodeSuccess := DecodeMessage(@Buffer, BytesWritten, DecodedMsg, BytesConsumed);
  TestResult('Bitfield round-trip encode successful', Success);
  TestResult('Bitfield round-trip decode successful', DecodeSuccess);
  TestResult('Bitfield round-trip data correct', 
             DecodeSuccess and (DecodedMsg.BitfieldLen = 64) and
             CompareMem(@Data, DecodedMsg.BitfieldData, 64));
end;

{ ============================================================================ }
{ Main Program                                                                 }
{ ============================================================================ }

begin
  BeginSuite('PEER WIRE PROTOCOL UNIT TESTS');
  
  { Handshake tests }
  TestHandshakeEncodeDecode;
  TestHandshakeValidation;
  TestHandshakeEdgeCases;
  
  { Message tests }
  TestSimpleMessages;
  TestHaveMessage;
  TestRequestMessage;
  TestPieceMessage;
  TestBitfieldMessage;
  TestCancelMessage;
  TestPortMessage;
  
  { Edge cases }
  TestMessageCompleteness;
  TestInvalidMessages;
  
  { Utilities }
  TestUtilityFunctions;
  
  { Round-trips }
  TestRoundTrips;
  
  EndSuite;
  ExitWithResult;
end.
