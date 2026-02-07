{
  metainfo.pas - Torrent metadata parser for PascalTorrent
  
  This unit handles parsing of .torrent files and management of
  torrent metadata including files, pieces, and tracker information.
  
  Uses only imperative/procedural programming - no OOP.
}

unit metainfo;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, bencode, sha1utils, utils, logging;

const
  { Maximum piece size (typically 1-4 MB in practice) }
  MAX_PIECE_LENGTH = 16 * 1024 * 1024;  { 16 MB }
  
  { Default piece length if not specified }
  DEFAULT_PIECE_LENGTH = 262144;  { 256 KB }

type
  { Forward declarations }
  PFileEntry = ^TFileEntry;
  PTorrentMeta = ^TTorrentMeta;
  PTrackerInfo = ^TTrackerInfo;
  
  { File entry for multi-file torrents }
  TFileEntry = record
    Path: string;           { Full path relative to torrent name }
    Filename: string;       { Just the filename }
    Length: QWord;          { File size in bytes }
    Md5Sum: string;         { Optional MD5 checksum }
    Offset: QWord;          { Starting offset in the torrent data }
    Next: PFileEntry;       { Linked list }
  end;
  
  { Tracker information }
  TTrackerInfo = record
    Url: string;            { Tracker URL }
    Tier: Integer;          { Priority tier (lower = higher priority) }
    Protocol: (tpHTTP, tpUDP);  { Tracker protocol }
    Next: PTrackerInfo;     { Linked list }
  end;
  
  { Piece information }
  PPieceInfo = ^TPieceInfo;
  TPieceInfo = record
    Index: Integer;         { Piece index (0-based) }
    Length: Cardinal;       { Piece length (may differ for last piece) }
    Hash: TSHA1Digest;      { Expected SHA1 hash }
    State: (psMissing, psDownloading, psComplete, psVerified);
    Next: PPieceInfo;       { Linked list }
  end;
  
  { Torrent metadata structure }
  TTorrentMeta = record
    { Info dictionary fields }
    PieceLength: Cardinal;  { Size of each piece in bytes }
    Pieces: PByteArray;     { Concatenated SHA1 hashes (20 bytes each) }
    PieceCount: Integer;    { Number of pieces }
    
    { Single file mode fields }
    Name: string;           { Name of file or directory }
    Length: QWord;          { Total length (single file) }
    Md5Sum: string;         { Optional MD5 checksum (single file) }
    
    { Multi-file mode fields }
    Files: PFileEntry;      { Linked list of files }
    FileCount: Integer;     { Number of files }
    
    { Common fields }
    PrivateFlag: Boolean;   { If true, don't use DHT/PEX }
    Source: string;         { Optional source field }
    
    { Non-info fields }
    Announce: string;       { Primary tracker URL }
    Trackers: PTrackerInfo; { Linked list of trackers (from announce-list) }
    TrackerCount: Integer;  { Total number of trackers }
    Comment: string;        { Optional comment }
    CreatedBy: string;     { Creator software info }
    CreationDate: Int64;    { Unix timestamp }
    Encoding: string;       { Character encoding (e.g., 'UTF-8') }
    
    { Computed fields }
    InfoHash: TSHA1Digest;  { SHA1 hash of the info dictionary }
    IsSingleFile: Boolean;  { True if single file mode }
    TotalLength: QWord;     { Total size of all files }
    
    { Raw data (for re-encoding if needed) }
    RawInfo: PBencodeValue; { The info dictionary (retained for hashing) }
  end;
  
  { Parse result }
  TMetainfoResult = record
    Success: Boolean;
    ErrorMsg: string;
  end;

{ ============================================================================ }
{ Torrent File Parsing                                                         }
{ ============================================================================ }

{ Parse a .torrent file from disk }
function ParseTorrentFile(const Filename: string; 
                          out Meta: PTorrentMeta): TMetainfoResult;

{ Parse torrent data from a buffer }
function ParseTorrentBuffer(const Buffer: PChar; Len: Integer;
                            out Meta: PTorrentMeta): TMetainfoResult;

{ Parse torrent data from a bencode value (already decoded) }
function ParseTorrentBencode(Root: PBencodeValue;
                             out Meta: PTorrentMeta): TMetainfoResult;

{ Free all memory associated with a torrent metadata structure }
procedure FreeTorrentMeta(Meta: PTorrentMeta);

{ ============================================================================ }
{ Info Hash Computation                                                        }
{ ============================================================================ }

{ Compute the info hash from a bencode info dictionary }
function ComputeInfoHashFromBencode(InfoDict: PBencodeValue;
                                    out Hash: TSHA1Digest): Boolean;

{ Get the info hash as a hex string }
function GetInfoHashHex(Meta: PTorrentMeta): string;

{ Get the info hash URL-encoded for tracker announces }
function GetInfoHashURLEncoded(Meta: PTorrentMeta): string;

{ ============================================================================ }
{ Piece Access                                                                 }
{ ============================================================================ }

{ Get the expected hash for a specific piece }
function GetPieceHash(Meta: PTorrentMeta; PieceIndex: Integer): PSHA1Digest;

{ Get the length of a specific piece (handles last piece correctly) }
function GetPieceLength(Meta: PTorrentMeta; PieceIndex: Integer): Cardinal;

{ Get the total number of bytes in all pieces (may include padding) }
function GetTotalPiecesLength(Meta: PTorrentMeta): QWord;

{ ============================================================================ }
{ File Access                                                                  }
{ ============================================================================ }

{ Find which file contains a specific byte offset }
function FindFileAtOffset(Meta: PTorrentMeta; Offset: QWord;
                          out FileEntry: PFileEntry): Boolean;

{ Get the full path for a file entry (combines download path with file path) }
function GetFullFilePath(Meta: PTorrentMeta; FileEntry: PFileEntry;
                         const BasePath: string): string;

{ Calculate byte offset within a file for a given torrent offset }
function GetFileOffsetForTorrentOffset(Meta: PTorrentMeta; 
                                       FileEntry: PFileEntry;
                                       TorrentOffset: QWord): QWord;

{ ============================================================================ }
{ Utility Functions                                                            }
{ ============================================================================ }

{ Validate a torrent metadata structure }
function ValidateTorrentMeta(Meta: PTorrentMeta; out ErrorMsg: string): Boolean;

{ Get torrent info as a human-readable string }
function TorrentMetaToString(Meta: PTorrentMeta): string;

{ Check if a torrent is a single-file torrent }
function IsSingleFileTorrent(Meta: PTorrentMeta): Boolean;

{ Get the completion percentage from a bitfield }
function GetCompletionFromBitfield(Bitfield: PByteArray; 
                                   BitfieldSize: Integer;
                                   PieceCount: Integer): Double;

{ Sanitize a file path to prevent directory traversal attacks }
function SanitizePath(const Path: string): string;

{ Check if a path contains traversal sequences }
function ContainsTraversal(const Path: string): Boolean;

implementation

{ ============================================================================ }
{ Helper Functions                                                             }
{ ============================================================================ }

{ Create a new file entry }
function NewFileEntry: PFileEntry;
begin
  New(Result);
  if Result <> nil then
  begin
    Result^.Path := '';
    Result^.Filename := '';
    Result^.Length := 0;
    Result^.Md5Sum := '';
    Result^.Offset := 0;
    Result^.Next := nil;
  end;
end;

{ Free a linked list of file entries }
procedure FreeFileEntries(Head: PFileEntry);
var
  Current, Next: PFileEntry;
begin
  Current := Head;
  while Current <> nil do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;
end;

{ Free a linked list of tracker entries }
procedure FreeTrackerEntries(Head: PTrackerInfo);
var
  Current, Next: PTrackerInfo;
begin
  Current := Head;
  while Current <> nil do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;
end;

{ Sanitize a file path to prevent directory traversal attacks }
function SanitizePath(const Path: string): string;
var
  Components: array of string;
  CompCount: Integer;
  I, Start: Integer;
  Comp: string;
begin
  Result := '';
  if Path = '' then Exit;
  
  { Count path components }
  CompCount := 0;
  for I := 1 to Length(Path) do
    if Path[I] = '/' then
      Inc(CompCount);
  Inc(CompCount);  { Last component }
  
  { Allocate array (max possible size) }
  SetLength(Components, CompCount);
  CompCount := 0;
  
  { Split path into components }
  Start := 1;
  for I := 1 to Length(Path) do
  begin
    if Path[I] = '/' then
    begin
      if I > Start then
      begin
        Comp := Copy(Path, Start, I - Start);
        { Skip empty, ., and .. components }
        if (Comp <> '') and (Comp <> '.') and (Comp <> '..') then
        begin
          Components[CompCount] := Comp;
          Inc(CompCount);
        end;
      end;
      Start := I + 1;
    end;
  end;
  
  { Don't forget last component }
  if Start <= Length(Path) then
  begin
    Comp := Copy(Path, Start, Length(Path) - Start + 1);
    if (Comp <> '') and (Comp <> '.') and (Comp <> '..') then
    begin
      Components[CompCount] := Comp;
      Inc(CompCount);
    end;
  end;
  
  { Rebuild path }
  for I := 0 to CompCount - 1 do
  begin
    if I > 0 then
      Result := Result + '/';
    Result := Result + Components[I];
  end;
end;

{ Check if a path contains traversal sequences }
function ContainsTraversal(const Path: string): Boolean;
begin
  Result := (Pos('..', Path) > 0) or 
            (Pos('\', Path) > 0) or 
            (Path <> '' ) and (Path[1] = '/');
end;

{ Build path from bencode path list }
function BuildPathFromList(PathList: PBencodeValue): string;
var
  Entry: PBencodeListEntry;
  PathPart: string;
  First: Boolean;
begin
  Result := '';
  if (PathList = nil) or (PathList^.ValueType <> btList) then Exit;
  
  First := True;
  Entry := PathList^.ListHead;
  while Entry <> nil do
  begin
    if (Entry^.Value <> nil) and (Entry^.Value^.ValueType = btString) then
    begin
      if Entry^.Value^.StrLen > 0 then
      begin
        SetLength(PathPart, Entry^.Value^.StrLen);
        Move(Entry^.Value^.StrVal^, PathPart[1], Entry^.Value^.StrLen);
        
        { Sanitize path component to prevent traversal attacks }
        PathPart := SanitizePath(PathPart);
        if PathPart = '' then
        begin
          Entry := Entry^.Next;
          Continue;
        end;
        
        if First then
        begin
          Result := PathPart;
          First := False;
        end
        else
        begin
          Result := JoinPath(Result, PathPart);
        end;
      end;
    end;
    Entry := Entry^.Next;
  end;
end;

{ Parse a single file entry from bencode }
function ParseFileEntry(FileDict: PBencodeValue; out Entry: PFileEntry): Boolean;
var
  LengthVal: PBencodeValue;
  PathList: PBencodeValue;
  Md5Val: PBencodeValue;
  Len: Int64;
  PathStr: string;
begin
  Result := False;
  Entry := nil;
  
  if (FileDict = nil) or (FileDict^.ValueType <> btDict) then Exit;
  
  { Get length (required) }
  LengthVal := BencodeDictGet(FileDict, 'length');
  if (LengthVal = nil) or (LengthVal^.ValueType <> btInteger) then Exit;
  
  Len := LengthVal^.IntVal;
  if Len < 0 then Exit;  { Negative length is invalid }
  
  { Get path (required) }
  PathList := BencodeDictGet(FileDict, 'path');
  if PathList = nil then Exit;
  
  PathStr := BuildPathFromList(PathList);
  if PathStr = '' then Exit;  { Empty path is invalid }
  
  { Create entry }
  Entry := NewFileEntry;
  if Entry = nil then Exit;
  
  Entry^.Length := QWord(Len);
  Entry^.Path := PathStr;
  Entry^.Filename := ExtractFile(PathStr);
  
  { Get optional MD5 sum }
  Md5Val := BencodeDictGet(FileDict, 'md5sum');
  if (Md5Val <> nil) and (Md5Val^.ValueType = btString) then
  begin
    if Md5Val^.StrLen = 32 then  { MD5 is 32 hex chars }
    begin
      SetLength(Entry^.Md5Sum, 32);
      Move(Md5Val^.StrVal^, Entry^.Md5Sum[1], 32);
    end;
  end;
  
  Result := True;
end;

{ Parse announce-list from bencode }
function ParseAnnounceList(ListRoot: PBencodeValue; 
                           out Head: PTrackerInfo;
                           out Count: Integer): Boolean;
var
  TierEntry, UrlEntry: PBencodeListEntry;
  TierList: PBencodeValue;
  Tracker: PTrackerInfo;
  TierIdx: Integer;
  UrlStr: string;
  LastTracker: PTrackerInfo;
begin
  Result := True;
  Head := nil;
  Count := 0;
  LastTracker := nil;
  
  if (ListRoot = nil) or (ListRoot^.ValueType <> btList) then Exit;
  
  TierIdx := 0;
  TierEntry := ListRoot^.ListHead;
  while TierEntry <> nil do
  begin
    if (TierEntry^.Value <> nil) and (TierEntry^.Value^.ValueType = btList) then
    begin
      TierList := TierEntry^.Value;
      UrlEntry := TierList^.ListHead;
      
      while UrlEntry <> nil do
      begin
        if (UrlEntry^.Value <> nil) and 
           (UrlEntry^.Value^.ValueType = btString) and
           (UrlEntry^.Value^.StrLen > 0) then
        begin
          SetLength(UrlStr, UrlEntry^.Value^.StrLen);
          Move(UrlEntry^.Value^.StrVal^, UrlStr[1], UrlEntry^.Value^.StrLen);
          
          New(Tracker);
          if Tracker <> nil then
          begin
            Tracker^.Url := UrlStr;
            Tracker^.Tier := TierIdx;
            
            { Detect protocol }
            if StartsWith(UrlStr, 'udp://') then
              Tracker^.Protocol := tpUDP
            else
              Tracker^.Protocol := tpHTTP;
            
            Tracker^.Next := nil;
            
            { Add to linked list }
            if Head = nil then
              Head := Tracker
            else
              LastTracker^.Next := Tracker;
            LastTracker := Tracker;
            
            Inc(Count);
          end;
        end;
        UrlEntry := UrlEntry^.Next;
      end;
    end;
    Inc(TierIdx);
    TierEntry := TierEntry^.Next;
  end;
end;

{ Parse pieces string into individual piece records }
function ParsePieces(PiecesData: PByteArray; PiecesLen: Integer;
                     PieceLength: Cardinal; TotalLength: QWord;
                     out Head: PPieceInfo; out Count: Integer): Boolean;
var
  NumPieces: Integer;
  ExpectedPieces: QWord;
  I: Integer;
  Piece: PPieceInfo;
  LastPiece: PPieceInfo;
begin
  Result := False;
  Head := nil;
  Count := 0;
  
  if (PiecesData = nil) or (PiecesLen <= 0) or (PieceLength = 0) then Exit;
  
  { Each piece hash is 20 bytes }
  if PiecesLen mod SHA1_HASH_SIZE <> 0 then Exit;
  
  NumPieces := PiecesLen div SHA1_HASH_SIZE;
  
  { Calculate expected piece count from total length }
  if TotalLength > 0 then
  begin
    ExpectedPieces := (TotalLength + PieceLength - 1) div PieceLength;
    if QWord(NumPieces) <> ExpectedPieces then
    begin
      { Log warning but continue }
      LogWarning('Metainfo', 'Piece count mismatch: expected %d, got %d',
                 [ExpectedPieces, NumPieces]);
    end;
  end;
  
  LastPiece := nil;
  for I := 0 to NumPieces - 1 do
  begin
    New(Piece);
    if Piece = nil then
    begin
      { Cleanup on allocation failure }
      while Head <> nil do
      begin
        Piece := Head;
        Head := Head^.Next;
        Dispose(Piece);
      end;
      Exit;
    end;
    
    Piece^.Index := I;
    
    { Calculate piece length (last piece may be shorter) }
    if I = NumPieces - 1 then
    begin
      { Last piece }
      if TotalLength > 0 then
        Piece^.Length := TotalLength - (QWord(I) * PieceLength)
      else
        Piece^.Length := PieceLength;  { Assume full if unknown }
    end
    else
    begin
      Piece^.Length := PieceLength;
    end;
    
    { Copy hash }
    Move(PiecesData^[I * SHA1_HASH_SIZE], Piece^.Hash, SHA1_HASH_SIZE);
    Piece^.State := psMissing;
    Piece^.Next := nil;
    
    { Add to linked list }
    if Head = nil then
      Head := Piece
    else
      LastPiece^.Next := Piece;
    LastPiece := Piece;
    
    Inc(Count);
  end;
  
  Result := True;
end;

{ ============================================================================ }
{ Main Parsing Functions                                                       }
{ ============================================================================ }

function ParseTorrentBencode(Root: PBencodeValue;
                             out Meta: PTorrentMeta): TMetainfoResult;
var
  InfoDict: PBencodeValue;
  AnnounceVal: PBencodeValue;
  AnnounceListVal: PBencodeValue;
  CommentVal: PBencodeValue;
  CreatedByVal: PBencodeValue;
  CreationDateVal: PBencodeValue;
  EncodingVal: PBencodeValue;
  PieceLengthVal: PBencodeValue;
  PiecesVal: PBencodeValue;
  NameVal: PBencodeValue;
  LengthVal: PBencodeValue;
  FilesVal: PBencodeValue;
  PrivateVal: PBencodeValue;
  SourceVal: PBencodeValue;
  Md5Val: PBencodeValue;
  FileDict: PBencodeValue;
  FileEntry, LastFileEntry: PFileEntry;
  PiecesData: PByteArray;
  PiecesLen: Integer;
  FileCount: Integer;
  TotalLen: QWord;
  FileListEntry: PBencodeListEntry;
begin
  Result.Success := False;
  Result.ErrorMsg := '';
  Meta := nil;
  
  if Root = nil then
  begin
    Result.ErrorMsg := 'Root value is nil';
    Exit;
  end;
  
  if Root^.ValueType <> btDict then
  begin
    Result.ErrorMsg := 'Root must be a dictionary';
    Exit;
  end;
  
  { Get info dictionary (required) }
  InfoDict := BencodeDictGet(Root, 'info');
  if (InfoDict = nil) or (InfoDict^.ValueType <> btDict) then
  begin
    Result.ErrorMsg := 'Missing or invalid info dictionary';
    Exit;
  end;
  
  { Create metadata structure }
  New(Meta);
  if Meta = nil then
  begin
    Result.ErrorMsg := 'Memory allocation failed';
    Exit;
  end;
  
  { Initialize all fields }
  FillChar(Meta^, SizeOf(TTorrentMeta), 0);
  Meta^.RawInfo := InfoDict;
  
  { === Parse info dictionary === }
  
  { piece length (required) }
  PieceLengthVal := BencodeDictGet(InfoDict, 'piece length');
  if (PieceLengthVal = nil) or (PieceLengthVal^.ValueType <> btInteger) then
  begin
    Result.ErrorMsg := 'Missing or invalid piece length';
    Dispose(Meta);
    Exit;
  end;
  
  if PieceLengthVal^.IntVal <= 0 then
  begin
    Result.ErrorMsg := 'Piece length must be positive';
    FreeTorrentMeta(Meta);
    Exit;
  end;
  
  Meta^.PieceLength := Cardinal(PieceLengthVal^.IntVal);
  
  { pieces (required) - concatenated SHA1 hashes }
  PiecesVal := BencodeDictGet(InfoDict, 'pieces');
  if (PiecesVal = nil) or (PiecesVal^.ValueType <> btString) then
  begin
    Result.ErrorMsg := 'Missing or invalid pieces';
    FreeTorrentMeta(Meta);
    Exit;
  end;
  
  PiecesLen := PiecesVal^.StrLen;
  if PiecesLen = 0 then
  begin
    Result.ErrorMsg := 'Empty pieces data';
    FreeTorrentMeta(Meta);
    Exit;
  end;
  
  if PiecesLen mod SHA1_HASH_SIZE <> 0 then
  begin
    Result.ErrorMsg := 'Pieces length is not a multiple of 20';
    FreeTorrentMeta(Meta);
    Exit;
  end;
  
  Meta^.PieceCount := PiecesLen div SHA1_HASH_SIZE;
  
  { Copy pieces data }
  if PiecesLen > 0 then
  begin
    GetMem(PiecesData, PiecesLen);
    if PiecesData = nil then
    begin
      Result.ErrorMsg := 'Memory allocation failed for pieces';
      FreeTorrentMeta(Meta);
      Exit;
    end;
    Move(PiecesVal^.StrVal^, PiecesData^, PiecesLen);
    Meta^.Pieces := PiecesData;
  end;
  
  { name (required) }
  NameVal := BencodeDictGet(InfoDict, 'name');
  if (NameVal <> nil) and (NameVal^.ValueType = btString) then
  begin
    if NameVal^.StrLen > 0 then
    begin
      SetLength(Meta^.Name, NameVal^.StrLen);
      Move(NameVal^.StrVal^, Meta^.Name[1], NameVal^.StrLen);
      { Sanitize the name to prevent traversal attacks }
      Meta^.Name := SanitizePath(Meta^.Name);
    end;
  end;
  
  if Meta^.Name = '' then
  begin
    Result.ErrorMsg := 'Missing or empty name';
    FreeTorrentMeta(Meta);
    Exit;
  end;
  
  { private flag (optional) }
  PrivateVal := BencodeDictGet(InfoDict, 'private');
  if (PrivateVal <> nil) and (PrivateVal^.ValueType = btInteger) then
    Meta^.PrivateFlag := PrivateVal^.IntVal <> 0;
  
  { source (optional) }
  SourceVal := BencodeDictGet(InfoDict, 'source');
  if (SourceVal <> nil) and (SourceVal^.ValueType = btString) then
  begin
    if SourceVal^.StrLen > 0 then
    begin
      SetLength(Meta^.Source, SourceVal^.StrLen);
      Move(SourceVal^.StrVal^, Meta^.Source[1], SourceVal^.StrLen);
    end;
  end;
  
  { Determine mode: single file or multi-file }
  LengthVal := BencodeDictGet(InfoDict, 'length');
  FilesVal := BencodeDictGet(InfoDict, 'files');
  
  if (LengthVal <> nil) and (LengthVal^.ValueType = btInteger) then
  begin
    { Single file mode }
    if LengthVal^.IntVal < 0 then
    begin
      Result.ErrorMsg := 'Negative file length';
      FreeTorrentMeta(Meta);
      Exit;
    end;
    
    Meta^.IsSingleFile := True;
    Meta^.Length := QWord(LengthVal^.IntVal);
    Meta^.TotalLength := Meta^.Length;
    
    { Optional MD5 sum for single file }
    Md5Val := BencodeDictGet(InfoDict, 'md5sum');
    if (Md5Val <> nil) and (Md5Val^.ValueType = btString) then
    begin
      if Md5Val^.StrLen = 32 then
      begin
        SetLength(Meta^.Md5Sum, 32);
        Move(Md5Val^.StrVal^, Meta^.Md5Sum[1], 32);
      end;
    end;
  end
  else if (FilesVal <> nil) and (FilesVal^.ValueType = btList) then
  begin
    { Multi-file mode }
    Meta^.IsSingleFile := False;
    
    { Parse file list }
    FileCount := 0;
    TotalLen := 0;
    LastFileEntry := nil;
    
    FileListEntry := FilesVal^.ListHead;
    while FileListEntry <> nil do
    begin
      FileDict := FileListEntry^.Value;
      
      if ParseFileEntry(FileDict, FileEntry) then
      begin
        FileEntry^.Offset := TotalLen;
        TotalLen := TotalLen + FileEntry^.Length;
        
        { Add to linked list }
        if Meta^.Files = nil then
          Meta^.Files := FileEntry
        else
          LastFileEntry^.Next := FileEntry;
        LastFileEntry := FileEntry;
        
        Inc(FileCount);
      end;
      
      FileListEntry := FileListEntry^.Next;
    end;
    
    if FileCount = 0 then
    begin
      Result.ErrorMsg := 'No valid files in multi-file torrent';
      FreeTorrentMeta(Meta);
      Exit;
    end;
    
    Meta^.FileCount := FileCount;
    Meta^.TotalLength := TotalLen;
  end
  else
  begin
    Result.ErrorMsg := 'Neither length nor files specified';
    FreeTorrentMeta(Meta);
    Exit;
  end;
  
  { === Parse non-info fields === }
  
  { announce (required per spec but we'll be lenient) }
  AnnounceVal := BencodeDictGet(Root, 'announce');
  if (AnnounceVal <> nil) and (AnnounceVal^.ValueType = btString) then
  begin
    if AnnounceVal^.StrLen > 0 then
    begin
      SetLength(Meta^.Announce, AnnounceVal^.StrLen);
      Move(AnnounceVal^.StrVal^, Meta^.Announce[1], AnnounceVal^.StrLen);
    end;
  end;
  
  { announce-list (optional, BEP 12) }
  AnnounceListVal := BencodeDictGet(Root, 'announce-list');
  if AnnounceListVal <> nil then
  begin
    ParseAnnounceList(AnnounceListVal, Meta^.Trackers, Meta^.TrackerCount);
  end;
  
  { If no announce-list but we have announce, create a tracker entry }
  if (Meta^.Trackers = nil) and (Meta^.Announce <> '') then
  begin
    New(Meta^.Trackers);
    if Meta^.Trackers <> nil then
    begin
      Meta^.Trackers^.Url := Meta^.Announce;
      Meta^.Trackers^.Tier := 0;
      
      if StartsWith(Meta^.Announce, 'udp://') then
        Meta^.Trackers^.Protocol := tpUDP
      else
        Meta^.Trackers^.Protocol := tpHTTP;
      
      Meta^.Trackers^.Next := nil;
      Meta^.TrackerCount := 1;
    end;
  end;
  
  { comment (optional) }
  CommentVal := BencodeDictGet(Root, 'comment');
  if (CommentVal <> nil) and (CommentVal^.ValueType = btString) then
  begin
    if CommentVal^.StrLen > 0 then
    begin
      SetLength(Meta^.Comment, CommentVal^.StrLen);
      Move(CommentVal^.StrVal^, Meta^.Comment[1], CommentVal^.StrLen);
    end;
  end;
  
  { created by (optional) }
  CreatedByVal := BencodeDictGet(Root, 'created by');
  if (CreatedByVal <> nil) and (CreatedByVal^.ValueType = btString) then
  begin
    if CreatedByVal^.StrLen > 0 then
    begin
      SetLength(Meta^.CreatedBy, CreatedByVal^.StrLen);
      Move(CreatedByVal^.StrVal^, Meta^.CreatedBy[1], CreatedByVal^.StrLen);
    end;
  end;
  
  { creation date (optional) }
  CreationDateVal := BencodeDictGet(Root, 'creation date');
  if (CreationDateVal <> nil) and (CreationDateVal^.ValueType = btInteger) then
    Meta^.CreationDate := CreationDateVal^.IntVal;
  
  { encoding (optional) }
  EncodingVal := BencodeDictGet(Root, 'encoding');
  if (EncodingVal <> nil) and (EncodingVal^.ValueType = btString) then
  begin
    if EncodingVal^.StrLen > 0 then
    begin
      SetLength(Meta^.Encoding, EncodingVal^.StrLen);
      Move(EncodingVal^.StrVal^, Meta^.Encoding[1], EncodingVal^.StrLen);
    end;
  end;
  
  { === Compute info hash === }
  if not ComputeInfoHashFromBencode(InfoDict, Meta^.InfoHash) then
  begin
    Result.ErrorMsg := 'Failed to compute info hash';
    FreeTorrentMeta(Meta);
    Exit;
  end;
  
  { Success }
  Result.Success := True;
  LogInfo('Metainfo', 'Parsed torrent: %s, %d pieces, %s',
          [Meta^.Name, Meta^.PieceCount, FormatBytes(Meta^.TotalLength)]);
end;

function ParseTorrentBuffer(const Buffer: PChar; Len: Integer;
                            out Meta: PTorrentMeta): TMetainfoResult;
var
  Root: PBencodeValue;
  ParseRes: TParseResult;
begin
  Result.Success := False;
  Result.ErrorMsg := '';
  Meta := nil;
  
  if (Buffer = nil) or (Len <= 0) then
  begin
    Result.ErrorMsg := 'Empty buffer';
    Exit;
  end;
  
  ParseRes := BencodeDecode(Buffer, Len, Root);
  if not ParseRes.Success then
  begin
    Result.ErrorMsg := 'Bencode decode failed: ' + ParseRes.ErrorMsg;
    Exit;
  end;
  
  try
    Result := ParseTorrentBencode(Root, Meta);
  finally
    BencodeFree(Root);
  end;
end;

function ParseTorrentFile(const Filename: string;
                          out Meta: PTorrentMeta): TMetainfoResult;
var
  F: File;
  Buffer: PChar;
  FileSize: LongInt;
  BytesRead: LongInt;
begin
  Result.Success := False;
  Result.ErrorMsg := '';
  Meta := nil;
  
  { Check if file exists }
  if not SysUtils.FileExists(Filename) then
  begin
    Result.ErrorMsg := 'File not found: ' + Filename;
    Exit;
  end;
  
  { Open file }
  Assign(F, Filename);
  {$I-}
  Reset(F, 1);
  {$I+}
  
  if IOResult <> 0 then
  begin
    Result.ErrorMsg := 'Cannot open file: ' + Filename;
    Exit;
  end;
  
  try
    FileSize := System.FileSize(F);
    
    if FileSize <= 0 then
    begin
      Result.ErrorMsg := 'Empty file: ' + Filename;
      Exit;
    end;
    
    if FileSize > MAX_TORRENT_FILE_SIZE then
    begin
      Result.ErrorMsg := 'File too large: ' + Filename;
      Exit;
    end;
    
    GetMem(Buffer, FileSize);
    if Buffer = nil then
    begin
      Result.ErrorMsg := 'Memory allocation failed';
      Exit;
    end;
    
    try
      BlockRead(F, Buffer^, FileSize, BytesRead);
      
      if BytesRead <> FileSize then
      begin
        Result.ErrorMsg := 'Failed to read entire file';
        Exit;
      end;
      
      Result := ParseTorrentBuffer(Buffer, FileSize, Meta);
    finally
      FreeMem(Buffer);
    end;
  finally
    {$I-}
    Close(F);
    {$I+}
  end;
end;

procedure FreeTorrentMeta(Meta: PTorrentMeta);
begin
  if Meta = nil then Exit;
  
  { Free pieces data }
  if Meta^.Pieces <> nil then
    FreeMem(Meta^.Pieces);
  
  { Free file entries }
  FreeFileEntries(Meta^.Files);
  
  { Free tracker entries }
  FreeTrackerEntries(Meta^.Trackers);
  
  { Free the structure itself }
  Dispose(Meta);
end;

{ ============================================================================ }
{ Info Hash Computation                                                        }
{ ============================================================================ }

function ComputeInfoHashFromBencode(InfoDict: PBencodeValue;
                                    out Hash: TSHA1Digest): Boolean;
var
  Encoded: PChar;
  EncodedLen: Integer;
begin
  Result := False;
  SHA1Clear(Hash);
  
  if (InfoDict = nil) or (InfoDict^.ValueType <> btDict) then Exit;
  
  { Encode the info dictionary }
  if not BencodeEncode(InfoDict, Encoded, EncodedLen) then Exit;
  
  try
    { Compute SHA1 hash of the encoded data }
    Hash := SHA1Buffer(Encoded^, EncodedLen);
    Result := True;
  finally
    FreeMem(Encoded);
  end;
end;

function GetInfoHashHex(Meta: PTorrentMeta): string;
begin
  if Meta = nil then
    Result := ''
  else
    Result := SHA1DigestToHex(Meta^.InfoHash);
end;

function GetInfoHashURLEncoded(Meta: PTorrentMeta): string;
var
  I: Integer;
  B: Byte;
begin
  Result := '';
  if Meta = nil then Exit;
  
  { URL-encode each byte of the hash }
  for I := 0 to SHA1_HASH_SIZE - 1 do
  begin
    B := Meta^.InfoHash[I];
    Result := Result + '%' + HEX_CHARS_UPPER[B shr 4] + HEX_CHARS_UPPER[B and $0F];
  end;
end;

{ ============================================================================ }
{ Piece Access                                                                 }
{ ============================================================================ }

function GetPieceHash(Meta: PTorrentMeta; PieceIndex: Integer): PSHA1Digest;
begin
  Result := nil;
  if Meta = nil then Exit;
  if (PieceIndex < 0) or (PieceIndex >= Meta^.PieceCount) then Exit;
  
  Result := PSHA1Digest(@Meta^.Pieces^[PieceIndex * SHA1_HASH_SIZE]);
end;

function GetPieceLength(Meta: PTorrentMeta; PieceIndex: Integer): Cardinal;
var
  StartOffset: QWord;
begin
  Result := 0;
  if Meta = nil then Exit;
  if (PieceIndex < 0) or (PieceIndex >= Meta^.PieceCount) then Exit;
  
  if PieceIndex = Meta^.PieceCount - 1 then
  begin
    { Last piece - calculate actual length }
    StartOffset := QWord(PieceIndex) * Meta^.PieceLength;
    if StartOffset < Meta^.TotalLength then
      Result := Meta^.TotalLength - StartOffset
    else
      Result := 0;
  end
  else
  begin
    Result := Meta^.PieceLength;
  end;
end;

function GetTotalPiecesLength(Meta: PTorrentMeta): QWord;
begin
  Result := 0;
  if Meta = nil then Exit;
  
  Result := QWord(Meta^.PieceCount) * Meta^.PieceLength;
end;

{ ============================================================================ }
{ File Access                                                                  }
{ ============================================================================ }

function FindFileAtOffset(Meta: PTorrentMeta; Offset: QWord;
                          out FileEntry: PFileEntry): Boolean;
var
  Entry: PFileEntry;
  FileEnd: QWord;
begin
  Result := False;
  FileEntry := nil;
  
  if Meta = nil then Exit;
  
  if Meta^.IsSingleFile then
  begin
    if Offset < Meta^.Length then
    begin
      { Single file - no file entry, return nil but success }
      Result := True;
    end;
    Exit;
  end;
  
  { Multi-file: search through file list }
  Entry := Meta^.Files;
  while Entry <> nil do
  begin
    FileEnd := Entry^.Offset + Entry^.Length;
    if (Offset >= Entry^.Offset) and (Offset < FileEnd) then
    begin
      FileEntry := Entry;
      Result := True;
      Exit;
    end;
    Entry := Entry^.Next;
  end;
end;

function GetFullFilePath(Meta: PTorrentMeta; FileEntry: PFileEntry;
                         const BasePath: string): string;
begin
  Result := '';
  if Meta = nil then Exit;
  
  if Meta^.IsSingleFile then
  begin
    { Single file: basepath + name }
    Result := JoinPath(BasePath, Meta^.Name);
  end
  else if FileEntry <> nil then
  begin
    { Multi-file: basepath + torrent name + file path }
    Result := JoinPath(BasePath, Meta^.Name);
    Result := JoinPath(Result, FileEntry^.Path);
  end;
end;

function GetFileOffsetForTorrentOffset(Meta: PTorrentMeta;
                                       FileEntry: PFileEntry;
                                       TorrentOffset: QWord): QWord;
begin
  Result := 0;
  if Meta = nil then Exit;
  
  if Meta^.IsSingleFile then
  begin
    { Single file: offset is the same }
    Result := TorrentOffset;
  end
  else if FileEntry <> nil then
  begin
    { Multi-file: subtract file's starting offset }
    if TorrentOffset >= FileEntry^.Offset then
      Result := TorrentOffset - FileEntry^.Offset;
  end;
end;

{ ============================================================================ }
{ Utility Functions                                                            }
{ ============================================================================ }

function ValidateTorrentMeta(Meta: PTorrentMeta; out ErrorMsg: string): Boolean;
var
  FileEntry: PFileEntry;
begin
  Result := False;
  ErrorMsg := '';
  
  if Meta = nil then
  begin
    ErrorMsg := 'Meta is nil';
    Exit;
  end;
  
  { Check required fields }
  if Meta^.Name = '' then
  begin
    ErrorMsg := 'Name is empty';
    Exit;
  end;
  
  { Check name doesn't contain path traversal }
  if ContainsTraversal(Meta^.Name) then
  begin
    ErrorMsg := 'Name contains path traversal sequences';
    Exit;
  end;
  
  if Meta^.PieceLength = 0 then
  begin
    ErrorMsg := 'Piece length is zero';
    Exit;
  end;
  
  if Meta^.PieceCount = 0 then
  begin
    ErrorMsg := 'Piece count is zero';
    Exit;
  end;
  
  if Meta^.Pieces = nil then
  begin
    ErrorMsg := 'Pieces data is nil';
    Exit;
  end;
  
  if Meta^.TotalLength = 0 then
  begin
    ErrorMsg := 'Total length is zero';
    Exit;
  end;
  
  { Check info hash is not empty }
  if SHA1IsEmpty(Meta^.InfoHash) then
  begin
    ErrorMsg := 'Info hash is empty';
    Exit;
  end;
  
  { Mode-specific checks }
  if Meta^.IsSingleFile then
  begin
    { Single file mode }
    if Meta^.Length <> Meta^.TotalLength then
    begin
      ErrorMsg := 'Single file length mismatch';
      Exit;
    end;
  end
  else
  begin
    { Multi-file mode }
    if Meta^.Files = nil then
    begin
      ErrorMsg := 'Multi-file torrent has no files';
      Exit;
    end;
    
    if Meta^.FileCount = 0 then
    begin
      ErrorMsg := 'File count is zero';
      Exit;
    end;
    
    { Check file paths for traversal }
    FileEntry := Meta^.Files;
    while FileEntry <> nil do
    begin
      if ContainsTraversal(FileEntry^.Path) then
      begin
        ErrorMsg := 'File path contains traversal: ' + FileEntry^.Path;
        Exit;
      end;
      FileEntry := FileEntry^.Next;
    end;
  end;
  
  Result := True;
end;

function TorrentMetaToString(Meta: PTorrentMeta): string;
var
  SB: string;
  FileEntry: PFileEntry;
  Tracker: PTrackerInfo;
begin
  if Meta = nil then
  begin
    Result := 'TTorrentMeta(nil)';
    Exit;
  end;
  
  SB := 'TTorrentMeta{' + LineEnding;
  SB := SB + '  Name: "' + Meta^.Name + '"' + LineEnding;
  SB := SB + '  InfoHash: ' + GetInfoHashHex(Meta) + LineEnding;
  SB := SB + '  Mode: ';
  if Meta^.IsSingleFile then
    SB := SB + 'Single File' + LineEnding
  else
    SB := SB + 'Multi File' + LineEnding;
  SB := SB + '  TotalLength: ' + FormatBytes(Meta^.TotalLength) + LineEnding;
  SB := SB + '  PieceLength: ' + FormatBytes(Meta^.PieceLength) + LineEnding;
  SB := SB + '  PieceCount: ' + IntToStr(Meta^.PieceCount) + LineEnding;
  
  if not Meta^.IsSingleFile then
  begin
    SB := SB + '  FileCount: ' + IntToStr(Meta^.FileCount) + LineEnding;
    SB := SB + '  Files:' + LineEnding;
    
    FileEntry := Meta^.Files;
    while FileEntry <> nil do
    begin
      SB := SB + '    - ' + FileEntry^.Path + ' (' + 
            FormatBytes(FileEntry^.Length) + ')' + LineEnding;
      FileEntry := FileEntry^.Next;
    end;
  end;
  
  if Meta^.Announce <> '' then
    SB := SB + '  Announce: ' + Meta^.Announce + LineEnding;
  
  if Meta^.Trackers <> nil then
  begin
    SB := SB + '  Trackers:' + LineEnding;
    Tracker := Meta^.Trackers;
    while Tracker <> nil do
    begin
      SB := SB + '    - [' + IntToStr(Tracker^.Tier) + '] ' + 
            Tracker^.Url + LineEnding;
      Tracker := Tracker^.Next;
    end;
  end;
  
  if Meta^.Comment <> '' then
    SB := SB + '  Comment: "' + Meta^.Comment + '"' + LineEnding;
  
  if Meta^.CreatedBy <> '' then
    SB := SB + '  CreatedBy: "' + Meta^.CreatedBy + '"' + LineEnding;
  
  if Meta^.PrivateFlag then
    SB := SB + '  Private: Yes' + LineEnding;
  
  SB := SB + '}';
  Result := SB;
end;

function IsSingleFileTorrent(Meta: PTorrentMeta): Boolean;
begin
  Result := (Meta <> nil) and Meta^.IsSingleFile;
end;

function GetCompletionFromBitfield(Bitfield: PByteArray;
                                   BitfieldSize: Integer;
                                   PieceCount: Integer): Double;
var
  I: Integer;
  CompleteCount: Integer;
  ByteVal: Byte;
  BitsInLastByte: Integer;
begin
  Result := 0.0;
  
  if (Bitfield = nil) or (BitfieldSize <= 0) or (PieceCount <= 0) then Exit;
  
  CompleteCount := 0;
  
  { Count complete pieces in full bytes }
  for I := 0 to BitfieldSize - 2 do
  begin
    ByteVal := Bitfield^[I];
    { Count bits set using Brian Kernighan's algorithm }
    while ByteVal <> 0 do
    begin
      ByteVal := ByteVal and (ByteVal - 1);
      Inc(CompleteCount);
    end;
  end;
  
  { Handle last byte (may have unused bits) }
  if BitfieldSize > 0 then
  begin
    ByteVal := Bitfield^[BitfieldSize - 1];
    BitsInLastByte := PieceCount mod 8;
    if BitsInLastByte = 0 then
      BitsInLastByte := 8;
    
    { Mask out unused bits - bits are stored MSB first (bit 0 at position 7) }
    ByteVal := ByteVal and ($FF shl (8 - BitsInLastByte));
    
    while ByteVal <> 0 do
    begin
      ByteVal := ByteVal and (ByteVal - 1);
      Inc(CompleteCount);
    end;
  end;
  
  Result := CompleteCount / PieceCount;
end;

end.
