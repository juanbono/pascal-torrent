{
  filemgr.pas - File management for PascalTorrent
  
  This unit handles reading and writing torrent data to/from files,
  supporting both single-file and multi-file torrents.
  
  Uses only imperative/procedural programming - no OOP.
}

unit filemgr;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, metainfo, sha1utils, utils, logging;

const
  { Buffer sizes }
  DEFAULT_BLOCK_SIZE = 16384;     { 16 KB - standard BitTorrent block size }
  MAX_BLOCK_SIZE = 131072;        { 128 KB max }
  MIN_BLOCK_SIZE = 1024;          { 1 KB min }
  
  { File modes }
  fmRead = 0;                     { Read-only }
  fmWrite = 1;                    { Write-only }
  fmReadWrite = 2;                { Read-write }

type
  { Forward declarations }
  POpenFile = ^TOpenFile;
  PFileManager = ^TFileManager;
  
  { Callback types }
  TPieceCheckProgressCallback = procedure(PieceIdx, TotalPieces: Integer);
  
  { Open file handle }
  TOpenFile = record
    Filename: string;             { Full path to file }
    Handle: file;                 { Pascal file handle }
    IsOpen: Boolean;              { Whether file is currently open }
    OpenMode: Integer;            { How the file was opened }
    LastAccess: TDateTime;        { For LRU cache eviction }
    RefCount: Integer;            { Number of operations using this file }
    Next: POpenFile;              { Linked list }
  end;
  
  { File manager state }
  TFileManager = record
    Meta: PTorrentMeta;           { Associated torrent metadata }
    BasePath: string;             { Base download directory }
    
    { Open file cache }
    OpenFiles: POpenFile;         { Linked list of open files }
    OpenFileCount: Integer;       { Number of open files }
    MaxOpenFiles: Integer;        { Maximum files to keep open }
    
    { Statistics }
    BytesRead: QWord;             { Total bytes read }
    BytesWritten: QWord;          { Total bytes written }
    ReadOps: Cardinal;            { Number of read operations }
    WriteOps: Cardinal;           { Number of write operations }
    Errors: Cardinal;             { Number of errors }
    
    { Piece verification }
    VerifiedPieces: PByteArray;   { Bitfield of verified pieces }
    
    { State }
    Initialized: Boolean;         { Whether manager is initialized }
  end;
  
  { I/O Result }
  TIOResult = record
    Success: Boolean;
    BytesTransferred: Integer;
    ErrorMsg: string;
  end;
  
  { Piece write result }
  TPieceWriteResult = record
    Success: Boolean;
    Verified: Boolean;            { Whether piece passed hash verification }
    ErrorMsg: string;
  end;

{ ============================================================================ }
{ Lifecycle                                                                    }
{ ============================================================================ }

{ Create a new file manager }
function FileManagerCreate(Meta: PTorrentMeta; const BasePath: string;
                           out FM: PFileManager): Boolean;

{ Destroy a file manager and close all files }
procedure FileManagerDestroy(FM: PFileManager);

{ Initialize file manager (create directories, check files) }
function FileManagerInitialize(FM: PFileManager): Boolean;

{ ============================================================================ }
{ Piece Operations                                                             }
{ ============================================================================ }

{ Read an entire piece from files }
function FileManagerReadPiece(FM: PFileManager; PieceIndex: Integer;
                              Buffer: Pointer; BufferSize: Integer;
                              out IOReg: TIOResult): Boolean;

{ Write an entire piece to files }
function FileManagerWritePiece(FM: PFileManager; PieceIndex: Integer;
                               Buffer: Pointer; DataLen: Integer;
                               out PieceReg: TPieceWriteResult): Boolean;

{ Verify a piece against its expected hash }
function FileManagerVerifyPiece(FM: PFileManager; PieceIndex: Integer;
                                out IOReg: TIOResult): Boolean;

{ Check if a piece is verified (from cache) }
function FileManagerIsPieceVerified(FM: PFileManager; PieceIndex: Integer): Boolean;

{ Mark a piece as verified in the cache }
procedure FileManagerMarkPieceVerified(FM: PFileManager; PieceIndex: Integer);

{ ============================================================================ }
{ Block Operations (sub-piece level)                                           }
{ ============================================================================ }

{ Read a block at a specific offset (across piece boundaries if needed) }
function FileManagerReadBlock(FM: PFileManager; Offset: QWord; 
                              Len: Integer; Buffer: Pointer;
                              out IOReg: TIOResult): Boolean;

{ Write a block at a specific offset }
function FileManagerWriteBlock(FM: PFileManager; Offset: QWord;
                               Len: Integer; Buffer: Pointer;
                               out IOReg: TIOResult): Boolean;

{ ============================================================================ }
{ File Operations (low-level)                                                  }
{ ============================================================================ }

{ Open a file for reading/writing (with caching) }
function FileManagerOpenFile(FM: PFileManager; const Filename: string;
                             Mode: Integer; out OpenFile: POpenFile): Boolean;

{ Close a specific open file }
procedure FileManagerCloseFile(FM: PFileManager; OpenFile: POpenFile);

{ Close all open files }
procedure FileManagerCloseAllFiles(FM: PFileManager);

{ Ensure a file exists (create if needed, with pre-allocation) }
function FileManagerEnsureFile(FM: PFileManager; FileEntry: PFileEntry): Boolean;

{ ============================================================================ }
{ Progress and Status                                                          }
{ ============================================================================ }

{ Get completion percentage based on verified pieces }
function FileManagerGetCompletion(FM: PFileManager): Double;

{ Get amount of data verified (in bytes) }
function FileManagerGetVerifiedBytes(FM: PFileManager): QWord;

{ Get statistics }
procedure FileManagerGetStats(FM: PFileManager; out ReadBytes, WrittenBytes: QWord;
                              out ReadOps, WriteOps, Errors: Cardinal);

{ Check which pieces are complete (verify all) }
function FileManagerCheckAllPieces(FM: PFileManager;
                                   ProgressCallback: TPieceCheckProgressCallback): Boolean;

{ ============================================================================ }
{ Utility Functions                                                            }
{ ============================================================================ }

{ Get the full path for a file in this torrent }
function FileManagerGetPath(FM: PFileManager; FileEntry: PFileEntry): string;

{ Check if all files exist (for seeding) }
function FileManagerFilesExist(FM: PFileManager): Boolean;

{ Pre-allocate all files (create sparse/truncated files) }
function FileManagerPreallocateFiles(FM: PFileManager): Boolean;

{ Delete all files (for cleanup) }
function FileManagerDeleteAllFiles(FM: PFileManager): Boolean;

implementation

{ ============================================================================ }
{ Helper Functions                                                             }
{ ============================================================================ }

{ Calculate bitfield size in bytes for given piece count }
function BitfieldSize(PieceCount: Integer): Integer;
begin
  Result := BitfieldBytes(PieceCount);
end;

{ Set a bit in a bitfield with bounds checking }
procedure SetBit(Bitfield: PByteArray; BitfieldSize: Integer; Index: Integer);
var
  ByteIdx: Integer;
  BitIdx: Integer;
begin
  if (Bitfield = nil) or (BitfieldSize <= 0) then Exit;
  if (Index < 0) or (Index >= BitfieldSize * 8) then Exit;
  
  ByteIdx := Index div 8;
  BitIdx := Index mod 8;
  
  if ByteIdx >= BitfieldSize then Exit;
  
  Bitfield^[ByteIdx] := Bitfield^[ByteIdx] or (1 shl (7 - BitIdx));
end;

{ Get a bit from a bitfield with bounds checking }
function GetBit(Bitfield: PByteArray; BitfieldSize: Integer; Index: Integer): Boolean;
var
  ByteIdx: Integer;
  BitIdx: Integer;
begin
  Result := False;
  if (Bitfield = nil) or (BitfieldSize <= 0) then Exit;
  if (Index < 0) or (Index >= BitfieldSize * 8) then Exit;
  
  ByteIdx := Index div 8;
  BitIdx := Index mod 8;
  
  if ByteIdx >= BitfieldSize then Exit;
  
  Result := (Bitfield^[ByteIdx] and (1 shl (7 - BitIdx))) <> 0;
end;

{ Find open file in cache }
function FindOpenFile(FM: PFileManager; const Filename: string): POpenFile;
begin
  Result := FM^.OpenFiles;
  while Result <> nil do
  begin
    if Result^.Filename = Filename then
      Exit;
    Result := Result^.Next;
  end;
end;

{ Close LRU file if at limit }
procedure CloseLRUFileIfNeeded(FM: PFileManager);
var
  Current, LRU: POpenFile;
  OldestTime: TDateTime;
begin
  if FM^.OpenFileCount < FM^.MaxOpenFiles then Exit;
  
  { Find oldest file with RefCount = 0 }
  LRU := nil;
  OldestTime := Now;
  
  Current := FM^.OpenFiles;
  while Current <> nil do
  begin
    if (Current^.RefCount = 0) and (Current^.LastAccess < OldestTime) then
    begin
      LRU := Current;
      OldestTime := Current^.LastAccess;
    end;
    Current := Current^.Next;
  end;
  
  if LRU <> nil then
    FileManagerCloseFile(FM, LRU);
end;

{ Create directory path if needed (creates all parent directories) }
function EnsureDirectory(const Path: string): Boolean;
var
  Dir: string;
begin
  Dir := ExtractDir(Path);
  if Dir = '' then
  begin
    Result := True;
    Exit;
  end;
  
  Result := EnsureDirectories(Dir);
end;

{ Get file entry by name (for multi-file torrents) }
function FindFileEntryByName(Meta: PTorrentMeta; const Filename: string): PFileEntry;
begin
  Result := nil;
  if Meta = nil then Exit;
  
  Result := Meta^.Files;
  while Result <> nil do
  begin
    if Result^.Path = Filename then
      Exit;
    Result := Result^.Next;
  end;
end;

{ ============================================================================ }
{ Lifecycle                                                                    }
{ ============================================================================ }

function FileManagerCreate(Meta: PTorrentMeta; const BasePath: string;
                           out FM: PFileManager): Boolean;
var
  BfSize: Integer;
begin
  Result := False;
  FM := nil;
  
  if Meta = nil then
  begin
    LogError('FileManager', 'Cannot create file manager: Meta is nil');
    Exit;
  end;
  
  if BasePath = '' then
  begin
    LogError('FileManager', 'Cannot create file manager: BasePath is empty');
    Exit;
  end;
  
  New(FM);
  if FM = nil then
  begin
    LogError('FileManager', 'Memory allocation failed');
    Exit;
  end;
  
  { Initialize fields }
  FillChar(FM^, SizeOf(TFileManager), 0);
  
  FM^.Meta := Meta;
  FM^.BasePath := BasePath;
  FM^.MaxOpenFiles := 16;  { Reasonable default }
  
  { Allocate verified pieces bitfield }
  BfSize := BitfieldSize(Meta^.PieceCount);
  GetMem(FM^.VerifiedPieces, BfSize);
  if FM^.VerifiedPieces = nil then
  begin
    LogError('FileManager', 'Failed to allocate verified pieces bitfield');
    Dispose(FM);
    FM := nil;
    Exit;
  end;
  
  FillChar(FM^.VerifiedPieces^, BfSize, 0);
  
  FM^.Initialized := True;
  Result := True;
  
  LogInfo('FileManager', 'Created file manager for "%s" in %s', 
          [Meta^.Name, BasePath]);
end;

procedure FileManagerDestroy(FM: PFileManager);
begin
  if FM = nil then Exit;
  
  { Close all open files }
  FileManagerCloseAllFiles(FM);
  
  { Free bitfield }
  if FM^.VerifiedPieces <> nil then
    FreeMem(FM^.VerifiedPieces);
  
  LogInfo('FileManager', 'Destroyed file manager (Read: %s, Written: %s)', [
          FormatBytes(FM^.BytesRead), FormatBytes(FM^.BytesWritten)]);
  
  Dispose(FM);
end;

function FileManagerInitialize(FM: PFileManager): Boolean;
var
  FileEntry: PFileEntry;
  FullPath: string;
begin
  Result := False;
  if FM = nil then Exit;
  if not FM^.Initialized then Exit;
  
  { Ensure base directory exists }
  if not DirExists(FM^.BasePath) then
  begin
    if not MakeDir(FM^.BasePath) then
    begin
      LogError('FileManager', 'Failed to create base directory: %s', 
               [FM^.BasePath]);
      Exit;
    end;
  end;
  
  { For multi-file torrents, ensure subdirectories exist }
  if not FM^.Meta^.IsSingleFile then
  begin
    FileEntry := FM^.Meta^.Files;
    while FileEntry <> nil do
    begin
      FullPath := FileManagerGetPath(FM, FileEntry);
      if not EnsureDirectory(FullPath) then
      begin
        LogError('FileManager', 'Failed to create directory for: %s', 
                 [FullPath]);
        Exit;
      end;
      FileEntry := FileEntry^.Next;
    end;
  end;
  
  Result := True;
  LogInfo('FileManager', 'File manager initialized');
end;

{ ============================================================================ }
{ File Operations (low-level)                                                  }
{ ============================================================================ }

function FileManagerOpenFile(FM: PFileManager; const Filename: string;
                             Mode: Integer; out OpenFile: POpenFile): Boolean;
var
  OpenedFile: POpenFile;
begin
  Result := False;
  OpenFile := nil;
  
  if FM = nil then Exit;
  if Filename = '' then Exit;
  
  { Check cache first }
  OpenedFile := FindOpenFile(FM, Filename);
  if OpenedFile <> nil then
  begin
    { File already open - check if mode is compatible }
    if (Mode = fmRead) or (OpenedFile^.OpenMode = Mode) then
    begin
      Inc(OpenedFile^.RefCount);
      OpenedFile^.LastAccess := Now;
      OpenFile := OpenedFile;
      Result := True;
      Exit;
    end
    else
    begin
      { Need to reopen with different mode }
      FileManagerCloseFile(FM, OpenedFile);
    end;
  end;
  
  { Make room if at limit }
  CloseLRUFileIfNeeded(FM);
  
  { Create new entry }
  New(OpenedFile);
  if OpenedFile = nil then Exit;
  
  OpenedFile^.Filename := Filename;
  OpenedFile^.IsOpen := False;
  OpenedFile^.OpenMode := Mode;
  OpenedFile^.RefCount := 1;
  OpenedFile^.LastAccess := Now;
  
  { Open the file }
  Assign(OpenedFile^.Handle, Filename);
  
  {$I-}
  if Mode = fmRead then
  begin
    if SysUtils.FileExists(Filename) then
      Reset(OpenedFile^.Handle, 1)
    else
    begin
      { File doesn't exist for reading }
      Dispose(OpenedFile);
      Exit;
    end;
  end
  else if Mode = fmWrite then
  begin
    { For writing, ensure directory exists }
    if not EnsureDirectory(Filename) then
    begin
      Dispose(OpenedFile);
      Exit;
    end;
    Rewrite(OpenedFile^.Handle, 1);
  end
  else  { fmReadWrite }
  begin
    if SysUtils.FileExists(Filename) then
      Reset(OpenedFile^.Handle, 1)
    else
    begin
      if not EnsureDirectory(Filename) then
      begin
        Dispose(OpenedFile);
        Exit;
      end;
      { Create file with Rewrite (write-only), then reopen with Reset (read/write) }
      Rewrite(OpenedFile^.Handle, 1);
      if IOResult = 0 then
      begin
        Close(OpenedFile^.Handle);
        Reset(OpenedFile^.Handle, 1);
      end;
    end;
  end;
  {$I+}
  
  if IOResult <> 0 then
  begin
    Dispose(OpenedFile);
    Exit;
  end;
  
  OpenedFile^.IsOpen := True;
  
  { Add to linked list }
  OpenedFile^.Next := FM^.OpenFiles;
  FM^.OpenFiles := OpenedFile;
  Inc(FM^.OpenFileCount);
  
  OpenFile := OpenedFile;
  Result := True;
end;

procedure FileManagerCloseFile(FM: PFileManager; OpenFile: POpenFile);
var
  Current, Prev: POpenFile;
begin
  if (FM = nil) or (OpenFile = nil) then Exit;
  
  { Decrement refcount }
  Dec(OpenFile^.RefCount);
  if OpenFile^.RefCount > 0 then Exit;
  
  { Actually close the file }
  if OpenFile^.IsOpen then
  begin
    {$I-}
    Close(OpenFile^.Handle);
    {$I+}
  end;
  
  { Remove from linked list }
  Prev := nil;
  Current := FM^.OpenFiles;
  while Current <> nil do
  begin
    if Current = OpenFile then
    begin
      if Prev = nil then
        FM^.OpenFiles := Current^.Next
      else
        Prev^.Next := Current^.Next;
      Break;
    end;
    Prev := Current;
    Current := Current^.Next;
  end;
  
  Dec(FM^.OpenFileCount);
  Dispose(OpenFile);
end;

procedure FileManagerCloseAllFiles(FM: PFileManager);
var
  Current, Next: POpenFile;
begin
  if FM = nil then Exit;
  
  Current := FM^.OpenFiles;
  while Current <> nil do
  begin
    Next := Current^.Next;
    if Current^.IsOpen then
    begin
      {$I-}
      Close(Current^.Handle);
      {$I+}
    end;
    Dispose(Current);
    Current := Next;
  end;
  
  FM^.OpenFiles := nil;
  FM^.OpenFileCount := 0;
end;

function FileManagerEnsureFile(FM: PFileManager; FileEntry: PFileEntry): Boolean;
var
  FullPath: string;
  F: file;
  FileSize: Int64;
begin
  Result := False;
  if FM = nil then Exit;
  
  if FM^.Meta^.IsSingleFile then
  begin
    FullPath := JoinPath(FM^.BasePath, FM^.Meta^.Name);
    FileSize := FM^.Meta^.Length;
  end
  else
  begin
    if FileEntry = nil then Exit;
    FullPath := FileManagerGetPath(FM, FileEntry);
    FileSize := FileEntry^.Length;
  end;
  
  { Check if file already exists with correct size }
  if SysUtils.FileExists(FullPath) then
  begin
    if GetFileSize(FullPath, FileSize) then
    begin
      if FM^.Meta^.IsSingleFile then
        Result := (FileSize = FM^.Meta^.Length)
      else
        Result := (FileSize = FileEntry^.Length);
      Exit;
    end;
  end;
  
  { Create/truncate file }
  if not EnsureDirectory(FullPath) then Exit;
  
  Assign(F, FullPath);
  {$I-}
  Rewrite(F, 1);
  {$I+}
  
  if IOResult <> 0 then Exit;
  
  { Truncate to desired size (pre-allocation) }
  if FileSize > 0 then
  begin
    {$I-}
    Seek(F, FileSize - 1);
    Truncate(F);
    {$I+}
  end;
  
  {$I-}
  Close(F);
  {$I+}
  
  Result := IOResult = 0;
end;

{ ============================================================================ }
{ Block Operations                                                             }
{ ============================================================================ }

function FileManagerReadBlock(FM: PFileManager; Offset: QWord;
                              Len: Integer; Buffer: Pointer;
                              out IOReg: TIOResult): Boolean;
var
  Meta: PTorrentMeta;
  FileEntry: PFileEntry;
  FullPath: string;
  OpenFile: POpenFile;
  FileOffset: QWord;
  BytesToRead: Integer;
  BytesRead: LongInt = 0;
  TotalRead: Integer;
  Remaining: QWord;
begin
  Result := False;
  IOReg.Success := False;
  IOReg.BytesTransferred := 0;
  IOReg.ErrorMsg := '';
  
  if (FM = nil) or (Buffer = nil) or (Len <= 0) then
  begin
    IOReg.ErrorMsg := 'Invalid parameters';
    Exit;
  end;
  
  Meta := FM^.Meta;
  if Offset >= Meta^.TotalLength then
  begin
    IOReg.ErrorMsg := 'Offset beyond end of data';
    Exit;
  end;
  
  { Clamp read to available data }
  Remaining := Meta^.TotalLength - Offset;
  if QWord(Len) > Remaining then
    Len := Integer(Remaining);
  
  TotalRead := 0;
  
  if Meta^.IsSingleFile then
  begin
    { Single file - simple case }
    FullPath := JoinPath(FM^.BasePath, Meta^.Name);
    
    if not FileManagerOpenFile(FM, FullPath, fmRead, OpenFile) then
    begin
      IOReg.ErrorMsg := 'Failed to open file: ' + FullPath;
      Inc(FM^.Errors);
      Exit;
    end;
    
    try
      {$I-}
      Seek(OpenFile^.Handle, Offset);
      {$I+}
      
      if IOResult <> 0 then
      begin
        IOReg.ErrorMsg := 'Seek failed';
        Inc(FM^.Errors);
        Exit;
      end;
      
      {$I-}
      BlockRead(OpenFile^.Handle, Buffer^, Len, BytesRead);
      {$I+}
      
      if IOResult <> 0 then
      begin
        IOReg.ErrorMsg := 'Read failed';
        Inc(FM^.Errors);
        Exit;
      end;
      
      TotalRead := BytesRead;
    finally
      FileManagerCloseFile(FM, OpenFile);
    end;
  end
  else
  begin
    { Multi-file - need to handle file boundaries }
    while (Len > 0) and FindFileAtOffset(Meta, Offset, FileEntry) do
    begin
      if FileEntry = nil then Break;
      
      FullPath := FileManagerGetPath(FM, FileEntry);
      FileOffset := GetFileOffsetForTorrentOffset(Meta, FileEntry, Offset);
      
      { Calculate how much to read from this file }
      BytesToRead := Len;
      if FileOffset + QWord(Len) > FileEntry^.Length then
        BytesToRead := FileEntry^.Length - FileOffset;
      
      if BytesToRead <= 0 then Break;
      
      if not FileManagerOpenFile(FM, FullPath, fmRead, OpenFile) then
      begin
        IOReg.ErrorMsg := 'Failed to open file: ' + FullPath;
        Inc(FM^.Errors);
        Exit;
      end;
      
      try
        {$I-}
        Seek(OpenFile^.Handle, FileOffset);
        BlockRead(OpenFile^.Handle, (PByte(Buffer) + TotalRead)^, BytesToRead, BytesRead);
        {$I+}
        
        if IOResult <> 0 then
        begin
          IOReg.ErrorMsg := 'Read failed';
          Inc(FM^.Errors);
          Exit;
        end;
        
        Inc(TotalRead, BytesRead);
        Inc(Offset, BytesRead);
        Dec(Len, BytesRead);
        
        if BytesRead < BytesToRead then
          Break;  { EOF reached }
      finally
        FileManagerCloseFile(FM, OpenFile);
      end;
    end;
  end;
  
  IOReg.Success := TotalRead > 0;
  IOReg.BytesTransferred := TotalRead;
  Inc(FM^.BytesRead, TotalRead);
  Inc(FM^.ReadOps);
  Result := IOReg.Success;
end;

function FileManagerWriteBlock(FM: PFileManager; Offset: QWord;
                               Len: Integer; Buffer: Pointer;
                               out IOReg: TIOResult): Boolean;
var
  Meta: PTorrentMeta;
  FileEntry: PFileEntry;
  FullPath: string;
  OpenFile: POpenFile;
  FileOffset: QWord;
  BytesToWrite: Integer;
  BytesWritten: LongInt = 0;
  TotalWritten: Integer;
begin
  Result := False;
  IOReg.Success := False;
  IOReg.BytesTransferred := 0;
  IOReg.ErrorMsg := '';
  
  if (FM = nil) or (Buffer = nil) or (Len <= 0) then
  begin
    IOReg.ErrorMsg := 'Invalid parameters';
    Exit;
  end;
  
  Meta := FM^.Meta;
  if Offset >= Meta^.TotalLength then
  begin
    IOReg.ErrorMsg := 'Offset beyond end of data';
    Exit;
  end;
  
  TotalWritten := 0;
  
  if Meta^.IsSingleFile then
  begin
    { Single file }
    FullPath := JoinPath(FM^.BasePath, Meta^.Name);
    
    if not FileManagerOpenFile(FM, FullPath, fmReadWrite, OpenFile) then
    begin
      IOReg.ErrorMsg := 'Failed to open file: ' + FullPath;
      Inc(FM^.Errors);
      Exit;
    end;
    
    try
      {$I-}
      Seek(OpenFile^.Handle, Offset);
      BlockWrite(OpenFile^.Handle, Buffer^, Len, BytesWritten);
      {$I+}
      
      if IOResult <> 0 then
      begin
        IOReg.ErrorMsg := 'Write failed';
        Inc(FM^.Errors);
        Exit;
      end;
      
      TotalWritten := BytesWritten;
    finally
      FileManagerCloseFile(FM, OpenFile);
    end;
  end
  else
  begin
    { Multi-file }
    while (Len > 0) and FindFileAtOffset(Meta, Offset, FileEntry) do
    begin
      if FileEntry = nil then Break;
      
      FullPath := FileManagerGetPath(FM, FileEntry);
      FileOffset := GetFileOffsetForTorrentOffset(Meta, FileEntry, Offset);
      
      BytesToWrite := Len;
      if FileOffset + QWord(Len) > FileEntry^.Length then
        BytesToWrite := FileEntry^.Length - FileOffset;
      
      if BytesToWrite <= 0 then Break;
      
      if not FileManagerOpenFile(FM, FullPath, fmReadWrite, OpenFile) then
      begin
        IOReg.ErrorMsg := 'Failed to open file: ' + FullPath;
        Inc(FM^.Errors);
        Exit;
      end;
      
      try
        {$I-}
        Seek(OpenFile^.Handle, FileOffset);
        BlockWrite(OpenFile^.Handle, (PByte(Buffer) + TotalWritten)^, 
                   BytesToWrite, BytesWritten);
        {$I+}
        
        if IOResult <> 0 then
        begin
          IOReg.ErrorMsg := 'Write failed';
          Inc(FM^.Errors);
          Exit;
        end;
        
        Inc(TotalWritten, BytesWritten);
        Inc(Offset, BytesWritten);
        Dec(Len, BytesWritten);
      finally
        FileManagerCloseFile(FM, OpenFile);
      end;
    end;
  end;
  
  IOReg.Success := TotalWritten > 0;
  IOReg.BytesTransferred := TotalWritten;
  Inc(FM^.BytesWritten, TotalWritten);
  Inc(FM^.WriteOps);
  Result := IOReg.Success;
end;

{ ============================================================================ }
{ Piece Operations                                                             }
{ ============================================================================ }

function FileManagerReadPiece(FM: PFileManager; PieceIndex: Integer;
                              Buffer: Pointer; BufferSize: Integer;
                              out IOReg: TIOResult): Boolean;
var
  PieceLength: Cardinal;
  Offset: QWord;
begin
  Result := False;
  IOReg.Success := False;
  IOReg.BytesTransferred := 0;
  IOReg.ErrorMsg := '';
  
  if FM = nil then
  begin
    IOReg.ErrorMsg := 'FileManager is nil';
    Exit;
  end;
  
  if (PieceIndex < 0) or (PieceIndex >= FM^.Meta^.PieceCount) then
  begin
    IOReg.ErrorMsg := 'Invalid piece index';
    Exit;
  end;
  
  PieceLength := GetPieceLength(FM^.Meta, PieceIndex);
  
  if BufferSize < Integer(PieceLength) then
  begin
    IOReg.ErrorMsg := 'Buffer too small';
    Exit;
  end;
  
  Offset := QWord(PieceIndex) * FM^.Meta^.PieceLength;
  
  Result := FileManagerReadBlock(FM, Offset, PieceLength, Buffer, IOReg);
  IOReg.Success := IOReg.BytesTransferred = Integer(PieceLength);
end;

function FileManagerWritePiece(FM: PFileManager; PieceIndex: Integer;
                               Buffer: Pointer; DataLen: Integer;
                               out PieceReg: TPieceWriteResult): Boolean;
var
  ExpectedLength: Cardinal;
  Offset: QWord;
  IOResult: TIOResult;
  Hash: TSHA1Digest;
begin
  Result := False;
  PieceReg.Success := False;
  PieceReg.Verified := False;
  PieceReg.ErrorMsg := '';
  
  if FM = nil then
  begin
    PieceReg.ErrorMsg := 'FileManager is nil';
    Exit;
  end;
  
  if (PieceIndex < 0) or (PieceIndex >= FM^.Meta^.PieceCount) then
  begin
    PieceReg.ErrorMsg := 'Invalid piece index';
    Exit;
  end;
  
  ExpectedLength := GetPieceLength(FM^.Meta, PieceIndex);
  
  if DataLen <> Integer(ExpectedLength) then
  begin
    PieceReg.ErrorMsg := 'Data length mismatch';
    Exit;
  end;
  
  { Verify hash before writing }
  Hash := SHA1Buffer(Buffer^, DataLen);
  if not SHA1Equal(Hash, GetPieceHash(FM^.Meta, PieceIndex)^) then
  begin
    PieceReg.ErrorMsg := 'Hash verification failed';
    Exit;
  end;
  
  PieceReg.Verified := True;
  
  Offset := QWord(PieceIndex) * FM^.Meta^.PieceLength;
  
  if not FileManagerWriteBlock(FM, Offset, DataLen, Buffer, IOResult) then
  begin
    PieceReg.ErrorMsg := IOResult.ErrorMsg;
    Exit;
  end;
  
  if IOResult.BytesTransferred <> DataLen then
  begin
    PieceReg.ErrorMsg := 'Incomplete write';
    Exit;
  end;
  
  { Mark as verified }
  FileManagerMarkPieceVerified(FM, PieceIndex);
  
  PieceReg.Success := True;
  Result := True;
end;

function FileManagerVerifyPiece(FM: PFileManager; PieceIndex: Integer;
                                out IOReg: TIOResult): Boolean;
var
  PieceLength: Cardinal;
  Buffer: Pointer;
  ActualHash: TSHA1Digest;
  ExpectedHash: PSHA1Digest;
begin
  Result := False;
  IOReg.Success := False;
  IOReg.BytesTransferred := 0;
  IOReg.ErrorMsg := '';
  
  if FM = nil then
  begin
    IOReg.ErrorMsg := 'FileManager is nil';
    Exit;
  end;
  
  if (PieceIndex < 0) or (PieceIndex >= FM^.Meta^.PieceCount) then
  begin
    IOReg.ErrorMsg := 'Invalid piece index';
    Exit;
  end;
  
  { Check cache first }
  if FileManagerIsPieceVerified(FM, PieceIndex) then
  begin
    IOReg.Success := True;
    Result := True;
    Exit;
  end;
  
  PieceLength := GetPieceLength(FM^.Meta, PieceIndex);
  
  GetMem(Buffer, PieceLength);
  if Buffer = nil then
  begin
    IOReg.ErrorMsg := 'Memory allocation failed';
    Exit;
  end;
  
  try
    if not FileManagerReadPiece(FM, PieceIndex, Buffer, PieceLength, IOReg) then
    begin
      IOReg.ErrorMsg := 'Failed to read piece: ' + IOReg.ErrorMsg;
      Exit;
    end;
    
    if IOReg.BytesTransferred <> Integer(PieceLength) then
    begin
      IOReg.ErrorMsg := 'Incomplete read';
      Exit;
    end;
    
    { Compute hash }
    ActualHash := SHA1Buffer(Buffer^, PieceLength);
    ExpectedHash := GetPieceHash(FM^.Meta, PieceIndex);
    
    if not SHA1Equal(ActualHash, ExpectedHash^) then
    begin
      IOReg.ErrorMsg := 'Hash mismatch';
      Exit;
    end;
    
    { Mark as verified }
    FileManagerMarkPieceVerified(FM, PieceIndex);
    
    IOReg.Success := True;
    Result := True;
  finally
    FreeMem(Buffer);
  end;
end;

function FileManagerIsPieceVerified(FM: PFileManager; PieceIndex: Integer): Boolean;
begin
  Result := False;
  if FM = nil then Exit;
  if (PieceIndex < 0) or (PieceIndex >= FM^.Meta^.PieceCount) then Exit;
  if FM^.VerifiedPieces = nil then Exit;
  
  Result := GetBit(FM^.VerifiedPieces, BitfieldSize(FM^.Meta^.PieceCount), PieceIndex);
end;

procedure FileManagerMarkPieceVerified(FM: PFileManager; PieceIndex: Integer);
begin
  if FM = nil then Exit;
  if (PieceIndex < 0) or (PieceIndex >= FM^.Meta^.PieceCount) then Exit;
  if FM^.VerifiedPieces = nil then Exit;
  
  SetBit(FM^.VerifiedPieces, BitfieldSize(FM^.Meta^.PieceCount), PieceIndex);
end;

{ ============================================================================ }
{ Progress and Status                                                          }
{ ============================================================================ }

function FileManagerGetCompletion(FM: PFileManager): Double;
begin
  Result := 0.0;
  if FM = nil then Exit;
  if FM^.Meta = nil then Exit;
  
  Result := GetCompletionFromBitfield(FM^.VerifiedPieces, 
            BitfieldSize(FM^.Meta^.PieceCount), FM^.Meta^.PieceCount);
end;

function FileManagerGetVerifiedBytes(FM: PFileManager): QWord;
var
  Completion: Double;
begin
  Result := 0;
  if FM = nil then Exit;
  if FM^.Meta = nil then Exit;
  
  Completion := FileManagerGetCompletion(FM);
  Result := Trunc(Completion * FM^.Meta^.TotalLength);
end;

procedure FileManagerGetStats(FM: PFileManager; out ReadBytes, WrittenBytes: QWord;
                              out ReadOps, WriteOps, Errors: Cardinal);
begin
  if FM = nil then
  begin
    ReadBytes := 0;
    WrittenBytes := 0;
    ReadOps := 0;
    WriteOps := 0;
    Errors := 0;
    Exit;
  end;
  
  ReadBytes := FM^.BytesRead;
  WrittenBytes := FM^.BytesWritten;
  ReadOps := FM^.ReadOps;
  WriteOps := FM^.WriteOps;
  Errors := FM^.Errors;
end;

function FileManagerCheckAllPieces(FM: PFileManager;
                                   ProgressCallback: TPieceCheckProgressCallback): Boolean;
var
  I: Integer;
  VerifyResult: TIOResult;
begin
  Result := True;
  if FM = nil then Exit;
  
  for I := 0 to FM^.Meta^.PieceCount - 1 do
  begin
    FileManagerVerifyPiece(FM, I, VerifyResult);
    
    if Assigned(ProgressCallback) then
      ProgressCallback(I, FM^.Meta^.PieceCount);
  end;
  
  Result := True;
end;

{ ============================================================================ }
{ Utility Functions                                                            }
{ ============================================================================ }

function FileManagerGetPath(FM: PFileManager; FileEntry: PFileEntry): string;
begin
  if FM = nil then
  begin
    Result := '';
    Exit;
  end;
  
  Result := GetFullFilePath(FM^.Meta, FileEntry, FM^.BasePath);
end;

function FileManagerFilesExist(FM: PFileManager): Boolean;
var
  FileEntry: PFileEntry;
  FullPath: string;
begin
  Result := False;
  if FM = nil then Exit;
  
  if FM^.Meta^.IsSingleFile then
  begin
    FullPath := JoinPath(FM^.BasePath, FM^.Meta^.Name);
    Result := SysUtils.FileExists(FullPath);
  end
  else
  begin
    FileEntry := FM^.Meta^.Files;
    while FileEntry <> nil do
    begin
      FullPath := FileManagerGetPath(FM, FileEntry);
      if not SysUtils.FileExists(FullPath) then
      begin
        Result := False;
        Exit;
      end;
      FileEntry := FileEntry^.Next;
    end;
    Result := True;
  end;
end;

function FileManagerPreallocateFiles(FM: PFileManager): Boolean;
var
  FileEntry: PFileEntry;
  F: file;
  FullPath: string;
  Size: Int64;
begin
  Result := False;
  if FM = nil then Exit;
  
  if FM^.Meta^.IsSingleFile then
  begin
    FullPath := JoinPath(FM^.BasePath, FM^.Meta^.Name);
    Size := FM^.Meta^.Length;
    
    if not EnsureDirectory(FullPath) then Exit;
    
    Assign(F, FullPath);
    {$I-}
    Rewrite(F, 1);
    if IOResult = 0 then
    begin
      if Size > 0 then
      begin
        Seek(F, Size - 1);
        Truncate(F);
      end;
      Close(F);
      Result := IOResult = 0;
    end;
    {$I+}
  end
  else
  begin
    FileEntry := FM^.Meta^.Files;
    while FileEntry <> nil do
    begin
      FullPath := FileManagerGetPath(FM, FileEntry);
      Size := FileEntry^.Length;
      
      if not EnsureDirectory(FullPath) then Exit;
      
      Assign(F, FullPath);
      {$I-}
      Rewrite(F, 1);
      if IOResult = 0 then
      begin
        if Size > 0 then
        begin
          Seek(F, Size - 1);
          Truncate(F);
        end;
        Close(F);
        if IOResult <> 0 then Exit;
      end
      else
      begin
        Exit;
      end;
      {$I+}
      
      FileEntry := FileEntry^.Next;
    end;
    Result := True;
  end;
end;

function FileManagerDeleteAllFiles(FM: PFileManager): Boolean;
var
  FileEntry: PFileEntry;
  FullPath: string;
begin
  Result := True;
  if FM = nil then Exit;
  
  { Close all files first }
  FileManagerCloseAllFiles(FM);
  
  if FM^.Meta^.IsSingleFile then
  begin
    FullPath := JoinPath(FM^.BasePath, FM^.Meta^.Name);
    if SysUtils.FileExists(FullPath) then
    begin
      if not DeleteFile(FullPath) then
        Result := False;
    end;
  end
  else
  begin
    FileEntry := FM^.Meta^.Files;
    while FileEntry <> nil do
    begin
      FullPath := FileManagerGetPath(FM, FileEntry);
      if SysUtils.FileExists(FullPath) then
      begin
        if not DeleteFile(FullPath) then
          Result := False;
      end;
      FileEntry := FileEntry^.Next;
    end;
    
    { Try to remove the main directory }
    FullPath := JoinPath(FM^.BasePath, FM^.Meta^.Name);
    {$I-}
    RmDir(FullPath);
    {$I+}
  end;
end;

end.
