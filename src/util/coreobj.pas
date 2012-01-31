{   Unit with core datastructures and bare access.

    Copyright (C) 2011-2012  Matthias Karbe

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; see COPYING.txt.
    if not, see <http://www.gnu.org/licenses/> or
    write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

{DOC>> This unit contains core datastructures, with bare access/bare routines
       which will be fast when used correctly. (and very unforseeable when used
       wrong, so read documentation and dont expect safety mechanisms)}

unit coreobj;

{$mode objfpc}{$H+}

interface

uses SysUtils, commontl, ucfs, corealg;

type
  {DOC>> Visitor for DataBucket.ForEach, xdata is passed by ForEach caller,
         data is the current Data for the visited element.
         Return @false if there is need to break the visitation.}
  TDataVisitorProc = function( data, xdata: Pointer ) : Boolean;

  {DOC>> Bare (Pointer) Bucket Type with several usefull Operations}
  TDataBucket = object
    {DOC>> Data Field}
    Data: array of Pointer;
    {DOC>> Init Data Array with Length ALen, Zero if AAndNil}
    procedure Init( AAndNil: Boolean; ALen: MachineInt ); inline;
    {DOC>> Grow Data Array by n, Zero the new Slots if AAndNil
           @param n must be > 0
           @raises ERangeError when OPT R+ and n is Invalid}
    procedure Grow( AAndNil: Boolean; n: MachineInt ); inline;
    {DOC>> Shrink Data Array by n
           @param n must be > 0 and <= Length(Data)
           @raises ERangeError when OPT R+ and n is Invalid}
    procedure Shrink( n: MachineInt ); inline;
    {DOC>> Swap Slot AIdx1 and AIdx2
           @raises ERangeError when OPT R+ and AIdx1 or AIdx2 is OOB}
    procedure Swap( AIdx1, AIdx2: MachineInt ); inline;
    {DOC>> Delete an Shift (ordered Deletion), deletes a Block of
           ANrElems starting at AIdx and shifts everything from
           AIdx+ANrElems..High(Data) down by ANrElems, if AAndNil
           the upper ANrElems places will be cleared out.
           @param AIdx Index of first Element to Delete, must be in [0..Length(Data)-ANrElems]
           @param ANrElems Nr of Elements to Delete, must be in [1..Length(Data)-AIdx)]
           @raises ERangeError when OPT R+ and any Param is Invalid}
    procedure DeleteShift( AAndNil: Boolean; AIdx: MachineInt; ANrElems: MachineInt ); inline;
    {DOC>> Delete an Swap (fast Deletion). Much like DeleteShift,
           but Moves a Block (if possible) Length(Data)-ANrElems..High(Data)
           at AIdx, rather than shifting the whole thing
           @param AIdx Index of first Element to Delete, must be in [0..Length(Data)-ANrElems]
           @param ANrElems Nr of Elements to Delete, must be in [1..Length(Data)-AIdx)]
           @raises ERangeError when OPT R+ and any Param is Invalid}
    procedure DeleteSwap( AAndNil: Boolean; AIdx: MachineInt; ANrElems: MachineInt ); inline;
    {DOC>> Move a Block AFromIdx..AFromIdx+(ABlockSize-1) down by ANrElems
           @param AFromIdx Index of First Block Element, must be in [ANrElems..Length(Data)-ABlockSize]
           @param ANrElems Nr of Elements to Move, must be in [1..AFromIdx]
           @param ABlockSize Size of Block to Move, must be in [1..Length(Data)-AFromIdx]
           @raises ERangeError when OPT R+ and any Param is Invalid}
    procedure MoveBlockDown( AFromIdx, ANrElems, ABlockSize: MachineInt ); inline;
    {DOC>> Move a Block AFromIdx..AFromIdx+(ABlockSize-1) up by ANrElems
           @param AFromIdx Index of First Block Element, must be in [0..Length(Data)-(ABlockSize+ANrElems)]
           @param ANrElems Nr of Elements to Move, must be in [1..Length(Data)-(AFromIdx+ABlocksize)]
           @param ABlockSize Size of Block to Move, must be in [1..Length(Data)-(AFromIdx+(ANrElems-1))]
           @raises ERangeError when OPT R+ and any Param is Invalid}
    procedure MoveBlockUp( AFromIdx, ANrElems, ABlockSize: MachineInt ); inline;
    {DOC>> Shift Down AFromIdx..High(Data) by ANrElems using ShiftBlockDown
           @param AFromIdx Index of First Element (for the Upper Part), must be in [ANrElems..High(Data)]
           @param ANrElems Nr of Elements to Shift, must be in [1..AFromIdx]
           @raises ERangeError when OPT R+ and any Param is Invalid}
    procedure ShiftDown( AFromIdx, ANrElems: MachineInt ); inline;
    {DOC>> Shift Up AFromIdx..(High(Data)-ANrElems) by ANrElems using ShiftBlockUp
           @param AFromIdx Index of First Element (for the Upper Part), must be in [0..High(Data)-ANrElems]
           @param ANrElems Nr of Elements to Shift, must be in [1..Length(Data)-(AFromIdx+ANrElems)]
           @raises ERangeError when OPT R+ and any Param is Invalid}
    procedure ShiftUp( AFromIdx, ANrElems: MachineInt ); inline;
    {DOC>> Visit all Elements of the Data Bucket, xdata is passed for each call,
           ForEach returns @false if any visit call returned false, otherwise it returns
           true (also for empty bucket)}
    function ForEach( AVisitor: TDataVisitorProc; xdata: Pointer ): Boolean; inline;
    {DOC>> Same as ForEach, but Visits only a certain Range from AFromIdx to
           AToIdx. If AFromIdx < AToIdx, nothing is done and ForIn returns @true.}
    function ForIn( AFromIdx, AToIdx: MachineInt; AVisitor: TDataVisitorProc; xdata: Pointer ): Boolean; {$IFOPT R-} inline; {$ENDIF}
  end;

  {DOC>> CritBit Trie Iterator. Mostly same as the DataBucket Iterator (TDataVisitorProc), passing
         the Current CritBit Entry position. Index can then be used to Access the
         Data contained in Node[Index] using GetNodeData, setting it with
         SetNodeData or getting the Value for this Entry using GetNodeValue.}
  TDataCritBitVisitorProc = function( index: MachineInt; xdata: Pointer ) : Boolean;

  {DOC>> A single CritBit Node. Contains DiffBit (differencing bit/"CritBit" position or
         a value that marks this Node as Leaf [data entry]), the Previous Node used
         for faster Deletion/Insertion and either both NextNodes [for inner node]
         or Value/Data pair [for data entry].}
  TDataCritBitNode = record
    //DOC>> if >= 0 Differing Bit for NextNode, if < 0, this is a Leaf and Value/Data can be read
    DiffBit: MachineInt;
    //DOC>> if >0 Previous Node, otherwise this is the Root
    PrevNode: MachineInt;
    case Boolean of
      True: (
        //DOC>> Next Node for Critical Bit Set=index 1 and Unset=index 0
        NextNode: array[0..1] of MachineInt
      );
      False: (
        //DOC>> Value, needed for Comparision
        Value: MachineWord;
        //DOC>> Data associated with Value
        Data: Pointer
      );
  end;

  {DOC>> Field of CritBitNodes.}
  TDataCritBitNodes = array of TDataCritBitNode;

  {DOC>> Bare SizeOf(MachineWord)-Fixed-Length Critical Bit Trie. ( or Lookup
         Tree which orders its Elements by a fixed value and its
         Suffix/Prefix. It has some nice properties:
         @unorderedList(
           @item for n contained values we got n-1 internal nodes
           @item an internal splits the element set into 2 subtries
         )
         For this implementation (Suffix, where msb is the End and lsb is the start):
         @unorderedList(
           @item all values in the left (Critbit=0) subtrie are < then those in the right
           @item the max. internal depth of the tree is the max number of path decisions = the number of bits
           @item each node needs SizeOf(Pointer)*4 Byte
           @item( those the Trie needs SizeOf(Pointer)*4*(2*n-1) Byte for n Elements,
            f.e. 32bit Addr and 1024 nodes = 2^2*2^2*2^1*2^10 - (2^2*2^2) ~ 32kb)
         )
         }
  TDataCritBitTrie = object
    private
      //DOC>> Node Field
      Nodes: TDataCritBitNodes;
      //DOC>> Nr Nodes currently in use
      AllocatedNodes: MachineInt;
      //DOC>> Trie Root Index (<0 if empty)
      Root: MachineInt;
      //DOC>> Linear Grows Value
      LinGrow: MachineInt;
    protected
      {DOC>> Grow Nodes Field and Inc AllocatedNodes by n}
      procedure GrowNodes( n: MachineInt );
      {DOC>> Remove a Node and Defragment the Nodes by swapping and correcting}
      procedure RemNode( i: MachineInt );
      {DOC>> Lookup "Nearest Value" Index, which may be same as val}
      function LookupNV( val: MachineWord ): MachineInt;
    public
      {DOC>> Returns the Nr of Allocated Nodes (both inner/data entries)}
      function GetNodeCount: MachineInt; inline;
      {DOC>> Returns the Nr of Data Entries. This is in effect
             (GetNodeCount+1) div 2 since
             the Trie size in Nodes is allways (Entries*2)-1.}
      function GetCount: MachineInt; inline;
      {DOC>> Returns true if Node with index i is a Data Node, otherwise false}
      function IsDataNode( i: MachineInt ): Boolean; inline;
      {DOC>> Return the Value for a Node or 0 if the Node is an inner Node.
             be aware that its invalid for "Exists" tests, since 0 is also
             a valid value.}
      function GetNodeValue( i: MachineInt ): MachineWord; inline;
      {DOC>> Sets the Value for a Data Entrie. Ignored if param i is not a
             data Node index.}
      procedure SetNodeData( i: MachineInt; data: Pointer ); inline;
      {DOC>> Gets the Value for a Data Entrie. Returns nil if param i is not a
             data Node index.}
      function GetNodeData( i: MachineInt ): Pointer; inline;
      {DOC>> Lookup and Check @returns value less than 0 if not found,
             otherwise the Leaf Index}
      function Lookup( val: MachineWord ): MachineInt; inline;
      {DOC>> Lookup or Add @returns the Leaf Index}
      function Add( val: MachineWord ): MachineInt;
      {DOC>> Delete Value and its associated Data}
      procedure Delete( val: MachineWord );
      {DOC>> Init structure, Preallocate APrealloc Nodes and Set
             Linear Grows.}
      procedure Init( APrealloc: MachineInt = 0; ALinGrow: MachineInt = 8 ); inline;
      {DOC>> Pack the Nodes Field, this should be done after massive deletions
             or whenever no new Adds are expected.}
      procedure Pack; inline;
      {DOC>> Visit all Data Nodes. xdata can be passed.
             Visitors Index is always a Data Node Index, so no checking
             is needed.}
      function ForEach( AVisitor: TDataCritBitVisitorProc; xdata: Pointer ): Boolean; inline;
      {DOC>> Clear the NodeField, Resets Trie to 0 Entries.}
      procedure Clear; inline;
      {DOC>> Same as Clear, just more intuitive alias for trie finalization.}
      procedure Done; inline;
  end;

  {DOC>> HashTrie Visitor Type}
  THashTrieVisitorProc = function( us: PUCFS32String; data, xdata: Pointer ) : Boolean;

  PHashTrieCollision = ^THashTrieCollision;
  {DOC>> HashTrie Collision Chain Element}
  THashTrieCollision = object
    {DOC>> Key}
    colstring: PUCFS32String;
    {DOC>> Saved Data}
    data: Pointer;
    {DOC>> Next Link in Collision}
    next: PHashTrieCollision;
  end;

  PHashTrie = ^THashTrie;
  {DOC>> HashTable using Pointer Size Hash and a CritBit Trie for
         Hash Lookup}
  THashTrie = object
    protected
      FTrie: TDataCritBitTrie;
      FCount: MachineInt;
      function Hash(s: PUCFS32String): MachineWord;
    public
      {DOC>> Get Nr Buckets (HashTrie Entries)}
      function GetBuckets: MachineInt; inline;
      {DOC>> Get Nr Entries}
      function GetCount: MachineInt; inline;
      {DOC>> Checks for Existence of a given Key (so, will work with
             nil data too)}
      function Exists( const key: String ): Boolean; deprecated;
      {DOC>> Checks for Existence of a given Key}
      function Exists( key: PUCFS32String ): Boolean;
      {DOC>> Lookup Data, returns the associated Data if key exists or nil}
      function Lookup( const key: String ): Pointer; deprecated;
      {DOC>> Lookup Data, returns the associated Data if key exists or nil}
      function Lookup( key: PUCFS32String ): Pointer;
      {DOC>> Lookup Data by hash, returns the associated Data if key exists or nil}
      function LookupByHash( h: MachineWord; const key: String ): Pointer; deprecated;
      {DOC>> Lookup Data by hash, returns the associated Data if key exists or nil}
      function LookupByHash( h: MachineWord; key: PUCFS32String ): Pointer;
      {DOC>> Add/Replace, Creates an association for Key or Replaces
             the current one (returning the old data)}
      function Add( const key: String; newdata: Pointer ): Pointer; deprecated;
      {DOC>> Add/Replace, Creates an association for Key or Replaces
             the current one (returning the old data)
             key is copied for new entries}
      function Add( key: PUCFS32String; newdata: Pointer ): Pointer;
      {DOC>> Remove an association, return associated Data}
      function Delete( const key: String ): Pointer; deprecated;
      {DOC>> Remove an association, return associated Data}
      function Delete( key: PUCFS32String ): Pointer;
      {DOC>> Initialize Trie with APrealloc (Default 0), Slots}
      procedure Init( APrealloc: MachineInt = 0; ALinGrow: MachineInt = 8 ); inline;
      {DOC>> Pack the Tries internal nodelist (f.e. after many removals,
             or clearing)}
      procedure Pack; inline;
      {DOC>> Clear the Trie, this deletes all associations}
      procedure Clear;
      {DOC>> Run Visitor, the Visitor Caller will scan the internal Nodes
             resulting in non-ordered, but fast Listing.
             xdata will be passed to the Vistor, If the visitor
             returns @false, this routine returns @false too, otherwise
             @true.}
      function ForEach( AVisitor: THashTrieVisitorProc; xdata: Pointer ): Boolean; inline;
      {DOC>> Same as Clear, more intuitive alias for Trie finalization.}
      procedure Done; inline;
  end;

  THashTrieIterator = record
    currcoll: PHashTrieCollision;
    nidx: MachineInt;
  end;

{DOC>> Iterate on Collision chains. It uses bare access to the coll chains
       for efficiency, not hacking. @br
       Returns false if there is no Next/First.
       Resurns true, if Next (or First) was found.
       iter.currcoll is the current collison entry.
       If false is returned, this will reset the Iterator. @br @br
       Iteration IS NOT SECURED. Modifications on the Trie while Iterating will
       Produce wrong Pointers and possible Crashes and undefined behaviour.
       @br @br
       Changing the Key in collision chains has the sideeffect, that in 99%
       (yes.. 99% is probably to high.. anyways its not 100% since variable
        sized string to some Integer Hashing is never Perfect.)
       of cases (whenever the hash is different) this element is not
       found again. Dont assume anything, just DONT CHANGE THE KEY.
       @br @br
       Just side node, dont allocate THashTrieIterator in a stack frame that
       vanishes while iterating ^^}
function hashtrie_iterator_next( phtrie: PHashTrie; var iter: THashTrieIterator ): Boolean;
{DOC>> init or reset Iterator.
       This simply sets nidx to -1, but "hashtrie_iterator_init"
       says more than "somevar.nix:= -1" ^^}
procedure hashtrie_iterator_reset( var iter: THashTrieIterator ); inline;
{DOC>> check if the Iterator is (re-)initialized. returns true if initialized.
       otherwise false.}
function hashtrie_iterator_done( const iter: THashTrieIterator ): Boolean; inline;

type
  PObjectHashTrie = ^TObjectHashTrie;
  {DOC>> Object Wrapper for THashTrie}
  TObjectHashTrie = object
    protected
      FTrie: THashTrie;
    public
      {DOC>> Get Nr Entries}
      function GetCount: MachineInt; inline;
      {DOC>> Checks for Existence of a given Key}
      function Exists( const key: String ): Boolean; inline;
      {DOC>> Lookup Data, returns the associated Data if key exists or nil}
      function Lookup( const key: String ): TObject;
      {DOC>> Add/Replace, Creates an association for Key or Replaces
             the current one (returning the old data, or freeind it)}
      function Add( const key: String; newdata: TObject; AAndFree: Boolean ): TObject;
      {DOC>> Remove an association, return associated Data, or free it}
      function Delete( const key: String; AAndFree: Boolean ): TObject;
      {DOC>> Initialize Trie with APrealloc (Default 0), Slots}
      procedure Init( APrealloc: MachineInt=0; ALinGrow: MachineInt = 16 ); inline;
      {DOC>> Pack the Tries internal nodelist (f.e. after many removals,
             or clearing)}
      procedure Pack; inline;
      {DOC>> Clear the Trie, this deletes all associations}
      procedure Clear( AAndFree: Boolean );
      {DOC>> Run Visitor, the Visitor Caller will scan the internal Nodes
             resulting in non-ordered, but fast Listing.
             xdata will be passed to the Vistor, If the visitor
             returns @false, this routine returns @false too, otherwise
             @true.}
      function ForEach( AVisitor: THashTrieVisitorProc; xdata: Pointer ): Boolean; inline;
      {DOC>> Same as Clear, more intuitive alias for Trie finalization.}
      procedure Done( AAndFree: Boolean ); inline;
  end;

implementation

{$MACRO ON}

{$IF SizeOf(MachineWord) = 4}
  {check for Bsr intrinsic (32bit)}
  {$IF DECLARED(BsrDWord)}
    {$DEFINE bitscan_from_msb := BsrDWord}
  {$ENDIF}
{$ELSEIF SizeOf(MachineWord) = 8}
  {check for Bsr intrinsic (32bit)}
  {$IF DECLARED(BsrDWord)}
    {$DEFINE bitscan_from_msb := BsrQWord}
  {$ENDIF}
{$ENDIF}

{no bsr intrinsic -> do it pure}
{$IF NOT DEFINED(bitscan_from_msb)}
function bitscan_from_msb( nval: MachineWord ): MachineWord; inline;
begin
  ASSERT(nval <> MachineWord(0));
  Result := (SizeOf(MachineWord)-1)*8;
  // first check bytes
  while ((nval shr Result) and $FF) = 0 do
    Dec(Result,8);
  // found in byte x, bit is at ((x+1)*8)-1..x*8, so need to scan this byte
  Inc(Result,7);
  // scan the byte
  while ((nval shr Result) and 1) = 0 do
    Dec(Result,1);
end;
{$ENDIF}

{ TObjectHashTrie }

function TObjectHashTrie.GetCount: MachineInt;
begin
  Result := FTrie.GetCount;
end;

function TObjectHashTrie.Exists( const key: String ): Boolean;
begin
  Result := FTrie.Exists(key);
end;

function TObjectHashTrie.Lookup( const key: String ): TObject;
begin
  Result := TObject(FTrie.Lookup(key));
end;

function TObjectHashTrie.Add( const key: String; newdata: TObject; AAndFree: Boolean ): TObject;
begin
  Result := TObject(FTrie.Add(key,newdata));
  if AAndFree and Assigned(Result) then
    begin
      TObject(Result).Free;
      Result := nil;
    end;
end;

function TObjectHashTrie.Delete( const key: String; AAndFree: Boolean ): TObject;
begin
  Result := TObject(FTrie.Delete(key));
  if AAndFree and Assigned(Result) then
    begin
      TObject(Result).Free;
      Result := nil;
    end;
end;

procedure TObjectHashTrie.Init( APrealloc: MachineInt; ALinGrow: MachineInt);
begin
  FTrie.Init(APrealloc,ALinGrow);
end;

procedure TObjectHashTrie.Pack;
begin
  FTrie.Pack;
end;

function ObjectHashTrieCleaner( unused: PUCFS32String; data, xunused: Pointer ): Boolean;
begin
  if Assigned(data) then
    TObject(data).Free;
  Result := True;
end;

procedure TObjectHashTrie.Clear( AAndFree: Boolean );
begin
  if AAndFree then
    ForEach(@ObjectHashTrieCleaner,nil);
  FTrie.Clear;
end;

function TObjectHashTrie.ForEach( AVisitor: THashTrieVisitorProc; xdata: Pointer ): Boolean;
begin
  Result := FTrie.ForEach(AVisitor,xdata);
end;

procedure TObjectHashTrie.Done(AAndFree: Boolean);
begin
  Clear(AAndFree);
end;

{ THashTrie }

function THashTrie.Hash(s: PUCFS32String): MachineWord;
begin
  if ucfs_length(s) > 0 then
    Result := mas3hash_sigma(s)
  else
    Result := mas3hash_len0;
end;

function THashTrie.GetBuckets: MachineInt;
begin
  Result := FTrie.GetCount;
end;

function THashTrie.GetCount: MachineInt;
begin
  Result := FCount;
end;

function THashTrie.Exists(const key: String): Boolean;
var tmps: PUCFS32String;
begin
  tmps := ucfs_utf8us(key);
  Result := Exists(tmps);
  ucfs_release(tmps);
end;

function THashTrie.Exists(key: PUCFS32String): Boolean;
var chain: PHashTrieCollision;
    idx: MachineInt;
begin
  Result := false;
  idx := FTrie.Lookup(Hash(key));
  if idx >= 0 then
    begin
      chain := FTrie.GetNodeData(idx);
      while Assigned(chain) and
            (ucfs_compare(chain^.colstring,key) <> 0) do
        chain := chain^.next;
      Result := Assigned(chain);
    end;
end;

function THashTrie.Lookup(const key: String): Pointer;
var tmps: PUCFS32String;
begin
  tmps := ucfs_utf8us(key);
  Result := Lookup(tmps);
  ucfs_release(tmps);
end;

function THashTrie.Lookup(key: PUCFS32String): Pointer;
var chain: PHashTrieCollision;
    idx: MachineInt;
begin
  Result := nil;
  idx := FTrie.Lookup(Hash(key));
  if idx >= 0 then
    begin
      chain := FTrie.GetNodeData(idx);
      while Assigned(chain) and
            (ucfs_compare(chain^.colstring,key) <> 0) do
        chain := chain^.next;
      if Assigned(chain) then
        Result := chain^.data;
    end;
end;

function THashTrie.LookupByHash(h: MachineWord; const key: String): Pointer;
var tmps: PUCFS32String;
begin
  tmps := ucfs_utf8us(key);
  Result := LookupByHash(h,tmps);
  ucfs_release(tmps);
end;

function THashTrie.LookupByHash(h: MachineWord; key: PUCFS32String): Pointer;
var chain: PHashTrieCollision;
    idx: MachineInt;
begin
  Result := nil;
  idx := FTrie.Lookup(h);
  if idx >= 0 then
    begin
      chain := FTrie.GetNodeData(idx);
      while Assigned(chain) and
            (ucfs_compare(chain^.colstring,key) <> 0) do
        chain := chain^.next;
      if Assigned(chain) then
        Result := chain^.data;
    end;
end;

function THashTrie.Add(const key: String; newdata: Pointer): Pointer;
var tmps: PUCFS32String;
begin
  tmps := ucfs_utf8us(key);
  Result := Add(tmps,newdata);
  ucfs_release(tmps);
end;

function THashTrie.Add(key: PUCFS32String; newdata: Pointer): Pointer;
var ochain,chain: PHashTrieCollision;
    idx: MachineInt;
begin
  Result := nil;
  {lookupadd, create entry if not existend}
  idx := FTrie.Add(Hash(key));
  ochain := FTrie.GetNodeData(idx);
  if Assigned(ochain) then
    begin
      {check for existing entry}
      chain := ochain;
      while Assigned(chain) and
            (ucfs_compare(chain^.colstring,key) <> 0) do
        chain := chain^.next;
      if Assigned(chain) then
        begin
          {replace, done}
          Result := chain^.data;
          chain^.data := newdata;
          Exit(Result);
        end;
    end;
  {add new (or first) collision}
  chain := New(PHashTrieCollision);
  chain^.colstring := ucfs_incref(key);
  chain^.data := newdata;
  chain^.next := ochain;
  Inc(FCount,1);
  FTrie.SetNodeData(idx,chain);
end;

function THashTrie.Delete(const key: String): Pointer;
var tmps: PUCFS32String;
begin
  tmps := ucfs_utf8us(key);
  Result := Delete(tmps);
  ucfs_release(tmps);
end;

function THashTrie.Delete(key: PUCFS32String): Pointer;
var pchain,chain: PHashTrieCollision;
    idx: MachineInt;
    h: MachineWord;
begin
  Result := nil;
  h := Hash(key);
  idx := FTrie.Lookup(h);
  if idx >= 0 then
    begin
      chain := FTrie.GetNodeData(idx);
      if Assigned(chain) then
        begin
          {check for existing entry, preserve predecessor}
          pchain := nil;
          while Assigned(chain) and
                (ucfs_compare(chain^.colstring,key) <> 0) do
            begin
              pchain := chain;
              chain := chain^.next;
            end;
          if Assigned(chain) then
            begin
              {found, kill chain element}
              Result := chain^.data;
              {kill chain element}
              if not Assigned(pchain) then
                begin
                  {no pred, either set succ or delete the val from trie}
                  if not Assigned(chain^.next) then
                    FTrie.Delete(h)
                  else
                    FTrie.SetNodeData(idx,chain);
                end
              else
                pchain^.next := chain^.next;
              ucfs_release(chain^.colstring);
              Dec(FCount,1);
              Dispose(chain);
            end;
        end;
    end;
end;

procedure THashTrie.Init(APrealloc: MachineInt; ALinGrow: MachineInt);
begin
  FTrie.Init(APrealloc,ALinGrow);
  FCount := 0;
end;

procedure THashTrie.Pack;
begin
  FTrie.Pack;
end;

function THashTrie.ForEach(AVisitor: THashTrieVisitorProc; xdata: Pointer
  ): Boolean;
var i: MachineInt;
    chain: PHashTrieCollision;
begin
  for i := 0 to FTrie.GetNodeCount-1 do
    begin
      chain := FTrie.GetNodeData(i);
      while Assigned(chain) do
        begin
          if not AVisitor(chain^.colstring,chain^.data,xdata) then
            Exit(false);
          chain := chain^.next;
        end;
    end;
  Result := true;
end;

procedure THashTrie.Done;
begin
  Clear;
end;

procedure THashTrie.Clear;
var i: MachineInt;
    chain, dchain: PHashTrieCollision;
begin
  for i := 0 to FTrie.GetNodeCount-1 do
    begin
      chain := FTrie.GetNodeData(i);
      while Assigned(chain) do
        begin
          dchain := chain^.next;
          ucfs_release(chain^.colstring);
          Dispose(chain);
          Dec(FCount,1);
          chain := dchain;
        end;
    end;
  FTrie.Clear;
end;

function hashtrie_iterator_next(phtrie: PHashTrie; var iter: THashTrieIterator
  ): Boolean;
begin
  Result := true;
  if iter.nidx >= 0 then
    begin
      {set next entry}
      iter.currcoll := iter.currcoll^.next;
      if not Assigned(iter.currcoll) then
        begin
          {scan for next coll chain}
          Inc(iter.nidx,1);
          while (iter.nidx < phtrie^.FTrie.AllocatedNodes) and
                (not phtrie^.FTrie.IsDataNode(iter.nidx)) do
            Inc(iter.nidx,1);
          {if found -> set collchain, otherwise clear iterator}
          if iter.nidx < phtrie^.FTrie.AllocatedNodes then
            iter.currcoll := phtrie^.FTrie.GetNodeData(iter.nidx)
          else
            begin
              Result := false;
              iter.nidx := -1;
            end;
        end;
    end
  else
    begin
      if phtrie^.GetCount > 0 then
        begin
          {find first data index}
          iter.nidx := 0;
          // assert: Hashtrie's dont have empty entries, since count>0
          // -> no need for length check, we must hit a first entry
          while not phtrie^.FTrie.IsDataNode(iter.nidx) do
            Inc(iter.nidx,1);
          iter.currcoll := phtrie^.FTrie.GetNodeData(iter.nidx);
        end
      else
        Result := false;
    end;
end;

procedure hashtrie_iterator_reset(var iter: THashTrieIterator);
begin
  iter.nidx := -1;
end;

function hashtrie_iterator_done(const iter: THashTrieIterator): Boolean;
begin
  Result := iter.nidx < 0;
end;

{ TDataCritBitTrie }

function TDataCritBitTrie.GetCount: MachineInt;
begin
  Result := (AllocatedNodes+1) div 2;
end;

function TDataCritBitTrie.IsDataNode(i: MachineInt): Boolean;
begin
  ASSERT(i >= 0);
  ASSERT(i < AllocatedNodes);
  Result := Nodes[i].DiffBit < 0;
end;

function TDataCritBitTrie.GetNodeCount: MachineInt;
begin
  Result := AllocatedNodes;
end;

function TDataCritBitTrie.GetNodeValue( i: MachineInt ): MachineWord;
begin
  ASSERT(i >= 0);
  ASSERT(i < AllocatedNodes);
  if Nodes[i].DiffBit < 0 then
    Result := Nodes[i].Value
  else
    Result := 0;
end;

procedure TDataCritBitTrie.SetNodeData( i: MachineInt; data: Pointer );
begin
  ASSERT(i >= 0);
  ASSERT(i < AllocatedNodes);
  if Nodes[i].DiffBit < 0 then
    Nodes[i].Data := data;
end;

function TDataCritBitTrie.GetNodeData( i: MachineInt ): Pointer;
begin
  ASSERT(i >= 0);
  ASSERT(i < AllocatedNodes);
  if Nodes[i].DiffBit < 0 then
    Result := Nodes[i].Data
  else
    Result := nil;
end;

procedure TDataCritBitTrie.Init(APrealloc: MachineInt; ALinGrow: MachineInt);
begin
  if ALinGrow > 0 then
    LinGrow := ALinGrow
  else
    LinGrow := 4;
  AllocatedNodes := 0;
  SetLength(Nodes,APrealloc);
  Root := -1;
end;

procedure TDataCritBitTrie.GrowNodes(n: MachineInt);
begin
  ASSERT((n>0) and (n<=2));
  if (n+AllocatedNodes) >= Length(Nodes) then
    SetLength(Nodes,Length(Nodes)+LinGrow*2); // grows max by 2 nodes
  Inc(AllocatedNodes,n);
end;

procedure TDataCritBitTrie.Pack;
begin
  if Length(Nodes) > (AllocatedNodes+LinGrow*2) then
    SetLength(Nodes,AllocatedNodes);
end;

function TDataCritBitTrie.ForEach(AVisitor: TDataCritBitVisitorProc;
  xdata: Pointer): Boolean;
{well.. just scan the Nodes field, since we have for n leafs, n-1 internal
 nodes, this is the best way. Ordered Iteration is much more creepy.}
var i: MachineInt;
begin
  for i := 0 to AllocatedNodes-1 do
    if Nodes[i].DiffBit < 0 then
      if not AVisitor(i,xdata) then
        Exit(false);
  Result := true;
end;

procedure TDataCritBitTrie.Clear;
begin
  SetLength(Nodes,0);
  AllocatedNodes := 0;
  Root := -1;
end;

procedure TDataCritBitTrie.Done;
begin
  Clear;
end;

procedure TDataCritBitTrie.RemNode(i: MachineInt);
begin
  if (i+1) < AllocatedNodes then
    begin
      Move(Nodes[AllocatedNodes-1],Nodes[i],SizeOf(TDataCritBitNode));
      if Nodes[i].PrevNode >= 0 then
        begin
          {correct pred index}
          if Nodes[Nodes[i].PrevNode].NextNode[0] = (AllocatedNodes-1) then
            Nodes[Nodes[i].PrevNode].NextNode[0] := i
          else
            Nodes[Nodes[i].PrevNode].NextNode[1] := i;
        end
      else
        begin
          {rooted}
          Root := i;
        end;
      if Nodes[i].DiffBit >= 0 then
        begin
          {correct following node preds}
          Nodes[Nodes[i].NextNode[0]].PrevNode := i;
          Nodes[Nodes[i].NextNode[1]].PrevNode := i;
        end;
    end;
  Dec(AllocatedNodes,1);
end;

function TDataCritBitTrie.LookupNV(val: MachineWord): MachineInt;
{lookup "nearest" val -- that is the value hit, by walking the
 inner nodes Path in the trie. If val exists in the Trie, it is val, otherwise
 its the _only value_ (and those path) which needs another crit bit in the
 path such that val and nearest val can be looked up (obviously also the other
 vals)}
begin
  Result := Root;
  if Result >= 0 then
    while Nodes[Result].DiffBit >= 0 do
      Result := Nodes[Result].NextNode[ (val shr MachineWord(Nodes[Result].DiffBit)) and 1 ];
end;

function TDataCritBitTrie.Lookup(val: MachineWord): MachineInt;
{check if nearest found val is the search value}
begin
  Result := LookupNV(val);
  if (Result < 0) or
     (Nodes[Result].Value <> val) then
    Result := -1;
end;

function TDataCritBitTrie.Add(val: MachineWord): MachineInt;
var nprev,nnext,nnew: MachineInt;
    nval, cbit: MachineWord;
begin
  Result := LookupNV(val);
  if (Result < 0) or
     (Nodes[Result].Value <> val) then
    begin
      {add it}
      if Result >= 0 then
        begin
          {harder case, need to add a new val. so we need to add the critical
           path decision (ciritcal) bit, which is the most significant
           difference. Or in Integer Logics: highest bit set in newval XOR val }
          nval := val xor Nodes[Result].Value;
          // be aware that nval cannot be 0, so there is always a crit bit.
          cbit := MachineWord(bitscan_from_msb(nval));
          nnext := Root;
          // now, after we have a critical bit, we need an insertion point
          // lookup again, but now check against the critbit position

          // this uses one assumptions: there is no Entrie on this path where
          // Node[path index on this path].DiffBit = cbit, otherwise
          // LookupNV would have walked another path on this CritBit and
          // resulting value would be different. so we can search for the
          // lowest DiffBit > cbit, which will be the Pred. of this entry.
          while (Nodes[nnext].DiffBit >= 0) and
                (MachineWord(Nodes[nnext].DiffBit) > cbit) do
            nnext := Nodes[nnext].NextNode[ (val shr MachineWord(Nodes[nnext].DiffBit)) and 1 ];
          {create a new diff node and a new element node}
          GrowNodes(2);
          Result := AllocatedNodes-1;
          nnew := AllocatedNodes-2;
          Nodes[Result].DiffBit := -1;
          Nodes[Result].PrevNode := nnew;
          Nodes[Result].Value := val;
          Nodes[Result].Data := nil;
          {add the new diff node}
          Nodes[nnew].DiffBit := cbit;
          {put new between nprev and nnext}
          // correct backlinks
          nprev := Nodes[nnext].PrevNode;
          Nodes[nnext].PrevNode := nnew;
          Nodes[nnew].PrevNode := nprev;
          // correct nprev or root
          if nprev >= 0 then
            begin
              // correct forward links nprev->nnew
              if Nodes[nprev].NextNode[0] = nnext then
                Nodes[nprev].NextNode[0] := nnew
              else
                Nodes[nprev].NextNode[1] := nnew;
            end
          else
            Root := nnew;
          Nodes[nnew].NextNode[ (val shr cbit) and 1 ] := Result;     // new path for val
          Nodes[nnew].NextNode[ (not(val) shr cbit) and 1 ] := nnext; // old path, that val doesnt belong to
        end
      else
        begin
          {simple case, add a root entry}
          GrowNodes(1);
          Result := AllocatedNodes-1;
          Nodes[Result].DiffBit := -1;
          Nodes[Result].Value := val;
          Nodes[Result].Data := nil;
          Nodes[Result].PrevNode := -1;
          Root := Result;
        end;
    end;
end;

procedure TDataCritBitTrie.Delete(val: MachineWord);
var idx,idx2,prev,prev2: MachineInt;
begin
  idx := Lookup(val);
  if idx >= 0 then
    begin
      prev := Nodes[idx].PrevNode;
      if prev >= 0 then
        begin
          if Nodes[prev].NextNode[0] = idx then
            idx2 := Nodes[prev].NextNode[1]
          else
            idx2 := Nodes[prev].NextNode[0];
          {prev will be deleted}
          prev2 := Nodes[prev].PrevNode;
          if prev2 >= 0 then
            begin
              {correct index}
              if Nodes[prev2].NextNode[0] = prev then
                Nodes[prev2].NextNode[0] := idx2
              else
                Nodes[prev2].NextNode[1] := idx2;
              Nodes[idx2].PrevNode := prev2;
            end
          else
            begin
              {rooted -> set other entry as root}
              Root := idx2;
              Nodes[idx2].PrevNode := -1;
            end;
          {remove/defrag prev}
          RemNode(prev);
        end
      else
        begin
          {rooted -> clear root entry}
          Root := -1;
        end;
      {remove/defrag leaf}
      RemNode(idx);
    end;
  if Length(Nodes) > (AllocatedNodes+LinGrow*4) then
    Pack;
end;


{ TDataBucket }

procedure TDataBucket.Init(AAndNil: Boolean; ALen: MachineInt);
begin
  ASSERT(ALen >= 0);
  SetLength(Data,ALen);
  if AAndNil and (ALen > 0) then
    FillByte(Data[0],SizeOf(Pointer)*ALen,0);
end;

procedure TDataBucket.Grow(AAndNil: Boolean; n: MachineInt);
var oldl: MachineInt;
begin
  ASSERT(n >= 0);
  if n > 0 then
    begin
      oldl := Length(Data);
      SetLength(Data,oldl+n);
      if AAndNil then
        FillByte(Data[oldl],SizeOf(Pointer)*n,0);
    end;
end;

procedure TDataBucket.Shrink(n: MachineInt);
begin
  ASSERT(n>=0);
  ASSERT(n<=Length(Data));
  SetLength(Data,Length(Data)-n);
end;

procedure TDataBucket.Swap(AIdx1, AIdx2: MachineInt);
var tmp: Pointer;
begin
  ASSERT(AIdx1 >= 0);
  ASSERT(AIdx1 < Length(Data));
  ASSERT(AIdx2 >= 0);
  ASSERT(AIdx2 < Length(Data));
  if AIdx1 <> AIdx2 then
    begin
      tmp := Data[AIdx1];
      Data[AIdx1] := Data[AIdx2];
      Data[AIdx2] := tmp;
    end;
end;

procedure TDataBucket.DeleteShift(AAndNil: Boolean; AIdx: MachineInt;
  ANrElems: MachineInt);
begin
  ASSERT(AIdx >= 0);
  ASSERT(ANrElems > 0);
  ASSERT((AIdx+ANrElems) <= Length(Data));
  if (AIdx+ANrElems) < Length(Data) then
    begin
      if (AIdx+ANrElems) <= (Length(Data)-ANrElems) then
        {shiftdown everything above, by ANrElems}
        ShiftDown(AIdx+ANrElems,ANrElems)
      else
        {no ANrElems above (but at least 1), move whats left}
        ShiftDown(AIdx+ANrElems,Length(Data)-(AIdx+ANrElems));
    end;
  if AAndNil then
    FillByte(Data[Length(Data)-ANrElems],SizeOf(Pointer)*ANrElems,0);
end;

procedure TDataBucket.DeleteSwap(AAndNil: Boolean; AIdx: MachineInt;
  ANrElems: MachineInt);
begin
  ASSERT(AIdx >= 0);
  ASSERT(ANrElems > 0);
  ASSERT((AIdx+ANrElems) <= Length(Data));
  if (AIdx+ANrElems) < Length(Data) then
    begin
      if (AIdx+ANrElems) <= (Length(Data)-ANrElems) then
        {simply move the top ANrElems down}
        Move(Data[Length(Data)-ANrElems],Data[AIdx],SizeOf(Pointer)*ANrElems)
      else
        {no ANrElems above (but at least 1), move whats left}
        ShiftDown(AIdx+ANrElems,Length(Data)-(AIdx+ANrElems));
    end;
  if AAndNil then
    FillByte(Data[Length(Data)-ANrElems],SizeOf(Pointer)*ANrElems,0);
end;

procedure TDataBucket.MoveBlockDown(AFromIdx, ANrElems, ABlockSize: MachineInt
  );
begin
  ASSERT((AFromIdx-ANrElems) > 0);
  ASSERT((AFromIdx+ABlockSize) <= Length(Data));
  ASSERT(ANrElems > 0);
  ASSERT(ABlockSize > 0);
  Move(Data[AFromIdx],Data[AFromIdx-ANrElems],SizeOf(Pointer)*ABlockSize);
end;

procedure TDataBucket.MoveBlockUp(AFromIdx, ANrElems, ABlockSize: MachineInt
  );
begin
  ASSERT(AFromIdx >= 0);
  ASSERT((AFromIdx+ABlockSize+ANrElems) <= Length(Data));
  ASSERT(ANrElems > 0);
  ASSERT(ABlockSize > 0);
  Move(Data[AFromIdx],Data[AFromIdx+ANrElems],SizeOf(Pointer)*ABlockSize);
end;

procedure TDataBucket.ShiftDown(AFromIdx, ANrElems: MachineInt);
begin
  MoveBlockDown(AFromIdx,ANrElems,Length(Data)-AFromIdx);
end;

procedure TDataBucket.ShiftUp(AFromIdx, ANrElems: MachineInt);
begin
  MoveBlockUp(AFromIdx,ANrElems,Length(Data)-(AFromIdx+ANrElems));
end;

function TDataBucket.ForEach(AVisitor: TDataVisitorProc; xdata: Pointer
  ): Boolean;
begin
  Result := ForIn(Low(Data),High(Data),AVisitor,xdata);
end;

function TDataBucket.ForIn(AFromIdx, AToIdx: MachineInt;
  AVisitor: TDataVisitorProc; xdata: Pointer): Boolean;
var i: MachineInt;
begin
  ASSERT(AFromIdx >= 0);
  ASSERT(AFromIdx < Length(Data));
  ASSERT(AToIdx >= 0);
  ASSERT(AToIdx < Length(Data));
  for i := AFromIdx to AToIdx do
    if not AVisitor( Data[i], xdata ) then
      Exit(false);
  Result := true;
end;

end.

