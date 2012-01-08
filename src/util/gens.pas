{   Unit with generics for common problems.

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

unit gens;

{$mode objfpc}{$H+}

interface

uses SysUtils, commontl;

{$WARNING add Inc which allows to use without PUSH (note that using LAST is bad with records)}

type
  generic TSimpleList<GSLType> = class
    public
      Items: array of GSLType;
      {dont doe stuff like Items[Push(..)].. as it is flawed (Stack addr taken,
       before Push's setlength call -> boom)}
      function Push( const value: GSLType ): MachineInt;
      {wont check stacklength}
      procedure Pop;
      {wont check stacklength}
      function Last: GSLType;
      function IsEmpty: Boolean;
      function Count: MachineInt;

      constructor Create;
      destructor Destroy; override;
  end;

  generic TLinearAllocList<RMLType> = class
    private
      FSlots: MachineInt;
      FGrowBy: MachineInt;
    public
      Items: array of RMLType;
      function Push( const value: RMLType ): MachineInt;
      procedure Pop;
      procedure PopCnt( nr: MachineInt );
      function Last: RMLType;
      function IsEmpty: Boolean;
      function Count: MachineInt;

      constructor Create( AGrowVal: MachineInt );
      destructor Destroy; override;
  end;

  generic TQuadAllocList<RMLType> = class
    private
      FSlots: MachineInt;
    public
      Items: array of RMLType;
      function Push( const value: RMLType ): MachineInt;
      procedure Pop;
      procedure PopCnt( nr: MachineInt );
      function Last: RMLType;
      function IsEmpty: Boolean;
      function Count: MachineInt;

      constructor Create( ABasePower: MachineInt );
      destructor Destroy; override;
  end;

implementation

{ TQuadAllocList }

function TQuadAllocList.Push(const value: RMLType): MachineInt;
{quad grow}
begin
  Result := FSlots;
  Inc(FSlots,1);
  if FSlots > Length(Items) then
    SetLength(Items,Length(Items)+Length(Items));
  Items[Result] := value;
end;

procedure TQuadAllocList.Pop;
{quad shrink}
begin
  if FSlots > 0 then
    begin
      Dec(FSlots,1);
      if FSlots < (Length(Items) div 4) then
        SetLength(Items,Length(Items) div 2);
    end;
end;

procedure TQuadAllocList.PopCnt(nr: MachineInt);
{quad shrink nr, no quad fix}
begin
  if nr > 0 then
    begin
      if (FSlots-nr) >= 0 then
        begin
          Dec(FSlots,nr);
          if FSlots < (Length(Items) div 4) then
            SetLength(Items,Length(Items) div 2);
        end
      else
        begin
          FSlots := 0;
          SetLength(Items,1 shl 4);
        end;
    end;
end;

function TQuadAllocList.Last: RMLType;
begin
  Result := Items[FSlots-1];
end;

function TQuadAllocList.IsEmpty: Boolean;
begin
  Result := FSlots <= 0;
end;

function TQuadAllocList.Count: MachineInt;
begin
  Result := FSlots;
end;

constructor TQuadAllocList.Create(ABasePower: MachineInt);
begin
  if ABasePower > 0 then
    begin
      if ABasePower > 16 then
        ABasePower := 16;
    end
  else
    ABasePower := 4;
  SetLength(Items,1 shl (ABasePower-1));
  FSlots := 0;
end;

destructor TQuadAllocList.Destroy;
begin
  SetLength(Items,0);
end;

{ TLinearAllocList }

function TLinearAllocList.Push(const value: RMLType): MachineInt;
{grow linear}
begin
  Result := FSlots;
  Inc(FSlots,1);
  if FSlots > Length(Items) then
    SetLength(Items,Length(Items)+FGrowBy);
  Items[Result] := value;
end;

procedure TLinearAllocList.Pop;
{shrink linear, but leave at least growby slots}
begin
  if FSlots > 0 then
    begin
      Dec(FSlots,1);
      if (FSlots > FGrowBy) and
         (FSlots < (Length(Items) - (2*FGrowBy))) then
        SetLength(Items,Length(Items)-FGrowBy);
    end;
end;

procedure TLinearAllocList.PopCnt(nr: MachineInt);
{shrink by nr, but leave at least growby slots}
begin
  if nr > 0 then
    begin
      if (FSlots-nr) >= 0 then
        begin
          Dec(FSlots,nr);
          if (FSlots > FGrowBy) and
             (((Length(Items) - FSlots) div FGrowBy) >= 2) then
            SetLength(Items,Length(Items)-(FGrowBy*((Length(Items) - FSlots) div FGrowBy)));
        end
      else
        begin
          FSlots := 0;
          SetLength(Items,FGrowBy);
        end;
    end;
end;

function TLinearAllocList.Last: RMLType;
begin
  Result := Items[FSlots-1];
end;

function TLinearAllocList.IsEmpty: Boolean;
begin
  Result := FSlots <= 0;
end;

function TLinearAllocList.Count: MachineInt;
begin
  Result := FSlots;
end;

constructor TLinearAllocList.Create(AGrowVal: MachineInt);
begin
  if AGrowVal > 0 then
    FGrowBy := AGrowVal
  else
    FGrowBy := CL_LinearRegrow_Slow;
  SetLength(Items,FGrowBy);
  FSlots := 0;
end;

destructor TLinearAllocList.Destroy;
begin
  SetLength(Items,0);
end;

{ TSimpleStack }

function TSimpleList.Push( const value: GSLType): MachineInt;
begin
  Result := Length(Items);
  SetLength(Items,Result+1);
  Items[Result] := value;
end;

procedure TSimpleList.Pop;
begin
  SetLength(Items,Length(Items)-1);
end;

function TSimpleList.Last: GSLType;
begin
  Result := Items[High(Items)];
end;

function TSimpleList.IsEmpty: Boolean;
begin
  Result := Length(Items) <= 0;
end;

function TSimpleList.Count: MachineInt;
begin
  Result := Length(Items);
end;

constructor TSimpleList.Create;
begin
  SetLength(Items,0);
end;

destructor TSimpleList.Destroy;
begin
  SetLength(Items,0);
end;

end.

