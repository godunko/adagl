------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2018, Vadim Godunko <vgodunko@gmail.com>                     --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Vadim Godunko, IE nor the names of its        --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams.Stream_IO;
separate (OpenGL.Textures)
procedure Load_TGA
 (Name : League.Strings.Universal_String;
  Data : out Image_Data)
is
   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;

   function Get_Natural
     (Raw : Ada.Streams.Stream_Element_Array) return OpenGL.GLsizei;
   --  Read Integer from Raw. Raw'Length should be 2

   procedure Swap (Left, Right : in out Ada.Streams.Stream_Element);

   -----------------
   -- Get_Natural --
   -----------------

   function Get_Natural
     (Raw : Ada.Streams.Stream_Element_Array) return OpenGL.GLsizei
   is
      use type OpenGL.GLsizei;
      Low  : Ada.Streams.Stream_Element renames Raw (Raw'First);
      High : Ada.Streams.Stream_Element renames Raw (Raw'Last);
   begin
      return OpenGL.GLsizei (High) * 256 + OpenGL.GLsizei (Low);
   end Get_Natural;

   ----------
   -- Swap --
   ----------

   procedure Swap (Left, Right : in out Ada.Streams.Stream_Element) is
      Save : constant Ada.Streams.Stream_Element := Left;
   begin
      Left := Right;
      Right := Save;
   end Swap;

   Input  : Ada.Streams.Stream_IO.File_Type;
   Header : Ada.Streams.Stream_Element_Array (1 .. 18);
   Last   : Ada.Streams.Stream_Element_Offset;
   Id_Length  : Ada.Streams.Stream_Element renames Header (1);
   Map_Kind   : Ada.Streams.Stream_Element renames Header (2);
   Image_Kind : Ada.Streams.Stream_Element renames Header (3);
   Depth      : Ada.Streams.Stream_Element renames Header (17);
   Flags      : Ada.Streams.Stream_Element renames Header (18);
   Length     : Ada.Streams.Stream_Element_Offset;
   Step       : Ada.Streams.Stream_Element_Offset;
begin
   Ada.Streams.Stream_IO.Open
     (Input, Ada.Streams.Stream_IO.In_File, Name.To_UTF_8_String);

   Ada.Streams.Stream_IO.Read (Input, Header, Last);
   pragma Assert (Last = Header'Last);
   pragma Assert (Map_Kind = 0, "Image with map isn't supported");
   pragma Assert (Image_Kind = 2, "Only uncompressed TrueColor is supported");
   pragma Assert (Depth in 24 | 32, "Only TrueColor image is supported");

   Data.Width := Get_Natural (Header (13 .. 14));
   Data.Height := Get_Natural (Header (15 .. 16));
   Data.Data_Type := OpenGL.GL_UNSIGNED_BYTE;

   if Depth = 24 then
      Data.Format := OpenGL.GL_RGB;
   else
      Data.Format := OpenGL.GL_RGBA;
   end if;

   Step   := Ada.Streams.Stream_Element_Offset (Depth) / 8;
   Length := Ada.Streams.Stream_Element_Offset (Data.Width)
     * Ada.Streams.Stream_Element_Offset (Data.Height)
     * Step;

   declare
      Skip : Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (Id_Length));
   begin
      Ada.Streams.Stream_IO.Read (Input, Skip, Last);
      pragma Assert (Last = Skip'Last);
   end;

   declare
      Raw   : Ada.Streams.Stream_Element_Array (1 .. Length);
      Index : Ada.Streams.Stream_Element_Offset := 1;
   begin
      Ada.Streams.Stream_IO.Read (Input, Raw, Last);
      pragma Assert (Last = Raw'Last);

      for X in 1 .. Data.Height loop
         for Y in 1 .. Data.Height loop
            --  Turn BGR into RGB:
            Swap (Raw (Index), Raw (Index + 2));
            Index := Index + Step;
         end loop;
      end loop;

      Data.Raw.Clear;
      Data.Raw.Append (Raw);
   end;

   Ada.Streams.Stream_IO.Close (Input);
end Load_TGA;
